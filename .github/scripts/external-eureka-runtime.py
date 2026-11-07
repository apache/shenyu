#!/usr/bin/env python3
# Licensed to the Apache Software Foundation (ASF) under one or more
# contributor license agreements.  See the NOTICE file distributed with
# this work for additional information regarding copyright ownership.
# The ASF licenses this file to You under the Apache License, Version 2.0
# (the "License"); you may not use this file except in compliance with
# the License.  You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""Build and install the pinned external provider for Spring Cloud CI fixtures."""

import argparse
import fnmatch
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import xml.etree.ElementTree as ET
import zipfile

RECEIVER_COMMIT = "be07c121085f219f46578515156e7a0bb20befdf"
MODULE = "shenyu-extension/shenyu-extension-eureka/shenyu-registry-eureka"
SPI = "META-INF/shenyu/org.apache.shenyu.registry.api.ShenyuInstanceRegisterRepository"
PROVIDER = "org.apache.shenyu.registry.eureka.EurekaInstanceRegisterRepository"
NS = {"m": "http://maven.apache.org/POM/4.0.0"}


def capture(command, cwd):
    return subprocess.check_output(command, cwd=cwd, text=True).strip()


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def classpath(path):
    return [Path(item).resolve() for item in path.read_text().strip().split(os.pathsep) if item]


def coordinate(path, repository):
    parts = path.relative_to(repository).parts
    if len(parts) < 4:
        raise ValueError("Not a Maven repository artifact: " + str(path))
    group, artifact, version = ".".join(parts[:-3]), parts[-3], parts[-2]
    prefix = artifact + "-" + version
    if not path.name.startswith(prefix) or path.suffix != ".jar":
        raise ValueError("Unexpected Maven artifact filename: " + str(path))
    classifier = path.name[len(prefix):-4]
    return group, artifact, classifier


def select_dependencies(paths, admin_paths, repository, shaded):
    # Keep the version already supplied by Admin; do not introduce a second core
    # API or transport implementation into the container's extension classpath.
    owned = {coordinate(path, repository): path for path in admin_paths}
    selected = []
    reused = []
    seen = set()
    for path in paths:
        key = coordinate(path, repository)
        ga = key[0] + ":" + key[1]
        if key in owned:
            reused.append({"artifact": ga, "requested_version": path.parent.name,
                           "admin_version": owned[key].parent.name})
            continue
        if key[0] == "org.apache.shenyu" or any(fnmatch.fnmatchcase(ga, pattern) for pattern in shaded):
            continue
        if key in seen:
            raise ValueError("Duplicate external dependency: " + ga)
        seen.add(key)
        selected.append(path)
    return selected, reused


def verify_provider(path):
    with zipfile.ZipFile(path) as archive:
        entries = [line.strip() for line in archive.read(SPI).decode().splitlines()
                   if line.strip() and not line.lstrip().startswith("#")]
        if entries != ["eureka=" + PROVIDER]:
            raise ValueError("The external JAR does not register the expected Eureka SPI")
        if PROVIDER.replace(".", "/") + ".class" not in archive.namelist():
            raise ValueError("The external JAR is missing the provider class")
        if "org/apache/shenyu/registry/api/ShenyuInstanceRegisterRepository.class" in archive.namelist():
            raise ValueError("The external JAR must not bundle the retained registry API")


def prepare(core, store, output):
    if output.exists():
        raise ValueError("Use a new output directory: " + str(output))
    if capture(["git", "rev-parse", "HEAD"], store) != RECEIVER_COMMIT:
        raise ValueError("The receiver checkout must match the pinned commit")
    if capture(["git", "status", "--porcelain", "--untracked-files=no"], store):
        raise ValueError("The receiver checkout has tracked modifications")
    version = ET.parse(core / "pom.xml").findtext("m:version", namespaces=NS)
    wrapper = str(core / "mvnw")
    common = ["-B", "-ntp", "-Dmaven.javadoc.skip=true", "-Djacoco.skip=true", "-DskipRemoteResources=true"]
    subprocess.run([wrapper, *common, "-f", str(store / "pom.xml"), "-pl", MODULE, "-am",
                    "clean", "verify", "dependency:build-classpath", "-Dshenyu.version=" + version,
                    "-Dmdep.outputFile=target/eureka-runtime-classpath.txt", "-DincludeScope=runtime"], cwd=core, check=True)
    subprocess.run([wrapper, *common, "-f", str(core / "shenyu-admin/pom.xml"),
                    "dependency:build-classpath", "-Dmdep.outputFile=target/eureka-runtime-classpath.txt",
                    "-DincludeScope=runtime"], cwd=core, check=True)
    # The wrapper can print a JAVA_HOME warning before Maven's evaluated value.
    repository_value = re.sub(r"\x1b\[[0-9;]*m", "", capture(
        [wrapper, "-q", "-N", "help:evaluate", "-Dexpression=settings.localRepository", "-DforceStdout"], core))
    repository = Path(repository_value.splitlines()[-1]).resolve()
    module = store / MODULE
    pom = ET.parse(module / "pom.xml")
    shaded = [item.text for item in pom.findall(".//m:artifactSet/m:includes/m:include", NS)]
    jars = [path for path in (module / "target").glob("shenyu-registry-eureka-*.jar")
            if not path.name.endswith(("-sources.jar", "-javadoc.jar"))]
    if len(jars) != 1:
        raise ValueError("Expected one packaged external provider")
    provider = jars[0]
    verify_provider(provider)
    admin_paths = classpath(core / "shenyu-admin/target/eureka-runtime-classpath.txt")
    dependencies, reused = select_dependencies(classpath(module / "target/eureka-runtime-classpath.txt"),
                                               admin_paths, repository, shaded)
    output.mkdir(parents=True)
    output.chmod(0o755)
    records = []
    for path in [provider, *dependencies]:
        destination = output / path.name
        if destination.exists():
            raise ValueError("External dependency filename collision: " + path.name)
        shutil.copyfile(path, destination)
        destination.chmod(0o644)
        records.append({"file": path.name, "sha256": digest(destination)})
    # Unit tests run before shading. Exercise the packaged, reduced classpath as
    # well, including discovery over a real loopback HTTP connection.
    java = str(Path(os.environ["JAVA_HOME"]) / "bin/java") if os.environ.get("JAVA_HOME") else "java"
    cp = os.pathsep.join(str(path) for path in [*admin_paths, output / provider.name,
                                               *(output / path.name for path in dependencies)])
    subprocess.run([java, "-cp", cp, str(Path(__file__).with_name("ExternalEurekaProbe.java")),
                    str(output / provider.name)], check=True, timeout=45)
    manifest = {"receiver_commit": RECEIVER_COMMIT, "core_commit": capture(["git", "rev-parse", "HEAD"], core),
                "core_version": version, "provider": provider.name, "files": records,
                "reused_admin_dependencies": reused, "packaged_spi_http_probe": "passed"}
    (output / "manifest.json").write_text(json.dumps(manifest, indent=2) + "\n")
    print("Prepared external Eureka runtime: " + str(output), flush=True)


def install(source, destination, core_commit, core_version):
    manifest = json.loads((source / "manifest.json").read_text())
    if manifest["receiver_commit"] != RECEIVER_COMMIT:
        raise ValueError("External runtime uses an unexpected receiver commit")
    if manifest["core_commit"] != core_commit or manifest["core_version"] != core_version:
        raise ValueError("External runtime was prepared for a different core checkout")
    if manifest.get("packaged_spi_http_probe") != "passed":
        raise ValueError("External runtime has no passing packaged SPI/HTTP probe")
    files = manifest["files"]
    names = [item["file"] for item in files]
    if len(names) != len(set(names)) or any(Path(name).name != name or not name.endswith(".jar") for name in names):
        raise ValueError("Invalid external runtime file inventory")
    if set(path.name for path in source.glob("*.jar")) != set(names) or manifest["provider"] not in names:
        raise ValueError("External runtime inventory is incomplete")
    for item in files:
        path = source / item["file"]
        if path.is_symlink() or digest(path) != item["sha256"]:
            raise ValueError("External runtime checksum mismatch: " + item["file"])
        existing = destination / item["file"]
        if existing.exists() and digest(existing) != item["sha256"]:
            raise ValueError("Refusing to replace a different runtime dependency: " + item["file"])
    verify_provider(source / manifest["provider"])
    destination.mkdir(parents=True, exist_ok=True)
    destination.chmod(0o755)
    for item in files:
        target = destination / item["file"]
        shutil.copyfile(source / item["file"], target)
        target.chmod(0o644)
    print("Installed external Eureka runtime into " + str(destination), flush=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    build = commands.add_parser("prepare")
    build.add_argument("store", type=Path)
    build.add_argument("output", type=Path)
    copy = commands.add_parser("install")
    copy.add_argument("source", type=Path)
    copy.add_argument("destination", type=Path)
    args = parser.parse_args()
    if args.command == "prepare":
        prepare(Path(__file__).resolve().parents[2], args.store.resolve(), args.output.resolve())
    else:
        core = Path(__file__).resolve().parents[2]
        install(args.source.resolve(), args.destination.resolve(), capture(["git", "rev-parse", "HEAD"], core),
                ET.parse(core / "pom.xml").findtext("m:version", namespaces=NS))


if __name__ == "__main__":
    main()
