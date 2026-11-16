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

"""Check the actual Maven archives, not merely the source legal files."""

import argparse
import hashlib
import json
from pathlib import Path, PurePosixPath
import re
import xml.etree.ElementTree as ET
import zipfile


SHADED_ARTIFACTS = (
    "shenyu-admin-listener-apollo",
    "shenyu-registry-apollo",
    "shenyu-spring-boot-starter-sync-data-apollo",
)


def check_jar(path, notice):
    with zipfile.ZipFile(path) as jar:
        names = jar.namelist()
        for name in ("META-INF/LICENSE", "META-INF/NOTICE"):
            if names.count(name) != 1:
                raise ValueError(f"{path}: expected exactly one {name}")
        license_text = jar.read("META-INF/LICENSE").decode("utf-8")
        notice_text = jar.read("META-INF/NOTICE").decode("utf-8")
        if not all(term in license_text for term in ("Apache License", "Version 2.0", "TERMS AND CONDITIONS")):
            raise ValueError(f"{path}: missing project Apache 2.0 license")
        if notice.strip() not in notice_text:
            raise ValueError(f"{path}: missing project NOTICE attribution")
        if "static/index.html" in names:
            for name in ("META-INF/licenses/LICENSE-dashboard.txt", "META-INF/licenses/NOTICE-dashboard.txt"):
                if name not in names or not jar.read(name).strip():
                    raise ValueError(f"{path}: bundled dashboard is missing {name}")
        if any(path.name.startswith(artifact + "-") for artifact in SHADED_ARTIFACTS) and not path.name.endswith(("-sources.jar", "-javadoc.jar")):
            for name in ("META-INF/LICENSE-shaded-dependencies.txt", "META-INF/NOTICE-shaded-dependencies.txt"):
                if name not in names:
                    raise ValueError(f"{path}: missing shaded dependency terms: {name}")
        return {"archive": path.name, "project_license": True, "project_notice": True,
                "sha512": hashlib.sha512(path.read_bytes()).hexdigest()}


def check_source(path, root):
    with zipfile.ZipFile(path) as archive:
        names = archive.namelist()
        prefix = names[0].split("/")[0] + "/"
        for relative in ("LICENSE", "NOTICE", "RELEASE-NOTES.md"):
            if archive.read(prefix + relative) != (root / relative).read_bytes():
                raise ValueError(f"{path}: source {relative} differs from the release commit")
        static = "shenyu-admin/src/main/resources/static/"
        html = archive.read(prefix + static + "index.html").decode("utf-8")
        references = re.findall(r'(?:src|href)=[\"\']([^\"\']+\.(?:js|css))(?:\?[^\"\']*)?[\"\']', html)
        if not references:
            raise ValueError(f"{path}: no dashboard JS/CSS references")
        for reference in references:
            if "://" in reference or reference.startswith("//"):
                raise ValueError(f"{path}: unexpected remote dashboard asset: {reference}")
            relative = static + reference.removeprefix("./").lstrip("/")
            if archive.read(prefix + relative) != (root / relative).read_bytes():
                raise ValueError(f"{path}: dashboard asset differs: {reference}")
        for name in names:
            relative = name.removeprefix(prefix)
            parts = PurePosixPath(relative).parts
            if "target" in parts or ".git" in parts or relative.startswith("actions/") or relative.endswith((".jar", ".class", ".war")):
                raise ValueError(f"{path}: unintended source archive entry: {relative}")
        return {"archive": path.name, "project_legal_files": True, "dashboard_assets": references}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repository", type=Path, required=True)
    parser.add_argument("--root", type=Path, default=Path.cwd())
    args = parser.parse_args()
    root = args.root
    pom = ET.parse(root / "pom.xml").getroot()
    version = pom.findtext("{http://maven.apache.org/POM/4.0.0}version")
    repository = args.repository
    notice = (root / "NOTICE").read_text()
    jars = sorted(repository.glob(f"*/{version}/*.jar"))
    if not jars:
        raise ValueError(f"No candidate JARs in {repository}")
    results = [check_jar(path, notice) for path in jars]
    for path in jars:
        if not path.name.endswith(("-sources.jar", "-javadoc.jar")):
            for classifier in ("sources", "javadoc"):
                if not path.with_name(path.stem + f"-{classifier}.jar").exists():
                    raise ValueError(f"{path}: missing {classifier} attachment")
    sources = list(repository.glob(f"*/{version}/*-src.zip")) + list(repository.glob(f"shenyu/{version}/*-source-release.zip"))
    if len(sources) != 2:
        raise ValueError(f"Expected both source archives, found {len(sources)}")
    report = {"version": version, "jars": results, "sources": [check_source(p, root) for p in sources]}
    output = root / "target/release-audit"
    output.mkdir(parents=True, exist_ok=True)
    (output / "project-artifacts.json").write_text(json.dumps(report, indent=2) + "\n")
    print(f"Verified project LICENSE/NOTICE in all {len(jars)} Maven JARs and dashboard resources in both source archives")


if __name__ == "__main__":
    main()
