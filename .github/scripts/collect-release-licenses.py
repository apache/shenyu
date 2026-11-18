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

"""Collect legal resources from the exact assembled distribution JARs."""

import argparse
import hashlib
import io
import json
from pathlib import Path, PurePosixPath
import re
import tarfile
import xml.etree.ElementTree as ET
import zipfile


LEGAL_NAME = re.compile(r"^(license|notice|copyright|copying)(?:[._-].*)?$", re.I)
TEXT_SUFFIXES = {"", ".txt", ".text", ".md", ".html", ".htm", ".rst", ".adoc"}
# Version-specific regressions: names alone cannot establish the applicable license.
LICENSE_DECLARATIONS = {
    "jersey-client-3.1.11.jar": ("jersey-client 3.1.11", "EPL 2.0"),
    "jersey-common-3.1.11.jar": ("jersey-common 3.1.11", "EPL 2.0"),
    "jersey-hk2-3.1.11.jar": ("jersey-hk2 3.1.11", "EPL 2.0"),
    "jersey-apache-connector-3.1.11.jar": ("jersey-apache-connector 3.1.11", "EPL 2.0"),
    "aspectjweaver-1.9.25.1.jar": ("aspectjweaver 1.9.25.1", "EPL-2.0 AND BSD-3-Clause AND Apache-1.1"),
    "osgi-resource-locator-1.0.3.jar": ("osgi-resource-locator 1.0.3", "EPL 2.0 OR GPL 2.0 with Classpath Exception"),
    "jakarta.annotation-api-2.1.1.jar": ("jakarta.annotation-api 2.1.1", "EPL 2.0 OR GPL 2.0 with Classpath Exception"),
    "lettuce-core-6.6.0.RELEASE.jar": ("lettuce-core 6.6.0.RELEASE", "MIT"),
    "jcl-over-slf4j-2.0.17.jar": ("jcl-over-slf4j 2.0.17", "Apache 2.0"),
}


def collect(archive):
    jars = []
    texts = {}
    documents = []
    index = ""
    with tarfile.open(archive, "r:gz") as distribution:
        for member in distribution:
            path = PurePosixPath(member.name)
            if member.isfile() and ((len(path.parts) == 2 and path.name in {"LICENSE", "NOTICE"})
                                    or (len(path.parts) > 2 and path.parts[1] == "licenses")):
                with distribution.extractfile(member) as document:
                    text = document.read().decode("utf-8-sig", errors="replace").replace("\r\n", "\n")
                    documents.append(text)
                    if len(path.parts) == 2 and path.name == "LICENSE":
                        index = text
            if not member.isfile() or path.parent.name != "lib" or path.suffix != ".jar":
                continue
            if path.name.startswith("shenyu-"):
                continue
            resources = []
            coordinates = []
            pom_licenses = []
            with distribution.extractfile(member) as jar_stream:
                with zipfile.ZipFile(io.BytesIO(jar_stream.read())) as jar:
                    for entry in sorted(jar.namelist()):
                        resource = PurePosixPath(entry)
                        if entry.startswith("META-INF/maven/") and entry.endswith("/pom.properties"):
                            properties = {}
                            for line in jar.read(entry).decode("utf-8").splitlines():
                                key, separator, value = line.partition("=")
                                if separator and key in {"groupId", "artifactId", "version"}:
                                    properties[key] = value.strip()
                            if len(properties) == 3:
                                coordinates.append(properties)
                        if entry.startswith("META-INF/maven/") and entry.endswith("/pom.xml"):
                            pom = ET.fromstring(jar.read(entry))
                            for license_element in pom.findall("{*}licenses/{*}license"):
                                pom_licenses.append({"pom": entry, "name": license_element.findtext("{*}name"),
                                                     "url": license_element.findtext("{*}url")})
                        legal_path = LEGAL_NAME.fullmatch(resource.name) or re.search(
                            r"\.(?:license|notice)(?:\.|$)", resource.name, re.I) or any(
                            part.lower() in {"license", "licenses"} for part in resource.parts[:-1])
                        if not legal_path or resource.suffix.lower() not in TEXT_SUFFIXES:
                            continue
                        # Legal resources are read in place; archive paths are never extracted.
                        data = jar.read(entry)
                        try:
                            text = data.decode("utf-8-sig")
                        except UnicodeDecodeError:
                            text = data.decode("latin-1")
                        text = text.replace("\r\n", "\n").strip()
                        if not text:
                            continue
                        digest = hashlib.sha256(text.encode("utf-8")).hexdigest()
                        texts[digest] = text
                        resources.append({
                            "path": entry,
                            "kind": "notice" if re.search(r"(^|[._-])notice([._-]|$)", resource.name, re.I) else "license",
                            "sha256": digest,
                        })
            jars.append({"name": path.name, "coordinates": coordinates, "resources": resources,
                         "embedded_pom_licenses": pom_licenses})
    if not jars:
        raise ValueError(f"No third-party library JARs found in {archive}")
    legal_documents = "\n".join(documents)
    missing = [jar["name"] + " / " + resource["path"] for jar in jars for resource in jar["resources"]
               if texts[resource["sha256"]] not in legal_documents]
    for jar in jars:
        if jar["name"] not in LICENSE_DECLARATIONS:
            continue
        component, expected = LICENSE_DECLARATIONS[jar["name"]]
        declarations = [line for line in index.splitlines() if line.strip().startswith(component + ":")]
        if len(declarations) != 1 or expected not in declarations[0]:
            missing.append(f"{jar['name']}: expected one component declaration with {expected}")
    return {"archive": archive.name, "jars": sorted(jars, key=lambda item: item["name"]), "texts": texts,
            "missing_legal_resources": missing}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--check", action="store_true", help="Require bundled legal documents to reproduce JAR legal resources")
    args = parser.parse_args()
    output = Path("target/release-audit")
    output.mkdir(parents=True, exist_ok=True)
    missing = []
    for component in ("admin", "bootstrap"):
        archives = list(Path(f"shenyu-dist/shenyu-{component}-dist/target").glob(f"apache-shenyu-*-{component}-bin.tar.gz"))
        if len(archives) != 1:
            raise ValueError(f"Expected one {component} distribution, found {len(archives)}")
        report = collect(archives[0])
        (output / f"{component}-legal.json").write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
        print(f"{component}: {len(report['jars'])} third-party JARs, {len(report['texts'])} distinct legal texts")
        missing.extend(component + ": " + resource for resource in report["missing_legal_resources"])
    if args.check and missing:
        raise ValueError("Legal resources missing from distribution documents:\n" + "\n".join(missing))


if __name__ == "__main__":
    main()
