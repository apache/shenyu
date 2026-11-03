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

import hashlib
import io
import json
from pathlib import Path, PurePosixPath
import re
import tarfile
import zipfile


LEGAL_NAME = re.compile(r"^(license|notice|copyright|copying)(?:[._-].*)?$", re.I)
TEXT_SUFFIXES = {"", ".txt", ".md", ".html", ".htm", ".rst"}


def collect(archive):
    jars = []
    texts = {}
    with tarfile.open(archive, "r:gz") as distribution:
        for member in distribution:
            path = PurePosixPath(member.name)
            if not member.isfile() or path.parent.name != "lib" or path.suffix != ".jar":
                continue
            if path.name.startswith("shenyu-"):
                continue
            resources = []
            coordinates = []
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
                        legal_path = LEGAL_NAME.fullmatch(resource.name) or any(
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
                            "kind": "notice" if resource.name.lower().startswith("notice") else "license",
                            "sha256": digest,
                        })
            jars.append({"name": path.name, "coordinates": coordinates, "resources": resources})
    if not jars:
        raise ValueError(f"No third-party library JARs found in {archive}")
    return {"archive": archive.name, "jars": sorted(jars, key=lambda item: item["name"]), "texts": texts}


def main():
    output = Path("target/release-audit")
    output.mkdir(parents=True, exist_ok=True)
    for component in ("admin", "bootstrap"):
        archives = list(Path(f"shenyu-dist/shenyu-{component}-dist/target").glob(f"apache-shenyu-*-{component}-bin.tar.gz"))
        if len(archives) != 1:
            raise ValueError(f"Expected one {component} distribution, found {len(archives)}")
        report = collect(archives[0])
        (output / f"{component}-legal.json").write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
        print(f"{component}: {len(report['jars'])} third-party JARs, {len(report['texts'])} distinct legal texts")


if __name__ == "__main__":
    main()
