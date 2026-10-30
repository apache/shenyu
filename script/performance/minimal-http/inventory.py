#!/usr/bin/env python3
# Licensed to the Apache Software Foundation (ASF) under one or more
# contributor license agreements. See the NOTICE file distributed with
# this work for additional information regarding copyright ownership.
# The ASF licenses this file to You under the Apache License, Version 2.0
# (the "License"); you may not use this file except in compliance with
# the License. You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""Inventory the actual assembly, including hashes, Maven coordinates and notices."""

import argparse
import hashlib
import json
import zipfile
from pathlib import Path

PLUGINS = {"shenyu-plugin-global", "shenyu-plugin-divide", "shenyu-plugin-uri", "shenyu-plugin-httpclient", "shenyu-plugin-response"}


def inventory(directory, minimal=False):
    directory = Path(directory)
    for required in ("lib", "conf/application.yml", "LICENSE", "NOTICE", "licenses"):
        if not (directory / required).exists():
            raise ValueError(f"Missing distribution entry: {required}")
    jars = []
    for jar in sorted((directory / "lib").glob("*.jar")):
        coordinates, notices = [], []
        with zipfile.ZipFile(jar) as archive:
            for name in archive.namelist():
                if name.startswith("META-INF/maven/") and name.endswith("/pom.properties"):
                    props = dict(line.split("=", 1) for line in archive.read(name).decode().splitlines()
                                 if "=" in line and not line.startswith("#"))
                    coordinates.append(":".join(props.get(key, "?") for key in ("groupId", "artifactId", "version")))
                if name.upper().startswith("META-INF/") and any(token in name.upper() for token in ("LICENSE", "NOTICE", "COPYING")):
                    notices.append(name)
        jars.append({"file": jar.name, "bytes": jar.stat().st_size,
                     "sha256": hashlib.sha256(jar.read_bytes()).hexdigest(),
                     "coordinates": coordinates, "embedded_legal_files": notices})
    if not jars:
        raise ValueError("Empty runtime classpath")
    if minimal:
        artifacts = {value.split(":")[1] for jar in jars for value in jar["coordinates"] if value.startswith("org.apache.shenyu:")}
        actual = {name for name in artifacts if name.startswith("shenyu-plugin-")} - {"shenyu-plugin-api", "shenyu-plugin-base"}
        if actual != PLUGINS:
            raise ValueError(f"Unexpected minimal plugin inventory: {sorted(actual)}")
        sync = {name for name in artifacts if name.startswith("shenyu-sync-data-")}
        if sync != {"shenyu-sync-data-api", "shenyu-sync-data-websocket"}:
            raise ValueError(f"Unexpected synchronization inventory: {sorted(sync)}")
        if any(name.startswith("shenyu-registry-") for name in artifacts):
            raise ValueError("Registry implementation leaked into the HTTP assembly")
    return {"jar_count": len(jars), "jar_bytes": sum(jar["bytes"] for jar in jars),
            "distribution_bytes": sum(path.stat().st_size for path in directory.rglob("*") if path.is_file()),
            "legal_policy": "Conservative full-distribution LICENSE/NOTICE/licenses superset; embedded legal files retained in JARs",
            "jars_without_embedded_legal_files": [jar["file"] for jar in jars if not jar["embedded_legal_files"]],
            "jars": jars}


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("directory", type=Path)
    parser.add_argument("--minimal", action="store_true")
    parser.add_argument("--output", required=True, type=Path)
    args = parser.parse_args()
    args.output.write_text(json.dumps(inventory(args.directory, args.minimal), indent=2) + "\n", encoding="utf-8")
