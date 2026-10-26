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

"""Keep main-owned production and test code independent of the business SDK."""

import json
import os
from pathlib import Path
import re
import sys
import xml.etree.ElementTree as ET

NS = {"m": "http://maven.apache.org/POM/4.0.0"}
CLIENT_REFERENCE = re.compile(
    r"org\.apache\.shenyu\.(?:client\.|register\.client\.(?:api|http)\.)"
)
PROTOCOL = "shenyu-register-center/shenyu-register-protocol"


def client_artifact(artifact):
    """Heartbeat artifacts are gateway runtime, even though their names say client."""
    return artifact not in {
        "shenyu-register-client-beat", "shenyu-spring-boot-starter-client-beat"
    } and (artifact.startswith(("shenyu-client-", "shenyu-spring-boot-starter-client-"))
           or artifact in {"shenyu-register-client-api", "shenyu-register-client-http"})


def source_files(root, suffix):
    for directory, children, files in os.walk(root):
        children[:] = [name for name in children if name not in {"target", ".git", "node_modules"}]
        for name in files:
            if name.endswith(suffix):
                yield Path(directory) / name


def check(root, inventory):
    exemptions = inventory["client_source_roots"] + inventory["client_example_roots"]
    management = set(inventory["staged_dependency_management"])
    failures = []

    def migrated(path):
        return any(path == prefix or path.startswith(prefix + "/") for prefix in exemptions)

    # Include inactive profiles and dependencyManagement: neither may hide a reverse edge.
    for pom in sorted(source_files(root, "pom.xml")):
        relative = pom.relative_to(root)
        if any(part in {"target", ".git"} for part in relative.parts):
            continue
        path = relative.as_posix()
        document = ET.parse(pom)
        if "shenyu.client.java.version" in pom.read_text():
            failures.append(f"{path}: independent client version override is forbidden")
        for dependency in document.findall(".//m:dependency", NS):
            artifact = dependency.findtext("m:artifactId", "", NS)
            if client_artifact(artifact):
                if not migrated(path) and path not in management:
                    failures.append(f"{path}: main-owned module depends on {artifact}")
                if dependency.findtext("m:version", "", NS) not in {"", "${project.version}"}:
                    failures.append(f"{path}: staged client {artifact} must come from this reactor")
        if path in management:
            # This exception allows management of staged examples, not runtime dependencies.
            for dependency in document.findall("m:dependencies/m:dependency", NS):
                if client_artifact(dependency.findtext("m:artifactId", "", NS)):
                    failures.append(f"{path}: only staged dependencyManagement is exempt")

    for source in sorted(source_files(root, ".java")):
        relative = source.relative_to(root)
        if any(part in {"target", ".git"} for part in relative.parts):
            continue
        path = relative.as_posix()
        text = source.read_text()
        if not migrated(path) and CLIENT_REFERENCE.search(text):
            failures.append(f"{path}: main-owned source references client implementation")
        if path.startswith(PROTOCOL + "/src/main/java/"):
            for imported in re.findall(r"^import (?:static )?([^;]+);", text, re.MULTILINE):
                if not imported.startswith(("java.", "org.apache.shenyu.register.common.")):
                    failures.append(f"{path}: protocol imports non-JDK implementation {imported}")
    return failures


TREE_MODULES = (
    "shenyu-admin",
    "shenyu-spring-boot-starter/shenyu-spring-boot-starter-client/shenyu-spring-boot-starter-client-beat",
)


def check_trees(root):
    failures = []
    for module in TREE_MODULES:
        path = root / module / "target/client-boundary.tgf"
        if not path.is_file():
            failures.append(f"Missing resolved dependency tree: {path}")
            continue
        nodes = path.read_text().split("#", 1)[0].splitlines()
        if not nodes:
            failures.append(f"Empty resolved dependency tree: {path}")
        for node in nodes[1:]:
            fields = node.split(" ", 1)
            coordinates = fields[-1].split(":")
            if len(fields) != 2 or len(coordinates) < 4:
                failures.append(f"Invalid dependency tree node: {node}")
            elif client_artifact(coordinates[1]):
                failures.append(f"{module}: resolved dependency on {coordinates[1]}")
    return failures


if __name__ == "__main__":
    repository = Path(__file__).resolve().parents[2]
    manifest = json.loads((repository / ".github/client-migration-inventory.json").read_text())
    errors = check_trees(repository) if sys.argv[1:] == ["--trees"] else check(repository, manifest)
    if errors:
        print("\n".join(errors), file=sys.stderr)
        sys.exit(1)
    print("Main/client source and POM boundaries verified (staged ownership inventory applied).")
