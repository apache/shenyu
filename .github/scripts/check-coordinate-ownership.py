#!/usr/bin/env python3
# Licensed to the Apache Software Foundation (ASF) under one
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

"""Non-publishing dry-run check for the #8421 coordinate ownership handoff.

Modes:
  default                  validate .github/coordinate-ownership.json against the
                           reactor POMs and the staged migration inventory.
  --repository <dir>       additionally validate a built local Maven repository
                           (a directory holding org/apache/shenyu/<artifact>/<version>)
                           for release readiness without publishing anything.
"""

import json
import os
import sys
import xml.etree.ElementTree as ET
from pathlib import Path

NS = {"m": "http://maven.apache.org/POM/4.0.0"}
PREPARATION = "preparation"
TRANSFERRED = "transferred"


def load_manifest(repository):
    manifest = json.loads((repository / ".github/coordinate-ownership.json").read_text(encoding="utf-8"))
    state = manifest.get("handoff_state")
    if state not in (PREPARATION, TRANSFERRED):
        raise SystemExit(f"handoff_state must be {PREPARATION!r} or {TRANSFERRED!r}, got {state!r}")
    return manifest


def artifact_of(pom):
    root = ET.parse(pom).getroot()
    return root.findtext("m:artifactId", namespaces=NS)


def source_files(root, suffix):
    for directory, children, files in os.walk(root):
        children[:] = [name for name in children if name not in {"target", ".git", "node_modules"}]
        for name in files:
            if name.endswith(suffix):
                yield Path(directory) / name


def reactor_artifacts(root):
    return {artifact_of(pom) for pom in source_files(root, "pom.xml")}


def inventory_staged_artifacts(root):
    inventory = json.loads((root / ".github/client-migration-inventory.json").read_text(encoding="utf-8"))
    staged = set()
    for prefix in inventory["client_source_roots"]:
        area = root / prefix
        if area.exists():
            staged |= {artifact_of(pom) for pom in source_files(area, "pom.xml")}
    return staged


def validate_manifest(root, manifest, failures):
    transferred = {entry["artifact"] for entry in manifest["transferred_coordinates"]}
    foundations = {entry["artifact"] for entry in manifest["retained_foundations"]}
    if transferred & foundations:
        failures.append(f"coordinates declared both transferred and retained: {sorted(transferred & foundations)}")

    reactors = reactor_artifacts(root)
    for artifact in sorted(transferred | foundations):
        if artifact not in reactors:
            failures.append(f"manifest coordinate {artifact} is not a module of this reactor")

    staged = inventory_staged_artifacts(root)
    unrecorded = staged - transferred
    if unrecorded:
        failures.append(f"staged inventory artifacts missing from transferred_coordinates: {sorted(unrecorded)}")

    boundaries = manifest["publication_boundaries"]["records"]
    if manifest["handoff_state"] == TRANSFERRED:
        missing = sorted(transferred - set(boundaries))
        if missing:
            failures.append(f"handoff_state is transferred but boundary records are missing: {missing}")
    elif boundaries:
        failures.append("boundary records must stay empty while handoff_state is preparation")
    return transferred, foundations


def validate_repository(directory, version, transferred, foundations, state, failures):
    present = {child.name for child in directory.iterdir() if child.is_dir()}
    leaked = sorted(transferred & present)
    if state == TRANSFERRED:
        if leaked:
            failures.append(f"main must not publish transferred coordinates: {leaked}")
        if not (present - transferred):
            failures.append("repository holds no main-owned artifacts; handoff_state=transferred needs the retained foundations")
    elif not leaked:
        failures.append("handoff_state is preparation but no staged coordinate was built; the release lane cannot verify what it still owns")
    missing = sorted(foundations - present)
    if missing:
        failures.append(f"retained foundations missing from the built repository: {missing}")
    if version and ("SNAPSHOT" in version or "prepare" in version):
        failures.append(f"release lane must not accept preparation version {version!r}")


def main():
    repository = Path(__file__).resolve().parents[2]
    args = sys.argv[1:]
    repo_dir = None
    if "--repository" in args:
        repo_dir = Path(args[args.index("--repository") + 1])
    version = None
    if "--version" in args:
        version = args[args.index("--version") + 1]

    manifest = load_manifest(repository)
    failures = []
    transferred, foundations = validate_manifest(repository, manifest, failures)
    if repo_dir is not None:
        validate_repository(repo_dir, version, transferred, foundations, manifest["handoff_state"], failures)

    for failure in failures:
        print(f"[ownership] {failure}", file=sys.stderr)
    if failures:
        sys.exit(1)
    print("Coordinate ownership manifest verified"
          + (" (release repository dry-run passed)" if repo_dir is not None else ""))


if __name__ == "__main__":
    main()
