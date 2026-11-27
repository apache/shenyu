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

"""Dry-run fixtures for check-coordinate-ownership.py: preparation state must
not pass as release-ready, a missing foundation must fail, a transferred
coordinate in the wrong publisher must fail, and a valid ownership selection
must pass."""

import importlib.util
import json
from pathlib import Path
import tempfile
import unittest

SPEC = importlib.util.spec_from_file_location(
    "ownership", Path(__file__).with_name("check-coordinate-ownership.py"))
OWNERSHIP = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(OWNERSHIP)

POM = """<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0">
  <modelVersion>4.0.0</modelVersion>
  <groupId>org.apache.shenyu</groupId>
  <artifactId>{artifact}</artifactId>
  <version>2.7.3-SNAPSHOT</version>
</project>
"""

INVENTORY = {
    "client_source_roots": ["shenyu-client"],
    "client_example_roots": [],
    "staged_dependency_management": [],
}
MANIFEST = {
    "handoff_state": "preparation",
    "transferred_coordinates": [
        {"artifact": "shenyu-client", "role": "aggregator"},
        {"artifact": "shenyu-client-core", "role": "implementation"},
    ],
    "retained_foundations": [
        {"artifact": "shenyu-register-protocol", "note": "foundation"},
    ],
    "retained_prefixes": ["shenyu-sdk"],
    "publication_boundaries": {"records": {}},
}


def write_project(root, manifest):
    (root / ".github").mkdir(parents=True)
    (root / ".github/coordinate-ownership.json").write_text(
        json.dumps(manifest), encoding="utf-8")
    (root / ".github/client-migration-inventory.json").write_text(
        json.dumps(INVENTORY), encoding="utf-8")
    staged = root / "shenyu-client"
    staged.mkdir()
    for artifact in ("shenyu-client", "shenyu-client-core"):
        (staged / f"{artifact}/pom.xml").parent.mkdir(parents=True, exist_ok=True)
        (staged / f"{artifact}/pom.xml").write_text(POM.format(artifact=artifact), encoding="utf-8")
    foundation = root / "shenyu-register-center"
    foundation.mkdir()
    (foundation / "pom.xml").write_text(
        POM.format(artifact="shenyu-register-protocol"), encoding="utf-8")


def build_repository(temp, artifacts):
    repo = Path(temp) / "org/apache/shenyu"
    repo.mkdir(parents=True)
    for artifact in artifacts:
        (repo / artifact / "2.7.2").mkdir(parents=True)
        (repo / artifact / "2.7.2" / f"{artifact}-2.7.2.jar").write_bytes(b"jar")
    return repo


class OwnershipTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)

    def run_check(self, manifest, repo=None, version="2.7.2"):
        write_project(self.root, manifest)
        failures = []
        transferred, foundations = OWNERSHIP.validate_manifest(self.root, manifest, failures)
        if repo is not None:
            OWNERSHIP.validate_repository(repo, version, transferred, foundations,
                                          manifest["handoff_state"], failures)
        return failures

    def test_preparation_with_staged_build_passes(self):
        repo = build_repository(self.temp.name, ["shenyu-client", "shenyu-client-core",
                                                 "shenyu-register-protocol"])
        self.assertEqual(self.run_check(MANIFEST, repo), [])

    def test_preparation_version_must_not_be_release_ready(self):
        repo = build_repository(self.temp.name, ["shenyu-client", "shenyu-client-core",
                                                 "shenyu-register-protocol"])
        failures = self.run_check(MANIFEST, repo, version="2.7.3-SNAPSHOT")
        self.assertTrue(any("preparation version" in f for f in failures))

    def test_missing_foundation_fails(self):
        repo = build_repository(self.temp.name, ["shenyu-client", "shenyu-client-core"])
        failures = self.run_check(MANIFEST, repo)
        self.assertTrue(any("foundations missing" in f for f in failures))

    def test_transferred_coordinate_in_wrong_publisher_fails(self):
        transferred_state = dict(MANIFEST)
        transferred_state["handoff_state"] = "transferred"
        transferred_state["publication_boundaries"] = {
            "records": {"shenyu-client": {"last_main_version": "2.7.2"},
                        "shenyu-client-core": {"last_main_version": "2.7.2"}}}
        repo = build_repository(self.temp.name, ["shenyu-client", "shenyu-register-protocol"])
        failures = self.run_check(transferred_state, repo)
        self.assertTrue(any("must not publish transferred" in f for f in failures))

    def test_valid_ownership_selection_passes(self):
        transferred_state = dict(MANIFEST)
        transferred_state["handoff_state"] = "transferred"
        transferred_state["publication_boundaries"] = {
            "records": {"shenyu-client": {"last_main_version": "2.7.2"},
                        "shenyu-client-core": {"last_main_version": "2.7.2"}}}
        repo = build_repository(self.temp.name, ["shenyu-register-protocol", "shenyu-sdk-core"])
        self.assertEqual(self.run_check(transferred_state, repo), [])

    def test_transferred_state_requires_boundary_records(self):
        transferred_state = dict(MANIFEST)
        transferred_state["handoff_state"] = "transferred"
        failures = self.run_check(transferred_state)
        self.assertTrue(any("boundary records are missing" in f for f in failures))

    def test_staged_artifact_missing_from_manifest_fails(self):
        incomplete = dict(MANIFEST)
        incomplete["transferred_coordinates"] = [{"artifact": "shenyu-client", "role": "aggregator"}]
        failures = self.run_check(incomplete)
        self.assertTrue(any("missing from transferred_coordinates" in f for f in failures))

    def test_preparation_state_rejects_boundary_records(self):
        polluted = dict(MANIFEST)
        polluted["publication_boundaries"] = {
            "records": {"shenyu-client": {"last_main_version": "2.7.2"}}}
        failures = self.run_check(polluted)
        self.assertTrue(any("records must stay empty" in f for f in failures))

    def test_transferred_state_rejects_blank_last_main_version(self):
        transferred_state = dict(MANIFEST)
        transferred_state["handoff_state"] = "transferred"
        transferred_state["publication_boundaries"] = {
            "records": {"shenyu-client": {"last_main_version": "2.7.2"},
                        "shenyu-client-core": {"last_main_version": "  "}}}
        failures = self.run_check(transferred_state)
        self.assertTrue(any("non-empty last_main_version" in f for f in failures))

    def test_retained_prefix_must_not_shadow_transferred_coordinate(self):
        shadowed = dict(MANIFEST)
        shadowed["retained_prefixes"] = ["shenyu-client"]
        failures = self.run_check(shadowed)
        self.assertTrue(any("shadowed by a retained prefix" in f for f in failures))


if __name__ == "__main__":
    unittest.main()
