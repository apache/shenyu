# Licensed to the Apache Software Foundation (ASF) under one or more
# contributor license agreements.  See the NOTICE file distributed with
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

"""Offline regression checks for fixture validation and failure cleanup."""

import importlib.util
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

sys.dont_write_bytecode = True
SPEC = importlib.util.spec_from_file_location("wasm_fixtures", Path(__file__).with_name("verify-wasm-fixtures.py"))
CHECK = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(CHECK)


class WatFixtureTest(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        self.base = Path(temporary.name)
        (self.base / "resources").mkdir()
        self.api = self.base / "api"
        (self.api / "resources").mkdir(parents=True)
        self.outputs = {}
        self.addCleanup(patch.stopall)
        patch.object(CHECK, "BASE", self.base).start()
        patch.object(CHECK, "API", self.api).start()
        patch.object(CHECK.subprocess, "check_output", return_value="1.0.42\n").start()
        patch.object(CHECK, "run", side_effect=self.build).start()
        self.family("wasm-concurrent", self.api)

    def family(self, name, root=None):
        root = self.base if root is None else root
        (root / name).mkdir()
        binary = name + ".wasm"
        self.outputs[name] = {binary: b"committed fixture"}
        (root / "resources" / binary).write_bytes(b"committed fixture")

    def build(self, command):
        family = Path(command[2]).name
        destination = Path(command[4].split("=", 1)[1])
        for name, content in self.outputs[family].items():
            (destination / name).write_bytes(content)

    def test_missing_gateway_is_rejected_even_when_bridge_exists(self):
        self.family("wasm-bridge")
        with self.assertRaisesRegex(RuntimeError, "Required WAT fixture family missing: wasm-gateway"):
            CHECK.verify_wat()

    def test_additional_fixture_families_are_verified(self):
        for family in ("wasm-gateway", "wasm-bridge", "wasm-future"):
            self.family(family)
        self.outputs["wasm-future"]["wasm-future.wasm"] = b"different binary"
        with self.assertRaisesRegex(RuntimeError, "WAT fixture differs.*wasm-future.wasm"):
            CHECK.verify_wat()
        self.assertEqual(b"committed fixture", (self.base / "resources/wasm-future.wasm").read_bytes())

    def test_empty_family_is_rejected(self):
        self.family("wasm-gateway")
        self.outputs["wasm-gateway"] = {}
        with self.assertRaisesRegex(RuntimeError, "No fixtures generated for wasm-gateway"):
            CHECK.verify_wat()

    def test_missing_concurrent_family_is_rejected(self):
        self.family("wasm-gateway")
        (self.api / "wasm-concurrent").rmdir()
        with self.assertRaisesRegex(RuntimeError, "Required WAT fixture family missing: wasm-concurrent"):
            CHECK.verify_wat()

    def test_concurrent_binary_uses_api_resources(self):
        self.family("wasm-gateway")
        self.outputs["wasm-concurrent"]["wasm-concurrent.wasm"] = b"different binary"
        with self.assertRaisesRegex(RuntimeError, "WAT fixture differs.*wasm-concurrent.wasm"):
            CHECK.verify_wat()
        self.assertEqual(b"committed fixture", (self.api / "resources/wasm-concurrent.wasm").read_bytes())

    def test_additional_api_families_are_verified(self):
        self.family("wasm-gateway")
        self.family("wasm-future-api", self.api)
        self.outputs["wasm-future-api"]["wasm-future-api.wasm"] = b"different binary"
        with self.assertRaisesRegex(RuntimeError, "WAT fixture differs.*wasm-future-api.wasm"):
            CHECK.verify_wat()

    def test_empty_api_family_is_rejected(self):
        self.family("wasm-gateway")
        self.outputs["wasm-concurrent"] = {}
        with self.assertRaisesRegex(RuntimeError, "No fixtures generated for wasm-concurrent"):
            CHECK.verify_wat()

    def test_api_family_path_must_be_a_directory(self):
        self.family("wasm-gateway")
        (self.api / "wasm-invalid").write_text("not a directory", encoding="utf-8")
        with self.assertRaisesRegex(RuntimeError, "WAT fixture family is not a directory: wasm-invalid"):
            CHECK.verify_wat()

    def test_matching_binaries_do_not_modify_resources(self):
        self.family("wasm-gateway")
        self.family("wasm-bridge")
        CHECK.verify_wat()
        for resource in (self.base / "resources").iterdir():
            self.assertEqual(b"committed fixture", resource.read_bytes())

    def test_family_path_must_be_a_directory(self):
        self.family("wasm-gateway")
        (self.base / "wasm-invalid").write_text("not a directory", encoding="utf-8")
        with self.assertRaisesRegex(RuntimeError, "WAT fixture family is not a directory: wasm-invalid"):
            CHECK.verify_wat()


class RustFixtureTest(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name)
        (self.root / "resources").mkdir()
        self.guests = []
        self.originals = {}
        for index in range(2):
            crate = "rust-guest-" + str(index)
            source = self.root / crate
            (source / "src").mkdir(parents=True)
            (source / "Cargo.toml").write_text("[package]\nname = '" + crate + "'\nversion = '0.1.0'\n", encoding="utf-8")
            (source / "src/lib.rs").write_text("", encoding="utf-8")
            name = crate + ".wasm"
            resource = self.root / "resources" / name
            original = ("original " + crate).encode()
            resource.write_bytes(original)
            self.originals[resource] = original
            self.guests.append((self.root, crate, name))
        self.builds = 0
        self.failure = None
        self.fail_build = False
        self.addCleanup(patch.stopall)
        patch.object(CHECK, "RUST_GUESTS", self.guests).start()
        patch.object(CHECK, "run", side_effect=self.run_command).start()

    def run_command(self, command, **kwargs):
        if command[0] == "cargo":
            self.builds += 1
            if self.fail_build and self.builds == 2:
                raise self.failure
            manifest = Path(command[command.index("--manifest-path") + 1])
            binary = manifest.parent / "target/wasm32-wasip1/release" / (manifest.parent.name.replace("-", "_") + ".wasm")
            binary.parent.mkdir(parents=True)
            binary.write_bytes(b"freshly compiled guest")
        else:
            for resource in self.originals:
                self.assertEqual(b"freshly compiled guest", resource.read_bytes())
            if self.failure:
                raise self.failure

    def assert_restored(self):
        for resource, original in self.originals.items():
            self.assertEqual(original, resource.read_bytes())

    def test_success_restores_every_resource(self):
        CHECK.verify_rust()
        self.assert_restored()

    def test_partial_build_failure_restores_already_replaced_resources(self):
        self.fail_build = True
        self.failure = subprocess.CalledProcessError(1, "cargo")
        with self.assertRaises(subprocess.CalledProcessError):
            CHECK.verify_rust()
        self.assert_restored()

    def test_maven_failure_restores_every_resource(self):
        self.failure = subprocess.CalledProcessError(1, "./mvnw")
        with self.assertRaises(subprocess.CalledProcessError):
            CHECK.verify_rust()
        self.assert_restored()

    def test_keyboard_interrupt_restores_every_resource(self):
        self.failure = KeyboardInterrupt()
        with self.assertRaises(KeyboardInterrupt):
            CHECK.verify_rust()
        self.assert_restored()


if __name__ == "__main__":
    unittest.main()
