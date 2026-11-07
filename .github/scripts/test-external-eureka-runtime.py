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

import importlib.util
import json
from pathlib import Path
import tempfile
import unittest
import zipfile

SPEC = importlib.util.spec_from_file_location("runtime", Path(__file__).with_name("external-eureka-runtime.py"))
RUNTIME = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(RUNTIME)


class ExternalRuntimeTest(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.source = self.root / "source"
        self.destination = self.root / "driver"
        self.source.mkdir()
        self.destination.mkdir()
        self.provider = self.source / "shenyu-registry-eureka-2.7.1-SNAPSHOT.jar"
        self.make_provider()
        self.manifest = {"receiver_commit": RUNTIME.RECEIVER_COMMIT, "core_commit": "core-sha",
                         "core_version": "2.7.3-SNAPSHOT", "packaged_spi_http_probe": "passed",
                         "provider": self.provider.name,
                         "files": [{"file": self.provider.name, "sha256": RUNTIME.digest(self.provider)}]}
        self.save_manifest()

    def make_provider(self, spi=None, include_core=False):
        with zipfile.ZipFile(self.provider, "w") as archive:
            archive.writestr(RUNTIME.SPI, spi or "# license comment\n\neureka=" + RUNTIME.PROVIDER + "\n")
            archive.writestr(RUNTIME.PROVIDER.replace(".", "/") + ".class", b"fixture")
            if include_core:
                archive.writestr("org/apache/shenyu/registry/api/ShenyuInstanceRegisterRepository.class", b"duplicate")

    def save_manifest(self):
        (self.source / "manifest.json").write_text(json.dumps(self.manifest))

    def install(self):
        RUNTIME.install(self.source, self.destination, "core-sha", "2.7.3-SNAPSHOT")

    def test_install_preserves_mysql_driver_and_is_idempotent(self):
        driver = self.destination / "mysql-connector.jar"
        driver.write_bytes(b"existing driver")
        self.install()
        self.install()
        self.assertEqual(b"existing driver", driver.read_bytes())
        self.assertEqual(self.provider.read_bytes(), (self.destination / self.provider.name).read_bytes())

    def test_checksum_failure_does_not_change_driver_directory(self):
        self.provider.write_bytes(b"truncated artifact")
        with self.assertRaisesRegex(ValueError, "checksum mismatch"):
            self.install()
        self.assertEqual([], list(self.destination.iterdir()))

    def test_existing_different_jar_is_not_overwritten(self):
        target = self.destination / self.provider.name
        target.write_bytes(b"another version")
        with self.assertRaisesRegex(ValueError, "Refusing to replace"):
            self.install()
        self.assertEqual(b"another version", target.read_bytes())

    def test_unlisted_jar_is_rejected(self):
        (self.source / "unexpected.jar").write_bytes(b"unlisted")
        with self.assertRaisesRegex(ValueError, "inventory is incomplete"):
            self.install()

    def test_wrong_source_pair_is_rejected(self):
        for field in ["receiver_commit", "core_commit", "core_version"]:
            with self.subTest(field=field):
                original = self.manifest[field]
                self.manifest[field] = "wrong"
                self.save_manifest()
                with self.assertRaisesRegex(ValueError, "unexpected receiver|different core"):
                    self.install()
                self.manifest[field] = original

    def test_missing_packaged_probe_is_rejected(self):
        self.manifest.pop("packaged_spi_http_probe")
        self.save_manifest()
        with self.assertRaisesRegex(ValueError, "no passing"):
            self.install()

    def test_manifest_cannot_escape_source_directory(self):
        self.manifest["files"][0]["file"] = "../outside.jar"
        self.save_manifest()
        with self.assertRaisesRegex(ValueError, "Invalid external runtime"):
            self.install()

    def test_missing_provider_is_rejected(self):
        self.manifest["provider"] = "missing.jar"
        self.save_manifest()
        with self.assertRaisesRegex(ValueError, "inventory is incomplete"):
            self.install()

    def test_bundled_core_api_is_rejected(self):
        self.make_provider(include_core=True)
        with self.assertRaisesRegex(ValueError, "must not bundle"):
            RUNTIME.verify_provider(self.provider)

    def test_wrong_spi_key_is_rejected(self):
        self.make_provider(spi="different=" + RUNTIME.PROVIDER)
        with self.assertRaisesRegex(ValueError, "expected Eureka SPI"):
            RUNTIME.verify_provider(self.provider)

    def test_dependency_selection_keeps_core_versions_and_excludes_shaded_closure(self):
        repo = self.root / "repository"

        def artifact(group, name, version):
            return repo.joinpath(*group.split("."), name, version, name + "-" + version + ".jar")

        core = artifact("com.example", "shared", "2")
        old = artifact("com.example", "shared", "1")
        distinct_group = artifact("other.example", "shared", "1")
        shaded = artifact("org.glassfish.jersey.core", "jersey-client", "3")
        api = artifact("org.apache.shenyu", "shenyu-common", "2.7.3-SNAPSHOT")
        selected, reused = RUNTIME.select_dependencies([old, distinct_group, shaded, api], [core], repo,
                                                       ["org.glassfish.jersey.*:*"])
        self.assertEqual([distinct_group], selected)
        self.assertEqual([{"artifact": "com.example:shared", "requested_version": "1", "admin_version": "2"}], reused)

    def test_duplicate_external_coordinate_is_rejected(self):
        repo = self.root / "repository"
        first = repo / "example/dependency/1/dependency-1.jar"
        second = repo / "example/dependency/2/dependency-2.jar"
        with self.assertRaisesRegex(ValueError, "Duplicate external"):
            RUNTIME.select_dependencies([first, second], [], repo, [])


if __name__ == "__main__":
    unittest.main()
