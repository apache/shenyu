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

"""Regression coverage for the archive-level release gates."""

from pathlib import Path
import runpy
import tempfile
import unittest
import zipfile


CHECKS = runpy.run_path(str(Path(__file__).with_name("check-release-artifacts.py")))
NOTICE = "Apache ShenYu\nCopyright 2021-2026 The Apache Software Foundation"
LICENSE = "Apache License\nVersion 2.0\nTERMS AND CONDITIONS"


class ReleaseArtifactsTest(unittest.TestCase):
    def test_javadoc_tool_license_does_not_license_the_project(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "shenyu-common-2.7.2-javadoc.jar"
            with zipfile.ZipFile(path, "w") as jar:
                jar.writestr("legal/LICENSE", "GNU GENERAL PUBLIC LICENSE")
            with self.assertRaisesRegex(ValueError, "META-INF/LICENSE"):
                CHECKS["check_jar"](path, NOTICE)

    def test_dependency_notice_does_not_replace_project_notice(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "shenyu-common-2.7.2.jar"
            with zipfile.ZipFile(path, "w") as jar:
                jar.writestr("META-INF/LICENSE", LICENSE)
                jar.writestr("META-INF/NOTICE", "Some shaded dependency")
            with self.assertRaisesRegex(ValueError, "project NOTICE"):
                CHECKS["check_jar"](path, NOTICE)

    def test_project_and_shaded_notices_can_coexist(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "shenyu-common-2.7.2.jar"
            with zipfile.ZipFile(path, "w") as jar:
                jar.writestr("META-INF/LICENSE", LICENSE)
                jar.writestr("META-INF/NOTICE", NOTICE + "\nDependency attribution")
                jar.writestr("legal/LICENSE", "GNU GENERAL PUBLIC LICENSE")
            self.assertTrue(CHECKS["check_jar"](path, NOTICE)["project_notice"])

    def test_source_archive_requires_dashboard_assets(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            for name in ("LICENSE", "NOTICE", "RELEASE-NOTES.md"):
                (root / name).write_text(name)
            path = root / "source.zip"
            with zipfile.ZipFile(path, "w") as archive:
                for name in ("LICENSE", "NOTICE", "RELEASE-NOTES.md"):
                    archive.writestr("source/" + name, name)
                archive.writestr("source/shenyu-admin/src/main/resources/static/index.html", '<script src="index.js"></script>')
            with self.assertRaises(KeyError):
                CHECKS["check_source"](path, root)

    def test_standalone_admin_jar_requires_dashboard_terms(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "shenyu-admin-2.7.2.jar"
            with zipfile.ZipFile(path, "w") as jar:
                jar.writestr("META-INF/LICENSE", LICENSE)
                jar.writestr("META-INF/NOTICE", NOTICE)
                jar.writestr("static/index.html", '<script src="index.js"></script>')
            with self.assertRaisesRegex(ValueError, "LICENSE-dashboard.txt"):
                CHECKS["check_jar"](path, NOTICE)

    def test_unshaded_apollo_adapter_needs_only_its_own_project_terms(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "shenyu-sync-data-apollo-2.7.2.jar"
            with zipfile.ZipFile(path, "w") as jar:
                jar.writestr("META-INF/LICENSE", LICENSE)
                jar.writestr("META-INF/NOTICE", NOTICE)
            self.assertTrue(CHECKS["check_jar"](path, NOTICE)["project_notice"])


if __name__ == "__main__":
    unittest.main()
