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

"""Offline regressions for benchmark snapshot source provenance."""

from pathlib import Path
import subprocess
import tempfile
import unittest
from unittest.mock import patch

import run


class SnapshotSourceTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.git("init", "-q")
        self.source = self.root / "module/src/main/java/Fixture.java"
        self.source.parent.mkdir(parents=True)
        self.source.write_text("original\n", encoding="utf-8")
        self.git("add", ".")
        self.git("-c", "user.name=Test", "-c", "user.email=test@example.invalid",
                 "commit", "-qm", "baseline")
        self.root_patch = patch.object(run, "ROOT", self.root)
        self.root_patch.start()
        self.addCleanup(self.root_patch.stop)

    def git(self, *arguments):
        return subprocess.check_output(["git", *arguments], cwd=self.root,
                                       stderr=subprocess.STDOUT)

    def digest(self):
        return run.production_diff_sha256()

    def test_staging_does_not_change_source_identity(self):
        self.source.write_text("changed\n", encoding="utf-8")
        unstaged = self.digest()
        self.git("add", ".")
        self.assertEqual(unstaged, self.digest())

    def test_staged_new_production_file_is_recorded(self):
        clean = self.digest()
        (self.source.parent / "Added.java").write_text("added\n", encoding="utf-8")
        self.git("add", ".")
        self.assertNotEqual(clean, self.digest())

    def test_mixed_edits_hash_final_worktree(self):
        self.source.write_text("staged\n", encoding="utf-8")
        self.git("add", ".")
        self.source.write_text("final\n", encoding="utf-8")
        mixed = self.digest()
        self.git("reset", "-q", "HEAD")
        self.assertEqual(mixed, self.digest())

    def test_binary_content_and_staging_identity(self):
        binary = self.source.parent / "fixture.bin"
        binary.write_bytes(b"\0original")
        self.git("add", ".")
        self.git("-c", "user.name=Test", "-c", "user.email=test@example.invalid",
                 "commit", "-qm", "binary baseline")
        binary.write_bytes(b"\0first")
        first = self.digest()
        binary.write_bytes(b"\0second")
        second = self.digest()
        self.assertNotEqual(first, second)
        self.git("add", ".")
        self.assertEqual(second, self.digest())

    def test_untracked_and_nonproduction_files_are_excluded(self):
        clean = self.digest()
        (self.root / "README.md").write_text("docs\n", encoding="utf-8")
        self.git("add", "README.md")
        (self.source.parent / "Untracked.java").write_text("not tracked\n", encoding="utf-8")
        self.assertEqual(clean, self.digest())


if __name__ == "__main__":
    unittest.main()
