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

"""Verify Maven artifact handoff and keep stale snapshots outside dependency caches."""

import os
import subprocess
import tarfile
import tempfile
import unittest
from pathlib import Path

SCRIPT = Path(__file__).with_name("restore-shenyu-local-repo.sh").resolve()


class RestoreRepositoryTest(unittest.TestCase):
    def test_archive_and_legacy_handoffs_replace_stale_artifacts(self):
        for archived in (True, False):
            with self.subTest(archived=archived), tempfile.TemporaryDirectory() as temporary:
                root = Path(temporary)
                artifact = root / "artifact"
                current = artifact / "org/apache/shenyu/module/2.7.3-SNAPSHOT"
                current.mkdir(parents=True)
                (current / "module.jar").write_text("current")
                if archived:
                    with tarfile.open(artifact / "repository.tar.gz", "w:gz") as archive:
                        archive.add(artifact / "org", arcname="org")
                    import shutil
                    shutil.rmtree(artifact / "org")
                repository = root / "repository"
                previous = repository / "org/apache/shenyu/stale"
                previous.mkdir(parents=True)
                (previous / "old.jar").write_text("stale")
                result = subprocess.run(["bash", str(SCRIPT), str(artifact), str(repository)],
                                        env={**os.environ, "RUNNER_TEMP": str(root)}, capture_output=True, text=True)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual((repository / "org/apache/shenyu/module/2.7.3-SNAPSHOT/module.jar").read_text(), "current")
                self.assertFalse(previous.exists())
                self.assertFalse(list(repository.rglob("shenyu.backup*")))
                self.assertEqual(len(list(root.glob("shenyu-m2-previous.*/shenyu/stale/old.jar"))), 1)

    def test_missing_artifact_fails_before_changing_the_repository(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            result = subprocess.run(["bash", str(SCRIPT), str(root / "missing"), str(root / "repository")],
                                    capture_output=True, text=True)
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("artifact is missing", result.stderr)
            self.assertFalse((root / "repository").exists())


if __name__ == "__main__":
    unittest.main()
