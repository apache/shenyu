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

"""Regression checks for the resolved dependency boundary assertions."""

import importlib.util
import sys
import tempfile
import unittest
from pathlib import Path

sys.dont_write_bytecode = True

SPEC = importlib.util.spec_from_file_location("registry_dependencies", Path(__file__).with_name("check-registry-dependencies.py"))
CHECK = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(CHECK)


class RegistryDependencyTest(unittest.TestCase):
    def test_transitive_clients_are_rejected_for_every_lean_consumer(self):
        for module in CHECK.LEAN_MODULES:
            for group in CHECK.CLIENT_GROUPS:
                with self.subTest(module=module, group=group):
                    self.assertTrue(CHECK.check_module(module, {(group, "client")}))

    def test_any_registry_implementation_is_rejected(self):
        self.assertTrue(CHECK.check_module(CHECK.LEAN_MODULES[1], {("org.apache.shenyu", "shenyu-registry-external")}))

    def test_api_and_spi_are_allowed(self):
        artifacts = {("org.apache.shenyu", "shenyu-registry-api"), ("org.apache.shenyu", "shenyu-spi")}
        self.assertFalse(CHECK.check_module(CHECK.LEAN_MODULES[1], artifacts))

    def test_implementation_cannot_depend_on_core(self):
        self.assertTrue(CHECK.check_module(CHECK.IMPLEMENTATION_MODULES[0], {("org.apache.shenyu", "shenyu-registry-core")}))

    def test_compatibility_starter_must_retain_all_original_providers(self):
        artifacts = {("org.apache.shenyu", "shenyu-registry-" + provider) for provider in CHECK.COMPATIBILITY_PROVIDERS}
        self.assertFalse(CHECK.check_module(CHECK.COMPATIBILITY_MODULE, artifacts))
        artifacts.pop()
        self.assertTrue(CHECK.check_module(CHECK.COMPATIBILITY_MODULE, artifacts))

    def test_tgf_nodes_include_transitive_artifacts_but_not_root_or_edges(self):
        with tempfile.TemporaryDirectory() as directory:
            tree = Path(directory) / "tree.tgf"
            tree.write_text("1 org.apache.shenyu:shenyu-registry-core:jar:2.7.2-SNAPSHOT\n"
                            "2 org.apache.shenyu:shenyu-registry-api:jar:2.7.2-SNAPSHOT:compile\n"
                            "3 org.apache.curator:curator-client:jar:5.7.1:runtime\n#\n1 2\n2 3\n", encoding="utf-8")
            artifacts = CHECK.read_tree(tree)
            self.assertEqual({("org.apache.shenyu", "shenyu-registry-api"), ("org.apache.curator", "curator-client")}, artifacts)
            self.assertTrue(CHECK.check_module(CHECK.LEAN_MODULES[1], artifacts))

    def test_empty_tree_fails_closed(self):
        with tempfile.TemporaryDirectory() as directory:
            tree = Path(directory) / "tree.tgf"
            tree.write_text("", encoding="utf-8")
            with self.assertRaises(ValueError):
                CHECK.read_tree(tree)


if __name__ == "__main__":
    unittest.main()
