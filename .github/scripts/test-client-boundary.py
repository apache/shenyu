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

"""Regression checks for staged client ownership and runtime dependency guards."""

import importlib.util
from pathlib import Path
import tempfile
import sys
import unittest

sys.dont_write_bytecode = True

SPEC = importlib.util.spec_from_file_location("boundary", Path(__file__).with_name("check-client-boundary.py"))
BOUNDARY = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(BOUNDARY)


class BoundaryTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.inventory = {
            "client_source_roots": ["shenyu-client"],
            "client_example_roots": ["shenyu-examples/migrating"],
            "staged_dependency_management": ["shenyu-examples/pom.xml"],
        }

    def write(self, path, text):
        target = self.root / path
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(text)

    def pom(self, path, body):
        self.write(path, '<project xmlns="http://maven.apache.org/POM/4.0.0">' + body + '</project>')

    def dependency(self, artifact, version="${project.version}", scope="test"):
        return (f"<dependencies><dependency><groupId>org.apache.shenyu</groupId><artifactId>{artifact}</artifactId>"
                f"<version>{version}</version><scope>{scope}</scope></dependency></dependencies>")

    def errors(self):
        return BOUNDARY.check(self.root, self.inventory)

    def test_inactive_profile_and_test_scope_do_not_hide_reverse_dependencies(self):
        self.pom("shenyu-admin/pom.xml", "<profiles><profile>" + self.dependency("shenyu-client-core") + "</profile></profiles>")
        self.assertIn("main-owned module depends", self.errors()[0])

    def test_heartbeat_is_allowed_but_cannot_depend_on_client_http(self):
        self.pom("shenyu-bootstrap/pom.xml", self.dependency("shenyu-spring-boot-starter-client-beat"))
        self.assertEqual([], self.errors())
        self.pom("shenyu-register-client-beat/pom.xml", self.dependency("shenyu-register-client-http"))
        self.assertEqual(1, len(self.errors()))

    def test_only_inventoried_examples_may_use_staged_client(self):
        self.pom("shenyu-examples/migrating/pom.xml", self.dependency("shenyu-client-core"))
        self.assertEqual([], self.errors())
        self.pom("shenyu-examples/retained/pom.xml", self.dependency("shenyu-client-core"))
        self.assertEqual(1, len(self.errors()))

    def test_independent_version_override_is_rejected_even_in_staged_code(self):
        self.pom("shenyu-client/core/pom.xml", self.dependency("shenyu-client-api", "2.7.0.3"))
        self.assertIn("must come from this reactor", self.errors()[0])

    def test_management_exception_does_not_allow_runtime_dependencies(self):
        dependencies = self.dependency("shenyu-client-core")
        self.pom("shenyu-examples/pom.xml", "<dependencyManagement>" + dependencies + "</dependencyManagement>")
        self.assertEqual([], self.errors())
        self.pom("shenyu-examples/pom.xml", dependencies)
        self.assertIn("only staged dependencyManagement", self.errors()[0])

    def test_fully_qualified_client_reference_in_main_test_is_rejected(self):
        self.write("shenyu-admin/src/test/Example.java", "Object value = new org.apache.shenyu.client.core.Client();")
        self.assertIn("references client implementation", self.errors()[0])

    def test_protocol_must_not_import_implementation_code(self):
        self.write(BOUNDARY.PROTOCOL + "/src/main/java/Example.java", "import org.springframework.context.ApplicationContext;")
        self.assertIn("protocol imports non-JDK", self.errors()[0])

    def test_resolved_transitive_dependency_is_rejected(self):
        for module in BOUNDARY.TREE_MODULES:
            self.write(module + "/target/client-boundary.tgf", "1 org.apache.shenyu:main:jar:1\n#\n")
        self.assertEqual([], BOUNDARY.check_trees(self.root))
        self.write(BOUNDARY.TREE_MODULES[0] + "/target/client-boundary.tgf",
                   "1 org.apache.shenyu:main:jar:1\n2 org.apache.shenyu:shenyu-client-core:jar:1:test\n#\n1 2\n")
        self.assertIn("resolved dependency", BOUNDARY.check_trees(self.root)[0])

    def test_resolved_tree_must_not_be_missing(self):
        self.assertEqual(len(BOUNDARY.TREE_MODULES), len(BOUNDARY.check_trees(self.root)))

    def test_generated_output_is_not_source(self):
        self.pom("shenyu-admin/target/generated/pom.xml", self.dependency("shenyu-client-core"))
        self.assertEqual([], self.errors())


if __name__ == "__main__":
    unittest.main()
