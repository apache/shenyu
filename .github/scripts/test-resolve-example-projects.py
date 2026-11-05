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

"""Check all deployed integration images and fail-closed example selection."""

import importlib.util
import tempfile
import unittest
from pathlib import Path

SPEC = importlib.util.spec_from_file_location("resolver", Path(__file__).with_name("resolve-example-projects.py"))
RESOLVER = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(RESOLVER)


class ExampleProjectsTest(unittest.TestCase):
    def test_all_deployed_cases(self):
        cases = sorted((RESOLVER.ROOT / "shenyu-integrated-test").glob("shenyu-integrated-test-*/Dockerfile"))
        for dockerfile in cases:
            if dockerfile.parent.name == "shenyu-integrated-test-upload-plugin":
                continue
            with self.subTest(case=dockerfile.parent.name):
                projects = RESOLVER.resolve(dockerfile.parent.name).split(",")
                self.assertTrue(all(project.startswith(":shenyu-example") for project in projects))
                self.assertEqual(len(projects), len(set(projects)))

    def test_combination_keeps_every_backend(self):
        self.assertEqual(RESOLVER.resolve("shenyu-integrated-test-combination"),
                         ":shenyu-examples-apache-dubbo-service,:shenyu-examples-grpc,:shenyu-examples-http")

    def test_sdk_keeps_provider_and_consumer(self):
        self.assertEqual(RESOLVER.resolve("shenyu-integrated-test-sdk-apache-dubbo"),
                         ":shenyu-examples-sdk-apache-dubbo-consumer,:shenyu-examples-sdk-apache-dubbo-provider")

    def test_invalid_missing_and_unmapped_cases(self):
        for case in ("../pom.xml", "shenyu-integrated-test-missing", "shenyu-integrated-test-upload-plugin"):
            with self.subTest(case=case), self.assertRaises(ValueError):
                RESOLVER.resolve(case)

    def test_unknown_image_fails_instead_of_silently_omitting_backend(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            case = root / "shenyu-integrated-test/shenyu-integrated-test-new"
            case.mkdir(parents=True)
            (case / "pom.xml").touch()
            (case / "docker-compose.yml").write_text("image: shenyu-examples-unknown:latest")
            with self.assertRaisesRegex(ValueError, "without Maven modules"):
                RESOLVER.resolve(case.name, root)


if __name__ == "__main__":
    unittest.main()
