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

"""Offline regressions for Java argument-file encoding and runner wiring."""

import argparse
import base64
import hashlib
import json
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest
from unittest.mock import patch

import run


class JavaArgumentFileTest(unittest.TestCase):
    def test_unicode_and_quoted_arguments_are_written_as_utf8(self):
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "fixture.args"
            arguments = ["-cp", "café 中文 space", 'quote" and back\\slash']
            run.write_java_argfile(path, arguments)
            self.assertEqual(
                "\n".join(json.dumps(value, ensure_ascii=False) for value in arguments),
                path.read_text(encoding="utf-8"))

    def test_real_java_launch_preserves_classpath_and_arguments(self):
        java = shutil.which("java")
        if not java:
            self.skipTest("Java runtime is not installed")
        javac = shutil.which("javac")
        compiler = [javac] if javac else [java, "-m", "jdk.compiler/com.sun.tools.javac.Main"]
        version = subprocess.run(compiler + ["-version"], capture_output=True, text=True, timeout=30)
        if version.returncode:
            self.skipTest("Java compiler is unavailable: " + version.stderr.strip())
        with tempfile.TemporaryDirectory() as temporary:
            classes = Path(temporary) / "café 中文 space"
            classes.mkdir()
            source = classes / "ArgumentProbe.java"
            source.write_text(
                'public class ArgumentProbe { public static void main(String[] args) { '
                'System.out.print(java.util.Base64.getEncoder().encodeToString('
                'args[0].getBytes(java.nio.charset.StandardCharsets.UTF_8))); } }', encoding="utf-8")
            compiled = subprocess.run(compiler + [str(source)], capture_output=True, text=True, timeout=30)
            self.assertEqual(0, compiled.returncode, compiled.stderr)
            expected = 'café 中文 "quoted" back\\slash space'
            argfile = classes / "fixture.args"
            run.write_java_argfile(argfile, ["-cp", str(classes), "ArgumentProbe", expected])
            launched = subprocess.run([java, "@" + str(argfile)], capture_output=True,
                                      encoding="utf-8", timeout=30)
            self.assertEqual(0, launched.returncode, launched.stderr)
            self.assertEqual(expected, base64.b64decode(launched.stdout).decode("utf-8"))

    def test_runner_launches_the_utf8_argument_file(self):
        class StopBeforeLaunch(Exception):
            pass

        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            build = root / "café 中文 build"
            build.mkdir()
            fixture = b"fixture snapshot"
            (build / "fixture.jar").write_bytes(fixture)
            (build / "build.json").write_text(json.dumps({
                "fixtureSha256": hashlib.sha256(fixture).hexdigest()}), encoding="utf-8")
            classpath = str(build / "fixture.jar")
            (build / "classpath.txt").write_text(classpath, encoding="utf-8")
            args = argparse.Namespace(output=root / "output", build=build, java="java", k6="k6",
                                      warmup=1, duration=1, repeat=1, pending=4)

            def inspect_launch(command, **kwargs):
                # Popen has not taken ownership of this log when launch fails.
                kwargs["stdout"].close()
                self.assertEqual("java", command[0])
                self.assertTrue(command[1].startswith("@"))
                contents = Path(command[1][1:]).read_text(encoding="utf-8")
                self.assertIn(json.dumps(classpath, ensure_ascii=False), contents.splitlines())
                self.assertEqual(json.dumps(run.MAIN), contents.splitlines()[-1])
                raise StopBeforeLaunch

            runtime = subprocess.CompletedProcess(["java", "-version"], 0, "", "test runtime")
            with patch.object(run.subprocess, "run", return_value=runtime), \
                    patch.object(run.subprocess, "check_output", return_value="test k6"), \
                    patch.object(run.subprocess, "Popen", side_effect=inspect_launch):
                with self.assertRaises(StopBeforeLaunch):
                    run.run(args)


if __name__ == "__main__":
    unittest.main()
