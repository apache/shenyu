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

"""Regression checks for the added-line filter of the new code quality gate."""

import importlib.util
import unittest
from pathlib import Path

SCRIPT = Path(__file__).with_name("check-new-code-quality.py")
SPEC = importlib.util.spec_from_file_location("check_new_code_quality", SCRIPT)
GATE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(GATE)

DIFF = """diff --git a/a/src/main/java/A.java b/a/src/main/java/A.java
index 1..2 100644
--- a/a/src/main/java/A.java
+++ b/a/src/main/java/A.java
@@ -10,0 +11,3 @@ class A {
+    int x = 3000;
+    int y = 4000;
+    int z = 5000;
@@ -20 +23 @@ class A {
-    old();
+    replaced(42);
@@ -30,2 +33,0 @@ class A {
-    removed();
-    removed();
diff --git a/b/src/main/java/New.java b/b/src/main/java/New.java
new file mode 100644
--- /dev/null
+++ b/b/src/main/java/New.java
@@ -0,0 +1,2 @@
+class New {
+}
diff --git a/c/src/main/java/Gone.java b/c/src/main/java/Gone.java
deleted file mode 100644
--- a/c/src/main/java/Gone.java
+++ /dev/null
@@ -1 +0,0 @@
-class Gone {}
"""


class AddedLinesTest(unittest.TestCase):

    def test_hunks_map_to_added_lines_only(self):
        added = GATE.parse_added_lines(DIFF)
        self.assertEqual({11, 12, 13, 23}, added["a/src/main/java/A.java"])
        self.assertEqual({1, 2}, added["b/src/main/java/New.java"])
        self.assertNotIn("c/src/main/java/Gone.java", added)


class CandidateTest(unittest.TestCase):

    def test_only_production_java_sources_are_checked(self):
        self.assertTrue(GATE.is_candidate("shenyu-web/src/main/java/org/apache/shenyu/web/A.java"))
        self.assertFalse(GATE.is_candidate("shenyu-web/src/test/java/org/apache/shenyu/web/ATest.java"))
        self.assertFalse(GATE.is_candidate("shenyu-web/src/main/resources/application.yml"))
        self.assertFalse(GATE.is_candidate("shenyu-admin/src/main/java/org/apache/shenyu/admin/transfer/X.java"))
        self.assertFalse(GATE.is_candidate("shenyu-x/src/main/java/generated/Y.java"))


class ReportFilterTest(unittest.TestCase):

    def test_only_violations_on_added_lines_are_kept(self):
        root = Path("/repo")
        report = """<?xml version="1.0" encoding="UTF-8"?>
<checkstyle version="9.3">
<file name="/repo/a/src/main/java/A.java">
<error line="12" column="13" severity="error" message="[Q1] &apos;4000&apos; is a magic number." source="Q1"/>
<error line="40" column="5" severity="error" message="[Q3] old method" source="Q3"/>
</file>
<file name="/repo/b/src/main/java/Untouched.java">
<error line="1" column="1" severity="error" message="[Q1] x" source="Q1"/>
</file>
</checkstyle>"""
        kept = GATE.filter_added(GATE.parse_report(report, root), {"a/src/main/java/A.java": {11, 12, 13}})
        self.assertEqual([GATE.Violation("a/src/main/java/A.java", 12, 13, "Q1",
                                         "[Q1] '4000' is a magic number.")], kept)

    def test_annotations_escape_workflow_command_characters(self):
        violation = GATE.Violation("a,b:c/A.java", 3, 4, "Q2", "100% done\nnext")
        self.assertEqual("::warning file=a%2Cb%3Ac/A.java,line=3,col=4,title=New code quality Q2::100%25 done%0Anext",
                         GATE.annotation(violation, "warning"))


if __name__ == "__main__":
    unittest.main()
