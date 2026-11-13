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

"""Report stricter Checkstyle findings only on lines added by the current change.

The build-wide rules in script/shenyu_checkstyle.xml apply to the whole tree. The
rules in script/shenyu_checkstyle_new_code.xml (magic numbers, complexity, ...)
would flag a large amount of existing code, so this script runs them on the
changed production sources and keeps only the findings on added lines.

Method-level checks (length, complexity, parameters) are reported by Checkstyle on
the method declaration line, so they apply to new methods and to methods whose
declaration line changed.
"""

import argparse
import os
import re
import subprocess
import sys
import xml.etree.ElementTree as ElementTree
from pathlib import Path
from typing import Dict, Iterable, List, NamedTuple, Optional, Set

ROOT = Path(__file__).resolve().parents[2]
DEFAULT_CONFIG = ROOT / "script" / "shenyu_checkstyle_new_code.xml"
HUNK_HEADER = re.compile(r"^@@ -\d+(?:,\d+)? \+(\d+)(?:,(\d+))? @@")
# Mirror the excludes of the build-wide maven-checkstyle-plugin configuration.
EXCLUDED_SEGMENTS = ("/transfer/", "/generated")
FILE_BATCH_SIZE = 200


class Violation(NamedTuple):
    path: str
    line: int
    column: int
    rule: str
    message: str


def is_candidate(path: str) -> bool:
    """Return whether a changed file is a production Java source checked by this gate."""
    normalized = "/" + path.replace(os.sep, "/")
    return (normalized.endswith(".java")
            and "/src/main/java/" in normalized
            and not any(segment in normalized for segment in EXCLUDED_SEGMENTS))


def parse_added_lines(diff_text: str) -> Dict[str, Set[int]]:
    """Map each file of a zero-context unified diff to the line numbers it adds."""
    added: Dict[str, Set[int]] = {}
    current: Optional[Set[int]] = None
    for line in diff_text.splitlines():
        if line.startswith("+++ "):
            target = line[4:]
            if target == "/dev/null":
                current = None
                continue
            target = target[2:] if target.startswith("b/") else target
            current = added.setdefault(target, set())
            continue
        match = HUNK_HEADER.match(line)
        if match and current is not None:
            start = int(match.group(1))
            count = 1 if match.group(2) is None else int(match.group(2))
            current.update(range(start, start + count))
    return {path: lines for path, lines in added.items() if lines}


def parse_report(report_xml: str, root: Path) -> List[Violation]:
    """Read a Checkstyle XML report into violations with repository-relative paths."""
    violations = []
    for file_element in ElementTree.fromstring(report_xml).iter("file"):
        name = Path(file_element.get("name", ""))
        try:
            relative = name.resolve().relative_to(root.resolve()).as_posix()
        except ValueError:
            relative = name.as_posix()
        for error in file_element.iter("error"):
            violations.append(Violation(relative, int(error.get("line", "0")), int(error.get("column", "0")),
                                        error.get("source", ""), error.get("message", "")))
    return violations


def filter_added(violations: Iterable[Violation], added: Dict[str, Set[int]]) -> List[Violation]:
    """Keep the violations located on added lines, in a stable order."""
    kept = [violation for violation in violations if violation.line in added.get(violation.path, set())]
    return sorted(kept, key=lambda violation: (violation.path, violation.line, violation.column, violation.rule))


def escape_data(value: str) -> str:
    return value.replace("%", "%25").replace("\r", "%0D").replace("\n", "%0A")


def escape_property(value: str) -> str:
    return escape_data(value).replace(":", "%3A").replace(",", "%2C")


def annotation(violation: Violation, level: str) -> str:
    """Format a GitHub Actions workflow command for one violation."""
    properties = "file={},line={},col={},title={}".format(
        escape_property(violation.path), violation.line, violation.column,
        escape_property("New code quality " + (violation.rule or "rule")))
    return "::{} {}::{}".format(level, properties, escape_data(violation.message))


def git(*args: str) -> str:
    return subprocess.run(["git", *args], cwd=ROOT, check=True, capture_output=True, text=True).stdout


def changed_candidates(merge_base: str) -> List[str]:
    names = git("diff", "--name-only", "--diff-filter=AMR", "--no-renames", merge_base, "HEAD", "--", "*.java")
    return [name for name in names.splitlines() if name and is_candidate(name)]


def run_checkstyle(java: str, jar: str, config: Path, files: List[str]) -> List[Violation]:
    violations: List[Violation] = []
    for start in range(0, len(files), FILE_BATCH_SIZE):
        batch = files[start:start + FILE_BATCH_SIZE]
        result = subprocess.run([java, "-Duser.language=en", "-Duser.country=US", "-jar", jar,
                                 "-c", str(config), "-f", "xml", *batch],
                                cwd=ROOT, capture_output=True, text=True)
        try:
            violations.extend(parse_report(result.stdout, ROOT))
        except ElementTree.ParseError as error:
            raise RuntimeError("Checkstyle did not produce a report (exit {}): {}".format(
                result.returncode, result.stderr.strip() or error)) from error
    return violations


def write_summary(violations: List[Violation], mode: str) -> None:
    summary = os.environ.get("GITHUB_STEP_SUMMARY")
    if not summary:
        return
    with open(summary, "a", encoding="utf-8") as output:
        output.write("### New code quality ({})\n\n".format(mode))
        if not violations:
            output.write("No findings on added lines.\n")
            return
        output.write("| File | Line | Rule | Message |\n|---|---|---|---|\n")
        for violation in violations:
            output.write("| `{}` | {} | {} | {} |\n".format(violation.path, violation.line, violation.rule,
                                                         violation.message.replace("|", "\\|")))


def main(argv: Optional[List[str]] = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--base", default="origin/master", help="base ref of the change")
    parser.add_argument("--checkstyle-jar", required=True, help="path to a checkstyle-<version>-all.jar")
    parser.add_argument("--java", default="java", help="java executable")
    parser.add_argument("--config", default=str(DEFAULT_CONFIG), help="Checkstyle configuration")
    parser.add_argument("--mode", choices=("advisory", "enforce"), default="advisory",
                        help="advisory reports findings and exits 0; enforce exits 1 on findings")
    args = parser.parse_args(argv)

    merge_base = git("merge-base", args.base, "HEAD").strip()
    files = changed_candidates(merge_base)
    if not files:
        print("No changed production Java sources.")
        write_summary([], args.mode)
        return 0
    added = parse_added_lines(git("diff", "-U0", "--no-color", "--no-ext-diff", "--no-renames",
                                  merge_base, "HEAD", "--", *files))
    violations = filter_added(run_checkstyle(args.java, args.checkstyle_jar, Path(args.config), files), added)

    level = "error" if args.mode == "enforce" else "warning"
    for violation in violations:
        print(annotation(violation, level) if os.environ.get("GITHUB_ACTIONS") == "true"
              else "{}:{}:{}: {}".format(violation.path, violation.line, violation.column, violation.message))
    write_summary(violations, args.mode)
    print("{} finding(s) on added lines in {} changed file(s).".format(len(violations), len(files)))
    return 1 if violations and args.mode == "enforce" else 0


if __name__ == "__main__":
    sys.exit(main())
