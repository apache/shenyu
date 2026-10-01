# Licensed to the Apache Software Foundation (ASF) under one or more
# contributor license agreements.  See the NOTICE file distributed with
# this work for additional information regarding copyright ownership.
# The ASF licenses this file to you under the Apache License, Version 2.0
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

"""Verify legacy Spring Boot auto-configuration against current imports."""

import argparse
import subprocess
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parent.parent
STARTER_ROOT = "shenyu-spring-boot-starter/"
FACTORIES_SUFFIX = "/src/main/resources/META-INF/spring.factories"
IMPORTS_SUFFIX = "/src/main/resources/META-INF/spring/org.springframework.boot.autoconfigure.AutoConfiguration.imports"
AUTO_CONFIGURATION_KEY = "org.springframework.boot.autoconfigure.EnableAutoConfiguration"


def run_git(*args):
    """Run git and return its UTF-8 output."""
    return subprocess.run(
        ["git", *args], cwd=ROOT, check=True, capture_output=True, text=True
    ).stdout


def read_properties(text):
    """Read the property lines needed from a Java properties file."""
    logical_lines = []
    pending = ""
    for line in text.splitlines():
        continuation = bool(pending)
        pending += line.lstrip() if continuation else line.rstrip()
        if pending.endswith("\\"):
            pending = pending[:-1]
            continue
        logical_lines.append(pending)
        pending = ""
    if pending:
        logical_lines.append(pending)

    properties = {}
    for line in logical_lines:
        stripped = line.lstrip()
        if not stripped or stripped.startswith(("#", "!")):
            continue
        separator = next((index for index, char in enumerate(line) if char in "=:"), -1)
        if separator < 0:
            continue
        key = line[:separator].strip()
        value = line[separator + 1:].strip()
        properties[key] = value
    return properties


def parse_args():
    """Parse command-line arguments."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "legacy_revision",
        help="Git revision containing the legacy spring.factories files",
    )
    return parser.parse_args()


def main():
    """Compare every non-client legacy registration with modern imports."""
    args = parse_args()
    paths = run_git(
        "ls-tree", "-r", "--name-only", args.legacy_revision, "--", STARTER_ROOT
    ).splitlines()
    factories_paths = [
        path for path in paths
        if path.endswith(FACTORIES_SUFFIX)
        and "/shenyu-spring-boot-starter-client/" not in f"/{path}"
    ]

    errors = []
    checked_entries = 0
    for factories_path in factories_paths:
        legacy_text = run_git("show", f"{args.legacy_revision}:{factories_path}")
        properties = read_properties(legacy_text)
        unexpected_keys = set(properties) - {AUTO_CONFIGURATION_KEY}
        if unexpected_keys:
            errors.append(f"{factories_path}: unexpected keys {sorted(unexpected_keys)}")
            continue

        legacy_classes = {
            name.strip()
            for name in properties.get(AUTO_CONFIGURATION_KEY, "").split(",")
            if name.strip()
        }
        module_path = factories_path.removesuffix(FACTORIES_SUFFIX)
        imports_path = ROOT / f"{module_path}{IMPORTS_SUFFIX}"
        if not imports_path.is_file():
            errors.append(f"{factories_path}: current imports file is missing")
            continue
        modern_classes = {
            line.strip()
            for line in imports_path.read_text(encoding="utf-8").splitlines()
            if line.strip() and not line.lstrip().startswith("#")
        }
        if legacy_classes != modern_classes:
            errors.append(
                f"{factories_path}: legacy-only={sorted(legacy_classes - modern_classes)}, "
                f"imports-only={sorted(modern_classes - legacy_classes)}"
            )
        checked_entries += len(legacy_classes)

    if errors:
        print("\n".join(errors), file=sys.stderr)
        return 1
    print(f"Verified {len(factories_paths)} legacy files and {checked_entries} entries.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
