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

"""Check Agent Gateway seed parity; not a native database syntax validator."""

import re
import sys
from pathlib import Path

DIALECTS = ("mysql", "ob", "og", "oracle", "pg")
EXPECTED = {
    "plugin": {"68"},
    "plugin_handle": {str(1960000000000001000 + i) for i in range(2)},
    "resource": {str(1960000000000001010 + i) for i in range(10)},
    "permission": {str(1960000000000001020 + i) for i in range(10)},
    "namespace_plugin_rel": {"1960000000000001030"},
}
# The seed statements use SQL literals, including JSON strings and Oracle hints.
# Tokenize quoted text before stripping comments or normalizing whitespace.
TOKEN = re.compile(r"'(?:''|[^'])*'|/\*.*?\*/|--[^\n]*|[^\s]", re.S)
INSERT = re.compile(
    r'INSERT\s+(?:/\*.*?\*/\s*)?INTO\s+(?:"public"\.)?'
    r'[`"]?(\w+)[`"]?\s*(.*?)\bVALUES\s*\(\s*\'(\d+)\'', re.I | re.S
)


def seeds(sql):
    """Extract exactly the reserved seed keys, rejecting duplicates and omissions."""
    result = {}
    # Remove disabled statements without touching quoted values or Oracle hints.
    sql = re.sub(r"'(?:''|[^'])*'|/\*.*?\*/|--[^\n]*",
                 lambda m: m.group() if m.group().startswith(("'", "/*+")) else " ", sql, flags=re.S)
    for statement in re.finditer(
            r"INSERT\s+(?:/\*.*?\*/\s*)?INTO\s+(?:'(?:''|[^'])*'|[^;'])*;", sql, re.I | re.S):
        text = statement.group()
        match = INSERT.match(text)
        if not match:
            continue
        table, _, row_id = match.groups()
        table = table.lower()
        if table not in EXPECTED or row_id not in EXPECTED[table]:
            continue
        key = (table, row_id)
        if key in result:
            raise ValueError(f"duplicate seed {key}")
        # Ignore formatting only; keep column lists, values, case and SQL hints.
        result[key] = tuple(m.group() for m in TOKEN.finditer(text))
    required = {(table, row_id) for table, ids in EXPECTED.items() for row_id in ids}
    if result.keys() != required:
        raise ValueError(f"missing seeds: {sorted(required - result.keys())}")
    return result


def check_pair(schema, migration):
    """Compare every value and column list in each dialect's own seed records."""
    expected, actual = seeds(schema), seeds(migration)
    for key in expected:
        if expected[key] != actual[key]:
            raise ValueError(f"fresh-install/upgrade mismatch: {key}")
    return len(expected)


def check(root):
    """Verify all supported upgrade dialects, regardless of working directory."""
    for dialect in DIALECTS:
        filename = "create-table.sql" if dialect in ("og", "pg") else "schema.sql"
        schema_dialect = "pg" if dialect == "og" else dialect
        schema = root / "db" / "init" / schema_dialect / filename
        migration = root / "db" / "upgrade" / f"2.7.1-upgrade-2.7.2-{dialect}.sql"
        try:
            count = check_pair(schema.read_text(encoding="utf-8"), migration.read_text(encoding="utf-8"))
        except ValueError as error:
            raise ValueError(f"{dialect}: {error}") from error
        print(f"{dialect}: {count} Agent Gateway seed rows match")


if __name__ == "__main__":
    try:
        check(Path(__file__).resolve().parents[2])
    except (OSError, ValueError) as error:
        print(str(error), file=sys.stderr)
        sys.exit(1)
