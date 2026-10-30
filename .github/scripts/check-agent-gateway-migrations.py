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
RELEASED_ORDER = 198
CURRENT_ORDER = 169
ORDER_KEYS = {
    ("plugin", "68"),
    ("namespace_plugin_rel", "1960000000000001030"),
}
EXPECTED = {
    "plugin": {"68"},
    "plugin_handle": {str(1960000000000001000 + i) for i in range(2)},
    "resource": {str(1960000000000001010 + i) for i in range(10)},
    "permission": {str(1960000000000001020 + i) for i in range(10)},
    "namespace_plugin_rel": {"1960000000000001030"},
}
# The seed statements use SQL literals, including JSON strings and Oracle hints.
# Tokenize quoted text before stripping comments or normalizing whitespace.
TOKEN = re.compile(r"'(?:''|[^'])*'|/\*.*?\*/|--[^\n]*|\bNULL\b|[^\s]", re.S | re.I)
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
        # NULL is case-insensitive; preserve quoted values, column lists and hints.
        result[key] = tuple("NULL" if m.group().lower() == "null" else m.group() for m in TOKEN.finditer(text))
    required = {(table, row_id) for table, ids in EXPECTED.items() for row_id in ids}
    if result.keys() != required:
        raise ValueError(f"missing seeds: {sorted(required - result.keys())}")
    return result


def agent_order_token(values):
    """Return the Agent Gateway sort token range and value."""
    tokens = normalized_tokens(values)
    if "'agentGateway'" in tokens:
        marker = tokens.index("'agentGateway'")
    else:
        marker = tokens.index("'68'")
    order_start = next(index for index, token in enumerate(tokens[marker + 1:], marker + 1) if token.isdigit())
    order_end = order_start + 1
    while order_end < len(tokens) and tokens[order_end].isdigit() and order_end - order_start < 3:
        order_end += 1
    return (order_start, order_end), int("".join(tokens[order_start:order_end]))


def normalized_tokens(values):
    """Normalize tokenizer character runs into complete numeric literals."""
    tokens = []
    number = []

    def flush_number():
        if number:
            tokens.append("".join(number))
            number.clear()

    for token in values:
        if len(token) == 1 and token.isdigit():
            number.append(token)
        else:
            flush_number()
            tokens.append(token)
    flush_number()
    return tuple(tokens)


def only_order_moved(expected, actual):
    """Whether two seeds differ in exactly the allowed Agent Gateway order value."""
    expected = normalized_tokens(expected)
    actual = normalized_tokens(actual)
    (expected_start, expected_end), expected_order = agent_order_token(expected)
    (actual_start, actual_end), actual_order = agent_order_token(actual)
    return (
        expected_order == CURRENT_ORDER
        and actual_order == RELEASED_ORDER
        and len(expected) == len(actual)
        and all(expected[index] == actual[index]
                for index in range(len(expected))
                if not (expected_start <= index < expected_end or actual_start <= index < actual_end))
    )


def check_pair(schema, migration):
    """Compare released seeds, allowing only the deliberate 2.7.3 order move."""
    expected, actual = seeds(schema), seeds(migration)
    for key in expected:
        if expected[key] == actual[key]:
            continue
        if key in ORDER_KEYS:
            if only_order_moved(expected[key], actual[key]):
                continue
        raise ValueError(f"fresh-install/upgrade mismatch: {key}")
    return len(expected)


def check(root):
    """Verify all supported upgrade dialects, regardless of working directory."""
    for dialect in DIALECTS:
        filename = "create-table.sql" if dialect in ("og", "pg") else "schema.sql"
        schema_dialect = {"ob": "mysql", "og": "pg"}.get(dialect, dialect)
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
