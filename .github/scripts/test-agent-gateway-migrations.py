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

"""Regression tests for Agent Gateway migration/schema consistency checks."""

import importlib.util
from pathlib import Path
import sys
import tempfile
import unittest
from unittest.mock import patch

sys.dont_write_bytecode = True

HERE = Path(__file__).resolve().parent
SPEC = importlib.util.spec_from_file_location("migration_check", HERE / "check-agent-gateway-migrations.py")
CHECK = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(CHECK)
ROOT = HERE.parents[1]


class MigrationConsistencyTest(unittest.TestCase):
    def setUp(self):
        self.schema = (ROOT / "db/init/mysql/schema.sql").read_text(encoding="utf-8")
        self.migration = (ROOT / "db/upgrade/2.7.1-upgrade-2.7.2-mysql.sql").read_text(encoding="utf-8")

    def test_all_dialects_match(self):
        CHECK.check(ROOT)

    def test_opengauss_uses_shared_schema_and_its_own_upgrade(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            schema = root / "db/init/pg/create-table.sql"
            migration = root / "db/upgrade/2.7.1-upgrade-2.7.2-og.sql"
            schema.parent.mkdir(parents=True)
            migration.parent.mkdir(parents=True)
            schema.write_text((ROOT / "db/init/pg/create-table.sql").read_text(encoding="utf-8"), encoding="utf-8")
            released_upgrade = (ROOT / "db/upgrade/2.7.1-upgrade-2.7.2-og.sql").read_text(encoding="utf-8")
            migration.write_text(released_upgrade, encoding="utf-8")
            with patch.object(CHECK, "DIALECTS", ("og",)):
                CHECK.check(root)
                migration.write_text(released_upgrade.replace("plugin:agentGatewayRule:delete", "plugin:agentGatewayRule:edit"), encoding="utf-8")
                with self.assertRaisesRegex(ValueError, "og: fresh-install/upgrade mismatch"):
                    CHECK.check(root)

    def test_missing_each_required_record_is_detected(self):
        for table, ids in CHECK.EXPECTED.items():
            for row_id in ids:
                with self.subTest(table=table, row_id=row_id):
                    changed = self.migration.replace(f"VALUES ('{row_id}'", "VALUES ('missing'", 1)
                    with self.assertRaisesRegex(ValueError, "missing seeds"):
                        CHECK.check_pair(self.schema, changed)

    def test_duplicate_is_detected(self):
        with self.assertRaisesRegex(ValueError, "duplicate seed"):
            CHECK.check_pair(self.schema, self.migration + self.migration)

    def test_changed_permission_is_detected(self):
        changed = self.migration.replace("plugin:agentGatewayRule:delete", "plugin:agentGatewayRule:edit")
        with self.assertRaisesRegex(ValueError, "mismatch"):
            CHECK.check_pair(self.schema, changed)

    def test_changed_default_is_detected(self):
        changed = self.migration.replace('"defaultValue":"LLM"', '"defaultValue":"MCP"')
        with self.assertRaisesRegex(ValueError, "mismatch"):
            CHECK.check_pair(self.schema, changed)

    def test_formatting_is_ignored(self):
        self.assertEqual(24, CHECK.check_pair(self.schema, self.migration.replace("VALUES (", "VALUES\n  (")))

    def test_commented_out_migration_is_rejected(self):
        disabled = "\n".join("-- " + line for line in self.migration.splitlines())
        with self.assertRaisesRegex(ValueError, "missing seeds"):
            CHECK.check_pair(self.schema, disabled)

    def test_both_sides_missing_is_rejected(self):
        with self.assertRaisesRegex(ValueError, "missing seeds"):
            CHECK.check_pair("", "")


if __name__ == "__main__":
    unittest.main()
