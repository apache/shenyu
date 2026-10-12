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

"""Offline tests for the native SQL harness; these do not execute databases."""

import importlib.util
import json
import os
import re
import textwrap
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import MagicMock, patch

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location("matrix", Path(__file__).with_name("native-sql-matrix.py"))
sys.dont_write_bytecode = True
matrix = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(matrix)
ROWS = """selector|idx_selector_plugin_id|plugin_id|1
permission|idx_permission_object_resource|object_id|1
permission|idx_permission_object_resource|resource_id|2
permission|idx_permission_resource_id|resource_id|1
resource|idx_resource_parent|parent_id|1
user_role|idx_user_role_user_role|user_id|1
user_role|idx_user_role_user_role|role_id|2
"""


def workflow_python_blocks():
    workflow = (ROOT / ".github/workflows/native-sql-matrix.yml").read_text(encoding="utf-8")
    return [textwrap.dedent(block) for block in re.findall(r"python3 - <<'PY'\n(.*?)\n\s+PY", workflow, re.S)]


class NativeSqlMatrixTest(unittest.TestCase):
    def test_aggregate_gate_rejects_any_failed_canceled_or_missing_sql_result(self):
        block = workflow_python_blocks()[-1]
        base = {"SCOPE_RESULT": "success", "RUNNER_RESULT": "success", "REQUIRED": "true",
                "LIST_RESULT": "success", "SQL_RESULT": "success"}
        with patch.dict(os.environ, base):
            exec(block, {})
        for key in ("SCOPE_RESULT", "RUNNER_RESULT", "LIST_RESULT", "SQL_RESULT"):
            for status in ("failure", "cancelled", "skipped", ""):
                with patch.dict(os.environ, dict(base, **{key: status})), self.assertRaises(AssertionError):
                    exec(block, {})

    def test_aggregate_not_applicable_requires_explicit_successful_scope(self):
        block = workflow_python_blocks()[-1]
        base = {"SCOPE_RESULT": "success", "RUNNER_RESULT": "success", "REQUIRED": "false",
                "LIST_RESULT": "skipped", "SQL_RESULT": "skipped"}
        with patch.dict(os.environ, base):
            exec(block, {})
        for value in ("", "unknown"):
            with patch.dict(os.environ, dict(base, REQUIRED=value)), self.assertRaises(AssertionError):
                exec(block, {})

    def test_workflow_scope_includes_sql_admin_and_harness_changes(self):
        block = workflow_python_blocks()[0]
        for path in ("db/upgrade/test.sql", "db/init/mysql/schema.sql", "shenyu-admin/src/test/java/Test.java",
                     ".github/scripts/native-sql-matrix.py", ".github/workflows/native-sql-matrix.yml", "README.md"):
            with tempfile.TemporaryDirectory() as directory:
                output = Path(directory) / "output"
                with patch.dict(os.environ, {"BASE_SHA": "1" * 40, "EVENT_NAME": "pull_request", "GITHUB_OUTPUT": str(output)}), \
                        patch.object(subprocess, "run"), patch.object(subprocess, "check_output", return_value=path):
                    exec(block, {})
                self.assertEqual(output.read_text().strip(), "required=" + str(path != "README.md").lower())

    def test_exactly_five_digest_pinned_native_engines(self):
        self.assertEqual(set(matrix.DIALECTS), {"mysql", "ob", "pg", "og", "oracle"})
        for image, schema in matrix.DIALECTS.values():
            self.assertRegex(image, r"@sha256:[0-9a-f]{64}$")
            self.assertTrue((ROOT / schema).is_file())
        self.assertRegex(matrix.BASELINE, r"^[0-9a-f]{40}$")

    def test_oceanbase_shares_current_mysql_schema_but_keeps_released_baseline(self):
        self.assertEqual(matrix.DIALECTS["mysql"][1], matrix.DIALECTS["ob"][1])
        engine = MagicMock()
        engine.sql.return_value = "unchanged"
        with tempfile.TemporaryDirectory() as directory:
            with patch.object(matrix, "Engine", return_value=engine), \
                    patch.object(matrix, "run", return_value="-- released OceanBase schema") as invoke:
                matrix.execute_matrix("ob", ROOT, Path(directory))
        invoke.assert_called_once_with(["git", "show", f"{matrix.BASELINE}:db/init/ob/schema.sql"])
        self.assertEqual(engine.sql.call_args_list[0].args[0], "-- released OceanBase schema")
        self.assertIn(unittest.mock.call((ROOT / "db/upgrade/2.7.1-upgrade-2.7.2-ob.sql").read_text(encoding="utf-8")),
                      engine.sql.call_args_list)
        schema = (ROOT / "db/init/mysql/schema.sql").read_text(encoding="utf-8")
        self.assertIn(unittest.mock.call(schema, database=False), engine.sql.call_args_list)
        engine.check_fresh_mysql_schema.assert_called_once_with(schema, engine.indexes.return_value)

    def test_fresh_shared_schema_rejects_missing_table_index_and_constraint(self):
        schema = "CREATE TABLE IF NOT EXISTS plugin (id int); CREATE INDEX idx_plugin ON `plugin` (id);"
        with tempfile.TemporaryDirectory() as directory:
            engine = matrix.Engine("ob", "fresh", Path(directory))
            with patch.object(engine, "sql", return_value=""), self.assertRaisesRegex(AssertionError, "Fresh tables"):
                engine.check_fresh_mysql_schema(schema, {})
            with patch.object(engine, "sql", return_value="plugin"), self.assertRaisesRegex(AssertionError, "secondary index"):
                engine.check_fresh_mysql_schema(schema, {})
            with patch.object(engine, "sql", return_value="plugin"), self.assertRaisesRegex(AssertionError, "unique constraint"):
                engine.check_fresh_mysql_schema(schema, {("plugin", "idx_plugin"): ["id"]})

    def test_fresh_shared_schema_rejects_orphan_and_duplicate_seeds(self):
        unique = "\n".join(("dashboard_user|unique_user_name", "proxy_api_key_mapping|uk_selector_proxy_key",
                            "plugin_handle|plugin_id_field_type", "shenyu_dict|dict_type_dict_code_dict_name",
                            "discovery_upstream|discovery_upstream_discovery_handler_id_IDX"))
        for violation, label in (("FROM permission child LEFT JOIN resource", "permission.resource_id orphan"),
                                 ("FROM plugin_handle GROUP BY", "plugin_handle duplicate natural key"),
                                 ("FROM resource r LEFT JOIN permission", "missing admin resource permission"),
                                 ("FROM plugin p LEFT JOIN namespace_plugin_rel", "missing default namespace plugin")):
            def sql(query):
                if "information_schema.tables" in query:
                    return "plugin"
                if "information_schema.statistics" in query:
                    return unique
                return "1" if violation in query else "0"

            with self.subTest(violation=violation), tempfile.TemporaryDirectory() as directory:
                engine = matrix.Engine("mysql", "fresh", Path(directory))
                with patch.object(engine, "sql", side_effect=sql), self.assertRaisesRegex(AssertionError, label):
                    engine.check_fresh_mysql_schema("CREATE TABLE `plugin` (id int);", {})

    def test_actual_mapper_projects_metadata_and_preserves_detail_jar(self):
        listing, detail = matrix.fragment_columns(ROOT / "shenyu-admin/src/main/resources/mappers/plugin-sqlmap.xml")
        self.assertNotIn("plugin_jar", listing)
        self.assertEqual(detail, listing + ["plugin_jar"])

    def test_reject_list_blob_regression(self):
        xml = '<mapper><sql id="List_Column_List">id,plugin_jar</sql><sql id="Base_Column_List">id,plugin_jar</sql></mapper>'
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "mapper.xml"
            path.write_text(xml, encoding="utf-8")
            with self.assertRaises(AssertionError):
                matrix.fragment_columns(path)

    def test_reject_recursive_fragment(self):
        xml = '<mapper><sql id="List_Column_List"><include refid="List_Column_List"/></sql><sql id="Base_Column_List">id</sql></mapper>'
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "mapper.xml"
            path.write_text(xml, encoding="utf-8")
            with self.assertRaisesRegex(AssertionError, "Recursive"):
                matrix.fragment_columns(path)

    def test_mysql_and_oracle_catalogs_check_ordinal_position(self):
        indexes = matrix.verify_index_rows(ROWS)
        self.assertEqual(indexes[("permission", "idx_permission_object_resource")], ["object_id", "resource_id"])

    def test_composite_nonleading_column_is_not_an_equivalent_index(self):
        with self.assertRaisesRegex(AssertionError, "permission.resource_id"):
            matrix.verify_index_rows(ROWS.replace("permission|idx_permission_resource_id|resource_id|1\n", ""))

    def test_wrong_index_column_fails(self):
        with self.assertRaises(AssertionError):
            matrix.verify_index_rows(ROWS.replace("idx_selector_plugin_id|plugin_id", "idx_selector_plugin_id|namespace_id"))

    def test_pg_and_og_catalogs(self):
        rows = "\n".join([
            "selector|idx_selector_plugin_id|CREATE INDEX idx_selector_plugin_id ON public.selector USING btree (plugin_id)",
            "permission|idx_permission_resource_id|CREATE INDEX idx_permission_resource_id ON public.permission USING btree (resource_id)",
            "permission|permission_object|CREATE INDEX permission_object ON public.permission USING btree (object_id, resource_id)",
            "resource|resource_parent|CREATE INDEX resource_parent ON public.resource USING btree (parent_id)",
            "user_role|user_role_user|CREATE INDEX user_role_user ON public.user_role USING btree (user_id, role_id)",
        ])
        self.assertEqual(matrix.verify_index_rows(rows, True)[("selector", "idx_selector_plugin_id")], ["plugin_id"])
        opengauss_rows = "\n".join(line + " TABLESPACE pg_default" for line in rows.splitlines())
        self.assertEqual(matrix.verify_index_rows(opengauss_rows, True)[("selector", "idx_selector_plugin_id")], ["plugin_id"])
        partial_rows = rows.replace("(plugin_id)", "(plugin_id) WHERE plugin_id IS NOT NULL")
        with self.assertRaises(AssertionError):
            matrix.verify_index_rows(partial_rows, True)

    def test_oceanbase_waits_for_writable_ddl_not_only_select(self):
        with tempfile.TemporaryDirectory() as directory:
            engine = matrix.Engine("ob", "upgrade", Path(directory))
            with patch.object(matrix, "run") as invoke, patch.object(matrix.time, "monotonic", side_effect=[0, 1, 2]), \
                    patch.object(matrix.time, "sleep") as sleep, \
                    patch.object(engine, "sql", side_effect=["1", RuntimeError("4179 creating tenant"), "1", ""]) as sql:
                engine.start()
                self.assertEqual(sql.call_count, 4)
                self.assertIn("CREATE DATABASE IF NOT EXISTS sql_matrix_ready", sql.call_args_list[1].args[0])
                self.assertIn("DROP DATABASE sql_matrix_ready", sql.call_args_list[3].args[0])
                command = invoke.call_args_list[1].args[0]
                self.assertIn("--ulimit", command)
                self.assertIn("nofile=65536:65536", command)
                sleep.assert_called_once_with(5)

    def test_scalars_reject_empty_or_multiple_results(self):
        self.assertEqual(matrix.scalar("\n 1 \n"), "1")
        for output in ("", "1\n2", "ERROR\n1"):
            with self.assertRaises(AssertionError):
                matrix.scalar(output)

    def test_native_clients_fail_on_sql_error(self):
        with tempfile.TemporaryDirectory() as directory:
            for dialect in matrix.DIALECTS:
                engine = matrix.Engine(dialect, "upgrade", Path(directory))
                with patch.object(matrix, "run", return_value="1") as invoke:
                    self.assertEqual(engine.sql("SELECT 1;"), "1")
                    command, sql, _ = invoke.call_args.args
                    self.assertNotIn("--force", command)
                    if dialect in ("pg", "og"):
                        self.assertIn("ON_ERROR_STOP=1", command)
                    if dialect == "oracle":
                        self.assertIn("whenever sqlerror exit failure rollback", sql)
                        self.assertIn("whenever oserror exit failure", sql)
                        self.assertIn("set define off", sql)
                        self.assertIn(f'sqlmatrix/"{matrix.PASSWORD}"@//localhost:1521/FREEPDB1', command)

    def test_oracle_client_errors_are_not_false_success(self):
        with tempfile.TemporaryDirectory() as directory:
            engine = matrix.Engine("oracle", "upgrade", Path(directory))
            for output in ("ORA-00942: missing table", "SP2-0042: unknown command", "TNS-12541: no listener"):
                with patch.object(matrix, "run", return_value=output), self.assertRaises(RuntimeError):
                    engine.sql("bad SQL;")

    def test_subprocess_nonzero_sql_exit_is_failure(self):
        result = subprocess.CompletedProcess([], 1, "syntax error")
        with patch.object(subprocess, "run", return_value=result), self.assertRaisesRegex(RuntimeError, "syntax error"):
            matrix.run(["fake-client"])

    def test_failed_start_is_recorded_and_cleaned_up(self):
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory)
            with patch.object(matrix.Engine, "start", side_effect=RuntimeError("engine unavailable")), \
                    patch.object(matrix.Engine, "cleanup") as cleanup:
                with self.assertRaisesRegex(RuntimeError, "engine unavailable"):
                    matrix.execute_matrix("mysql", ROOT, output)
                cleanup.assert_called_once()
                results = json.loads((output / "result.json").read_text())
                self.assertEqual(results["flows"][0]["status"], "failure")
                self.assertIn("engine unavailable", results["flows"][0]["error"])

    def test_corrupted_jar_is_a_failure(self):
        with tempfile.TemporaryDirectory() as directory:
            engine = matrix.Engine("mysql", "upgrade", Path(directory))
            with patch.object(engine, "sql", side_effect=[matrix.SENTINEL, matrix.SENTINEL, "deadbeef"]):
                with self.assertRaisesRegex(AssertionError, "JAR bytes changed"):
                    engine.check_data(["id"], ["id", "plugin_jar"])

    def test_deleted_rows_are_detected_even_if_total_count_does_not_decrease(self):
        engine = MagicMock()
        migrated = False

        def sql(query, **kwargs):
            nonlocal migrated
            if "idx_selector_plugin_id" in query:
                migrated = True
            if query.startswith("SELECT id FROM"):
                return "new-id" if migrated else "old-id"
            return "unchanged-list-metadata"

        engine.sql.side_effect = sql
        with tempfile.TemporaryDirectory() as directory:
            with patch.object(matrix, "Engine", return_value=engine), patch.object(matrix, "run", return_value="-- baseline"):
                with self.assertRaisesRegex(AssertionError, "removed rows"):
                    matrix.execute_matrix("mysql", ROOT, Path(directory))
                engine.cleanup.assert_called_once()
                result = json.loads((Path(directory) / "result.json").read_text())
                self.assertEqual(result["flows"][0]["status"], "failure")

    def test_oracle_uses_quoted_resource_table(self):
        self.assertEqual(matrix.Engine("oracle", "upgrade", ROOT).table("resource"), '"resource"')
        self.assertEqual(matrix.Engine("mysql", "upgrade", ROOT).table("resource"), "resource")


if __name__ == "__main__":
    unittest.main()
