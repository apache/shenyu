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

"""Execute fresh schemas and the complete released-2.7.1 upgrade on native engines."""

import argparse
import json
import re
import subprocess
import time
import uuid
import xml.etree.ElementTree as ET
from pathlib import Path

BASELINE = "218c5634ebffb1f0ce7e8ea921b85cd5a633cde7"  # v2.7.1, peeled commit
PASSWORD = "SqlMatrix@2026"
SENTINEL = "sql-matrix-plugin"
JAR_HEX = "0001027fff"
DIALECTS = {
    "mysql": ("mysql:8.0@sha256:7dcddc01f13bab2f15cde676d44d01f61fc9f99fe7785e86196dfc07d358ae2b", "db/init/mysql/schema.sql"),
    "ob": ("oceanbase/oceanbase-ce:4.3.5-lts@sha256:31086a6900c21c479c2bcd942b6a28c53b17a51f4e9b9eb8eafcc596adfcd2e3", "db/init/ob/schema.sql"),
    "pg": ("postgres:15@sha256:724292da1f2e50bdccfc3302ce75bbba7f4a6076701b588cc795fcac65683550", "db/init/pg/create-table.sql"),
    "og": ("enmotech/opengauss-lite:5.0.1@sha256:b0e9d4e7452007cb0a216e39f1927c889178dac695176af7ab0eec43ce077af3", "db/init/pg/create-table.sql"),
    "oracle": ("gvenzl/oracle-free:23.26.3-slim@sha256:6d61d267a3b978c24c5ac1790e62e927416a0aec446bd86e4b3a1527562757bd", "db/init/oracle/schema.sql"),
}
TABLES = ("plugin", "selector", "rule", "resource", "permission", "user_role", "namespace_plugin_rel")
REQUIRED = {"selector": "plugin_id", "resource": "parent_id", "user_role": "user_id"}


def schema_source(dialect, flow):
    # Upgrade compatibility is checked against the released openGauss schema.
    if dialect == "og" and flow == "upgrade":
        return "db/init/og/create-table.sql"
    return DIALECTS[dialect][1]


def run(command, data=None, timeout=180):
    result = subprocess.run(command, input=data, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                            text=True, encoding="utf-8", errors="replace", timeout=timeout)
    if result.returncode:
        raise RuntimeError(f"Command failed (exit {result.returncode}):\n{result.stdout[-12000:]}")
    return result.stdout


def scalar(output):
    lines = [line.strip() for line in output.splitlines() if line.strip()]
    if len(lines) != 1:
        raise AssertionError(f"Expected exactly one scalar result, got {lines!r}")
    return lines[0]


def fragment_columns(mapper):
    document = ET.parse(mapper).getroot()
    fragments = {node.attrib["id"]: node for node in document.findall("sql")}

    def expand(name, active=()):
        if name in active:
            raise AssertionError("Recursive SQL fragment")
        node = fragments[name]
        text = node.text or ""
        for child in node:
            if child.tag != "include":
                raise AssertionError("Unexpected dynamic column fragment")
            text += expand(child.attrib["refid"], active + (name,)) + (child.tail or "")
        return text

    listing = [column.strip() for column in expand("List_Column_List").split(",")]
    detail = [column.strip() for column in expand("Base_Column_List").split(",")]
    if listing != ["id", "date_created", "date_updated", "name", "config", "role", "sort", "enabled"]:
        raise AssertionError(f"Unexpected plugin list projection: {listing}")
    if detail != listing + ["plugin_jar"]:
        raise AssertionError("Detail/export must retain plugin_jar")
    return listing, detail


def verify_index_rows(rows, postgres=False):
    indexes = {}
    for line in rows.splitlines():
        if not line.strip():
            continue
        table, name, value = line.strip().lower().split("|", 2)
        if postgres:
            match = re.search(r'\busing\s+(?:btree|ubtree)\s*\(([^()]*)\)'
                              r'(?:\s+tablespace\s+(?:"[^"]+"|[a-z_]\w*))?\s*$', value)
            if not match:
                continue  # expression indexes are not usable for these column lookups
            indexes[(table, name)] = [column.strip().split()[0].strip('"') for column in match[1].split(",")]
        else:
            column, position = value.split("|")
            indexes.setdefault((table, name), []).append((int(position), column))
    if not postgres:
        indexes = {key: [column for _, column in sorted(values)] for key, values in indexes.items()}
    for table, column in list(REQUIRED.items()) + [("permission", "object_id"), ("permission", "resource_id")]:
        if not any(t == table and columns and columns[0] == column for (t, _), columns in indexes.items()):
            raise AssertionError(f"Missing leading-column index: {table}({column})")
    for table, name, column in [("selector", "idx_selector_plugin_id", "plugin_id"),
                                ("permission", "idx_permission_resource_id", "resource_id")]:
        if indexes.get((table, name)) != [column]:
            raise AssertionError(f"Wrong index columns: {name}")
    return indexes


class Engine:
    def __init__(self, dialect, flow, output):
        self.dialect = dialect
        self.name = f"shenyu-sql-{dialect}-{flow}-{uuid.uuid4().hex[:8]}"
        self.output = output

    def start(self):
        image, _ = DIALECTS[self.dialect]
        run(["docker", "pull", image], timeout=900)
        environment = {
            "mysql": ["MYSQL_ROOT_PASSWORD=" + PASSWORD],
            "ob": ["MODE=mini", "OB_TENANT_NAME=test"],
            "pg": ["POSTGRES_PASSWORD=" + PASSWORD, "POSTGRES_DB=shenyu"],
            "og": ["GS_PASSWORD=" + PASSWORD, "GS_DB=shenyu"],
            "oracle": ["ORACLE_PASSWORD=" + PASSWORD, "APP_USER=sqlmatrix", "APP_USER_PASSWORD=" + PASSWORD],
        }[self.dialect]
        command = ["docker", "run", "-d", "--name", self.name, "--shm-size=1g"]
        for variable in environment:
            command += ["-e", variable]
        run(command + [image])
        deadline = time.monotonic() + 900
        last_error = ""
        while time.monotonic() < deadline:
            try:
                query = "SELECT 1 FROM dual;" if self.dialect == "oracle" else "SELECT 1;"
                if scalar(self.sql(query, database=False, timeout=20)) == "1":
                    if self.dialect == "ob":
                        # SELECT works before OceanBase finishes tenant creation. Probe
                        # disposable DDL before executing either real schema, never retry
                        # or suppress an error inside the actual schema/upgrade script.
                        self.sql("CREATE DATABASE IF NOT EXISTS sql_matrix_ready; DROP DATABASE sql_matrix_ready;",
                                 database=False, timeout=20)
                    return
            except (RuntimeError, subprocess.TimeoutExpired, AssertionError) as error:
                last_error = str(error)
            time.sleep(5)
        raise RuntimeError("Database readiness timeout: " + last_error)

    def sql(self, sql, database=True, timeout=180):
        command = ["docker", "exec", "-i"]
        if self.dialect == "og":
            command += ["-e", "LD_LIBRARY_PATH=/usr/local/opengauss/lib"]
        command += [self.name]
        if self.dialect in ("mysql", "ob"):
            client = "mysql" if self.dialect == "mysql" else "obclient"
            user = "root" if self.dialect == "mysql" else "root@test"
            command += [client, "-h127.0.0.1", "-P3306" if self.dialect == "mysql" else "-P2881", "-u" + user, "-N", "-B"]
            if self.dialect == "mysql":
                command += ["-p" + PASSWORD]
            if database:
                command += ["shenyu"]
        elif self.dialect in ("pg", "og"):
            if self.dialect == "pg":
                command += ["psql", "-U", "postgres"]
            else:
                command += ["/usr/local/opengauss/bin/gsql", "-U", "gaussdb", "--password", PASSWORD, "-h", "127.0.0.1"]
            command += ["-X", "-d", "shenyu", "-v", "ON_ERROR_STOP=1", "-q", "-t", "-A"]
        else:
            command += ["sqlplus", "-s", "-L", f'sqlmatrix/"{PASSWORD}"@//localhost:1521/FREEPDB1']
            sql = ("whenever sqlerror exit failure rollback\nwhenever oserror exit failure\n"
                   "set define off feedback off heading off pagesize 0 linesize 32767 trimspool on sqlblanklines on\n"
                   + sql + "\ncommit;\nexit;\n")
        output = run(command, sql, timeout)
        # MySQL's known test-password warning is not a SQL result.
        output = "\n".join(line for line in output.splitlines() if not line.startswith("mysql: [Warning]"))
        if self.dialect == "oracle" and re.search(r"(?m)^(?:ORA|SP2|TNS)-\d+", output):
            raise RuntimeError("Oracle client/SQL error:\n" + output)
        return output

    def cleanup(self):
        logs = subprocess.run(["docker", "logs", self.name], capture_output=True, text=True,
                              encoding="utf-8", errors="replace", timeout=30)
        (self.output / (self.name + ".log")).write_text(logs.stdout + logs.stderr, encoding="utf-8")
        subprocess.run(["docker", "rm", "-f", self.name], capture_output=True, timeout=30, check=False)

    def table(self, name):
        return '"resource"' if self.dialect == "oracle" and name == "resource" else name

    def insert_sentinel(self):
        binary = {"mysql": f"UNHEX('{JAR_HEX}')", "ob": f"UNHEX('{JAR_HEX}')",
                  "pg": f"decode('{JAR_HEX}','hex')", "og": f"decode('{JAR_HEX}','hex')",
                  "oracle": f"hextoraw('{JAR_HEX}')"}[self.dialect]
        self.sql("INSERT INTO plugin (id,name,config,role,sort,enabled,plugin_jar) "
                 f"VALUES ('{SENTINEL}','sqlMatrixPlugin','{{}}','test',1,1,{binary});")

    def check_data(self, listing, detail):
        where = f" WHERE id='{SENTINEL}';"
        for columns in (listing, detail):
            output = self.sql("SELECT " + ",".join(columns) + " FROM plugin" + where)
            if SENTINEL not in output:
                raise AssertionError("List/detail projection lost the sentinel plugin")
        binary = {"mysql": "HEX(plugin_jar)", "ob": "HEX(plugin_jar)",
                  "pg": "encode(plugin_jar,'hex')", "og": "encode(plugin_jar,'hex')",
                  "oracle": "rawtohex(dbms_lob.substr(plugin_jar,2000,1))"}[self.dialect]
        if scalar(self.sql("SELECT " + binary + " FROM plugin" + where)).lower() != JAR_HEX:
            raise AssertionError("Plugin JAR bytes changed")
        if scalar(self.sql("SELECT name FROM plugin" + where)) != "sqlMatrixPlugin":
            raise AssertionError("Plugin metadata changed")

    def indexes(self):
        if self.dialect in ("mysql", "ob"):
            query = "SELECT CONCAT(table_name,'|',index_name,'|',column_name,'|',seq_in_index) FROM information_schema.statistics WHERE table_schema='shenyu';"
        elif self.dialect in ("pg", "og"):
            query = "SELECT tablename||'|'||indexname||'|'||indexdef FROM pg_indexes WHERE schemaname='public';"
        else:
            query = "SELECT lower(table_name)||'|'||lower(index_name)||'|'||lower(column_name)||'|'||column_position FROM user_ind_columns;"
        rows = self.sql(query)
        (self.output / (self.name + ".indexes.txt")).write_text(rows, encoding="utf-8")
        return verify_index_rows(rows, self.dialect in ("pg", "og"))

    def check_fresh_postgres_schema(self, schema, indexes):
        """Validate the canonical PostgreSQL/openGauss schema and fresh seeds."""
        declarations = re.findall(r'CREATE TABLE (?:IF NOT EXISTS )?"public"\."(\w+)"\s*\((.*?)\)\s*;', schema, re.S)
        expected_columns = set()
        expected_primary = set()
        for table, definition in declarations:
            for column, rest in re.findall(r'^\s*"(\w+)"\s+([^\n]+)', definition, re.M):
                nullable = "NO" if "NOT NULL" in rest or "PRIMARY KEY" in rest else "YES"
                expected_columns.add(f"{table}|{column}|{nullable}")
            primary = re.search(r'PRIMARY KEY\s*\(([^)]+)\)', definition)
            if primary:
                expected_primary.add(table + "|" + re.sub(r'["\s]', '', primary[1]))
            else:
                inline = re.search(r'^\s*"(\w+)"[^\n]*PRIMARY KEY', definition, re.M)
                if inline:
                    expected_primary.add(table + "|" + inline[1])
        for table, columns in re.findall(r'ALTER TABLE "public"\."(\w+)" ADD CONSTRAINT "\w+" PRIMARY KEY\s*\(([^)]+)\)', schema):
            expected_primary.add(table + "|" + re.sub(r'["\s]', '', columns))
        actual_columns = set(self.sql("SELECT table_name||'|'||column_name||'|'||is_nullable FROM information_schema.columns "
                                     "WHERE table_schema='public';").splitlines())
        if actual_columns != expected_columns:
            raise AssertionError(f"Fresh columns differ: missing={expected_columns - actual_columns}, "
                                 f"extra={actual_columns - expected_columns}")
        actual_primary = set(self.sql("SELECT t.relname||'|'||replace(replace(pg_get_constraintdef(c.oid), "
                                     "'PRIMARY KEY (',''),')','') FROM pg_constraint c JOIN pg_class t ON t.oid=c.conrelid "
                                     "JOIN pg_namespace n ON n.oid=t.relnamespace WHERE n.nspname='public' AND c.contype='p';")
                             .replace(' ', '').replace('"', '').splitlines())
        if actual_primary != expected_primary:
            raise AssertionError("Fresh primary keys differ")
        unique = set(self.sql("SELECT tablename||'|'||indexname FROM pg_indexes WHERE schemaname='public' "
                              "AND indexdef LIKE 'CREATE UNIQUE INDEX%';").lower().splitlines())
        for is_unique, name, table, columns in re.findall(
                r'CREATE (UNIQUE )?INDEX "?(\w+)"? ON "public"\."(\w+)" USING btree\s*\(([^)]+)\)', schema):
            key = (table, name.lower())
            expected = [column.strip().split()[0].strip('"') for column in columns.split(',')]
            if indexes.get(key) != expected:
                raise AssertionError("Missing shared index: " + name)
            if is_unique and '|'.join(key) not in unique:
                raise AssertionError("Missing shared unique index: " + name)
        self.check_fresh_postgres_seeds()
        self.check_discovery_upstream_uniqueness()

    def check_fresh_postgres_seeds(self):
        checks = []
        for table, column, parent, parent_column in (
                ("plugin_handle", "plugin_id", "plugin", "id"),
                ("permission", "resource_id", "resource", "id"),
                ("permission", "object_id", "role", "id"),
                ("user_role", "user_id", "dashboard_user", "id"),
                ("user_role", "role_id", "role", "id"),
                ("namespace_plugin_rel", "plugin_id", "plugin", "id"),
                ("namespace_plugin_rel", "namespace_id", "namespace", "namespace_id"),
                ("namespace_user_rel", "namespace_id", "namespace", "namespace_id"),
                ("namespace_user_rel", "user_id", "dashboard_user", "id"),
                ("selector", "plugin_id", "plugin", "id"),
                ("selector", "namespace_id", "namespace", "namespace_id"),
                ("rule", "selector_id", "selector", "id"),
                ("rule", "namespace_id", "namespace", "namespace_id")):
            checks.append((f"{table}.{column} orphan", f"SELECT child.id FROM {table} child LEFT JOIN {parent} parent "
                           f"ON child.{column}=parent.{parent_column} WHERE parent.{parent_column} IS NULL"))
        checks.append(("resource.parent_id orphan", "SELECT child.id FROM resource child LEFT JOIN resource parent "
                       "ON child.parent_id=parent.id WHERE child.parent_id NOT IN ('', '0') AND parent.id IS NULL"))
        for table, columns in (("plugin_handle", "plugin_id,lower(field),type"),
                               ("permission", "object_id,resource_id"),
                               ("namespace_plugin_rel", "namespace_id,plugin_id")):
            checks.append((table + " duplicate natural key",
                           f"SELECT {columns} FROM {table} GROUP BY {columns} HAVING COUNT(*)>1"))
        checks.append(("missing admin resource permission", "SELECT r.id FROM resource r LEFT JOIN permission p "
                       "ON p.resource_id=r.id AND p.object_id='1346358560427216896' WHERE p.id IS NULL"))
        checks.append(("missing default namespace plugin", "SELECT p.id FROM plugin p LEFT JOIN namespace_plugin_rel r "
                       "ON r.plugin_id=p.id AND r.namespace_id='649330b6-c2d7-4edc-be8e-8a54df9eb385' "
                       f"WHERE p.id<>'{SENTINEL}' AND r.id IS NULL"))
        for label, query in checks:
            if scalar(self.sql("SELECT COUNT(*) FROM (" + query + ") violations;")) != "0":
                raise AssertionError("Invalid fresh seed: " + label)
        if scalar(self.sql("SELECT COUNT(*) FROM namespace WHERE namespace_id='649330b6-c2d7-4edc-be8e-8a54df9eb385' "
                           "AND name='default';")) != "1":
            raise AssertionError("Missing default namespace")
        for table, column in (("plugin", "config"), ("namespace_plugin_rel", "config"), ("plugin_handle", "ext_obj")):
            for value in self.sql(f"SELECT {column} FROM {table} WHERE {column} IS NOT NULL;").splitlines():
                json.loads(value)  # Validate escaped basicAuth JSON on both native engines too.
        for plugin_id in ("22", "23", "33", "44", "50"):
            config = json.loads(scalar(self.sql(f"SELECT config FROM plugin WHERE id='{plugin_id}';")))
            namespace_config = json.loads(scalar(self.sql("SELECT config FROM namespace_plugin_rel "
                                                        f"WHERE plugin_id='{plugin_id}' AND "
                                                        "namespace_id='649330b6-c2d7-4edc-be8e-8a54df9eb385';")))
            if config != namespace_config:
                raise AssertionError("Default namespace config differs for plugin " + plugin_id)
            if plugin_id == "33" and config.get("bootstrapServer") != "localhost:9092":
                raise AssertionError("loggingKafka must seed bootstrapServer")

    def check_discovery_upstream_uniqueness(self):
        # A failed client disconnect rolls back this disposable transaction.
        insert = ("INSERT INTO discovery_upstream (id,discovery_handler_id,namespace_id,protocol,upstream_url,upstream_status,weight) "
                  "VALUES ('{}','sql-matrix-handler','sql-matrix-namespace','http','127.0.0.1:8080',0,100);")
        try:
            self.sql("BEGIN;" + insert.format("sql-matrix-upstream-1") + insert.format("sql-matrix-upstream-2") + "ROLLBACK;")
        except RuntimeError as error:
            if "discovery_upstream_discovery_handler_id_idx" not in str(error).lower():
                raise
        else:
            raise AssertionError("Duplicate discovery upstream was accepted")


def execute_matrix(dialect, root, output):
    output.mkdir(parents=True, exist_ok=True)
    listing, detail = fragment_columns(root / "shenyu-admin/src/main/resources/mappers/plugin-sqlmap.xml")
    results = {"dialect": dialect, "baseline": BASELINE, "image": DIALECTS[dialect][0], "flows": []}
    for flow in ("upgrade", "fresh"):
        engine = Engine(dialect, flow, output)
        start = time.monotonic()
        report = {"flow": flow, "status": "failure"}
        results["flows"].append(report)
        try:
            engine.start()
            schema_path = schema_source(dialect, flow)
            schema = (run(["git", "show", f"{BASELINE}:{schema_path}"]) if flow == "upgrade"
                      else (root / schema_path).read_text(encoding="utf-8"))
            engine.sql(schema, database=False)
            engine.insert_sentinel()
            original_listing = engine.sql("SELECT " + ",".join(listing) + f" FROM plugin WHERE id='{SENTINEL}';")
            before = {table: set(engine.sql("SELECT id FROM " + engine.table(table) + ";").split()) for table in TABLES}
            if flow == "upgrade":
                migration = root / f"db/upgrade/2.7.1-upgrade-2.7.2-{dialect}.sql"
                engine.sql(migration.read_text(encoding="utf-8"))
            engine.check_data(listing, detail)
            current_listing = engine.sql("SELECT " + ",".join(listing) + f" FROM plugin WHERE id='{SENTINEL}';")
            if current_listing != original_listing:
                raise AssertionError("Plugin list metadata changed during migration")
            for table, identifiers in before.items():
                after = set(engine.sql("SELECT id FROM " + engine.table(table) + ";").split())
                if not identifiers.issubset(after):
                    raise AssertionError(f"Migration removed rows from {table}: {sorted(identifiers - after)}")
            indexes = engine.indexes()
            if flow == "fresh" and dialect in ("pg", "og"):
                engine.check_fresh_postgres_schema(schema, indexes)
            report["status"] = "success"
            print(f"{dialect}: {flow} SQL, projections, JAR preservation and index metadata passed", flush=True)
        except Exception as error:
            report["error"] = str(error)
            raise
        finally:
            report["seconds"] = round(time.monotonic() - start, 1)
            (output / "result.json").write_text(json.dumps(results, indent=2), encoding="utf-8")
            engine.cleanup()
    return results


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--dialect", required=True, choices=tuple(DIALECTS))
    parser.add_argument("--output", required=True, type=Path)
    args = parser.parse_args()
    execute_matrix(args.dialect, Path(__file__).resolve().parents[2], args.output)
