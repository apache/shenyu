/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.shenyu.admin.mapper;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Script contract check for the {@code operation_record_log} audit-log indexes.
 * Every supported dialect must declare both the {@code operation_time} index and the
 * {@code (operator, operation_time)} index that back the time-range and per-operator audit-log
 * queries, in the fresh-install schema as well as the 2.7.1 to 2.7.2 upgrade script.
 * OceanBase reuses the MySQL fresh-install script and openGauss reuses the PostgreSQL one, so
 * those dialects are covered by the {@code mysql} and {@code pg} init cases.
 * This verifies the scripts only; it is not an execution test on native database engines.
 */
class OperationLogIndexContractTest {

    private static final String TIME = "operation_time";

    private static final String OPERATOR_TIME = "operator, operation_time";

    /**
     * The PostgreSQL scripts quote each column, so the normalized column list keeps no space
     * after the comma.
     */
    private static final String PG_OPERATOR_TIME = "operator,operation_time";

    /**
     * Oracle rejects identifiers longer than 30 characters, so the composite index uses a
     * shortened name there; the column list is identical to the other dialects.
     */
    private static final String ORACLE_OPERATOR_INDEX = "idx_operation_log_oper_time";

    @ParameterizedTest(name = "{0} {1}")
    @MethodSource("scripts")
    void testOperationLogIndexDeclarations(final String dialect, final String kind, final String script,
                                           final String timeIndex, final String operatorIndex) throws IOException {
        final List<String> statements = statementsOf(repositoryRoot().resolve(script));
        assertTrue(statements.contains(timeIndex), dialect + " " + kind + " is missing: " + timeIndex);
        assertTrue(statements.contains(operatorIndex), dialect + " " + kind + " is missing: " + operatorIndex);
    }

    private static Stream<Arguments> scripts() {
        return Stream.of(
                Arguments.of("mysql", "init", "db/init/mysql/schema.sql",
                        createIndex("idx_operation_log_time", TIME),
                        createIndex("idx_operation_log_operator_time", OPERATOR_TIME)),
                Arguments.of("pg", "init", "db/init/pg/create-table.sql",
                        pgIndex("idx_operation_log_time", TIME),
                        pgIndex("idx_operation_log_operator_time", PG_OPERATOR_TIME)),
                Arguments.of("oracle", "init", "db/init/oracle/schema.sql",
                        createIndex("idx_operation_log_time", TIME),
                        createIndex(ORACLE_OPERATOR_INDEX, OPERATOR_TIME)),
                Arguments.of("mysql", "upgrade", "db/upgrade/2.7.1-upgrade-2.7.2-mysql.sql",
                        createIndex("idx_operation_log_time", TIME),
                        createIndex("idx_operation_log_operator_time", OPERATOR_TIME)),
                Arguments.of("ob", "upgrade", "db/upgrade/2.7.1-upgrade-2.7.2-ob.sql",
                        alterAddIndex("idx_operation_log_time", TIME),
                        alterAddIndex("idx_operation_log_operator_time", OPERATOR_TIME)),
                Arguments.of("pg", "upgrade", "db/upgrade/2.7.1-upgrade-2.7.2-pg.sql",
                        pgIndex("idx_operation_log_time", TIME),
                        pgIndex("idx_operation_log_operator_time", PG_OPERATOR_TIME)),
                Arguments.of("og", "upgrade", "db/upgrade/2.7.1-upgrade-2.7.2-og.sql",
                        pgIndex("idx_operation_log_time", TIME),
                        pgIndex("idx_operation_log_operator_time", PG_OPERATOR_TIME)),
                Arguments.of("oracle", "upgrade", "db/upgrade/2.7.1-upgrade-2.7.2-oracle.sql",
                        createIndex("idx_operation_log_time", TIME),
                        createIndex(ORACLE_OPERATOR_INDEX, OPERATOR_TIME)));
    }

    private static String createIndex(final String name, final String columns) {
        return "create index " + name + " on operation_record_log (" + columns + ")";
    }

    private static String alterAddIndex(final String name, final String columns) {
        return "alter table operation_record_log add index " + name + " (" + columns + ") using btree";
    }

    private static String pgIndex(final String name, final String columns) {
        return "create index " + name + " on public.operation_record_log using btree (" + columns + ")";
    }

    private static Path repositoryRoot() {
        Path root = Paths.get(System.getProperty("user.dir")).toAbsolutePath();
        while (Objects.nonNull(root) && !Files.isDirectory(root.resolve("db/upgrade"))) {
            root = root.getParent();
        }
        assertNotNull(root, "Repository db scripts must be available to the test");
        return root;
    }

    private static List<String> statementsOf(final Path script) throws IOException {
        final String sql = new String(Files.readAllBytes(script), StandardCharsets.UTF_8)
                .replaceAll("(?m)^\\s*--[^\\r\\n]*", "")
                .replace("`", "").replace("\"", "").toLowerCase(Locale.ROOT);
        return Arrays.stream(sql.split(";"))
                .map(value -> value.trim().replaceAll("\\s+", " "))
                .collect(Collectors.toList());
    }
}
