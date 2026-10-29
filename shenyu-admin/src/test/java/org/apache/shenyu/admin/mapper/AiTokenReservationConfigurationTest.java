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

import com.google.gson.JsonObject;
import org.apache.shenyu.common.utils.GsonUtils;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.UUID;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Execute UI metadata seeds on H2 and compare fresh/upgrade values in every dialect.
 * Oracle seeds use the actual Oracle DDL with H2 Oracle identifier folding; native database CI remains required.
 */
class AiTokenReservationConfigurationTest {

    @ParameterizedTest
    @ValueSource(strings = {"h2", "mysql", "ob", "pg", "og", "oracle"})
    void testFreshSchemaPreservesReservationSeedsInDeclarationOrder(final String dialect) throws Exception {
        final Path root = repositoryRoot();
        final Path h2 = root.resolve("shenyu-admin/src/main/resources/sql-script/h2/schema.sql");
        final String schemaDialect = "ob".equals(dialect) ? "mysql" : "og".equals(dialect) ? "pg" : dialect;
        final Path fresh = "h2".equals(dialect) ? h2 : root.resolve("db/init/" + schemaDialect + ("pg".equals(schemaDialect) ? "/create-table.sql" : "/schema.sql"));
        final boolean oracle = "oracle".equals(dialect);
        final String schema = Files.readString(oracle ? fresh : h2);
        final String mode = oracle ? ";MODE=Oracle" : ";MODE=MySQL;DATABASE_TO_LOWER=TRUE";
        // Replay only the relevant table lifecycle and seeds in source order, using H2-compatible DDL.
        // Unlike executeSeeds, this must not create tables before their declarations in the source.
        final Pattern operations = Pattern.compile("(?im)^(?:(DROP TABLE(?: IF EXISTS)?|CREATE TABLE(?: IF NOT EXISTS)?)\\s+"
                + "(?:\"public\"\\.)?[`\"]?(plugin_handle|shenyu_dict)\\b|INSERT [^\\r\\n]*196000000000000770[0-3][^\\r\\n]*)");
        final Matcher operation = operations.matcher(Files.readString(fresh));
        try (Connection connection = DriverManager.getConnection("jdbc:h2:mem:" + UUID.randomUUID() + mode);
                Statement statement = connection.createStatement()) {
            while (operation.find()) {
                if (Objects.isNull(operation.group(1))) {
                    statement.execute(operation.group());
                } else if (operation.group(1).toUpperCase(Locale.ROOT).startsWith("DROP")) {
                    statement.execute("DROP TABLE IF EXISTS " + operation.group(2));
                } else {
                    statement.execute(tableDefinition(schema, operation.group(2), oracle));
                }
            }
            final List<List<String>> rows = new ArrayList<>();
            readRows(statement, "SELECT id FROM plugin_handle ORDER BY id", rows);
            readRows(statement, "SELECT id FROM shenyu_dict ORDER BY id", rows);
            assertEquals(List.of(List.of("1960000000000007700"), List.of("1960000000000007701"),
                    List.of("1960000000000007702"), List.of("1960000000000007703")), rows,
                    "Fresh schema must create tables before seeding and retain all reservation metadata");
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"h2", "mysql", "ob", "pg", "og", "oracle"})
    void testReservationFieldsAndDefaultsMatchFreshInstallAndUpgrade(final String dialect) throws Exception {
        final Path root = repositoryRoot();
        final Path h2 = root.resolve("shenyu-admin/src/main/resources/sql-script/h2/schema.sql");
        final String schemaDialect = "ob".equals(dialect) ? "mysql" : "og".equals(dialect) ? "pg" : dialect;
        final Path fresh = "h2".equals(dialect) ? h2 : root.resolve("db/init/" + schemaDialect + ("pg".equals(schemaDialect) ? "/create-table.sql" : "/schema.sql"));
        final boolean oracle = "oracle".equals(dialect);
        final String ddl = Files.readString(oracle ? fresh : h2);
        final List<List<String>> freshRows = executeSeeds(ddl, Files.readString(fresh), "h2".equals(dialect), oracle);
        final Path upgrade = root.resolve("db/upgrade/2.7.2-upgrade-2.7.3-" + dialect + ".sql");
        assertEquals(freshRows, executeSeeds(ddl, Files.readString(upgrade), false, oracle));

        assertEquals(4, freshRows.size());
        final List<String> enabled = freshRows.get(0);
        assertEquals(List.of("1960000000000007700", "51", "reservationEnabled", "reservationEnabled", "3", "2", "4"), enabled.subList(0, 7));
        final JsonObject enabledExt = GsonUtils.getInstance().fromJson(enabled.get(7), JsonObject.class);
        assertFalse(enabledExt.get("defaultValue").getAsBoolean());
        assertEquals("0", enabledExt.get("required").getAsString());
        final List<String> amount = freshRows.get(1);
        assertEquals(List.of("1960000000000007701", "51", "reservationTokens", "reservationTokens", "1", "2", "5"), amount.subList(0, 7));
        final JsonObject amountExt = GsonUtils.getInstance().fromJson(amount.get(7), JsonObject.class);
        assertFalse(amountExt.has("defaultValue"), "No implicit estimate should enable reservations for old rules");
        assertEquals("0", amountExt.get("required").getAsString());
        assertEquals(List.of("1960000000000007702", "reservationEnabled", "RESERVATION_ENABLED", "disabled", "false", "0", "1"), freshRows.get(2));
        assertEquals(List.of("1960000000000007703", "reservationEnabled", "RESERVATION_ENABLED", "enabled", "true", "1", "1"), freshRows.get(3));
    }

    private List<List<String>> executeSeeds(final String schema, final String source, final boolean replay, final boolean oracle) throws Exception {
        final List<String> inserts = source.lines().filter(line -> line.startsWith("INSERT ") && line.contains("196000000000000770"))
                .collect(Collectors.toList());
        assertEquals(4, inserts.size(), "Exactly two form fields and two boolean choices must be seeded");
        final String mode = oracle ? ";MODE=Oracle" : ";MODE=MySQL;DATABASE_TO_LOWER=TRUE";
        try (Connection connection = DriverManager.getConnection("jdbc:h2:mem:" + UUID.randomUUID() + mode);
                Statement statement = connection.createStatement()) {
            for (String table : List.of("plugin_handle", "shenyu_dict")) {
                statement.execute(tableDefinition(schema, table, oracle));
            }
            for (String insert : inserts) {
                statement.execute(insert);
            }
            if (replay) {
                for (String insert : inserts) {
                    statement.execute(insert);
                }
            }
            final List<List<String>> result = new ArrayList<>();
            readRows(statement, "SELECT id, plugin_id, field, label, data_type, type, sort, ext_obj FROM plugin_handle ORDER BY id", result);
            readRows(statement, "SELECT id, type, dict_code, dict_name, dict_value, sort, enabled FROM shenyu_dict ORDER BY id", result);
            return result;
        }
    }

    private String tableDefinition(final String schema, final String table, final boolean oracle) {
        final String prefix = oracle ? "create\\s+table\\s+" + table + "\\s*\\(" : "CREATE TABLE IF NOT EXISTS `" + table + "`";
        final Matcher ddl = Pattern.compile("(?s)" + prefix + ".*?\\);", Pattern.CASE_INSENSITIVE).matcher(schema);
        assertTrue(ddl.find(), "Use the actual table definition with dialect identifier folding");
        return ddl.group();
    }

    private void readRows(final Statement statement, final String sql, final List<List<String>> target) throws Exception {
        try (ResultSet rows = statement.executeQuery(sql)) {
            while (rows.next()) {
                final List<String> values = new ArrayList<>();
                for (int column = 1; column <= rows.getMetaData().getColumnCount(); column++) {
                    values.add(rows.getString(column));
                }
                target.add(values);
            }
        }
    }

    private Path repositoryRoot() {
        Path root = Paths.get(System.getProperty("user.dir")).toAbsolutePath();
        while (Objects.nonNull(root) && !Files.isDirectory(root.resolve("db/upgrade"))) {
            root = root.getParent();
        }
        assertNotNull(root);
        return root;
    }
}
