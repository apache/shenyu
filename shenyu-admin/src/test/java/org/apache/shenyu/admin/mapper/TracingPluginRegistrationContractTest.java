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
import org.junit.jupiter.params.provider.MethodSource;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Guards the control plane registration of the tracing plugin in the initialization schemas and in
 * the 2.7.2 to 2.7.3 upgrade scripts. This is a script contract check, not execution on native
 * database engines: the H2 schema is executed by the admin integration tests, and the other dialects
 * have no engine available here.
 *
 * <p>The tracing plugin shipped in 2.7.2 without a row, so it could not be managed from the
 * dashboard. A row registered disabled would push a disable event through the sync channel and
 * remove the plugin from the gateway chain, so every script registers the row enabled, at the sort of
 * the plugin enum, together with the relation of the default namespace. The dashboard entry and the
 * button permissions are seeded as well, which is the set the admin registers on startup for a
 * plugin without a resource.</p>
 */
class TracingPluginRegistrationContractTest {

    private static final String TRACING_PLUGIN_ID = "'70'";

    private static final String TRACING_PLUGIN_NAME = "'tracing'";

    private static final String DEFAULT_NAMESPACE_PLUGIN_REL_ID = "'1890000000000000011'";

    private static final String ADMIN_ROLE_ID = "'1346358560427216896'";

    private static final String PLUGIN_MENU_RESOURCE_ID = "'1890000000000000020'";

    private static final String PLUGIN_MENU_PARENT_ID = "'1346775491550474240'";

    private static final String TRACING_MENU_VALUES = "values (" + PLUGIN_MENU_RESOURCE_ID + ", " + PLUGIN_MENU_PARENT_ID
            + ", 'tracing', 'tracing', '/plug/tracing', 'tracing', 1, 0, 'pic-center', 0, 0, '', 1,";

    /**
     * The menu resource and the nine button resources of the plugin, followed by the ten admin
     * permissions, use consecutive ids in every dialect script.
     */
    private static final long FIRST_RESOURCE_ID = 1890000000000000020L;

    private static final long FIRST_PERMISSION_ID = 1890000000000000040L;

    private static final int RESOURCE_COUNT = 10;

    private static final List<String> BUTTONS = Arrays.asList(
            "selector.add", "selector.query", "selector.edit", "selector.delete",
            "rule.add", "rule.query", "rule.edit", "rule.delete", "synchronize");

    private static final List<String> BUTTON_PERMS = Arrays.asList(
            "plugin:tracingselector:add", "plugin:tracingselector:query", "plugin:tracingselector:edit",
            "plugin:tracingselector:delete", "plugin:tracingrule:add", "plugin:tracingrule:query",
            "plugin:tracingrule:edit", "plugin:tracingrule:delete", "plugin:tracing:modify");

    private static final Pattern PLUGIN_INSERT = Pattern.compile("\\binsert\\b.*\\binto\\s+(?:public\\.)?plugin\\b");

    private static final Pattern NAMESPACE_PLUGIN_REL_INSERT =
            Pattern.compile("\\binsert\\b.*\\binto\\s+(?:public\\.)?namespace_plugin_rel\\b");

    private static final Pattern RESOURCE_INSERT = Pattern.compile("\\binsert\\b.*\\binto\\s+(?:public\\.)?resource\\b");

    private static final Pattern PERMISSION_INSERT = Pattern.compile("\\binsert\\b.*\\binto\\s+(?:public\\.)?permission\\b");

    /**
     * The row registers sort 0 and the enabled flag in the two columns which follow it, because a
     * registered-but-disabled row would stop an existing tracing deployment on upgrade.
     */
    private static final Pattern SORT_ZERO_ENABLED = Pattern.compile("\\b0,\\s*'?1'?\\s*[,)]");

    private static final List<String> SCRIPTS = Arrays.asList(
            "db/init/mysql/schema.sql",
            "db/init/oracle/schema.sql",
            "db/init/pg/create-table.sql",
            "shenyu-admin/src/main/resources/sql-script/h2/schema.sql",
            "db/upgrade/2.7.2-upgrade-2.7.3-mysql.sql",
            "db/upgrade/2.7.2-upgrade-2.7.3-ob.sql",
            "db/upgrade/2.7.2-upgrade-2.7.3-og.sql",
            "db/upgrade/2.7.2-upgrade-2.7.3-oracle.sql",
            "db/upgrade/2.7.2-upgrade-2.7.3-pg.sql",
            "db/upgrade/2.7.2-upgrade-2.7.3-h2.sql");

    private static Stream<String> scripts() {
        return SCRIPTS.stream();
    }

    @ParameterizedTest
    @MethodSource("scripts")
    void testTracingPluginRegisteredEnabled(final String script) throws Exception {
        final List<String> statements = statementsOf(script);
        final List<String> pluginInserts = matching(statements, PLUGIN_INSERT, TRACING_PLUGIN_NAME);
        assertEquals(1, pluginInserts.size(), script + ": the tracing plugin row must be registered once");
        assertTrue(pluginInserts.get(0).contains(TRACING_PLUGIN_ID), script + ": the tracing plugin row uses its own id");
        assertTrue(SORT_ZERO_ENABLED.matcher(pluginInserts.get(0)).find(),
                script + ": the tracing plugin row registers the sort 0 and enabled pair");
        // the name keeps its lower case and the role is the dashboard group, so both are checked on the raw row
        final List<String> rawPluginInserts = rawStatementsOf(script).stream()
                .filter(statement -> PLUGIN_INSERT.matcher(statement.toLowerCase(Locale.ROOT)).find())
                .filter(statement -> statement.toLowerCase(Locale.ROOT).contains(TRACING_PLUGIN_NAME))
                .collect(Collectors.toList());
        assertEquals(1, rawPluginInserts.size(), script + ": the raw tracing plugin row must be unique");
        assertTrue(rawPluginInserts.get(0).contains("'70', 'tracing'"),
                script + ": the plugin row registers the plugin name before the role");
        assertTrue(rawPluginInserts.get(0).contains("'Tracing'"),
                script + ": the plugin row registers its dashboard group");
        final List<String> relationInserts = matching(statements, NAMESPACE_PLUGIN_REL_INSERT, DEFAULT_NAMESPACE_PLUGIN_REL_ID);
        assertEquals(1, relationInserts.size(), script + ": the tracing plugin must be related to the default namespace once");
        assertTrue(relationInserts.get(0).contains(TRACING_PLUGIN_ID),
                script + ": the namespace relation points at the tracing plugin row");
        assertTrue(SORT_ZERO_ENABLED.matcher(relationInserts.get(0)).find(),
                script + ": the namespace relation registers the sort 0 and enabled pair");
    }

    @ParameterizedTest
    @MethodSource("scripts")
    void testTracingDashboardEntryAndPermissions(final String script) throws Exception {
        final List<String> statements = statementsOf(script);
        assertSingleStatement(statements, RESOURCE_INSERT, TRACING_MENU_VALUES, script, "the tracing menu resource");
        for (int index = 0; index < BUTTONS.size(); index++) {
            final long resourceId = FIRST_RESOURCE_ID + index + 1;
            assertSingleStatement(statements, RESOURCE_INSERT, buttonValues(resourceId, index), script,
                    "the " + BUTTONS.get(index) + " button of the tracing plugin");
        }
        for (int index = 0; index < RESOURCE_COUNT; index++) {
            final String resourceId = quote(FIRST_RESOURCE_ID + index);
            final String permissionValues = "values (" + quote(FIRST_PERMISSION_ID + index) + ", " + ADMIN_ROLE_ID
                    + ", " + resourceId + ",";
            assertSingleStatement(statements, PERMISSION_INSERT, permissionValues, script,
                    "the admin permission of the tracing resource " + resourceId);
            assertSingleStatement(statements, RESOURCE_INSERT, "values (" + resourceId + ",", script,
                    "the tracing resource " + resourceId);
        }
        assertEquals(RESOURCE_COUNT,
                statements.stream()
                        .filter(statement -> RESOURCE_INSERT.matcher(statement).find())
                        .filter(statement -> statement.contains("'plugin:tracing") || statement.contains(TRACING_MENU_VALUES))
                        .count(),
                script + ": the tracing dashboard entry must consist of exactly ten resources");
    }

    private String buttonValues(final long resourceId, final int index) {
        return "values (" + quote(resourceId) + ", " + PLUGIN_MENU_RESOURCE_ID
                + ", 'shenyu.button.plugin." + BUTTONS.get(index) + "', '', '', '', 2, 0, '', 1, 0, "
                + "'" + BUTTON_PERMS.get(index) + "', 1,";
    }

    private String quote(final long id) {
        return "'" + id + "'";
    }

    private void assertSingleStatement(final List<String> statements, final Pattern insert, final String values,
                                       final String script, final String what) {
        final List<String> matches = statements.stream()
                .filter(statement -> insert.matcher(statement).find())
                .filter(statement -> statement.contains(values))
                .collect(Collectors.toList());
        assertEquals(1, matches.size(), script + ": " + what + " must be registered exactly once with the expected values");
    }

    private List<String> matching(final List<String> statements, final Pattern insert, final String value) {
        return statements.stream()
                .filter(statement -> insert.matcher(statement).find())
                .filter(statement -> statement.contains(value))
                .collect(Collectors.toList());
    }

    private String rawSqlOf(final String script) throws Exception {
        return new String(Files.readAllBytes(root().resolve(script)), StandardCharsets.UTF_8);
    }

    /**
     * Splits a script into statements while keeping the case of the literals, for the checks which
     * look at a value the dialect comparison has to be case sensitive about.
     */
    private List<String> rawStatementsOf(final String script) throws Exception {
        return split(rawSqlOf(script));
    }

    private List<String> statementsOf(final String script) throws Exception {
        return split(rawSqlOf(script).toLowerCase(Locale.ROOT));
    }

    private List<String> split(final String source) {
        final String sql = source
                .replaceAll("(?m)^\\s*--[^\\r\\n]*", "")
                .replace("`", "")
                .replace("\"", "");
        return Arrays.stream(sql.split(";"))
                .map(statement -> statement.trim().replaceAll("\\s+", " "))
                .filter(statement -> !statement.isEmpty())
                .collect(Collectors.toList());
    }

    private Path root() {
        Path root = Paths.get(System.getProperty("user.dir")).toAbsolutePath();
        while (Objects.nonNull(root) && !Files.isDirectory(root.resolve("db/upgrade"))) {
            root = root.getParent();
        }
        assertNotNull(root, "Repository scripts must be available to the test");
        return root;
    }
}
