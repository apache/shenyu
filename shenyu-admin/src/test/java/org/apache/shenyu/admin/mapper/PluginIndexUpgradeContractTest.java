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
import org.junit.jupiter.params.provider.ValueSource;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Guards this PR's index declarations in every supported 2.7.1 to 2.7.2 upgrade script.
 * This is a script contract check, not execution on native database engines.
 */
class PluginIndexUpgradeContractTest {

    @ParameterizedTest
    @ValueSource(strings = {"mysql", "ob", "pg", "og", "oracle"})
    void testUpgradeIndexDeclarations(final String dialect) throws Exception {
        Path root = Paths.get(System.getProperty("user.dir")).toAbsolutePath();
        while (Objects.nonNull(root) && !Files.isDirectory(root.resolve("db/upgrade"))) {
            root = root.getParent();
        }
        assertNotNull(root, "Repository upgrade scripts must be available to the test");
        Path script = root.resolve("db/upgrade/2.7.1-upgrade-2.7.2-" + dialect + ".sql");
        String sql = new String(Files.readAllBytes(script), StandardCharsets.UTF_8)
                .replaceAll("(?m)^\\s*--[^\\r\\n]*", "").replace("`", "").replace("\"", "").toLowerCase(Locale.ROOT);
        List<String> statements = Arrays.stream(sql.split(";"))
                .map(value -> value.trim().replaceAll("\\s+", " "))
                .collect(Collectors.toList());
        Map<String, String> expected = expectedIndexes(dialect);
        for (Map.Entry<String, String> index : expected.entrySet()) {
            List<String> declarations = statements.stream()
                    .filter(value -> value.matches("(?s).*\\b" + index.getKey() + "\\b.*"))
                    .collect(Collectors.toList());
            assertEquals(List.of(index.getValue()), declarations, dialect + ": " + index.getKey());
        }
        long actualCount = statements.stream().filter(value -> value.contains("idx_selector_plugin_id")
                || value.contains("idx_permission_object_id") || value.contains("idx_permission_resource_id")
                || value.contains("idx_resource_parent_id") || value.contains("idx_user_role_user_id")).count();
        assertEquals(expected.size(), actualCount, "No duplicate or unexpected plugin-query index declarations");
    }

    private Map<String, String> expectedIndexes(final String dialect) {
        Map<String, String> indexes = new LinkedHashMap<>();
        addIndex(indexes, dialect, "selector", "idx_selector_plugin_id", "plugin_id");
        addIndex(indexes, dialect, "permission", "idx_permission_resource_id", "resource_id");
        if (!"mysql".equals(dialect)) {
            addIndex(indexes, dialect, "permission", "idx_permission_object_id", "object_id");
            addIndex(indexes, dialect, "resource", "idx_resource_parent_id", "parent_id");
            addIndex(indexes, dialect, "user_role", "idx_user_role_user_id", "user_id");
        }
        return indexes;
    }

    private void addIndex(final Map<String, String> indexes, final String dialect,
                          final String table, final String name, final String column) {
        final String statement;
        if ("mysql".equals(dialect) || "ob".equals(dialect)) {
            statement = "alter table " + table + " add index " + name + " (" + column + ") using btree";
        } else if ("pg".equals(dialect) || "og".equals(dialect)) {
            statement = "create index if not exists " + name + " on public." + table + " using btree (" + column + ")";
        } else {
            statement = "create index " + name + " on " + table + " (" + column + ")";
        }
        indexes.put(name, statement);
    }
}
