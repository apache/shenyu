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

package org.apache.shenyu.common.enums;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Default order and database seed contract for the single Agent Gateway entry.
 */
class AgentGatewayPluginOrderTest {

    @Test
    void shouldPlaceGatewayAfterAuthenticationAndBeforeTheExistingAiChain() {
        List<String> ordered = Arrays.stream(new PluginEnum[]{PluginEnum.AI_PROXY, PluginEnum.AI_TOKEN_LIMITER,
            PluginEnum.AGENT_GATEWAY, PluginEnum.KEY_AUTH, PluginEnum.AI_PROMPT})
                .sorted(Comparator.comparingInt(PluginEnum::getCode))
                .map(PluginEnum::getName).collect(Collectors.toList());
        assertEquals(Arrays.asList("keyAuth", "agentGateway", "aiPrompt", "aiTokenLimiter", "aiProxy"), ordered);
        assertTrue(PluginEnum.BASIC_AUTH.getCode() < PluginEnum.AGENT_GATEWAY.getCode());
        assertTrue(PluginEnum.JWT.getCode() < PluginEnum.AGENT_GATEWAY.getCode());
        assertTrue(PluginEnum.OAUTH2.getCode() < PluginEnum.AGENT_GATEWAY.getCode());
        assertEquals(1, Arrays.stream(PluginEnum.values())
                .filter(plugin -> plugin.getCode() == PluginEnum.AGENT_GATEWAY.getCode()).count());
    }

    @ParameterizedTest
    @ValueSource(strings = {
        "db/init/mysql/schema.sql",
        "db/init/pg/create-table.sql",
        "db/init/oracle/schema.sql",
        "shenyu-admin/src/main/resources/sql-script/h2/schema.sql",
        "db/upgrade/2.7.1-upgrade-2.7.2-mysql.sql",
        "db/upgrade/2.7.1-upgrade-2.7.2-pg.sql",
        "db/upgrade/2.7.1-upgrade-2.7.2-og.sql",
        "db/upgrade/2.7.1-upgrade-2.7.2-ob.sql",
        "db/upgrade/2.7.1-upgrade-2.7.2-oracle.sql"
    })
    void shouldKeepGatewayAndNamespaceSeedsInSync(final String script) throws IOException {
        String sql = Files.readString(repositoryRoot().resolve(script));
        assertSeedOrder(sql, "68", PluginEnum.AGENT_GATEWAY, script);
    }

    @ParameterizedTest
    @ValueSource(strings = {
        "db/init/mysql/schema.sql",
        "db/init/pg/create-table.sql",
        "db/init/oracle/schema.sql",
        "shenyu-admin/src/main/resources/sql-script/h2/schema.sql"
    })
    void shouldKeepTheCompleteFreshInstallAiChainInSync(final String script) throws IOException {
        String sql = Files.readString(repositoryRoot().resolve(script));
        assertSeedOrder(sql, "40", PluginEnum.KEY_AUTH, script);
        assertSeedOrder(sql, "52", PluginEnum.AI_PROMPT, script);
        assertSeedOrder(sql, "51", PluginEnum.AI_TOKEN_LIMITER, script);
        assertSeedOrder(sql, "50", PluginEnum.AI_PROXY, script);
    }

    private void assertSeedOrder(final String sql, final String id, final PluginEnum plugin, final String script) {
        String value = Integer.toString(plugin.getCode());
        String field = "(?:NULL|'(?:''|[^'])*')";
        String pluginRow = "'" + id + "'[ ]*,[ ]*'" + plugin.getName()
                + "'[ ]*,[ ]*(?:'[^']+'[ ]*,[ ]*" + value + "|" + field + "[ ]*,[ ]*'[^']+'[ ]*,[ ]*" + value + ")[ ]*,";
        String namespaceRow = "'[^']+'[ ]*,[ ]*'649330b6-c2d7-4edc-be8e-8a54df9eb385'[ ]*,[ ]*'" + id
                + "'[ ]*,[ ]*" + field + "[ ]*,[ ]*" + value + "[ ]*,";
        String normalized = sql.lines().collect(Collectors.joining(" ")).replace((char) 9, ' ');
        assertTrue(Pattern.compile(pluginRow, Pattern.CASE_INSENSITIVE).matcher(normalized).find(), script + ": plugin " + plugin.getName());
        assertTrue(Pattern.compile(namespaceRow, Pattern.CASE_INSENSITIVE).matcher(normalized).find(), script + ": namespace " + plugin.getName());
    }

    @Test
    void shouldKeepTheExistingInstallationMigrationExplicitAndDefaultOnly() throws IOException {
        String sql = Files.readString(repositoryRoot().resolve("db/upgrade/2.7.2-agent-gateway-order.sql"));
        String statements = sql.lines().filter(line -> !line.startsWith("--"))
                .map(String::trim).filter(line -> !line.isEmpty()).collect(Collectors.joining(" "));
        assertEquals("UPDATE plugin SET sort = " + PluginEnum.AGENT_GATEWAY.getCode()
                + " WHERE name = 'agentGateway' AND sort = 198; UPDATE namespace_plugin_rel SET sort = "
                + PluginEnum.AGENT_GATEWAY.getCode()
                + " WHERE sort = 198 AND plugin_id IN (SELECT id FROM plugin WHERE name = 'agentGateway');", statements);
        assertFalse(statements.contains("enabled"));
        assertFalse(statements.contains("config"));
    }

    private Path repositoryRoot() {
        Path path = Paths.get("").toAbsolutePath();
        while (Objects.nonNull(path)) {
            if (Files.isDirectory(path.resolve("db/init")) && Files.isDirectory(path.resolve("shenyu-common"))) {
                return path;
            }
            path = path.getParent();
        }
        throw new IllegalStateException("Run the seed contract tests inside the ShenYu checkout");
    }
}
