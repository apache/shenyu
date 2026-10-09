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


package org.apache.shenyu.common.dto;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Shared rule configuration validation and immutable default contract.
 */
class AgentGatewayMcpConfigTest {

    @Test
    void shouldDefaultToNoToolsAndBoundedJson() {
        AgentGatewayMcpConfig config = parse("{}");
        assertEquals(Set.of(), config.getAllowedTools());
        assertEquals(Set.of(), config.getAllowedOrigins());
        assertEquals("json", config.getResponseMode());
        assertEquals(30000, config.getTimeoutMs());
        assertEquals(262144, config.getMaxRequestBytes());
        assertEquals(1048576, config.getMaxResponseBytes());
    }

    @Test
    void shouldFreezeExplicitSetsAndAcceptBounds() {
        JsonObject source = JsonParser.parseString("{\"allowedTools\":[\"read\"],\"allowedOrigins\":[\"https://example.com\"],"
                + "\"responseMode\":\"sse\",\"timeoutMs\":100,\"maxRequestBytes\":1024,\"maxResponseBytes\":4194304}").getAsJsonObject();
        AgentGatewayMcpConfig config = AgentGatewayMcpConfig.parse(source, true);
        source.getAsJsonArray("allowedTools").add("write");
        assertEquals(Set.of("read"), config.getAllowedTools());
        assertThrows(UnsupportedOperationException.class, () -> config.getAllowedTools().add("write"));
        assertEquals("sse", config.getResponseMode());
        assertEquals(100, config.getTimeoutMs());
        assertEquals(1024, config.getMaxRequestBytes());
        assertEquals(4194304, config.getMaxResponseBytes());
        AgentGatewayMcpConfig upper = parse("{\"timeoutMs\":120000,\"maxRequestBytes\":1048576,\"maxResponseBytes\":1024}");
        assertEquals(120000, upper.getTimeoutMs());
        assertEquals(1048576, upper.getMaxRequestBytes());
    }

    @Test
    void shouldRejectUnknownOnlyAtAdminBoundary() {
        JsonObject object = JsonParser.parseString("{\"future\":true}").getAsJsonObject();
        assertThrows(IllegalArgumentException.class, () -> AgentGatewayMcpConfig.parse(object, true));
        assertEquals(Set.of(), AgentGatewayMcpConfig.parse(object, false).getAllowedTools());
    }

    @ParameterizedTest
    @ValueSource(strings = {
        "null", "[]", "{\"allowedTools\":null}", "{\"allowedTools\":\"read\"}", "{\"allowedTools\":[null]}",
        "{\"allowedTools\":[\"*\"]}", "{\"allowedTools\":[\" \" ]}", "{\"allowedTools\":[\"read\",\"read\"]}",
        "{\"allowedOrigins\":[\"null\"]}", "{\"allowedOrigins\":[\"https://example.com/\"]}",
        "{\"allowedOrigins\":[\"https://user@example.com\"]}", "{\"allowedOrigins\":[\"https://example.com?x=1\"]}",
        "{\"allowedOrigins\":[\"ftp://example.com\"]}", "{\"responseMode\":\"stream\"}", "{\"responseMode\":null}",
        "{\"timeoutMs\":99}", "{\"timeoutMs\":120001}", "{\"timeoutMs\":1.5}", "{\"timeoutMs\":\"100\"}",
        "{\"timeoutMs\":1e30}", "{\"timeoutMs\":null}", "{\"maxRequestBytes\":1023}", "{\"maxRequestBytes\":1048577}",
        "{\"maxResponseBytes\":1023}", "{\"maxResponseBytes\":4194305}"
    })
    void shouldRejectInvalidKnownValues(final String source) {
        assertThrows(IllegalArgumentException.class, () -> parse(source));
    }

    private AgentGatewayMcpConfig parse(final String source) {
        return AgentGatewayMcpConfig.parse(JsonParser.parseString(source), true);
    }
}
