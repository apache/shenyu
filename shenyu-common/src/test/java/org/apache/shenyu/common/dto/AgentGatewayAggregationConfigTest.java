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

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.ValueSource;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AgentGatewayAggregationConfigTest {

    private static final String SERVER_FIELDS = "\"name\":\"orders\",\"endpoint\":\"https://192.0.2.10:443/mcp\","
            + "\"credentialRef\":\"service/orders\",\"credentialVersion\":\"v1\"";

    @ParameterizedTest
    @ValueSource(strings = {"", "{}", "{\"aggregation\":{\"revision\":\"v1\",\"servers\":[]}}"})
    void emptyConfigurationIsExplicitLocalOnly(final String source) {
        assertTrue(AgentGatewayAggregationConfig.parsePluginConfig(source).servers().isEmpty());
    }

    @ParameterizedTest
    @ValueSource(strings = {"[]", "null", "{\"unknown\":1}", "{\"aggregation\":null}",
        "{\"aggregation\":{\"revision\":\"v1\"}}", "{\"aggregation\":{\"revision\":\"v1\",\"servers\":{}}}",
        "{\"aggregation\":{\"revision\":\"v1\",\"servers\":[],\"bearerToken\":\"SECRET\"}}",
        "{\"aggregation\":{\"revision\":1,\"servers\":[]}}", "{\"aggregation\":{\"revision\":\"*\",\"servers\":[]}}",
        "{\"aggregation\":{\"revision\":\"v1\",\"revision\":\"v2\",\"servers\":[]}}", "{} {}"})
    void rejectsUnknownMissingDuplicateAndSecretFields(final String source) {
        IllegalArgumentException error = assertThrows(IllegalArgumentException.class, () -> AgentGatewayAggregationConfig.parsePluginConfig(source));
        assertTrue(!error.toString().contains("SECRET"));
        assertEquals(null, error.getCause());
    }

    @Test
    void freezesServerReferencesAndAcceptsHttpsSyntax() {
        String server = "{" + SERVER_FIELDS + "}";
        AgentGatewayAggregationConfig config = AgentGatewayAggregationConfig.parsePluginConfig(wrap(server));
        AgentGatewayAggregationConfig.Server parsed = config.servers().get(0);
        assertEquals("orders", parsed.name());
        assertEquals(AgentGatewayAggregationConfig.ProtocolProfile.LEGACY_2025_06_18, parsed.protocolProfile());
        assertEquals(parsed, new AgentGatewayAggregationConfig.Server(parsed.name(), parsed.endpoint(), parsed.credentialRef(), parsed.credentialVersion()));
        assertThrows(UnsupportedOperationException.class, () -> config.servers().clear());
        assertThrows(IllegalArgumentException.class, () -> AgentGatewayAggregationConfig.parsePluginConfig(wrap(server + "," + server)));
    }

    @ParameterizedTest
    @EnumSource(AgentGatewayAggregationConfig.ProtocolProfile.class)
    void acceptsExplicitProfile(final AgentGatewayAggregationConfig.ProtocolProfile profile) {
        String source = wrap("{" + SERVER_FIELDS + ",\"protocolProfile\":\"" + profile.id() + "\"}");
        assertEquals(profile, AgentGatewayAggregationConfig.parsePluginConfig(source).servers().get(0).protocolProfile());
    }

    @ParameterizedTest
    @ValueSource(strings = {"null", "\"\"", "\" \"", "\"unknown\"", "\" native-2026-07-28\"", "42", "{}", "[]", "false"})
    void rejectsInvalidExplicitProfile(final String value) {
        String source = wrap("{" + SERVER_FIELDS + ",\"protocolProfile\":" + value + "}");
        assertThrows(IllegalArgumentException.class, () -> AgentGatewayAggregationConfig.parsePluginConfig(source));
    }

    @ParameterizedTest
    @ValueSource(strings = {"http://user:pass@127.0.0.1:8080/mcp", "http://127.0.0.1/mcp", "http://127.0.0.1:8080/a/../mcp",
        "http://127.0.0.1:8080/mcp?key=SECRET", "http://127.0.0.1:8080/mcp#SECRET", "http://127.0.0.1:8080/%2e/mcp",
        "https://example.com:443/mcp", "http://192.0.2.10:8080/mcp", "https://0.0.0.0:443/mcp", "https://255.255.255.255:443/mcp"})
    void rejectsAmbiguousEndpoints(final String endpoint) {
        String server = "{\"name\":\"orders\",\"endpoint\":\"" + endpoint + "\",\"credentialRef\":\"service/orders\",\"credentialVersion\":\"v1\"}";
        assertThrows(IllegalArgumentException.class, () -> AgentGatewayAggregationConfig.parsePluginConfig(wrap(server)));
    }

    private static String wrap(final String servers) {
        return "{\"aggregation\":{\"revision\":\"v1\",\"servers\":[" + servers + "]}}";
    }
}
