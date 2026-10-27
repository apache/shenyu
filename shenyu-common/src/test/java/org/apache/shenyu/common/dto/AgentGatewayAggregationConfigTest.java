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
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;

import java.time.Duration;
import java.util.List;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AgentGatewayAggregationConfigTest {

    @ParameterizedTest
    @ValueSource(strings = {"", "{}", "{\"aggregation\":{\"revision\":\"v1\",\"servers\":[]}}"})
    void emptyConfigurationIsExplicitLocalOnly(final String source) {
        assertTrue(AgentGatewayAggregationConfig.parsePluginConfig(source).servers().isEmpty());
        assertEquals(1, AgentGatewayAggregationConfig.parsePluginConfig(source).refreshConcurrency());
        assertEquals(AgentGatewayRefreshPolicy.defaults(), AgentGatewayAggregationConfig.parsePluginConfig(source).refreshPolicy());
    }

    @Test
    void nullConfigurationUsesSerialRefresh() {
        assertEquals(1, AgentGatewayAggregationConfig.parsePluginConfig(null).refreshConcurrency());
    }

    @Test
    void existingConstructorUsesSerialRefresh() {
        assertEquals(1, new AgentGatewayAggregationConfig("v1", List.of()).refreshConcurrency());
        assertEquals(AgentGatewayRefreshPolicy.defaults(), new AgentGatewayAggregationConfig("v1", List.of()).refreshPolicy());
        assertEquals(AgentGatewayRefreshPolicy.defaults(), new AgentGatewayAggregationConfig("v1", List.of(), 2).refreshPolicy());
    }

    @Test
    void readsExplicitRefreshPolicyTogetherWithConcurrency() {
        String source = "{\"aggregation\":{\"revision\":\"v1\",\"servers\":[],\"refreshConcurrency\":2,\"refreshPolicy\":{"
                + "\"refreshIntervalMs\":120000,\"initialBackoffMs\":2000,\"maxBackoffMs\":16000,\"maxRetries\":4}}}";
        AgentGatewayAggregationConfig config = AgentGatewayAggregationConfig.parsePluginConfig(source);
        assertEquals(2, config.refreshConcurrency());
        assertEquals(new AgentGatewayRefreshPolicy(Duration.ofSeconds(120), Duration.ofSeconds(2), Duration.ofSeconds(16), 4), config.refreshPolicy());
    }

    @Test
    void constructorRejectsMissingPolicy() {
        assertThrows(IllegalArgumentException.class, () -> new AgentGatewayAggregationConfig("v1", List.of(), 1, null));
    }

    @ParameterizedTest
    @ValueSource(strings = {"null", "[]", "{}", "true", "\"SECRET\"",
        "{\"refreshIntervalMs\":60000,\"initialBackoffMs\":1000,\"maxBackoffMs\":30000}",
        "{\"refreshIntervalMs\":60000,\"initialBackoffMs\":1000,\"maxBackoffMs\":30000,\"maxRetries\":3,\"unknown\":\"SECRET\"}",
        "{\"refreshIntervalMs\":60000,\"initialBackoffMs\":1000,\"maxBackoffMs\":30000,\"maxRetries\":3,\"maxRetries\":4}"})
    void rejectsMalformedRefreshPolicyWithoutLeakingInput(final String policy) {
        String source = "{\"aggregation\":{\"revision\":\"v1\",\"servers\":[],\"refreshPolicy\":" + policy + "}}";
        IllegalArgumentException error = assertThrows(IllegalArgumentException.class, () -> AgentGatewayAggregationConfig.parsePluginConfig(source));
        assertTrue(!error.toString().contains("SECRET"));
        assertEquals(null, error.getCause());
    }

    @ParameterizedTest
    @MethodSource("invalidPolicyValues")
    void rejectsInvalidPolicyNumbers(final String field, final String value) {
        String policy = "{\"refreshIntervalMs\":60000,\"initialBackoffMs\":1000,\"maxBackoffMs\":30000,\"maxRetries\":3}";
        String changed = policy.replaceAll("\"" + field + "\":\\d+", "\"" + field + "\":" + value);
        String source = "{\"aggregation\":{\"revision\":\"v1\",\"servers\":[],\"refreshPolicy\":" + changed + "}}";
        assertThrows(IllegalArgumentException.class, () -> AgentGatewayAggregationConfig.parsePluginConfig(source));
    }

    private static Stream<Arguments> invalidPolicyValues() {
        return Stream.of("refreshIntervalMs", "initialBackoffMs", "maxBackoffMs", "maxRetries")
                .flatMap(field -> Stream.of("null", "\"2\"", "1.5", "2.0", "true", "[]", "{}", "4294967297", "-1")
                        .map(value -> Arguments.of(field, value)));
    }

    @ParameterizedTest
    @ValueSource(strings = {
        "{\"refreshIntervalMs\":4999,\"initialBackoffMs\":1000,\"maxBackoffMs\":2000,\"maxRetries\":3}",
        "{\"refreshIntervalMs\":3600001,\"initialBackoffMs\":1000,\"maxBackoffMs\":30000,\"maxRetries\":3}",
        "{\"refreshIntervalMs\":60000,\"initialBackoffMs\":999,\"maxBackoffMs\":30000,\"maxRetries\":3}",
        "{\"refreshIntervalMs\":60000,\"initialBackoffMs\":2000,\"maxBackoffMs\":1000,\"maxRetries\":3}",
        "{\"refreshIntervalMs\":60000,\"initialBackoffMs\":1000,\"maxBackoffMs\":60001,\"maxRetries\":3}",
        "{\"refreshIntervalMs\":60000,\"initialBackoffMs\":1000,\"maxBackoffMs\":30000,\"maxRetries\":9}"})
    void parserEnforcesPolicyTimingAndRetryBounds(final String policy) {
        String source = "{\"aggregation\":{\"revision\":\"v1\",\"servers\":[],\"refreshPolicy\":" + policy + "}}";
        assertThrows(IllegalArgumentException.class, () -> AgentGatewayAggregationConfig.parsePluginConfig(source));
    }

    @ParameterizedTest
    @ValueSource(ints = {1, 2, 8})
    void readsExplicitRefreshConcurrency(final int concurrency) {
        String source = "{\"aggregation\":{\"revision\":\"v1\",\"servers\":[],\"refreshConcurrency\":" + concurrency + "}}";
        AgentGatewayAggregationConfig config = AgentGatewayAggregationConfig.parsePluginConfig(source);
        assertEquals(concurrency, config.refreshConcurrency());
        assertEquals("v1", config.revision());
        assertTrue(config.servers().isEmpty());
    }

    @ParameterizedTest
    @ValueSource(strings = {"0", "9", "-1", "null", "\"2\"", "1.5", "2.0", "true", "[]", "{}", "4294967297", "999999999999999999999999"})
    void rejectsInvalidRefreshConcurrency(final String value) {
        String source = "{\"aggregation\":{\"revision\":\"v1\",\"servers\":[],\"refreshConcurrency\":" + value + "}}";
        assertThrows(IllegalArgumentException.class, () -> AgentGatewayAggregationConfig.parsePluginConfig(source));
    }

    @ParameterizedTest
    @ValueSource(strings = {
        "{\"aggregation\":{\"revision\":\"v1\",\"refreshConcurrency\":2}}",
        "{\"aggregation\":{\"servers\":[],\"refreshConcurrency\":2}}",
        "{\"aggregation\":{\"revision\":\"v1\",\"servers\":[],\"refreshConcurreny\":2}}",
        "{\"aggregation\":{\"revision\":\"v1\",\"servers\":[],\"refreshConcurrency\":2,\"unknown\":1}}",
        "{\"aggregation\":{\"revision\":\"v1\",\"servers\":[],\"refreshConcurrency\":2,\"refreshConcurrency\":3}}"})
    void optionalConcurrencyDoesNotRelaxOtherFieldChecks(final String source) {
        assertThrows(IllegalArgumentException.class, () -> AgentGatewayAggregationConfig.parsePluginConfig(source));
    }

    @ParameterizedTest
    @ValueSource(ints = {1, 2, 8})
    void constructorAcceptsRefreshConcurrencyWithinBounds(final int concurrency) {
        AgentGatewayAggregationConfig config = new AgentGatewayAggregationConfig("v1", List.of(), concurrency);
        assertEquals(concurrency, config.refreshConcurrency());
    }

    @ParameterizedTest
    @ValueSource(ints = {Integer.MIN_VALUE, -1, 0, 9, Integer.MAX_VALUE})
    void constructorRejectsRefreshConcurrencyOutsideBounds(final int concurrency) {
        assertThrows(IllegalArgumentException.class, () -> new AgentGatewayAggregationConfig("v1", List.of(), concurrency));
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
        String server = "{\"name\":\"orders\",\"endpoint\":\"https://192.0.2.10:443/mcp\",\"credentialRef\":\"service/orders\",\"credentialVersion\":\"v1\"}";
        AgentGatewayAggregationConfig config = AgentGatewayAggregationConfig.parsePluginConfig(wrap(server));
        assertEquals("orders", config.servers().get(0).name());
        assertThrows(UnsupportedOperationException.class, () -> config.servers().clear());
        assertThrows(IllegalArgumentException.class, () -> AgentGatewayAggregationConfig.parsePluginConfig(wrap(server + "," + server)));
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
