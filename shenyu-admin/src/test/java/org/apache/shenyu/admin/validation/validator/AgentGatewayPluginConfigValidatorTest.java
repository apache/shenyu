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

package org.apache.shenyu.admin.validation.validator;

import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.apache.shenyu.common.dto.AgentGatewayAggregationConfig;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;

class AgentGatewayPluginConfigValidatorTest {

    @ParameterizedTest
    @ValueSource(strings = {"{}", "", "{\"aggregation\":{\"revision\":\"clear\",\"servers\":[]}}"})
    void acceptsExplicitLocalOnlyConfiguration(final String source) {
        assertDoesNotThrow(() -> AgentGatewayPluginConfigValidator.validate("agentGateway", source));
    }

    @ParameterizedTest
    @ValueSource(strings = {"[]", "{\"unknown\":1}", "{\"aggregation\":{\"revision\":\"v1\",\"servers\":[],\"bearerToken\":\"SECRET\"}}", "{} {}"})
    void rejectsInvalidAgentConfigButDoesNotChangeOtherPlugins(final String source) {
        assertThrows(ShenyuAdminException.class, () -> AgentGatewayPluginConfigValidator.validate("agentGateway", source));
        assertDoesNotThrow(() -> AgentGatewayPluginConfigValidator.validate("aiProxy", source));
    }

    @ParameterizedTest
    @ValueSource(ints = {1, 2, 8})
    void acceptsTrustedRefreshConcurrencyWithCompleteTimingPolicy(final int concurrency) {
        String source = "{\"aggregation\":{\"revision\":\"v1\",\"servers\":[],\"refreshConcurrency\":" + concurrency + ",\"refreshPolicy\":{"
                + "\"refreshIntervalMs\":60000,\"initialBackoffMs\":1000,\"maxBackoffMs\":30000,\"maxRetries\":3}}}";
        assertDoesNotThrow(() -> AgentGatewayPluginConfigValidator.validate("agentGateway", source));
        assertEquals(concurrency, AgentGatewayAggregationConfig.parsePluginConfig(source).refreshConcurrency());
    }

    @ParameterizedTest
    @ValueSource(strings = {"0", "9", "null", "\"2\"", "2.0", "4294967297"})
    void adminAndRuntimeRejectTheSameInvalidConcurrency(final String value) {
        String source = "{\"aggregation\":{\"revision\":\"v1\",\"servers\":[],\"refreshConcurrency\":" + value + "}}";
        assertThrows(ShenyuAdminException.class, () -> AgentGatewayPluginConfigValidator.validate("agentGateway", source));
        assertThrows(IllegalArgumentException.class, () -> AgentGatewayAggregationConfig.parsePluginConfig(source));
    }

    @ParameterizedTest
    @ValueSource(strings = {"null", "{}", "{\"refreshIntervalMs\":60000,\"initialBackoffMs\":1000,\"maxBackoffMs\":30000,\"maxRetries\":9}",
        "{\"refreshIntervalMs\":60000,\"initialBackoffMs\":3000,\"maxBackoffMs\":1000,\"maxRetries\":3}"})
    void adminAndRuntimeRejectTheSameInvalidRefreshPolicy(final String policy) {
        String source = "{\"aggregation\":{\"revision\":\"v1\",\"servers\":[],\"refreshPolicy\":" + policy + "}}";
        ShenyuAdminException error = assertThrows(ShenyuAdminException.class, () -> AgentGatewayPluginConfigValidator.validate("agentGateway", source));
        assertEquals("Invalid Agent Gateway aggregation configuration", error.getMessage());
        assertThrows(IllegalArgumentException.class, () -> AgentGatewayAggregationConfig.parsePluginConfig(source));
    }
}
