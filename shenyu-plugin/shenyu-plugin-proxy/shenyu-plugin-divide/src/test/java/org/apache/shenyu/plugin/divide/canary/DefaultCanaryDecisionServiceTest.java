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

package org.apache.shenyu.plugin.divide.canary;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.convert.rule.canary.CanaryConfig;
import org.apache.shenyu.common.dto.convert.rule.canary.StickyKeyConfig;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.base.utils.HostAddressUtils;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.springframework.http.HttpCookie;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mockStatic;

/**
 * Tests the partition contract, including portable hash vectors and rollout stability.
 */
class DefaultCanaryDecisionServiceTest {

    private final DefaultCanaryDecisionService service = new DefaultCanaryDecisionService();

    @Test
    void testPortableHashVectors() {
        // Independently calculated SHA-256 vectors, including unsigned values and UTF-8 byte lengths.
        assertEquals(3033, service.hashToBucket("rule-1", "user-1"));
        assertEquals(9925, service.hashToBucket("规则甲", "用户乙"));
        assertEquals(912, service.hashToBucket("ab", "c"));
        assertEquals(6726, service.hashToBucket("a", "bc"));
    }

    @Test
    void testDisabledConfigurationAlwaysSelectsStable() {
        CanaryConfig config = config(100, "unknown");
        config.setEnabled(false);
        config.setConditions(List.of(condition("user-1")));
        assertEquals(CanaryDecision.STABLE, service.decide(exchange("user-1"), "rule-1", config));
        config.setStickyKey(null);
        assertEquals(CanaryDecision.STABLE, service.decide(exchange("user-1"), "rule-1", config));
    }

    @Test
    void testPercentageBoundariesAndEmptyOrConditions() {
        CanaryConfig config = config(0, "header");
        config.setStickyKey(null);
        ServerWebExchange exchange = exchange("user-1");
        assertEquals(CanaryDecision.STABLE, service.decide(exchange, "rule-1", config));
        config.setPercentage(100);
        config.setMatchMode(1);
        config.setConditions(List.of());
        assertEquals(CanaryDecision.CANARY, service.decide(exchange, "rule-1", config));
    }

    @Test
    void testConditionsAreEligibilityBeforePercentage() {
        CanaryConfig config = config(100, "header");
        config.setConditions(List.of(condition("user-1"), condition("another-user")));
        ServerWebExchange exchange = exchange("user-1");
        assertEquals(CanaryDecision.STABLE, service.decide(exchange, "rule-1", config));
        config.setMatchMode(1);
        assertEquals(CanaryDecision.CANARY, service.decide(exchange, "rule-1", config));
        config.setMatchMode(null);
        assertEquals(CanaryDecision.STABLE, service.decide(exchange, "rule-1", config));
        config.setConditions(List.of(condition("user-1")));
        config.setPercentage(30);
        assertEquals(CanaryDecision.STABLE, service.decide(exchange, "rule-1", config));
        config.setPercentage(31);
        assertEquals(CanaryDecision.CANARY, service.decide(exchange, "rule-1", config));
    }

    @Test
    void testStickySourcesAndMissingIdentifiers() {
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/?user=user-1")
                .header("user", "user-1").cookie(new HttpCookie("user", "user-1")));
        for (String type : List.of("header", "cookie", "query")) {
            assertEquals(CanaryDecision.CANARY, service.decide(exchange, "rule-1", config(31, type)));
            assertEquals(CanaryDecision.STABLE, service.decide(MockServerWebExchange.from(MockServerHttpRequest.get("/")), "rule-1", config(31, type)));
        }
        try (MockedStatic<HostAddressUtils> addresses = mockStatic(HostAddressUtils.class)) {
            addresses.when(() -> HostAddressUtils.acquireIp(exchange)).thenReturn("user-1");
            CanaryConfig config = config(31, "ip");
            config.getStickyKey().setParamName(null);
            assertEquals(CanaryDecision.CANARY, service.decide(exchange, "rule-1", config));
        }
        assertEquals(CanaryDecision.STABLE, service.decide(exchange("   "), "rule-1", config(99, "header")));
    }

    @Test
    void testAdditionalBuiltinSourcesWithoutParameterName() {
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("http://example.com/orders?ignored=value"));
        // Thresholds come from independently calculated buckets: /orders=9824, GET=4747, example.com=391.
        assertSourceThreshold(exchange, "uri", null, 99);
        assertSourceThreshold(exchange, "req_method", null, 48);
        assertSourceThreshold(exchange, "domain", null, 4);
        try (MockedStatic<HostAddressUtils> addresses = mockStatic(HostAddressUtils.class)) {
            addresses.when(() -> HostAddressUtils.acquireHost(exchange)).thenReturn("client.example.com");
            assertSourceThreshold(exchange, "host", null, 55);
        }
    }

    @Test
    void testPostSourceReusesShenyuContextFields() {
        ServerWebExchange exchange = exchange("ignored");
        ShenyuContext context = new ShenyuContext();
        context.setModule("user-1");
        exchange.getAttributes().put(Constants.CONTEXT, context);
        assertSourceThreshold(exchange, "post", "module", 31);
    }

    @Test
    void testCustomSourceLoadedFromSpiResource() {
        ServerWebExchange exchange = exchange("ignored");
        exchange.getAttributes().put("authenticatedUserId", "user-1");
        exchange.getAttributes().put("tenantUser", "user-2");
        // Neither the source name nor its optional parameter name is built into the decision service.
        assertSourceThreshold(exchange, "test_attribute", null, 31);
        assertSourceThreshold(exchange, "test_attribute", "tenantUser", 77);
    }

    @Test
    void testCustomSourceMissingValueAndExtractionFailure() {
        ServerWebExchange exchange = exchange("ignored");
        CanaryConfig config = config(99, "test_attribute");
        config.getStickyKey().setParamName(null);
        assertEquals(CanaryDecision.STABLE, service.decide(exchange, "rule-1", config));
        exchange.getAttributes().put("authenticatedUserId", "   ");
        assertEquals(CanaryDecision.STABLE, service.decide(exchange, "rule-1", config));
        exchange.getAttributes().put("authenticatedUserId", new Object());
        assertThrows(ClassCastException.class, () -> service.decide(exchange, "rule-1", config));
    }

    @Test
    void testRequiredSourceType() {
        ServerWebExchange exchange = exchange("user-1");
        CanaryConfig missingType = config(50, null);
        assertThrows(IllegalArgumentException.class, () -> service.decide(exchange, "rule-1", missingType));
        for (String type : List.of("", "   ")) {
            assertThrows(IllegalArgumentException.class, () -> service.decide(exchange, "rule-1", config(50, type)));
        }
    }

    @Test
    void testBlankParameterNamesUseBuiltinExtraction() {
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/?user=user-1")
                .header("user", "user-1").cookie(new HttpCookie("user", "user-1")));
        ShenyuContext context = new ShenyuContext();
        context.setModule("user-1");
        exchange.getAttributes().put(Constants.CONTEXT, context);
        for (String type : List.of("header", "cookie", "query", "post")) {
            CanaryConfig missingName = config(50, type);
            for (String paramName : new String[] {null, "", "   "}) {
                missingName.getStickyKey().setParamName(paramName);
                assertEquals(CanaryDecision.STABLE, service.decide(exchange, "rule-1", missingName));
            }
        }
    }

    @Test
    void testRolloutExpansionPreservesUsersAcrossInstances() {
        CanaryConfig tenPercent = config(10, "header");
        CanaryConfig twentyPercent = config(20, "header");
        DefaultCanaryDecisionService otherInstance = new DefaultCanaryDecisionService();
        int originalCanaries = 0;
        int newCanaries = 0;
        for (int i = 0; i < 1000; i++) {
            ServerWebExchange exchange = exchange("user-" + i);
            CanaryDecision before = service.decide(exchange, "rule-1", tenPercent);
            assertEquals(before, otherInstance.decide(exchange, "rule-1", tenPercent));
            CanaryDecision after = otherInstance.decide(exchange, "rule-1", twentyPercent);
            if (before == CanaryDecision.CANARY) {
                originalCanaries++;
                assertEquals(CanaryDecision.CANARY, after);
            }
            if (after == CanaryDecision.CANARY) {
                newCanaries++;
            }
        }
        assertTrue(originalCanaries > 0);
        assertTrue(newCanaries > originalCanaries);
    }

    @Test
    void testInvalidConfigurationIsNotARequestKeyMiss() {
        ServerWebExchange exchange = exchange("user-1");
        assertThrows(IllegalArgumentException.class, () -> service.decide(exchange, "rule-1", config(-1, "header")));
        assertThrows(IllegalArgumentException.class, () -> service.decide(exchange, "rule-1", config(101, "header")));
        assertThrows(IllegalArgumentException.class, () -> service.decide(exchange, "rule-1", config(50, "unknown")));
        CanaryConfig missing = config(50, "header");
        missing.setStickyKey(null);
        assertThrows(IllegalArgumentException.class, () -> service.decide(exchange, "rule-1", missing));
    }

    private void assertSourceThreshold(final ServerWebExchange exchange, final String type, final String paramName, final int percentage) {
        CanaryConfig config = config(percentage, type);
        config.getStickyKey().setParamName(paramName);
        assertEquals(CanaryDecision.CANARY, service.decide(exchange, "rule-1", config));
        config.setPercentage(percentage - 1);
        assertEquals(CanaryDecision.STABLE, service.decide(exchange, "rule-1", config));
    }

    private CanaryConfig config(final int percentage, final String type) {
        CanaryConfig config = new CanaryConfig();
        config.setEnabled(true);
        config.setPercentage(percentage);
        StickyKeyConfig stickyKey = new StickyKeyConfig();
        stickyKey.setParamType(type);
        stickyKey.setParamName("user");
        config.setStickyKey(stickyKey);
        return config;
    }

    private ServerWebExchange exchange(final String user) {
        return MockServerWebExchange.from(MockServerHttpRequest.get("/").header("user", user));
    }

    private ConditionData condition(final String value) {
        ConditionData condition = new ConditionData();
        condition.setParamType("header");
        condition.setParamName("user");
        condition.setOperator("equals");
        condition.setParamValue(value);
        return condition;
    }
}
