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

package org.apache.shenyu.plugin.divide;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.canary.CanaryConfig;
import org.apache.shenyu.common.dto.convert.rule.impl.DivideRuleHandle;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.CanaryContext;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.circuitbreaker.UpstreamCircuitBreaker;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.base.utils.LoadbalancerUtils;
import org.apache.shenyu.plugin.divide.canary.CanaryDecision;
import org.apache.shenyu.plugin.divide.canary.CanaryDecisionService;
import org.apache.shenyu.plugin.divide.canary.DefaultCanaryDecisionService;
import org.apache.shenyu.plugin.divide.handler.DividePluginDataHandler;
import org.apache.shenyu.plugin.divide.handler.DivideUpstreamDataHandler;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.http.HttpStatus;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Consumer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * Verifies partition selection before load balancing, empty pools and legacy compatibility.
 */
class DivideCanaryRoutingTest {

    private RuleData rule;

    private SelectorData selector;

    private DivideRuleHandle handle;

    private CanaryConfig config;

    private CanaryDecisionService decisions;

    private DividePlugin plugin;

    @BeforeEach
    void setUp() {
        decisions = mock(CanaryDecisionService.class);
        plugin = new DividePlugin(decisions);
        rule = new RuleData();
        rule.setId("canary-routing-rule");
        rule.setSelectorId("canary-routing-selector");
        selector = new SelectorData();
        selector.setId(rule.getSelectorId());
        rule.setHandle(GsonUtils.getGson().toJson(new DivideRuleHandle()));
        new DividePluginDataHandler().handlerRule(rule);
        handle = DividePluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(rule));
        config = new CanaryConfig();
        config.setEnabled(true);
        config.setCanaryLabels(Map.of("release", "canary", "region", "east"));
        config.setStableLabels(Map.of("release", "stable"));
        handle.setCanary(config);
        when(decisions.decide(any(), anyString(), any())).thenReturn(CanaryDecision.CANARY);
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        when(context.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());
        SpringBeanUtils.getInstance().setApplicationContext(context);
    }

    @AfterEach
    void tearDown() {
        new DividePluginDataHandler().removeRule(rule);
        UpstreamCacheManager.getInstance().removeByKey(selector.getId());
    }

    @Test
    void testAllLabelsMustMatchAndBackendFailureDoesNotFallback() {
        Upstream canary = upstream("canary:8080", Map.of("release", "canary", "region", "east", "extra", "value"));
        Upstream wrongRegion = upstream("other:8080", Map.of("release", "canary", "region", "west"));
        ServerWebExchange exchange = exchange(null);
        ShenyuPluginChain chain = current -> {
            assertEquals("http://canary:8080", current.getAttribute(Constants.HTTP_DOMAIN));
            return Mono.error(new IllegalStateException("backend failed"));
        };
        try (MockedStatic<UpstreamCacheManager> cache = cache(List.of(wrongRegion, canary, upstream("stable:8080", Map.of("release", "stable"))))) {
            StepVerifier.create(plugin.doExecute(exchange, chain, selector, rule)).expectError(IllegalStateException.class).verify();
        } finally {
            UpstreamCircuitBreaker.getInstance().reset(UpstreamCircuitBreaker.buildKey(selector.getId(), canary));
        }
    }

    @Test
    void testEmptyCanaryPoolFallsBackToStable() {
        ServerWebExchange exchange = exchange(null);
        try (MockedStatic<UpstreamCacheManager> cache = cache(List.of(upstream("stable:8080", Map.of("release", "stable"))))) {
            assertDomain(exchange, "http://stable:8080");
        }
        CanaryContext observation = exchange.getAttribute(Constants.SHENYU_CANARY_CONTEXT);
        assertEquals("canary", observation.getIntendedPartition());
        assertEquals("stable", observation.getActualPartition());
        assertEquals(CanaryContext.CANARY_POOL_EMPTY, observation.getFallbackReason());
        assertNull(observation.getRejectReason());
    }

    @Test
    void testRejectPolicyAndEmptyStablePool() {
        config.setFallbackPolicy("REJECT");
        assertRejected(List.of(upstream("stable:8080", Map.of("release", "stable"))));
        config.setFallbackPolicy("STABLE");
        assertRejected(List.of(upstream("unlabelled:8080", Map.of())));
        when(decisions.decide(any(), anyString(), any())).thenReturn(CanaryDecision.STABLE);
        assertRejected(List.of(upstream("canary:8080", Map.of("release", "canary", "region", "east"))));
    }

    @Test
    void testMissingLabelsCannotSelectWholePool() {
        config.setCanaryLabels(null);
        config.setStableLabels(Map.of());
        assertRejected(List.of(upstream("unlabelled:8080", Map.of())));
    }

    @Test
    void testNoHealthyUpstreamRecordsTheIntendedPartition() {
        ServerWebExchange exchange = exchange(null);
        try (MockedStatic<UpstreamCacheManager> cache = cache(List.of())) {
            StepVerifier.create(plugin.doExecute(exchange, current -> Mono.error(new AssertionError("must reject before forwarding")), selector, rule)).verifyComplete();
        }
        assertEquals(HttpStatus.SERVICE_UNAVAILABLE, exchange.getResponse().getStatusCode());
        CanaryContext observation = exchange.getAttribute(Constants.SHENYU_CANARY_CONTEXT);
        assertEquals("canary", observation.getIntendedPartition());
        assertNull(observation.getActualPartition());
        assertEquals(CanaryContext.STABLE_POOL_EMPTY, observation.getRejectReason());
        assertNull(observation.getFallbackReason());
        verify(decisions).decide(exchange, rule.getId(), config);
    }

    @Test
    void testRoutingCallbackIsOptionalAndCannotBreakForwarding() {
        ServerWebExchange exchange = exchange(null);
        exchange.getAttributes().put(Constants.METRICS_CANARY, (Consumer<CanaryContext>) observation -> {
            assertEquals("canary", observation.getActualPartition());
            assertTrue(observation.getDecisionDurationNanos() >= 0);
            throw new IllegalStateException("reporter unavailable");
        });
        try (MockedStatic<UpstreamCacheManager> cache = cache(List.of(upstream("canary:8080", Map.of("release", "canary", "region", "east"))))) {
            assertDomain(exchange, "http://canary:8080");
        }
    }

    @Test
    void testFallbackWithoutASelectedUpstreamIsRejected() {
        ServerWebExchange exchange = exchange(null);
        try (MockedStatic<UpstreamCacheManager> cache = cache(List.of(upstream("stable:8080", Map.of("release", "stable"))));
                MockedStatic<LoadbalancerUtils> balancing = mockStatic(LoadbalancerUtils.class)) {
            balancing.when(() -> LoadbalancerUtils.getForExchange(any(), any(), any())).thenReturn(null);
            StepVerifier.create(plugin.doExecute(exchange, current -> Mono.error(new AssertionError("must not forward")), selector, rule)).verifyComplete();
        }
        CanaryContext observation = exchange.getAttribute(Constants.SHENYU_CANARY_CONTEXT);
        assertNull(observation.getActualPartition());
        assertNull(observation.getFallbackReason());
        assertEquals(CanaryContext.NO_UPSTREAM_SELECTED, observation.getRejectReason());
    }

    @Test
    void testAbsentConfigurationKeepsLegacyGrayPool() {
        Upstream gray = upstream("gray:8080", Map.of());
        gray.setGray(true);
        try (MockedStatic<UpstreamCacheManager> cache = cache(List.of(gray, upstream("normal:8080", Map.of())))) {
            handle.setCanary(null);
            assertDomain(exchange(null), "http://gray:8080");
        }
        verifyNoInteractions(decisions);
    }

    @Test
    void testNewAndLegacyRulesShareSelectorWithoutLosingStableNodes() {
        DiscoverySyncData data = new DiscoverySyncData();
        data.setSelectorId(selector.getId());
        data.setUpstreamDataList(List.of(
                instance("gray:8080", true, "canary"), instance("stable:8080", false, "stable")));
        new DivideUpstreamDataHandler().handlerDiscoveryUpstreamData(data);
        plugin = new DividePlugin(new DefaultCanaryDecisionService());
        config.setEnabled(false);
        assertDomain(exchange(null), "http://stable:8080");
        config.setEnabled(true);
        config.setPercentage(100);
        assertDomain(exchange(null), "http://gray:8080");
        handle.setCanary(null);
        assertDomain(exchange(null), "http://gray:8080");
        assertEquals(1, UpstreamCacheManager.getInstance().findUpstreamListBySelectorId(selector.getId()).size());
        assertEquals(2, UpstreamCacheManager.getInstance().findAllUpstreamListBySelectorId(selector.getId()).size());
    }

    @Test
    void testSpecifyDomainAndLegacyDoNotEscapeUnavailableGrayPool() {
        UpstreamCacheManager manager = mock(UpstreamCacheManager.class);
        when(manager.findAllUpstreamListBySelectorId(selector.getId())).thenReturn(List.of(upstream("stable:8080", Map.of("release", "stable"))));
        when(manager.findUpstreamListBySelectorId(selector.getId())).thenReturn(List.of());
        ShenyuPluginChain chain = mock(ShenyuPluginChain.class);
        try (MockedStatic<UpstreamCacheManager> cache = mockStatic(UpstreamCacheManager.class)) {
            cache.when(UpstreamCacheManager::getInstance).thenReturn(manager);
            ServerWebExchange specified = exchange("stable:8080");
            StepVerifier.create(plugin.doExecute(specified, chain, selector, rule)).verifyComplete();
            assertNull(specified.getAttribute(Constants.HTTP_DOMAIN));
            handle.setCanary(null);
            ServerWebExchange legacy = exchange(null);
            StepVerifier.create(plugin.doExecute(legacy, chain, selector, rule)).verifyComplete();
            assertNull(legacy.getAttribute(Constants.HTTP_DOMAIN));
        }
        verifyNoInteractions(chain, decisions);
    }

    @Test
    void testSpecifyDomainBypassesCanaryWithoutMutatingSharedNode() {
        Upstream shared = upstream("original:8080", Map.of());
        Upstream specified = upstream("override:9090", Map.of());
        try (MockedStatic<UpstreamCacheManager> cache = cache(List.of(shared, specified))) {
            assertDomain(exchange("override:9090"), "http://override:9090");
            assertEquals("original:8080", shared.getUrl());
        }
        try (MockedStatic<UpstreamCacheManager> cache = cache(List.of(shared))) {
            handle.setCanary(null);
            assertDomain(exchange("unconfigured:9090"), "http://original:8080");
            assertDomain(exchange(null), "http://original:8080");
        }
        verifyNoInteractions(decisions);
    }

    @Test
    void testCircuitBreakerCannotSelectOutsideCanaryPartition() {
        Upstream blocked = upstream("blocked-canary:8080", Map.of("release", "canary", "region", "east"));
        Upstream available = upstream("available-canary:8080", Map.of("release", "canary", "region", "east"));
        Upstream stable = upstream("stable:8080", Map.of("release", "stable"));
        UpstreamCircuitBreaker breaker = UpstreamCircuitBreaker.getInstance();
        String key = UpstreamCircuitBreaker.buildKey(selector.getId(), blocked);
        for (int i = 0; i < UpstreamCircuitBreaker.DEFAULT_FAILURE_THRESHOLD; i++) {
            breaker.recordFailure(key);
        }
        try {
            try (MockedStatic<UpstreamCacheManager> cache = cache(List.of(blocked, available, stable))) {
                assertDomain(exchange(null), "http://available-canary:8080");
            }
            assertRejected(List.of(blocked, stable));
        } finally {
            breaker.reset(key);
        }
    }

    @Test
    void testHalfOpenProbeStaysInCanaryPool() {
        Upstream canary = upstream("probe-canary:8080", Map.of("release", "canary", "region", "east"));
        Upstream stable = upstream("probe-stable:8080", Map.of("release", "stable"));
        UpstreamCircuitBreaker breaker = UpstreamCircuitBreaker.getInstance();
        String key = UpstreamCircuitBreaker.buildKey(selector.getId(), canary);
        for (int i = 0; i < UpstreamCircuitBreaker.DEFAULT_FAILURE_THRESHOLD; i++) {
            breaker.recordFailure(key);
        }
        Map<?, ?> breakers = (Map<?, ?>) ReflectionTestUtils.getField(breaker, "breakers");
        ReflectionTestUtils.setField(breakers.get(key), "openedAtMillis", System.currentTimeMillis() - 60000L);
        try (MockedStatic<UpstreamCacheManager> cache = cache(List.of(stable, canary))) {
            ServerWebExchange exchange = exchange(null);
            StepVerifier.create(plugin.doExecute(exchange, current -> {
                assertEquals("http://probe-canary:8080", current.getAttribute(Constants.HTTP_DOMAIN));
                assertTrue(Boolean.TRUE.equals(current.getAttribute(UpstreamCircuitBreaker.HALF_OPEN_PROBE)));
                return Mono.empty();
            }, selector, rule)).verifyComplete();
            assertFalse(breaker.isBlocking(key));
        } finally {
            breaker.reset(key);
        }
    }

    private void assertRejected(final List<Upstream> upstreams) {
        ServerWebExchange exchange = exchange(null);
        ShenyuPluginChain chain = mock(ShenyuPluginChain.class);
        try (MockedStatic<UpstreamCacheManager> cache = cache(upstreams)) {
            StepVerifier.create(plugin.doExecute(exchange, chain, selector, rule)).verifyComplete();
        }
        verifyNoInteractions(chain);
        assertNull(exchange.getAttribute(Constants.HTTP_DOMAIN));
        assertEquals(HttpStatus.SERVICE_UNAVAILABLE, exchange.getResponse().getStatusCode());
        CanaryContext observation = exchange.getAttribute(Constants.SHENYU_CANARY_CONTEXT);
        assertNotNull(observation);
        assertNull(observation.getActualPartition());
        assertNull(observation.getFallbackReason());
        assertNotNull(observation.getRejectReason());
    }

    private void assertDomain(final ServerWebExchange exchange, final String domain) {
        StepVerifier.create(plugin.doExecute(exchange, current -> Mono.empty(), selector, rule)).verifyComplete();
        assertEquals(domain, exchange.getAttribute(Constants.HTTP_DOMAIN));
    }

    private MockedStatic<UpstreamCacheManager> cache(final List<Upstream> upstreams) {
        UpstreamCacheManager manager = mock(UpstreamCacheManager.class);
        when(manager.findAllUpstreamListBySelectorId(selector.getId())).thenReturn(upstreams);
        List<Upstream> gray = upstreams.stream().filter(Upstream::isGray).toList();
        when(manager.findUpstreamListBySelectorId(selector.getId())).thenReturn(gray.isEmpty() ? upstreams : gray);
        MockedStatic<UpstreamCacheManager> cache = mockStatic(UpstreamCacheManager.class);
        cache.when(UpstreamCacheManager::getInstance).thenReturn(manager);
        return cache;
    }

    private ServerWebExchange exchange(final String domain) {
        MockServerHttpRequest.BaseBuilder<?> request = MockServerHttpRequest.get("/test");
        if (Objects.nonNull(domain)) {
            request.header(Constants.SPECIFY_DOMAIN, domain);
        }
        ServerWebExchange exchange = MockServerWebExchange.from(request);
        exchange.getAttributes().put(Constants.CONTEXT, new ShenyuContext());
        return exchange;
    }

    private Upstream upstream(final String url, final Map<String, String> labels) {
        Upstream upstream = Upstream.builder().url(url).protocol("http://").weight(100).build();
        upstream.setLabels(labels);
        return upstream;
    }

    private DiscoveryUpstreamData instance(final String url, final boolean gray, final String release) {
        Map<String, Object> props = Map.of("gray", gray, "healthCheckEnabled", false, "labels", Map.of("release", release, "region", "east"));
        return DiscoveryUpstreamData.builder().url(url).protocol("http://").status(0).props(GsonUtils.getInstance().toJson(props)).build();
    }
}
