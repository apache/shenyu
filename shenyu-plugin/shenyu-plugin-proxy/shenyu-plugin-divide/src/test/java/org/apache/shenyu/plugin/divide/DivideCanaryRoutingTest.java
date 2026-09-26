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
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.base.utils.LoadbalancerUtils;
import org.apache.shenyu.plugin.divide.canary.CanaryDecision;
import org.apache.shenyu.plugin.divide.canary.CanaryDecisionService;
import org.apache.shenyu.plugin.divide.canary.DefaultCanaryDecisionService;
import org.apache.shenyu.plugin.divide.handler.DividePluginDataHandler;
import org.apache.shenyu.plugin.divide.handler.DivideUpstreamDataHandler;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.http.HttpStatus;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.net.InetSocketAddress;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * Verifies initial routing, fallback, exchange snapshots and legacy bypass.
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
        selector.setId("canary-routing-selector");
        DivideRuleHandle initial = new DivideRuleHandle();
        rule.setHandle(GsonUtils.getGson().toJson(initial));
        new DividePluginDataHandler().handlerRule(rule);
        handle = DividePluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(rule));
        config = new CanaryConfig();
        config.setEnabled(true);
        config.setCanaryLabels(new HashMap<>(Map.of("release", "canary", "region", "east")));
        config.setStableLabels(new HashMap<>(Map.of("release", "stable")));
        handle.setCanary(config);
        when(decisions.decide(any(), anyString(), any())).thenReturn(CanaryDecision.CANARY);
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        when(context.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());
        SpringBeanUtils.getInstance().setApplicationContext(context);
    }

    @Test
    void testAllLabelsAndSnapshotBeforeBackendFailure() {
        Upstream canary = upstream("canary:8080", Map.of("release", "canary", "region", "east", "extra", "value"));
        Upstream wrongRegion = upstream("other:8080", Map.of("release", "canary", "region", "west"));
        ServerWebExchange exchange = exchange(null);
        ShenyuPluginChain chain = current -> {
            assertEquals("canary", current.getAttribute(Constants.SHENYU_CANARY_PARTITION));
            assertEquals("http://canary:8080", current.getAttribute(Constants.HTTP_DOMAIN));
            CanaryContext observation = current.getAttribute(Constants.SHENYU_CANARY_CONTEXT);
            assertEquals(rule.getId(), observation.getRuleId());
            assertEquals(selector.getId(), observation.getSelectorId());
            assertEquals("canary", observation.getIntendedPartition());
            assertEquals("canary", observation.getActualPartition());
            assertNull(observation.getFallbackReason());
            assertNull(observation.getRejectReason());
            assertTrue(observation.getDecisionDurationNanos() >= 0);
            return Mono.error(new IllegalStateException("backend failed"));
        };
        try (MockedStatic<UpstreamCacheManager> cache = cache(List.of(wrongRegion, canary))) {
            StepVerifier.create(plugin.doExecute(exchange, chain, selector, rule)).expectError(IllegalStateException.class).verify();
        }
        Map<String, String> labels = exchange.getAttribute(Constants.SHENYU_CANARY_LABELS);
        config.getCanaryLabels().put("region", "west");
        assertEquals("east", labels.get("region"));
        assertThrows(UnsupportedOperationException.class, () -> labels.put("region", "south"));
        assertEquals("canary", exchange.getAttribute(Constants.SHENYU_CANARY_PARTITION));
    }

    @Test
    void testInitialFallbackSavesStable() {
        ServerWebExchange exchange = exchange(null);
        try (MockedStatic<UpstreamCacheManager> cache = cache(List.of(upstream("stable:8080", Map.of("release", "stable"))))) {
            StepVerifier.create(plugin.doExecute(exchange, current -> Mono.empty(), selector, rule)).verifyComplete();
        }
        assertEquals("stable", exchange.getAttribute(Constants.SHENYU_CANARY_PARTITION));
        assertEquals(Map.of("release", "stable"), exchange.getAttribute(Constants.SHENYU_CANARY_LABELS));
        CanaryContext observation = exchange.getAttribute(Constants.SHENYU_CANARY_CONTEXT);
        assertEquals("canary", observation.getIntendedPartition());
        assertEquals("stable", observation.getActualPartition());
        assertEquals(CanaryContext.CANARY_POOL_EMPTY, observation.getFallbackReason());
        assertNull(observation.getRejectReason());
    }

    @Test
    void testRejectAndEmptyStableNeverPublishPartition() {
        config.setFallbackPolicy("REJECT");
        assertRejected(List.of(upstream("stable:8080", Map.of("release", "stable"))));
        config.setFallbackPolicy("STABLE");
        assertRejected(List.of(upstream("unlabelled:8080", Map.of())));
        when(decisions.decide(any(), anyString(), any())).thenReturn(CanaryDecision.STABLE);
        assertRejected(List.of(upstream("canary:8080", Map.of("release", "canary", "region", "east"))));
    }

    @Test
    void testMissingLabelsDoNotSelectWholePool() {
        config.setCanaryLabels(null);
        config.setStableLabels(Map.of());
        assertRejected(List.of(upstream("unlabelled:8080", Map.of())));
    }

    @Test
    void testNoHealthyUpstreamDoesNotDecide() {
        assertRejected(List.of());
        verifyNoInteractions(decisions);
    }

    @Test
    void testPoolIsSavedBeforeLoadBalancerAndNullNodeKeepsLegacyError() {
        ServerWebExchange exchange = exchange(null);
        ShenyuPluginChain chain = mock(ShenyuPluginChain.class);
        try (MockedStatic<LoadbalancerUtils> balance = mockStatic(LoadbalancerUtils.class);
                MockedStatic<UpstreamCacheManager> cache = cache(List.of(upstream("canary:8080", Map.of("release", "canary", "region", "east"))))) {
            balance.when(() -> LoadbalancerUtils.getForExchange(anyList(), anyString(), any())).thenAnswer(invocation -> {
                assertEquals("canary", exchange.getAttribute(Constants.SHENYU_CANARY_PARTITION));
                return null;
            });
            StepVerifier.create(plugin.doExecute(exchange, chain, selector, rule)).verifyComplete();
        }
        verifyNoInteractions(chain);
        assertNull(exchange.getResponse().getStatusCode());
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
            assertLegacy(exchange(null), "http://gray:8080");
        }
        verifyNoInteractions(decisions);
    }

    @Test
    void testNewAndLegacyRulesShareSelectorWithoutLosingStableNodes() {
        DiscoverySyncData data = new DiscoverySyncData();
        data.setSelectorId(selector.getId());
        data.setUpstreamDataList(List.of(
                DiscoveryUpstreamData.builder().url("gray:8080").protocol("http://").status(0)
                        .props("{\"gray\":true,\"healthCheckEnabled\":false,\"labels\":{\"release\":\"canary\",\"region\":\"east\"}}").build(),
                DiscoveryUpstreamData.builder().url("stable:8080").protocol("http://").status(0)
                        .props("{\"healthCheckEnabled\":false,\"labels\":{\"release\":\"stable\"}}").build()));
        new DivideUpstreamDataHandler().handlerDiscoveryUpstreamData(data);
        try {
            plugin = new DividePlugin(new DefaultCanaryDecisionService());
            config.setEnabled(false);
            ServerWebExchange stableRequest = exchange(null);
            StepVerifier.create(plugin.doExecute(stableRequest, current -> Mono.empty(), selector, rule)).verifyComplete();
            assertEquals("http://stable:8080", stableRequest.getAttribute(Constants.HTTP_DOMAIN));
            assertEquals("stable", stableRequest.getAttribute(Constants.SHENYU_CANARY_PARTITION));
            CanaryContext observation = stableRequest.getAttribute(Constants.SHENYU_CANARY_CONTEXT);
            assertEquals("stable", observation.getIntendedPartition());
            assertEquals("stable", observation.getActualPartition());
            config.setEnabled(true);
            config.setPercentage(100);
            ServerWebExchange canaryRequest = exchange(null);
            StepVerifier.create(plugin.doExecute(canaryRequest, current -> Mono.empty(), selector, rule)).verifyComplete();
            assertEquals("http://gray:8080", canaryRequest.getAttribute(Constants.HTTP_DOMAIN));
            assertEquals("canary", canaryRequest.getAttribute(Constants.SHENYU_CANARY_PARTITION));
            handle.setCanary(null);
            assertLegacy(exchange(null), "http://gray:8080");
            assertEquals(2, UpstreamCacheManager.getInstance().findUpstreamListBySelectorId(selector.getId()).size());
        } finally {
            UpstreamCacheManager.getInstance().removeByKey(selector.getId());
        }
    }

    @Test
    void testSpecifyDomainAndLegacyDoNotEscapeUnavailableGrayPool() {
        UpstreamCacheManager manager = mock(UpstreamCacheManager.class);
        when(manager.findUpstreamListBySelectorId(selector.getId())).thenReturn(List.of(upstream("stable:8080", Map.of("release", "stable"))));
        when(manager.findLegacyUpstreamListBySelectorId(selector.getId())).thenReturn(List.of());
        ShenyuPluginChain chain = mock(ShenyuPluginChain.class);
        try (MockedStatic<UpstreamCacheManager> cache = mockStatic(UpstreamCacheManager.class)) {
            cache.when(UpstreamCacheManager::getInstance).thenReturn(manager);
            ServerWebExchange specified = exchange("override:9090");
            StepVerifier.create(plugin.doExecute(specified, chain, selector, rule)).verifyComplete();
            assertNull(specified.getAttribute(Constants.HTTP_DOMAIN));
            assertNull(specified.getAttribute(Constants.SHENYU_CANARY_PARTITION));
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
        try (MockedStatic<UpstreamCacheManager> cache = cache(List.of(shared))) {
            assertLegacy(exchange("override:9090"), "http://override:9090");
            assertEquals("original:8080", shared.getUrl());
            handle.setCanary(null);
            assertLegacy(exchange(null), "http://original:8080");
        }
        verifyNoInteractions(decisions);
    }

    @Test
    void testSpecifyDomainStillRequiresAvailableNode() {
        ShenyuPluginChain chain = mock(ShenyuPluginChain.class);
        try (MockedStatic<UpstreamCacheManager> cache = cache(List.of())) {
            StepVerifier.create(plugin.doExecute(exchange("override:9090"), chain, selector, rule)).verifyComplete();
        }
        verifyNoInteractions(chain, decisions);
    }

    @Test
    void testObservationCallbackCannotBreakRouting() {
        ServerWebExchange exchange = exchange(null);
        AtomicInteger callbacks = new AtomicInteger();
        exchange.getAttributes().put(Constants.METRICS_CANARY, (Consumer<CanaryContext>) observation -> {
            callbacks.incrementAndGet();
            throw new IllegalStateException("metrics failed");
        });
        try (MockedStatic<UpstreamCacheManager> cache = cache(List.of(upstream("stable:8080", Map.of("release", "stable"))))) {
            StepVerifier.create(plugin.doExecute(exchange, current -> Mono.empty(), selector, rule)).verifyComplete();
        }
        assertEquals(1, callbacks.get());
        assertEquals("http://stable:8080", exchange.getAttribute(Constants.HTTP_DOMAIN));
        CanaryContext observation = exchange.getAttribute(Constants.SHENYU_CANARY_CONTEXT);
        assertEquals(CanaryContext.CANARY_POOL_EMPTY, observation.getFallbackReason());
    }

    @Test
    void testFailedStableFallbackKeepsOriginalCanaryIntent() {
        ServerWebExchange exchange = exchange(null);
        try (MockedStatic<UpstreamCacheManager> cache = cache(List.of(upstream("unlabelled:8080", Map.of())))) {
            StepVerifier.create(plugin.doExecute(exchange, current -> Mono.empty(), selector, rule)).verifyComplete();
        }
        CanaryContext observation = exchange.getAttribute(Constants.SHENYU_CANARY_CONTEXT);
        assertEquals("canary", observation.getMetricPartition());
        assertNull(observation.getActualPartition());
        assertNull(observation.getFallbackReason());
        assertEquals(CanaryContext.STABLE_POOL_EMPTY, observation.getRejectReason());
    }

    @Test
    void testLoadBalancerExceptionStillPublishesDecision() {
        ServerWebExchange exchange = exchange(null);
        try (MockedStatic<LoadbalancerUtils> balance = mockStatic(LoadbalancerUtils.class);
                MockedStatic<UpstreamCacheManager> cache = cache(List.of(upstream("stable:8080", Map.of("release", "stable"))))) {
            balance.when(() -> LoadbalancerUtils.getForExchange(anyList(), anyString(), any())).thenThrow(new IllegalStateException("balance failed"));
            assertThrows(IllegalStateException.class, () -> plugin.doExecute(exchange, current -> Mono.empty(), selector, rule));
        }
        CanaryContext observation = exchange.getAttribute(Constants.SHENYU_CANARY_CONTEXT);
        assertNotNull(observation);
        assertNull(observation.getFallbackReason());
        assertNull(observation.getRejectReason());
        assertEquals("canary", observation.getIntendedPartition());
    }

    private void assertRejected(final List<Upstream> upstreams) {
        ServerWebExchange exchange = exchange(null);
        ShenyuPluginChain chain = mock(ShenyuPluginChain.class);
        try (MockedStatic<UpstreamCacheManager> cache = cache(upstreams)) {
            StepVerifier.create(plugin.doExecute(exchange, chain, selector, rule)).verifyComplete();
        }
        verifyNoInteractions(chain);
        assertNull(exchange.getAttribute(Constants.SHENYU_CANARY_PARTITION));
        assertNull(exchange.getAttribute(Constants.SHENYU_CANARY_LABELS));
        CanaryContext observation = exchange.getAttribute(Constants.SHENYU_CANARY_CONTEXT);
        if (!upstreams.isEmpty()) {
            assertEquals(HttpStatus.SERVICE_UNAVAILABLE, exchange.getResponse().getStatusCode());
            assertNotNull(observation);
            assertNull(observation.getActualPartition());
            assertNull(observation.getFallbackReason());
            assertNotNull(observation.getRejectReason());
        } else {
            assertNull(observation);
        }
    }

    private void assertLegacy(final ServerWebExchange exchange, final String domain) {
        StepVerifier.create(plugin.doExecute(exchange, current -> Mono.empty(), selector, rule)).verifyComplete();
        assertEquals(domain, exchange.getAttribute(Constants.HTTP_DOMAIN));
        assertNull(exchange.getAttribute(Constants.SHENYU_CANARY_CONTEXT));
        assertNull(exchange.getAttribute(Constants.SHENYU_CANARY_PARTITION));
        assertNull(exchange.getAttribute(Constants.SHENYU_CANARY_LABELS));
    }

    private MockedStatic<UpstreamCacheManager> cache(final List<Upstream> upstreams) {
        UpstreamCacheManager manager = mock(UpstreamCacheManager.class);
        when(manager.findUpstreamListBySelectorId(selector.getId())).thenReturn(upstreams);
        List<Upstream> gray = upstreams.stream().filter(Upstream::isGray).toList();
        when(manager.findLegacyUpstreamListBySelectorId(selector.getId())).thenReturn(gray.isEmpty() ? upstreams : gray);
        MockedStatic<UpstreamCacheManager> cache = mockStatic(UpstreamCacheManager.class);
        cache.when(UpstreamCacheManager::getInstance).thenReturn(manager);
        return cache;
    }

    private ServerWebExchange exchange(final String domain) {
        MockServerHttpRequest.BaseBuilder<?> request = MockServerHttpRequest.get("/test")
                .remoteAddress(new InetSocketAddress("127.0.0.1", 12345));
        if (Objects.nonNull(domain)) {
            request.header(Constants.SPECIFY_DOMAIN, domain);
        }
        ServerWebExchange exchange = MockServerWebExchange.from(request);
        exchange.getAttributes().put(Constants.CONTEXT, new ShenyuContext());
        return exchange;
    }

    private Upstream upstream(final String url, final Map<String, String> labels) {
        Upstream upstream = Upstream.builder().url(url).protocol("http://").weight(100).build();
        upstream.setMetadata(labels);
        return upstream;
    }
}
