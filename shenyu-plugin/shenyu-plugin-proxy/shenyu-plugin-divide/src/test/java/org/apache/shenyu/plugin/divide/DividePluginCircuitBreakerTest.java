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
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.DivideRuleHandle;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.circuitbreaker.UpstreamCircuitBreaker;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.divide.handler.DividePluginDataHandler;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;
import reactor.core.Disposable;
import reactor.test.StepVerifier;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ConcurrentMap;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test for the built-in circuit breaker behaviour of {@link DividePlugin}.
 */
public final class DividePluginCircuitBreakerTest {

    private static final String SELECTOR_ID = "breaker-selector";

    private static final String UPSTREAM_URL = "127.0.0.1:9090";

    private DividePlugin dividePlugin;

    private ShenyuPluginChain chain;

    private ServerWebExchange exchange;

    private SelectorData selectorData;

    private RuleData ruleData;

    private Upstream upstream;

    private String breakerKey;

    private MockedStatic<UpstreamCacheManager> cacheMock;

    @BeforeEach
    public void setup() {
        this.dividePlugin = new DividePlugin();
        this.chain = mock(ShenyuPluginChain.class);
        this.selectorData = mock(SelectorData.class);
        this.ruleData = mock(RuleData.class);
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.get("http://localhost/test").build());
        ShenyuContext context = mock(ShenyuContext.class);
        exchange.getAttributes().put(Constants.CONTEXT, context);
        this.upstream = Upstream.builder().protocol("http://").url(UPSTREAM_URL).build();
        this.breakerKey = UpstreamCircuitBreaker.buildKey(SELECTOR_ID, upstream);
        when(selectorData.getId()).thenReturn(SELECTOR_ID);
        when(ruleData.getHandle()).thenReturn(GsonUtils.getGson().toJson(new DivideRuleHandle()));
        DividePluginDataHandler dividePluginDataHandler = new DividePluginDataHandler();
        dividePluginDataHandler.handlerRule(ruleData);
        dividePluginDataHandler.handlerSelector(selectorData);
        ConfigurableApplicationContext applicationContext = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setApplicationContext(applicationContext);
        when(applicationContext.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());
        UpstreamCacheManager cacheManager = mock(UpstreamCacheManager.class);
        when(cacheManager.findUpstreamListBySelectorId(SELECTOR_ID)).thenAnswer(invocation -> new ArrayList<>(Collections.singletonList(upstream)));
        this.cacheMock = mockStatic(UpstreamCacheManager.class);
        cacheMock.when(UpstreamCacheManager::getInstance).thenReturn(cacheManager);
    }

    @AfterEach
    public void tearDown() {
        cacheMock.close();
        UpstreamCircuitBreaker.getInstance().reset(breakerKey);
    }

    @Test
    public void testFailFastWhenAllUpstreamsBlocked() {
        openBreaker();
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        StepVerifier.create(dividePlugin.doExecute(exchange, chain, selectorData, ruleData))
                .verifyComplete();
        verify(chain, times(0)).execute(any());
    }

    @Test
    public void testConsecutiveFailuresOpenBreakerThenFailFast() {
        when(chain.execute(exchange)).thenReturn(Mono.error(new IllegalStateException("upstream down")));
        for (int i = 0; i < 3; i++) {
            StepVerifier.create(dividePlugin.doExecute(exchange, chain, selectorData, ruleData))
                    .expectError(IllegalStateException.class)
                    .verify();
        }
        assertTrue(UpstreamCircuitBreaker.getInstance().isBlocking(breakerKey));
        // the next request fails fast without reaching the chain any more
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        StepVerifier.create(dividePlugin.doExecute(exchange, chain, selectorData, ruleData))
                .verifyComplete();
        verify(chain, times(3)).execute(any());
    }

    @Test
    public void testHalfOpenProbeRecoversBreaker() throws Exception {
        openBreaker();
        expireBreaker();
        // the probe request is forwarded and its success closes the breaker
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        StepVerifier.create(dividePlugin.doExecute(exchange, chain, selectorData, ruleData))
                .verifyComplete();
        verify(chain, times(1)).execute(any());
        assertFalse(UpstreamCircuitBreaker.getInstance().isBlocking(breakerKey));
    }

    private void expireBreaker() throws Exception {
        elapseWaitWindow(breakerKey);
    }

    private void elapseWaitWindow(final String key) throws Exception {
        Field breakersField = UpstreamCircuitBreaker.class.getDeclaredField("breakers");
        breakersField.setAccessible(true);
        ConcurrentMap<String, ?> breakers = (ConcurrentMap<String, ?>) breakersField.get(UpstreamCircuitBreaker.getInstance());
        Object state = breakers.get(key);
        Field openedAtField = state.getClass().getDeclaredField("openedAtMillis");
        openedAtField.setAccessible(true);
        openedAtField.setLong(state, System.currentTimeMillis() - 60000L);
    }

    @Test
    public void testHalfOpenProbeIsSelectedWhileAnotherUpstreamIsHealthy() throws Exception {
        openBreaker();
        elapseWaitWindow(breakerKey);
        Upstream healthy = Upstream.builder().protocol("http://").url("127.0.0.1:9091").build();
        UpstreamCacheManager cacheManager = mock(UpstreamCacheManager.class);
        when(cacheManager.findUpstreamListBySelectorId(SELECTOR_ID)).thenAnswer(invocation -> new ArrayList<>(List.of(upstream, healthy)));
        cacheMock.when(UpstreamCacheManager::getInstance).thenReturn(cacheManager);
        when(chain.execute(exchange)).thenReturn(Mono.empty());

        StepVerifier.create(dividePlugin.doExecute(exchange, chain, selectorData, ruleData))
                .verifyComplete();

        assertEquals(upstream.buildDomain(), exchange.getAttribute(Constants.HTTP_DOMAIN));
        assertFalse(UpstreamCircuitBreaker.getInstance().isBlocking(breakerKey));
        verify(chain, times(1)).execute(any());
    }

    @Test
    public void testSpecifiedHealthyUpstreamIsNotOverriddenByAnotherProbe() throws Exception {
        openBreaker();
        elapseWaitWindow(breakerKey);
        Upstream healthy = Upstream.builder().protocol("http://").url("127.0.0.1:9091").build();
        UpstreamCacheManager cacheManager = mock(UpstreamCacheManager.class);
        when(cacheManager.findUpstreamListBySelectorId(SELECTOR_ID)).thenAnswer(invocation -> new ArrayList<>(List.of(upstream, healthy)));
        cacheMock.when(UpstreamCacheManager::getInstance).thenReturn(cacheManager);
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.get("http://localhost/test")
                .header(Constants.SPECIFY_DOMAIN, healthy.getUrl()).build());
        exchange.getAttributes().put(Constants.CONTEXT, mock(ShenyuContext.class));
        when(chain.execute(exchange)).thenReturn(Mono.empty());

        StepVerifier.create(dividePlugin.doExecute(exchange, chain, selectorData, ruleData))
                .verifyComplete();

        assertEquals(healthy.buildDomain(), exchange.getAttribute(Constants.HTTP_DOMAIN));
        assertTrue(UpstreamCircuitBreaker.getInstance().isBlocking(breakerKey));
        verify(chain, times(1)).execute(any());
    }

    @Test
    public void testUnavailableSpecifiedUpstreamFallsBackToAvailableProbe() throws Exception {
        openBreaker();
        elapseWaitWindow(breakerKey);
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.get("http://localhost/test")
                .header(Constants.SPECIFY_DOMAIN, "127.0.0.1:9091").build());
        exchange.getAttributes().put(Constants.CONTEXT, mock(ShenyuContext.class));
        when(chain.execute(exchange)).thenReturn(Mono.empty());

        StepVerifier.create(dividePlugin.doExecute(exchange, chain, selectorData, ruleData))
                .verifyComplete();

        assertEquals(upstream.buildDomain(), exchange.getAttribute(Constants.HTTP_DOMAIN));
        assertFalse(UpstreamCircuitBreaker.getInstance().isBlocking(breakerKey));
        verify(chain, times(1)).execute(any());
    }

    @Test
    public void testCancelledHalfOpenProbeReopensBreaker() throws Exception {
        openBreaker();
        elapseWaitWindow(breakerKey);
        when(chain.execute(exchange)).thenReturn(Mono.never());

        StepVerifier.create(dividePlugin.doExecute(exchange, chain, selectorData, ruleData))
                .thenCancel()
                .verify();

        assertFalse(UpstreamCircuitBreaker.getInstance().tryAcquireHalfOpenProbe(breakerKey));
        elapseWaitWindow(breakerKey);
        assertTrue(UpstreamCircuitBreaker.getInstance().tryAcquireHalfOpenProbe(breakerKey));
    }

    @Test
    public void testCancelledClosedRequestLeavesBreakerClosed() {
        when(chain.execute(exchange)).thenReturn(Mono.never());

        StepVerifier.create(dividePlugin.doExecute(exchange, chain, selectorData, ruleData))
                .thenCancel()
                .verify();

        assertTrue(UpstreamCircuitBreaker.getInstance().isRequestAllowed(breakerKey));
    }

    @Test
    public void testHealthyUpstreamsBypassBreaker() {
        Upstream healthy = Upstream.builder().protocol("http://").url("127.0.0.1:9091").build();
        UpstreamCircuitBreaker.getInstance().reset(UpstreamCircuitBreaker.buildKey(SELECTOR_ID, healthy));
        openBreaker();
        UpstreamCacheManager cacheManager = mock(UpstreamCacheManager.class);
        List<Upstream> upstreams = List.of(upstream, healthy);
        when(cacheManager.findUpstreamListBySelectorId(SELECTOR_ID)).thenAnswer(invocation -> new ArrayList<>(upstreams));
        cacheMock.when(UpstreamCacheManager::getInstance).thenReturn(cacheManager);
        // the blocked upstream is filtered out, the request still goes through the healthy one
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        StepVerifier.create(dividePlugin.doExecute(exchange, chain, selectorData, ruleData))
                .verifyComplete();
        verify(chain, times(1)).execute(any());
        UpstreamCircuitBreaker.getInstance().reset(UpstreamCircuitBreaker.buildKey(SELECTOR_ID, healthy));
    }

    private void openBreaker() {
        for (int i = 0; i < 3; i++) {
            UpstreamCircuitBreaker.getInstance().recordFailure(breakerKey);
        }
    }

    @Test
    public void testForwardingReusesFilteredKeyForExplicitTarget() {
        exchange = MockServerWebExchange.from(MockServerHttpRequest.get("http://localhost/test")
                .header(Constants.SPECIFY_DOMAIN, UPSTREAM_URL).build());
        exchange.getAttributes().put(Constants.CONTEXT, new ShenyuContext());
        when(chain.execute(exchange)).thenReturn(Mono.error(new IllegalStateException("upstream down")));
        try (MockedStatic<UpstreamCircuitBreaker> breaker = mockStatic(UpstreamCircuitBreaker.class, CALLS_REAL_METHODS)) {
            for (int index = 0; index < 3; index++) {
                StepVerifier.create(dividePlugin.doExecute(exchange, chain, selectorData, ruleData))
                        .expectError(IllegalStateException.class).verify();
            }
            assertEquals(upstream.buildDomain(), exchange.getAttribute(Constants.HTTP_DOMAIN));
            assertTrue(UpstreamCircuitBreaker.getInstance().isBlocking(breakerKey));
            breaker.verify(() -> UpstreamCircuitBreaker.buildKey(SELECTOR_ID, upstream), times(3));
        }
    }

    @Test
    public void testBlockedExplicitTargetFallsBackToHealthyTarget() {
        Upstream healthy = Upstream.builder().protocol("http://").url("127.0.0.1:9092").weight(9).build();
        String healthyKey = UpstreamCircuitBreaker.buildKey(SELECTOR_ID, healthy);
        try {
            openBreaker();
            exchange = MockServerWebExchange.from(MockServerHttpRequest.get("http://localhost/test")
                    .header(Constants.SPECIFY_DOMAIN, UPSTREAM_URL).build());
            exchange.getAttributes().put(Constants.CONTEXT, new ShenyuContext());
            when(UpstreamCacheManager.getInstance().findUpstreamListBySelectorId(SELECTOR_ID))
                    .thenAnswer(invocation -> new ArrayList<>(List.of(upstream, healthy)));
            when(chain.execute(exchange)).thenReturn(Mono.empty());
            StepVerifier.create(dividePlugin.doExecute(exchange, chain, selectorData, ruleData)).verifyComplete();
            assertEquals(healthy.buildDomain(), exchange.getAttribute(Constants.HTTP_DOMAIN));
            assertTrue(UpstreamCircuitBreaker.getInstance().isBlocking(breakerKey));
            assertFalse(UpstreamCircuitBreaker.getInstance().isBlocking(healthyKey));
        } finally {
            UpstreamCircuitBreaker.getInstance().reset(healthyKey);
        }
    }

    @Test
    public void testCancellationAndSuccessKeepExistingAccountingAndRetryAttributes() {
        UpstreamCircuitBreaker.getInstance().recordFailure(breakerKey);
        DivideRuleHandle handle = DividePluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(ruleData));
        handle.setRetry(2);
        handle.setRetryStrategy("failover");
        when(chain.execute(exchange)).thenReturn(Mono.never());
        StepVerifier.create(dividePlugin.doExecute(exchange, chain, selectorData, ruleData)).thenCancel().verify();
        assertEquals(2, (Integer) exchange.getAttribute(Constants.HTTP_RETRY));
        assertEquals("failover", exchange.getAttribute(Constants.RETRY_STRATEGY));
        assertEquals(SELECTOR_ID, exchange.getAttribute(Constants.DIVIDE_SELECTOR_ID));
        UpstreamCircuitBreaker.getInstance().recordFailure(breakerKey);
        assertFalse(UpstreamCircuitBreaker.getInstance().isBlocking(breakerKey));
        UpstreamCircuitBreaker.getInstance().recordFailure(breakerKey);
        assertTrue(UpstreamCircuitBreaker.getInstance().isBlocking(breakerKey));
        UpstreamCircuitBreaker.getInstance().reset(breakerKey);
        UpstreamCircuitBreaker.getInstance().recordFailure(breakerKey);
        UpstreamCircuitBreaker.getInstance().recordFailure(breakerKey);
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        StepVerifier.create(dividePlugin.doExecute(exchange, chain, selectorData, ruleData)).verifyComplete();
        UpstreamCircuitBreaker.getInstance().recordFailure(breakerKey);
        UpstreamCircuitBreaker.getInstance().recordFailure(breakerKey);
        assertFalse(UpstreamCircuitBreaker.getInstance().isBlocking(breakerKey));
    }

    @Test
    public void testOneProbeWhileHealthySiblingContinuesServing() throws Exception {
        openBreaker();
        expireBreaker();
        Upstream healthy = Upstream.builder().protocol("http://").url("127.0.0.1:9093").build();
        when(UpstreamCacheManager.getInstance().findUpstreamListBySelectorId(SELECTOR_ID))
                .thenAnswer(invocation -> new ArrayList<>(List.of(healthy, upstream)));
        Sinks.Empty<Void> probe = Sinks.empty();
        when(chain.execute(any())).thenAnswer(invocation -> {
            ServerWebExchange request = invocation.getArgument(0);
            return upstream.buildDomain().equals(request.getAttribute(Constants.HTTP_DOMAIN)) ? probe.asMono() : Mono.empty();
        });
        Disposable subscription = dividePlugin.doExecute(exchange, chain, selectorData, ruleData).subscribe();
        try {
            assertEquals(upstream.buildDomain(), exchange.getAttribute(Constants.HTTP_DOMAIN));
            for (int index = 0; index < 32; index++) {
                ServerWebExchange other = newExchange();
                StepVerifier.create(dividePlugin.doExecute(other, chain, selectorData, ruleData)).verifyComplete();
                assertEquals(healthy.buildDomain(), other.getAttribute(Constants.HTTP_DOMAIN));
            }
            assertNull(UpstreamCircuitBreaker.getInstance().acquireHalfOpenProbe(breakerKey));
            probe.tryEmitEmpty();
            assertTrue(UpstreamCircuitBreaker.getInstance().isRequestAllowed(breakerKey));
        } finally {
            subscription.dispose();
        }
    }

    @Test
    public void testUnsubscribedRequestDoesNotReserveAProbe() throws Exception {
        openBreaker();
        expireBreaker();
        dividePlugin.doExecute(exchange, chain, selectorData, ruleData);
        assertNotNull(UpstreamCircuitBreaker.getInstance().acquireHalfOpenProbe(breakerKey));
        verify(chain, times(0)).execute(any());
    }

    @Test
    public void testProbeCancellationReleasesItsPermission() throws Exception {
        openBreaker();
        expireBreaker();
        when(chain.execute(exchange)).thenReturn(Mono.never());
        StepVerifier.create(dividePlugin.doExecute(exchange, chain, selectorData, ruleData)).thenCancel().verify();
        assertNull(UpstreamCircuitBreaker.getInstance().acquireHalfOpenProbe(breakerKey));
        expireBreaker();
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        StepVerifier.create(dividePlugin.doExecute(exchange, chain, selectorData, ruleData)).verifyComplete();
        assertTrue(UpstreamCircuitBreaker.getInstance().isRequestAllowed(breakerKey));
    }

    @Test
    public void testProbeCancellationBalancesP2cInflightAccounting() throws Exception {
        openBreaker();
        expireBreaker();
        DividePluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(ruleData)).setLoadBalance("p2c");
        long initial = upstream.getInflight().get();
        when(chain.execute(exchange)).thenReturn(Mono.never());
        Disposable subscription = dividePlugin.doExecute(exchange, chain, selectorData, ruleData).subscribe();
        assertEquals(initial + 1, upstream.getInflight().get());
        subscription.dispose();
        assertEquals(initial, upstream.getInflight().get());
        assertTrue(UpstreamCircuitBreaker.getInstance().isBlocking(breakerKey));
    }

    @Test
    public void testSynchronousForwardingErrorReleasesProbe() throws Exception {
        openBreaker();
        expireBreaker();
        when(chain.execute(exchange)).thenThrow(new IllegalStateException("before publisher"));
        StepVerifier.create(dividePlugin.doExecute(exchange, chain, selectorData, ruleData)).expectError(IllegalStateException.class).verify();
        expireBreaker();
        assertNotNull(UpstreamCircuitBreaker.getInstance().acquireHalfOpenProbe(breakerKey));
    }

    @Test
    public void testSpecifiedHealthyTargetDoesNotReserveSiblingProbe() throws Exception {
        openBreaker();
        expireBreaker();
        Upstream healthy = Upstream.builder().protocol("http://").url("127.0.0.1:9094").build();
        when(UpstreamCacheManager.getInstance().findUpstreamListBySelectorId(SELECTOR_ID))
                .thenAnswer(invocation -> new ArrayList<>(List.of(upstream, healthy)));
        ServerWebExchange specified = MockServerWebExchange.from(MockServerHttpRequest.get("http://localhost/test")
                .header(Constants.SPECIFY_DOMAIN, healthy.getUrl()).build());
        specified.getAttributes().put(Constants.CONTEXT, new ShenyuContext());
        when(chain.execute(specified)).thenReturn(Mono.empty());
        StepVerifier.create(dividePlugin.doExecute(specified, chain, selectorData, ruleData)).verifyComplete();
        assertEquals(healthy.buildDomain(), specified.getAttribute(Constants.HTTP_DOMAIN));
        assertNotNull(UpstreamCircuitBreaker.getInstance().acquireHalfOpenProbe(breakerKey));
    }

    private ServerWebExchange newExchange() {
        ServerWebExchange request = MockServerWebExchange.from(MockServerHttpRequest.get("http://localhost/test").build());
        request.getAttributes().put(Constants.CONTEXT, new ShenyuContext());
        return request;
    }
}
