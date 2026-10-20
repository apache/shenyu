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
import reactor.test.StepVerifier;

import java.lang.reflect.Field;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ConcurrentMap;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
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
        when(cacheManager.findUpstreamListBySelectorId(SELECTOR_ID)).thenReturn(Collections.singletonList(upstream));
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
        elapseWaitWindow(breakerKey);
        // the probe request is forwarded and its success closes the breaker
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        StepVerifier.create(dividePlugin.doExecute(exchange, chain, selectorData, ruleData))
                .verifyComplete();
        verify(chain, times(1)).execute(any());
        assertFalse(UpstreamCircuitBreaker.getInstance().isBlocking(breakerKey));
    }

    @Test
    public void testHalfOpenProbeIsSelectedWhileAnotherUpstreamIsHealthy() throws Exception {
        openBreaker();
        elapseWaitWindow(breakerKey);
        Upstream healthy = Upstream.builder().protocol("http://").url("127.0.0.1:9091").build();
        UpstreamCacheManager cacheManager = mock(UpstreamCacheManager.class);
        when(cacheManager.findUpstreamListBySelectorId(SELECTOR_ID)).thenReturn(List.of(upstream, healthy));
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
        when(cacheManager.findUpstreamListBySelectorId(SELECTOR_ID)).thenReturn(List.of(upstream, healthy));
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
        when(cacheManager.findUpstreamListBySelectorId(SELECTOR_ID)).thenReturn(upstreams);
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

    private void elapseWaitWindow(final String key) throws Exception {
        Field breakersField = UpstreamCircuitBreaker.class.getDeclaredField("breakers");
        breakersField.setAccessible(true);
        ConcurrentMap<String, ?> breakers = (ConcurrentMap<String, ?>) breakersField.get(UpstreamCircuitBreaker.getInstance());
        Object state = breakers.get(key);
        Field openedAtField = state.getClass().getDeclaredField("openedAtMillis");
        openedAtField.setAccessible(true);
        openedAtField.setLong(state, System.currentTimeMillis() - 60000L);
    }
}
