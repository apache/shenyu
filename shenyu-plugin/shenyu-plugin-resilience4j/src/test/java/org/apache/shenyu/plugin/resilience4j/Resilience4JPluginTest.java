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

package org.apache.shenyu.plugin.resilience4j;

import io.github.resilience4j.circuitbreaker.CallNotPermittedException;
import io.github.resilience4j.circuitbreaker.CircuitBreaker;
import io.github.resilience4j.ratelimiter.RateLimiter;
import io.github.resilience4j.ratelimiter.RequestNotPermitted;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.Resilience4JHandle;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.resilience4j.executor.CombinedExecutor;
import org.apache.shenyu.plugin.resilience4j.executor.RateLimiterExecutor;
import org.apache.shenyu.plugin.resilience4j.handler.Resilience4JHandler;
import org.junit.Assert;
import org.junit.Before;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.MethodSorters;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.http.HttpStatus;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.mockito.Mockito.RETURNS_DEEP_STUBS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Resilience4J plugin test.
 */
@RunWith(MockitoJUnitRunner.class)
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public final class Resilience4JPluginTest {

    private static final String HANDLER = "{\"limitForPeriod\":\"1\",\"limitRefreshPeriod\":\"2000\",\"timeoutDurationRate\":\"500\",\"circuitEnable\":\"0\","
            + "\"failureRateThreshold\":\"50\",\"fallbackUri\":\"\",\"minimumNumberOfCalls\":\"50\","
            + "\"permittedNumberOfCallsInHalfOpenState\":\"1\",\"slidingWindowSize\":\"100\",\"slidingWindowType\":\"0\","
            + "\"timeoutDuration\":\"20000000\",\"waitIntervalFunctionInOpenState\":\"100000\"}";

    @Mock
    private ShenyuPluginChain chain;

    private ServerWebExchange exchange;

    private Resilience4JPlugin resilience4JPlugin;

    private RateLimiter rateLimiter;

    private CircuitBreaker circuitBreaker;

    @Before
    public void setup() {
        rateLimiter = mock(RateLimiter.class, RETURNS_DEEP_STUBS);
        circuitBreaker = mock(CircuitBreaker.class, RETURNS_DEEP_STUBS);
        ShenyuContext context = mock(ShenyuContext.class);
        exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost").build());
        exchange.getAttributes().put(Constants.CONTEXT, context);
    }

    @Test
    public void normalTest() {
        resilience4JPlugin = new Resilience4JPlugin(new CombinedExecutor(), new RateLimiterExecutor());
        RuleData data = mock(RuleData.class);
        data.setSelectorId("SHENYU");
        data.setId("SHENYU");
        Resilience4JHandle resilience4JHandle = GsonUtils.getGson().fromJson(HANDLER, Resilience4JHandle.class);
        Resilience4JHandler.CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(data), resilience4JHandle);
        when(data.getHandle()).thenReturn(HANDLER);
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        SelectorData selectorData = mock(SelectorData.class);
        StepVerifier.create(resilience4JPlugin.doExecute(exchange, chain, selectorData, data)).expectSubscription().verifyComplete();
    }

    @Test
    public void rateLimiterTest() {
        RuleData data = mock(RuleData.class);
        data.setSelectorId("SHENYU");
        data.setId("SHENYU");
        Resilience4JHandle resilience4JHandle = GsonUtils.getGson().fromJson(HANDLER, Resilience4JHandle.class);
        Resilience4JHandler.CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(data), resilience4JHandle);
        CombinedExecutor combinedExecutor = mock(CombinedExecutor.class);
        resilience4JPlugin = new Resilience4JPlugin(combinedExecutor, new RateLimiterExecutor());
        Mono mono = Mono.error(RequestNotPermitted.createRequestNotPermitted(rateLimiter)).onErrorResume(Mono::error);
        when(data.getHandle()).thenReturn(HANDLER);
        when(chain.execute(exchange)).thenReturn(mono);
        SelectorData selectorData = mock(SelectorData.class);
        StepVerifier.create(resilience4JPlugin.doExecute(exchange, chain, selectorData, data)).expectSubscription().expectError().verify();
    }

    @Test
    public void circuitBreakerTest() {
        RuleData data = mock(RuleData.class);
        data.setSelectorId("SHENYU");
        data.setId("SHENYU");
        Resilience4JHandle resilience4JHandle = GsonUtils.getGson().fromJson(HANDLER, Resilience4JHandle.class);
        Resilience4JHandler.CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(data), resilience4JHandle);
        CombinedExecutor combinedExecutor = new CombinedExecutor();
        resilience4JPlugin = new Resilience4JPlugin(combinedExecutor, new RateLimiterExecutor());
        Mono mono = Mono.error(CallNotPermittedException.createCallNotPermittedException(circuitBreaker)).onErrorResume(throwable -> {
            if (CallNotPermittedException.class.isInstance(throwable)) {
                exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
            }
            return Mono.error(throwable);
        });
        when(data.getHandle()).thenReturn(HANDLER);
        when(chain.execute(exchange)).thenReturn(mono);
        when(data.getSelectorId()).thenReturn("circuitBreaker");
        when(data.getName()).thenReturn("ruleData");
        Resilience4JHandler.CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(data), resilience4JHandle);
        SelectorData selectorData = mock(SelectorData.class);
        StepVerifier.create(resilience4JPlugin.doExecute(exchange, chain, selectorData, data))
                .expectSubscription()
                .expectError()
                .verify();
        Assert.assertEquals(exchange.getResponse().getStatusCode(), HttpStatus.INTERNAL_SERVER_ERROR);
    }
}
