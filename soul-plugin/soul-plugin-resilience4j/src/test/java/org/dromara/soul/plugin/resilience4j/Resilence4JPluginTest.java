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

package org.dromara.soul.plugin.resilience4j;

import io.github.resilience4j.circuitbreaker.CallNotPermittedException;
import io.github.resilience4j.circuitbreaker.CircuitBreaker;
import io.github.resilience4j.ratelimiter.RateLimiter;
import io.github.resilience4j.ratelimiter.RequestNotPermitted;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.plugin.api.SoulPluginChain;
import org.dromara.soul.plugin.api.context.SoulContext;
import org.dromara.soul.plugin.resilience4j.executor.CombinedExecutor;
import org.dromara.soul.plugin.resilience4j.executor.RateLimiterExecutor;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.http.HttpStatus;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.mockito.Mockito.when;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.RETURNS_DEEP_STUBS;

/**
 * Resilence4J plugin test.
 *
 * @author zhanglei
 */
@RunWith(MockitoJUnitRunner.class)
public final class Resilence4JPluginTest {

    @Mock
    private SoulPluginChain chain;

    private ServerWebExchange exchange;

    private Resilience4JPlugin resilence4JPlugin;

    private RateLimiter rateLimiter;

    private CircuitBreaker circuitBreaker;

    private static final String handle = "{\"limitForPeriod\":\"1\",\"limitRefreshPeriod\":\"2000\",\"timeoutDurationRate\":\"500\",\"circuitEnable\":\"0\",\"failureRateThreshold\":\"50\",\"fallbackUri\":\"\",\"minimumNumberOfCalls\":\"50\",\"permittedNumberOfCallsInHalfOpenState\":\"1\",\"slidingWindowSize\":\"100\",\"slidingWindowType\":\"0\",\"timeoutDuration\":\"20000000\",\"waitIntervalFunctionInOpenState\":\"100000\"}";

    @Before
    public void setup() {
        rateLimiter = mock(RateLimiter.class, RETURNS_DEEP_STUBS);
        circuitBreaker = mock(CircuitBreaker.class, RETURNS_DEEP_STUBS);
        SoulContext context = mock(SoulContext.class);
        exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost").build());
        exchange.getAttributes().put(Constants.CONTEXT, context);
    }

    @Test
    public void noramlTest() {
        resilence4JPlugin = new Resilience4JPlugin(new CombinedExecutor(), new RateLimiterExecutor());
        RuleData data = mock(RuleData.class);
        when(data.getHandle()).thenReturn(handle);
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        SelectorData selectorData = mock(SelectorData.class);
        StepVerifier.create(resilence4JPlugin.doExecute(exchange, chain, selectorData, data)).expectSubscription().verifyComplete();
    }

    @Test
    public void rateLimterTest() {
        RuleData data = mock(RuleData.class);
        CombinedExecutor combinedExecutor = mock(CombinedExecutor.class);
        resilence4JPlugin = new Resilience4JPlugin(combinedExecutor, new RateLimiterExecutor());
        Mono mono = Mono.error(RequestNotPermitted.createRequestNotPermitted(rateLimiter)).onErrorResume(throwable -> {
            return Mono.error(throwable);
        });
        when(data.getHandle()).thenReturn(handle);
        when(chain.execute(exchange)).thenReturn(mono);
        SelectorData selectorData = mock(SelectorData.class);
        StepVerifier.create(resilence4JPlugin.doExecute(exchange, chain, selectorData, data)).expectSubscription().expectError().verify();
    }

    @Test
    public void circuitBreakerTest() {
        RuleData data = mock(RuleData.class);
        SelectorData selectorData = mock(SelectorData.class);
        CombinedExecutor combinedExecutor = new CombinedExecutor();
        resilence4JPlugin = new Resilience4JPlugin(combinedExecutor, new RateLimiterExecutor());
        Mono mono = Mono.error(CallNotPermittedException.createCallNotPermittedException(circuitBreaker)).onErrorResume(throwable -> {
            if (CallNotPermittedException.class.isInstance(throwable)) {
                exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
            }
            return Mono.error(throwable);
        });
        when(data.getHandle()).thenReturn(handle);
        when(chain.execute(exchange)).thenReturn(mono);
        when(data.getSelectorId()).thenReturn("circuitBreaker");
        when(data.getName()).thenReturn("ruleData");
        StepVerifier.create(resilence4JPlugin.doExecute(exchange, chain, selectorData, data))
                .expectSubscription()
                .expectError()
                .verify();
        Assert.assertEquals(exchange.getResponse().getStatusCode(), HttpStatus.INTERNAL_SERVER_ERROR);
    }
}
