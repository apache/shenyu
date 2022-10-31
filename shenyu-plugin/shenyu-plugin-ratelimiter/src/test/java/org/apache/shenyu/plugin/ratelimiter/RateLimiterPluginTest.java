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

package org.apache.shenyu.plugin.ratelimiter;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.RateLimiterHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.ratelimiter.executor.RedisRateLimiter;
import org.apache.shenyu.plugin.ratelimiter.handler.RateLimiterPluginDataHandler;
import org.apache.shenyu.plugin.ratelimiter.response.RateLimiterResponse;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.http.HttpStatus;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * RateLimiterPlugin test.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class RateLimiterPluginTest {

    private ShenyuPluginChain chain;

    private RedisRateLimiter redisRateLimiter;

    private RuleData ruleData;

    private SelectorData selectorData;

    private RateLimiterPlugin rateLimiterPlugin;

    private ServerWebExchange exchange;

    @BeforeEach
    public void setup() {
        this.redisRateLimiter = mock(RedisRateLimiter.class);
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost").build());
        this.chain = mock(ShenyuPluginChain.class);
        this.ruleData = mock(RuleData.class);
        this.selectorData = mock(SelectorData.class);
        this.rateLimiterPlugin = new RateLimiterPlugin(redisRateLimiter);
    }

    /**
     * rateLimiterPlugin doExecute , limiter allowed case.
     */
    @Test
    public void doExecuteAllowedTest() {
        doExecutePreInit();
        when(redisRateLimiter.isAllowed(anyString(), any(RateLimiterHandle.class))).thenReturn(
                Mono.just(new RateLimiterResponse(true, 1, null)));
        Mono<Void> result = rateLimiterPlugin.doExecute(exchange, chain, selectorData, ruleData);
        StepVerifier.create(result).expectSubscription().verifyComplete();
    }

    /**
     * rateLimiterPlugin doExecute , limiter not allowed case.
     */
    @Test
    public void doExecuteNotAllowedTest() {
        doExecutePreInit();
        when(redisRateLimiter.isAllowed(anyString(), any(RateLimiterHandle.class))).thenReturn(
                Mono.just(new RateLimiterResponse(false, 1, null)));
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        when(context.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());
        SpringBeanUtils.getInstance().setApplicationContext(context);
        Mono<Void> result = rateLimiterPlugin.doExecute(exchange, chain, selectorData, ruleData);
        StepVerifier.create(result).expectSubscription().verifyComplete();
        assertEquals(HttpStatus.TOO_MANY_REQUESTS, exchange.getResponse().getStatusCode());
    }

    /**
     * named default value test case.
     */
    @Test
    public void namedTest() {
        assertEquals(PluginEnum.RATE_LIMITER.getName(), rateLimiterPlugin.named());
    }

    /**
     * getOrder default value test case.
     */
    @Test
    public void getOrderTest() {
        assertEquals(PluginEnum.RATE_LIMITER.getCode(), rateLimiterPlugin.getOrder());
    }

    /**
     * rateLimiterPlugin doExecute Test prev init.
     */
    private void doExecutePreInit() {
        RateLimiterHandle rateLimiterHandle = mockRateLimiterHandler();
        when(chain.execute(any())).thenReturn(Mono.empty());
        RateLimiterPluginDataHandler.CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), rateLimiterHandle);
    }

    /**
     * rateLimiterHandler mock.
     */
    private RateLimiterHandle mockRateLimiterHandler() {
        RateLimiterHandle rateLimiterHandle = new RateLimiterHandle();
        rateLimiterHandle.setReplenishRate(1);
        rateLimiterHandle.setBurstCapacity(100);
        rateLimiterHandle.setKeyResolverName("WHOLE_KEY_RESOLVER");
        rateLimiterHandle.setLoged(false);
        return rateLimiterHandle;
    }
}
