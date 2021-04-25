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

package org.dromara.soul.plugin.ratelimiter;

import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.dto.convert.RateLimiterHandle;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.plugin.api.SoulPluginChain;
import org.dromara.soul.plugin.api.result.DefaultSoulResult;
import org.dromara.soul.plugin.api.result.SoulResult;
import org.dromara.soul.plugin.api.utils.SpringBeanUtils;
import org.dromara.soul.plugin.ratelimiter.cache.RatelimiterRuleHandleCache;
import org.dromara.soul.plugin.ratelimiter.executor.RedisRateLimiter;
import org.dromara.soul.plugin.ratelimiter.handler.RateLimiterPluginDataHandler;
import org.dromara.soul.plugin.ratelimiter.response.RateLimiterResponse;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.http.HttpStatus;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * RateLimiterPlugin test.
 *
 * @author wyc192273
 */
@RunWith(MockitoJUnitRunner.class)
public final class RateLimiterPluginTest {

    private SoulPluginChain chain;

    private RedisRateLimiter redisRateLimiter;

    private RuleData ruleData;

    private SelectorData selectorData;

    private RateLimiterPlugin rateLimiterPlugin;

    private ServerWebExchange exchange;

    @Before
    public void setup() {
        this.redisRateLimiter = mock(RedisRateLimiter.class);
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost").build());
        this.chain = mock(SoulPluginChain.class);
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
                Mono.just(new RateLimiterResponse(true, 1)));
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
                Mono.just(new RateLimiterResponse(false, 1)));
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        when(context.getBean(SoulResult.class)).thenReturn(new DefaultSoulResult());
        SpringBeanUtils.getInstance().setCfgContext(context);
        Mono<Void> result = rateLimiterPlugin.doExecute(exchange, chain, selectorData, ruleData);
        StepVerifier.create(result).expectSubscription().verifyComplete();
        Assert.assertEquals(HttpStatus.TOO_MANY_REQUESTS, exchange.getResponse().getStatusCode());
    }

    /**
     * named default value test case.
     */
    @Test
    public void namedTest() {
        Assert.assertEquals(PluginEnum.RATE_LIMITER.getName(), rateLimiterPlugin.named());
    }

    /**
     * getOrder default value test case.
     */
    @Test
    public void getOrderTest() {
        Assert.assertEquals(PluginEnum.RATE_LIMITER.getCode(), rateLimiterPlugin.getOrder());
    }

    /**
     * rateLimiterPlugin doExecute Test prev init.
     */
    private void doExecutePreInit() {
        RateLimiterHandle rateLimiterHandle = mockRateLimiterHandler();
        when(ruleData.getId()).thenReturn("test1");
        when(chain.execute(any())).thenReturn(Mono.empty());
        RatelimiterRuleHandleCache.getInstance().cachedHandle(RateLimiterPluginDataHandler.getCacheKeyName(ruleData), rateLimiterHandle);
    }

    /**
     * rateLimiterHandler mock.
     */
    private RateLimiterHandle mockRateLimiterHandler() {
        RateLimiterHandle rateLimiterHandle = new RateLimiterHandle();
        rateLimiterHandle.setReplenishRate(1);
        rateLimiterHandle.setBurstCapacity(100);
        rateLimiterHandle.setLoged(false);
        return rateLimiterHandle;
    }
}
