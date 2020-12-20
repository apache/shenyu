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

package org.dromara.soul.plugin.hystrix;

import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.dto.convert.HystrixHandle;
import org.dromara.soul.common.enums.HystrixIsolationModeEnum;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.plugin.api.SoulPluginChain;
import org.dromara.soul.plugin.api.context.SoulContext;
import org.junit.Before;
import org.junit.Test;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * The Test Case For HystrixPlugin.
 *
 * @author nuo-promise
 **/
public final class HystrixPluginTest {

    private HystrixPlugin hystrixPlugin;

    @Before
    public void setUp() {
        hystrixPlugin = new HystrixPlugin();
    }

    @Test(expected = NullPointerException.class)
    public void testDoExecuteNullException() {
        final ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost").build());
        SoulContext soulContext = mock(SoulContext.class);
        exchange.getAttributes().put(Constants.CONTEXT, soulContext);
        SoulPluginChain chain = mock(SoulPluginChain.class);
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        HystrixHandle hystrixHandle = new HystrixHandle();
        hystrixHandle.setMaxConcurrentRequests(0);
        hystrixHandle.setErrorThresholdPercentage(0);
        hystrixHandle.setRequestVolumeThreshold(0);
        hystrixHandle.setSleepWindowInMilliseconds(0);
        RuleData rule = new RuleData();
        rule.setHandle(GsonUtils.getInstance().toJson(hystrixHandle));
        SelectorData selectorData = mock(SelectorData.class);
        Mono<Void> mono = hystrixPlugin.doExecute(exchange, chain, selectorData, rule);
        StepVerifier.create(mono).expectSubscription().verifyComplete();
    }

    @Test
    public void testDoExecute() {
        final ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost").build());
        SoulContext soulContext = mock(SoulContext.class);
        exchange.getAttributes().put(Constants.CONTEXT, soulContext);
        SoulPluginChain chain = mock(SoulPluginChain.class);
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        HystrixHandle hystrixHandle = new HystrixHandle();
        hystrixHandle.setGroupKey("groupKey");
        hystrixHandle.setCommandKey(" commandKey");
        hystrixHandle.setMaxConcurrentRequests(0);
        hystrixHandle.setErrorThresholdPercentage(0);
        hystrixHandle.setRequestVolumeThreshold(0);
        hystrixHandle.setSleepWindowInMilliseconds(0);
        RuleData rule = new RuleData();
        rule.setHandle(GsonUtils.getInstance().toJson(hystrixHandle));
        SelectorData selectorData = mock(SelectorData.class);
        Mono<Void> mono = hystrixPlugin.doExecute(exchange, chain, selectorData, rule);
        StepVerifier.create(mono).expectSubscription().verifyComplete();
        hystrixHandle.setExecutionIsolationStrategy(HystrixIsolationModeEnum.THREAD_POOL.getCode());
        rule.setHandle(GsonUtils.getInstance().toJson(hystrixHandle));
        Mono<Void> threadMono = hystrixPlugin.doExecute(exchange, chain, selectorData, rule);
        StepVerifier.create(threadMono).expectSubscription().verifyComplete();
    }

    @Test
    public void testNamed() {
        assertEquals(hystrixPlugin.named(), PluginEnum.HYSTRIX.getName());
    }

    @Test
    public void testGetOrder() {
        assertEquals(hystrixPlugin.getOrder(), PluginEnum.HYSTRIX.getCode());
    }
}
