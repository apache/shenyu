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

package org.dromara.soul.plugin.sentinel;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.mock;

import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.dto.convert.SentinelHandle;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.plugin.api.SoulPluginChain;
import org.dromara.soul.plugin.api.context.SoulContext;
import org.dromara.soul.plugin.api.result.DefaultSoulResult;
import org.dromara.soul.plugin.api.result.SoulResult;
import org.dromara.soul.plugin.base.utils.SpringBeanUtils;
import org.dromara.soul.plugin.sentinel.fallback.SentinelFallbackHandler;
import org.dromara.soul.plugin.sentinel.handler.SentinelRuleHandle;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.http.HttpStatus;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.client.HttpStatusCodeException;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.net.InetSocketAddress;

@RunWith(MockitoJUnitRunner.class)
public final class SentinelPluginTest {

    private SentinelPlugin sentinelPlugin;

    private ServerWebExchange exchange;

    private SelectorData selectorData;

    @Mock
    private SentinelRuleHandle sentinelRuleHandle;

    @Mock
    private SentinelFallbackHandler sentinelFallbackHandler;

    @Mock
    private SoulPluginChain chain;

    @Before
    public void setUp() {
        this.chain = mock(SoulPluginChain.class);
        this.selectorData = mock(SelectorData.class);
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost")
                .remoteAddress(new InetSocketAddress(8090))
                .build());
        sentinelFallbackHandler = new SentinelFallbackHandler();
        sentinelRuleHandle = new SentinelRuleHandle();
        SoulContext context = mock(SoulContext.class);
        exchange.getAttributes().put(Constants.CONTEXT, context);
        when(chain.execute(exchange)).thenReturn(Mono.empty());

        ConfigurableApplicationContext applicationContext = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setCfgContext(applicationContext);
        when(applicationContext.getBean(SoulResult.class)).thenReturn(new DefaultSoulResult());

        sentinelPlugin = new SentinelPlugin(sentinelFallbackHandler);
    }

    /**
     * Test Sentinel Flow.
     */
    @Test
    public void testSentinelPluginFlowException() {
        RuleData data = new RuleData();
        data.setSelectorId("sentinel");
        data.setName("testSentinelPluginFlowException");
        SentinelHandle sentinelHandle = new SentinelHandle();
        sentinelHandle.setFlowRuleEnable(1);
        sentinelHandle.setFlowRuleCount(0);
        sentinelHandle.setFlowRuleGrade(1);
        sentinelHandle.setFlowRuleControlBehavior(0);
        sentinelHandle.setDegradeRuleEnable(0);
        sentinelHandle.setDegradeRuleCount(1);
        sentinelHandle.setDegradeRuleGrade(1);
        sentinelHandle.setDegradeRuleTimeWindow(10);
        data.setHandle(GsonUtils.getGson().toJson(sentinelHandle));
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        sentinelRuleHandle.handlerRule(data);
        StepVerifier.create(sentinelPlugin.doExecute(exchange, chain, selectorData, data))
                .expectSubscription().verifyComplete();

        // remove rule
        sentinelRuleHandle.removeRule(data);
    }


    /**
     * Test Sentinel Degrade.
     */
    @Test
    public void testSentinelPluginDegradeException() {
        RuleData data = new RuleData();
        data.setSelectorId("sentinel");
        data.setName("testSentinelPluginDegradeException");
        SentinelHandle sentinelHandle = new SentinelHandle();
        sentinelHandle.setFlowRuleEnable(0);
        sentinelHandle.setFlowRuleCount(10);
        sentinelHandle.setFlowRuleGrade(1);
        sentinelHandle.setFlowRuleControlBehavior(0);
        sentinelHandle.setDegradeRuleEnable(1);
        sentinelHandle.setDegradeRuleCount(1);
        sentinelHandle.setDegradeRuleGrade(2);
        sentinelHandle.setDegradeRuleTimeWindow(10);
        data.setHandle(GsonUtils.getGson().toJson(sentinelHandle));
        sentinelRuleHandle.handlerRule(data);
        Mono mono = Mono.error(RuntimeException::new);
        when(chain.execute(exchange)).thenReturn(mono);
        for (int i = 0; i < 5; i++) {
            StepVerifier.create(sentinelPlugin.doExecute(exchange, chain, selectorData, data))
                    .expectError(RuntimeException.class).verify();
        }
        StepVerifier.create(sentinelPlugin.doExecute(exchange, chain, selectorData, data))
                .expectSubscription().verifyComplete();

        // remove rule
        sentinelRuleHandle.removeRule(data);
    }

    /**
     * Test chain.execute doOnSuccess return HttpStatus.OK.
     */
    @Test
    public void testSentinelPluginHttpStatusOK() {
        RuleData data = new RuleData();
        data.setSelectorId("sentinel");
        data.setName("testSentinelPluginNullPointException");
        SentinelHandle sentinelHandle = new SentinelHandle();
        sentinelHandle.setFlowRuleEnable(1);
        sentinelHandle.setFlowRuleCount(10);
        sentinelHandle.setFlowRuleGrade(0);
        sentinelHandle.setFlowRuleControlBehavior(0);
        sentinelHandle.setDegradeRuleCount(2);
        sentinelHandle.setDegradeRuleGrade(2);
        sentinelHandle.setDegradeRuleTimeWindow(5);
        data.setHandle(GsonUtils.getGson().toJson(sentinelHandle));
        sentinelRuleHandle.handlerRule(data);

        Mono mono = Mono.empty().doOnSuccess(v -> {
            exchange.getResponse().setStatusCode(HttpStatus.OK);
        });
        when(chain.execute(exchange)).thenReturn(mono);
        StepVerifier.create(sentinelPlugin.doExecute(exchange, chain, selectorData, data))
                .expectSubscription().verifyComplete();

        // remove rule
        sentinelRuleHandle.removeRule(data);
    }

    /**
     * Test chain.execute doOnSuccess return other status.
     */
    @Test
    public void testSentinelPluginNotHttpStatusOK() {
        RuleData data = new RuleData();
        data.setSelectorId("sentinel");
        data.setName("testSentinelPluginNullPointException");
        SentinelHandle sentinelHandle = new SentinelHandle();
        sentinelHandle.setFlowRuleEnable(1);
        sentinelHandle.setFlowRuleCount(10);
        sentinelHandle.setFlowRuleGrade(0);
        sentinelHandle.setFlowRuleControlBehavior(0);
        sentinelHandle.setDegradeRuleCount(2);
        sentinelHandle.setDegradeRuleGrade(2);
        sentinelHandle.setDegradeRuleTimeWindow(5);
        data.setHandle(GsonUtils.getGson().toJson(sentinelHandle));
        sentinelRuleHandle.handlerRule(data);

        Mono mono = Mono.empty().doOnSuccess(v -> {
            exchange.getResponse().setStatusCode(HttpStatus.TOO_MANY_REQUESTS);
        });
        when(chain.execute(exchange)).thenReturn(mono);
        StepVerifier.create(sentinelPlugin.doExecute(exchange, chain, selectorData, data))
                .expectError(HttpStatusCodeException.class).verify();

        // remove rule
        sentinelRuleHandle.removeRule(data);
    }

    @Test
    public void testNamed() {
        final String result = sentinelPlugin.named();
        assertEquals(PluginEnum.SENTINEL.getName(), result);
    }

    @Test
    public void testGetOrder() {
        final int result = sentinelPlugin.getOrder();
        assertEquals(PluginEnum.SENTINEL.getCode(), result);
    }
}
