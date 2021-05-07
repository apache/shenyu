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

package org.apache.shenyu.plugin.tars.response;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.SoulPluginChain;
import org.apache.shenyu.plugin.api.context.SoulContext;
import org.apache.shenyu.plugin.api.result.DefaultSoulResult;
import org.apache.shenyu.plugin.api.result.SoulResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link TarsResponsePlugin}.
 *
 * @author HoldDie
 */
@RunWith(MockitoJUnitRunner.class)
public class TarsResponsePluginTest {

    @Mock
    private SoulPluginChain chain;

    private ServerWebExchange exchange;

    private TarsResponsePlugin tarsResponsePluginUnderTest;

    @Before
    public void setUp() {
        ConfigurableApplicationContext applicationContext = mock(ConfigurableApplicationContext.class);
        when(applicationContext.getBean(SoulResult.class)).thenReturn(new DefaultSoulResult());
        SpringBeanUtils springBeanUtils = SpringBeanUtils.getInstance();
        springBeanUtils.setCfgContext(applicationContext);
        exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost").build());
        tarsResponsePluginUnderTest = new TarsResponsePlugin();
        Mockito.when(chain.execute(exchange)).thenReturn(Mono.empty());
    }

    @Test
    public void testExecuteWithResult() {
        String response = "{}";
        exchange.getAttributes().put(Constants.TARS_RPC_RESULT, response);
        StepVerifier.create(tarsResponsePluginUnderTest.execute(exchange, chain)).expectSubscription().verifyComplete();
    }

    @Test
    public void testExecuteWithNoResult() {
        StepVerifier.create(tarsResponsePluginUnderTest.execute(exchange, chain)).expectSubscription().verifyComplete();
    }

    @Test
    public void testSkip() {
        SoulContext soulContext = new SoulContext();
        soulContext.setRpcType(RpcTypeEnum.TARS.getName());
        exchange.getAttributes().put(Constants.CONTEXT, soulContext);
        final Boolean result = tarsResponsePluginUnderTest.skip(exchange);
        assertFalse(result);
    }

    @Test
    public void testGetOrder() {
        final int result = tarsResponsePluginUnderTest.getOrder();
        assertEquals(PluginEnum.RESPONSE.getCode(), result);
    }

    @Test
    public void testNamed() {
        final String result = tarsResponsePluginUnderTest.named();
        assertEquals(PluginEnum.RESPONSE.getName(), result);
    }
}
