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

package org.dromara.soul.plugin.alibaba.dubbo.response;

import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.plugin.api.SoulPluginChain;
import org.dromara.soul.plugin.api.context.SoulContext;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
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
 * Test for DubboResponsePlugin.
 *
 * @author lw1243925457
 */
@RunWith(MockitoJUnitRunner.class)
public final class DubboResponsePluginTest {

    @Mock
    private SoulPluginChain chain;

    private DubboResponsePlugin dubboResponsePlugin;

    private ServerWebExchange exchange;

    @Before
    public void setUp() {
        dubboResponsePlugin = new DubboResponsePlugin();
        exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost").build());
    }

    @Test
    public void testNamed() {
        final String result = dubboResponsePlugin.named();
        assertEquals(PluginEnum.RESPONSE.getName(), result);
    }

    @Test
    public void testSkip() {
        MetaData metaData = new MetaData();
        metaData.setId("1332017966661636096");
        metaData.setAppName("dubbo");
        metaData.setPath("/dubbo/findAll");
        metaData.setServiceName("org.dromara.soul.test.dubbo.api.service.DubboTestService");
        metaData.setMethodName("findAll");
        metaData.setRpcType(RpcTypeEnum.DUBBO.getName());

        SoulContext context = mock(SoulContext.class);
        when(context.getRpcType()).thenReturn(RpcTypeEnum.DUBBO.getName());
        exchange.getAttributes().put(Constants.CONTEXT, context);
        exchange.getAttributes().put(Constants.META_DATA, metaData);

        final Boolean result = dubboResponsePlugin.skip(exchange);
        assertFalse(result);
    }

    @Test
    public void testGetOrder() {
        final int result = dubboResponsePlugin.getOrder();
        assertEquals(PluginEnum.RESPONSE.getCode(), result);
    }

    @Test
    public void testNoResult() {
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        dubboResponsePlugin = new DubboResponsePlugin();
        Mono<Void> voidMono = dubboResponsePlugin.execute(exchange, chain);
        StepVerifier.create(voidMono).expectSubscription().verifyError();
    }

    @Test
    public void testGetResult() {
        final String response = "{}";
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        dubboResponsePlugin = new DubboResponsePlugin();
        exchange.getAttributes().put(Constants.DUBBO_RPC_RESULT, response);
        Mono<Void> voidMono = dubboResponsePlugin.execute(exchange, chain);
        StepVerifier.create(voidMono).expectSubscription().verifyError(NullPointerException.class);
    }
}
