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

package org.dromara.soul.plugin.alibaba.dubbo.param;

import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.plugin.api.SoulPluginChain;
import org.dromara.soul.plugin.api.context.SoulContext;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.http.MediaType;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;

/**
 * Test case for {@link BodyParamPlugin}.
 *
 * @author Phoenix Luo
 **/

@RunWith(MockitoJUnitRunner.class)
public final class BodyParamPluginTest {
    private SoulPluginChain chain;
    
    private BodyParamPlugin bodyParamPlugin;
    
    @Before
    public void setUp() {
        bodyParamPlugin = new BodyParamPlugin();
        chain = mock(SoulPluginChain.class);
    }
    
    @Test
    public void testGetOrder() {
        final int result = bodyParamPlugin.getOrder();
        assertEquals(PluginEnum.DUBBO.getCode() - 1, result);
    }
    
    @Test
    public void testNamed() {
        final String result = bodyParamPlugin.named();
        assertEquals("alibaba-dubbo-body-param", result);
    }
    
    @Test
    public void testJsonBody() {
        final ServerWebExchange exchange = MockServerWebExchange.from(
                MockServerHttpRequest.post("localhost").contentType(MediaType.APPLICATION_JSON).body("{}"));
        Mockito.when(chain.execute(exchange)).thenReturn(Mono.empty());
        SoulContext context = new SoulContext();
        context.setRpcType(RpcTypeEnum.DUBBO.getName());
        exchange.getAttributes().put(Constants.CONTEXT, context);
        final Mono<Void> result = bodyParamPlugin.execute(exchange, chain);
        StepVerifier.create(result).expectSubscription().verifyComplete();
    }
    
    @Test
    public void testFormatBody() {
        final ServerWebExchange exchange = MockServerWebExchange.from(
                MockServerHttpRequest.post("localhost").contentType(MediaType.APPLICATION_FORM_URLENCODED).body("test=test"));
        Mockito.when(chain.execute(exchange)).thenReturn(Mono.empty());
        SoulContext context = new SoulContext();
        context.setRpcType(RpcTypeEnum.DUBBO.getName());
        exchange.getAttributes().put(Constants.CONTEXT, context);
        final Mono<Void> result = bodyParamPlugin.execute(exchange, chain);
        StepVerifier.create(result).expectSubscription().verifyComplete();
    }
    
    @Test
    public void testNoBody() {
        final ServerWebExchange exchange = MockServerWebExchange.from(
                MockServerHttpRequest.post("localhost"));
        Mockito.when(chain.execute(exchange)).thenReturn(Mono.empty());
        SoulContext context = new SoulContext();
        context.setRpcType(RpcTypeEnum.DUBBO.getName());
        exchange.getAttributes().put(Constants.CONTEXT, context);
        final Mono<Void> result = bodyParamPlugin.execute(exchange, chain);
        StepVerifier.create(result).expectSubscription().verifyComplete();
    }
    
    @Test
    public void testSimpleBody() {
        final ServerWebExchange exchange = MockServerWebExchange.from(
                MockServerHttpRequest.post("localhost").body("test"));
        Mockito.when(chain.execute(exchange)).thenReturn(Mono.empty());
        SoulContext context = new SoulContext();
        context.setRpcType(RpcTypeEnum.DUBBO.getName());
        exchange.getAttributes().put(Constants.CONTEXT, context);
        final Mono<Void> result = bodyParamPlugin.execute(exchange, chain);
        StepVerifier.create(result).expectSubscription().verifyComplete();
    }
    
}
