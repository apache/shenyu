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

package org.apache.shenyu.plugin.base;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.MediaType;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;

/**
 * TThe param transform plugin test.
 */
@ExtendWith(MockitoExtension.class)
public final class RpcParamTransformPluginTest {
    
    private ShenyuPluginChain chain;
    
    private RpcParamTransformPlugin rpcParamTransformPlugin;
    
    /**
     * Sets up.
     */
    @BeforeEach
    public void setUp() {
        rpcParamTransformPlugin = new RpcParamTransformPlugin();
        chain = mock(ShenyuPluginChain.class);
    }
    
    /**
     * Test get order.
     */
    @Test
    public void testGetOrder() {
        int result = rpcParamTransformPlugin.getOrder();
        assertEquals(PluginEnum.RPC_PARAM_TRANSFORM.getCode(), result);
    }
    
    /**
     * Test named.
     */
    @Test
    public void testNamed() {
        String result = rpcParamTransformPlugin.named();
        assertEquals(PluginEnum.RPC_PARAM_TRANSFORM.getName(), result);
    }
    
    /**
     * Test json body.
     */
    @Test
    public void testJsonBody() {
        ServerWebExchange exchange = MockServerWebExchange.from(
                MockServerHttpRequest.post("localhost").contentType(MediaType.APPLICATION_JSON).body("{}"));
        Mockito.when(chain.execute(exchange)).thenReturn(Mono.empty());
        ShenyuContext context = new ShenyuContext();
        context.setRpcType(RpcTypeEnum.DUBBO.getName());
        exchange.getAttributes().put(Constants.CONTEXT, context);
        Mono<Void> result = rpcParamTransformPlugin.execute(exchange, chain);
        StepVerifier.create(result).expectSubscription().verifyComplete();
    }
    
    /**
     * Test format body.
     */
    @Test
    public void testFormatBody() {
        final ServerWebExchange exchange = MockServerWebExchange.from(
                MockServerHttpRequest.post("localhost").contentType(MediaType.APPLICATION_FORM_URLENCODED).body("test=test"));
        Mockito.when(chain.execute(exchange)).thenReturn(Mono.empty());
        ShenyuContext context = new ShenyuContext();
        context.setRpcType(RpcTypeEnum.DUBBO.getName());
        exchange.getAttributes().put(Constants.CONTEXT, context);
        Mono<Void> result = rpcParamTransformPlugin.execute(exchange, chain);
        StepVerifier.create(result).expectSubscription().verifyComplete();
    }
    
    /**
     * Test no body.
     */
    @Test
    public void testNoBody() {
        ServerWebExchange exchange = MockServerWebExchange.from(
                MockServerHttpRequest.post("localhost"));
        Mockito.when(chain.execute(exchange)).thenReturn(Mono.empty());
        ShenyuContext context = new ShenyuContext();
        context.setRpcType(RpcTypeEnum.DUBBO.getName());
        exchange.getAttributes().put(Constants.CONTEXT, context);
        Mono<Void> result = rpcParamTransformPlugin.execute(exchange, chain);
        StepVerifier.create(result).expectSubscription().verifyComplete();
    }
    
    /**
     * Test simple body.
     */
    @Test
    public void testSimpleBody() {
        ServerWebExchange exchange = MockServerWebExchange.from(
                MockServerHttpRequest.post("localhost").body("test"));
        Mockito.when(chain.execute(exchange)).thenReturn(Mono.empty());
        ShenyuContext context = new ShenyuContext();
        context.setRpcType(RpcTypeEnum.DUBBO.getName());
        exchange.getAttributes().put(Constants.CONTEXT, context);
        Mono<Void> result = rpcParamTransformPlugin.execute(exchange, chain);
        StepVerifier.create(result).expectSubscription().verifyComplete();
    }
}
