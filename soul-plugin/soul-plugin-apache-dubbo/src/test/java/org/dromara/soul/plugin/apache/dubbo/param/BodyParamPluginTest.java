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

package org.dromara.soul.plugin.apache.dubbo.param;

import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.plugin.api.SoulPluginChain;
import org.dromara.soul.plugin.api.context.SoulContext;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
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
 * @author HoldDie
 */
public final class BodyParamPluginTest {

    private SoulPluginChain chain;

    private BodyParamPlugin bodyParamPluginUnderTest;

    @Before
    public void setUp() {
        bodyParamPluginUnderTest = new BodyParamPlugin();
        chain = mock(SoulPluginChain.class);
    }

    @Test
    public void testGetOrder() {
        final int result = bodyParamPluginUnderTest.getOrder();

        assertEquals(PluginEnum.DUBBO.getCode() - 1, result);
    }

    @Test
    public void testNamed() {
        final String result = bodyParamPluginUnderTest.named();

        assertEquals("apache-dubbo-body-param", result);
    }

    @Test
    public void testExecuteWithNoBody() {
        ServerWebExchange exchange = MockServerWebExchange.from(
                MockServerHttpRequest.post("localhost").contentType(MediaType.APPLICATION_JSON).body("{}"));
        Mockito.when(chain.execute(exchange)).thenReturn(Mono.empty());

        final Mono<Void> result = bodyParamPluginUnderTest.execute(exchange, chain);

        StepVerifier.create(result).expectSubscription().verifyComplete();
    }

    @Test
    public void testExecuteWithSimpleBody() {
        final ServerWebExchange simpleExchange = MockServerWebExchange.from(
                MockServerHttpRequest.get("localhost").build());
        Mockito.when(chain.execute(simpleExchange)).thenReturn(Mono.empty());
        SoulContext context = new SoulContext();
        context.setRpcType(RpcTypeEnum.DUBBO.getName());
        simpleExchange.getAttributes().put(Constants.CONTEXT, context);

        final Mono<Void> result = bodyParamPluginUnderTest.execute(simpleExchange, chain);

        StepVerifier.create(result).expectSubscription().verifyComplete();
    }

    @Test
    public void testExecuteWithJsonBody() {
        final ServerWebExchange exchange = MockServerWebExchange.from(
                MockServerHttpRequest.post("localhost").contentType(MediaType.APPLICATION_JSON).body("{}"));
        Mockito.when(chain.execute(exchange)).thenReturn(Mono.empty());
        SoulContext context = new SoulContext();
        context.setRpcType(RpcTypeEnum.DUBBO.getName());
        exchange.getAttributes().put(Constants.CONTEXT, context);

        final Mono<Void> result = bodyParamPluginUnderTest.execute(exchange, chain);

        StepVerifier.create(result).expectSubscription().verifyComplete();
    }

    @Test
    public void testExecuteWithFormBody() {
        final ServerWebExchange formExchange = MockServerWebExchange.from(
                MockServerHttpRequest.post("localhost")
                        .contentType(MediaType.APPLICATION_FORM_URLENCODED).body("{}"));
        Mockito.when(chain.execute(formExchange)).thenReturn(Mono.empty());
        SoulContext context = new SoulContext();
        context.setRpcType(RpcTypeEnum.DUBBO.getName());
        formExchange.getAttributes().put(Constants.CONTEXT, context);

        final Mono<Void> result = bodyParamPluginUnderTest.execute(formExchange, chain);

        StepVerifier.create(result).expectSubscription().verifyComplete();
    }
}
