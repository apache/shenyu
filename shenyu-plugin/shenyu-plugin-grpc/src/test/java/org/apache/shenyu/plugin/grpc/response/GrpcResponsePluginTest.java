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

package org.apache.shenyu.plugin.grpc.response;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.http.HttpStatus;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.reactive.function.client.ClientResponse;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static groovyjarjarpicocli.CommandLine.Help.Ansi.Style.reset;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.reset;

/**
 * The Test Case For {@link GrpcResponsePlugin}.
 */
@RunWith(MockitoJUnitRunner.class)
public class GrpcResponsePluginTest {

    private ShenyuPluginChain chain;

    private GrpcResponsePlugin grpcResponsePlugin;

    @Before
    public void setUp() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setCfgContext(context);
        when(context.getBean(ShenyuResult.class)).thenReturn(mock(ShenyuResult.class));
        chain = mock(ShenyuPluginChain.class);
        grpcResponsePlugin = new GrpcResponsePlugin();
    }

    @Test
    public void testExecuted() {
        ServerWebExchange exchangeNormal = generateServerWebExchange(true);
        reset(chain);
        when(chain.execute(exchangeNormal)).thenReturn(Mono.empty());
        Mono<Void> monoSuccess = grpcResponsePlugin.execute(exchangeNormal, chain);
        StepVerifier.create(monoSuccess).expectSubscription().verifyComplete();

        ServerWebExchange exchangeNullResponse = generateServerWebExchange(false);
        reset(chain);
        when(chain.execute(exchangeNullResponse)).thenReturn(Mono.empty());
        Mono<Void> monoNullResponse = grpcResponsePlugin.execute(exchangeNullResponse, chain);
        StepVerifier.create(monoNullResponse).expectSubscription().verifyComplete();

        ServerWebExchange exchangeInternalServerError = generateServerWebExchange(true);
        exchangeInternalServerError.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
        reset(chain);
        when(chain.execute(exchangeInternalServerError)).thenReturn(Mono.empty());
        Mono<Void> monoInternalServerError = grpcResponsePlugin.execute(exchangeInternalServerError, chain);
        StepVerifier.create(monoInternalServerError).expectSubscription().verifyComplete();

        ServerWebExchange exchangeBadGateway = generateServerWebExchange(true);
        exchangeBadGateway.getResponse().setStatusCode(HttpStatus.BAD_GATEWAY);
        reset(chain);
        when(chain.execute(exchangeBadGateway)).thenReturn(Mono.empty());
        Mono<Void> monoBadGateway = grpcResponsePlugin.execute(exchangeBadGateway, chain);
        StepVerifier.create(monoBadGateway).expectSubscription().verifyComplete();

        ServerWebExchange exchangeGatewayTimeout = generateServerWebExchange(true);
        exchangeGatewayTimeout.getResponse().setStatusCode(HttpStatus.GATEWAY_TIMEOUT);
        reset(chain);
        when(chain.execute(exchangeGatewayTimeout)).thenReturn(Mono.empty());
        Mono<Void> monoGatewayTimeout = grpcResponsePlugin.execute(exchangeGatewayTimeout, chain);
        StepVerifier.create(monoGatewayTimeout).expectSubscription().verifyComplete();
    }

    @Test
    public void testSkip() {
        assertFalse(grpcResponsePlugin.skip(generateServerWebExchange(true)));
    }

    @Test
    public void testGetOrder() {
        assertEquals(PluginEnum.RESPONSE.getCode(), grpcResponsePlugin.getOrder());
    }

    @Test
    public void testNamed() {
        assertEquals(PluginEnum.RESPONSE.getName(), grpcResponsePlugin.named());
    }

    private ServerWebExchange generateServerWebExchange(final boolean haveResponse) {
        ClientResponse mockResponse = mock(ClientResponse.class);
        ServerWebExchange exchange = MockServerWebExchange
                .from(MockServerHttpRequest.get("/test").build());
        ShenyuContext shenyuContext = mock(ShenyuContext.class);
        when(shenyuContext.getRpcType()).thenReturn(RpcTypeEnum.GRPC.getName());
        exchange.getAttributes().put(Constants.CONTEXT, shenyuContext);
        exchange.getAttributes().put(Constants.HTTP_URL, "/test");
        if (haveResponse) {
            exchange.getAttributes().put(Constants.GRPC_RPC_RESULT, mockResponse);
        }
        return exchange;
    }
}
