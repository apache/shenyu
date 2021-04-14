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

package org.dromara.soul.plugin.httpclient.response;

import io.netty.buffer.ByteBufAllocator;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.plugin.api.SoulPluginChain;
import org.dromara.soul.plugin.api.context.SoulContext;
import org.dromara.soul.plugin.api.result.SoulResult;
import org.dromara.soul.plugin.api.utils.SpringBeanUtils;
import org.junit.Before;
import org.junit.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.core.io.buffer.NettyDataBuffer;
import org.springframework.core.io.buffer.NettyDataBufferFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.CoreSubscriber;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.netty.ByteBufFlux;
import reactor.netty.Connection;
import reactor.netty.NettyInbound;
import reactor.test.StepVerifier;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * The test case for NettyClientResponsePluginTest.
 *
 * @author YuI
 **/
public class NettyClientResponsePluginTest {

    private NettyClientResponsePlugin nettyClientResponsePlugin;

    private SoulPluginChain chain;

    @Before
    public void setUp() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setCfgContext(context);
        when(context.getBean(SoulResult.class)).thenReturn(mock(SoulResult.class));

        chain = mock(SoulPluginChain.class);
        when(chain.execute(any())).thenReturn(Mono.empty());

        nettyClientResponsePlugin = new NettyClientResponsePlugin();
    }

    /**
     * test case for NettyClientResponsePluginTest
     * {@link NettyClientResponsePlugin#execute(ServerWebExchange, SoulPluginChain)}.
     */
    @Test
    public void testExecute() {
        ServerWebExchange exchangeNoClient = MockServerWebExchange.from(MockServerHttpRequest.get("/test")
                .build());
        StepVerifier.create(nettyClientResponsePlugin.execute(exchangeNoClient, chain)).expectSubscription().verifyComplete();

        ServerWebExchange exchange = mock(ServerWebExchange.class);
        when(exchange.getRequest()).thenReturn(mock(ServerHttpRequest.class));
        when(exchange.getResponse()).thenReturn(mock(ServerHttpResponse.class));
        when(exchange.getResponse().getHeaders()).thenReturn(HttpHeaders.EMPTY);
        NettyDataBufferFactory factory = new NettyDataBufferFactory(mock(ByteBufAllocator.class));
        when(exchange.getResponse().bufferFactory()).thenReturn(factory);

        Connection connection = mock(Connection.class);
        when(connection.inbound()).thenReturn(mock(NettyInbound.class));
        when(connection.inbound().receive()).thenReturn(mock(ByteBufFlux.class));
        when(connection.inbound().receive().retain()).thenReturn(mock(ByteBufFlux.class));

        final Flux<NettyDataBuffer> body = new Flux<NettyDataBuffer>() {
            @Override
            public void subscribe(final CoreSubscriber<? super NettyDataBuffer> coreSubscriber) {
                coreSubscriber.onComplete();
            }
        };
        when(connection.inbound().receive().retain().map(factory::wrap)).thenReturn(body);
        when(exchange.getAttribute(Constants.CLIENT_RESPONSE_CONN_ATTR)).thenReturn(connection);

        StepVerifier.create(nettyClientResponsePlugin.execute(exchange, chain)).expectSubscription().verifyError();
    }

    /**
     * test case for NettyClientResponsePluginTest {@link NettyClientResponsePlugin#skip(ServerWebExchange)}.
     */
    @Test
    public void testSkip() {
        ServerWebExchange exchangeNormal = generateServerWebExchange();
        assertTrue(nettyClientResponsePlugin.skip(exchangeNormal));

        ServerWebExchange exchangeHttp = generateServerWebExchange();
        when(((SoulContext) exchangeHttp.getAttributes().get(Constants.CONTEXT)).getRpcType())
                .thenReturn(RpcTypeEnum.HTTP.getName());
        assertFalse(nettyClientResponsePlugin.skip(exchangeHttp));

        ServerWebExchange exchangeSpringCloud = generateServerWebExchange();
        when(((SoulContext) exchangeSpringCloud.getAttributes().get(Constants.CONTEXT)).getRpcType())
                .thenReturn(RpcTypeEnum.SPRING_CLOUD.getName());
        assertFalse(nettyClientResponsePlugin.skip(exchangeSpringCloud));
    }

    /**
     * test case for NettyClientResponsePluginTest {@link NettyClientResponsePlugin#getOrder()}.
     */
    @Test
    public void testGetOrder() {
        assertEquals(PluginEnum.RESPONSE.getCode(), nettyClientResponsePlugin.getOrder());
    }

    /**
     * test case for NettyClientResponsePluginTest {@link NettyClientResponsePlugin#named()}.
     */
    @Test
    public void testNamed() {
        assertEquals("NettyClientResponse", nettyClientResponsePlugin.named());
    }

    private ServerWebExchange generateServerWebExchange() {
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/test").build());
        exchange.getAttributes().put(Constants.CONTEXT, mock(SoulContext.class));
        exchange.getAttributes().put(Constants.HTTP_URL, "/test");
        return exchange;
    }
}
