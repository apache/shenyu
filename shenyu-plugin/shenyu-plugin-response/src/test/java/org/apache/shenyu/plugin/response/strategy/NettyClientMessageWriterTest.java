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

package org.apache.shenyu.plugin.response.strategy;

import io.netty.buffer.ByteBufAllocator;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
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

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * The test case for {@link NettyClientMessageWriter}.
 */
public class NettyClientMessageWriterTest {

    private NettyClientMessageWriter nettyClientMessageWriter;

    private ShenyuPluginChain chain;

    @Before
    public void setUp() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setApplicationContext(context);
        when(context.getBean(ShenyuResult.class)).thenReturn(mock(ShenyuResult.class));
        chain = mock(ShenyuPluginChain.class);
        when(chain.execute(any())).thenReturn(Mono.empty());
        nettyClientMessageWriter = new NettyClientMessageWriter();
    }

    @Test
    public void testWriteWith() {
        ServerWebExchange exchangeNoClient = MockServerWebExchange.from(MockServerHttpRequest.get("/test")
                .build());
        StepVerifier.create(nettyClientMessageWriter.writeWith(exchangeNoClient, chain)).expectSubscription().verifyComplete();

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

        StepVerifier.create(nettyClientMessageWriter.writeWith(exchange, chain)).expectSubscription().verifyError();
    }
}
