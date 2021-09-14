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

package org.apache.shenyu.plugin.httpclient;

import io.netty.handler.codec.http.HttpHeaderNames;
import io.netty.handler.codec.http.HttpHeaderValues;
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
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.netty.http.client.HttpClient;
import reactor.test.StepVerifier;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * The test case for NettyHttpClientPlugin.
 */
@RunWith(MockitoJUnitRunner.class)
public final class NettyHttpClientPluginTest {

    private NettyHttpClientPlugin nettyHttpClientPlugin;

    private ShenyuPluginChain chain;

    @Before
    public void setUp() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setApplicationContext(context);
        when(context.getBean(ShenyuResult.class)).thenReturn(mock(ShenyuResult.class));
        chain = mock(ShenyuPluginChain.class);
        when(chain.execute(any())).thenReturn(Mono.empty());
        HttpClient httpClient = HttpClient.create();
        nettyHttpClientPlugin = new NettyHttpClientPlugin(httpClient);
    }

    /**
     * test case for NettyHttpClientPlugin {@link NettyHttpClientPlugin#execute(ServerWebExchange, ShenyuPluginChain)}.
     */
    @Test
    public void testExecute() {
        ServerWebExchange exchangeNoPath = MockServerWebExchange.from(MockServerHttpRequest.get("/test").build());
        exchangeNoPath.getAttributes().put(Constants.CONTEXT, mock(ShenyuContext.class));
        StepVerifier.create(nettyHttpClientPlugin.execute(exchangeNoPath, chain)).expectSubscription().verifyComplete();
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/test")
                .header(HttpHeaderNames.CONNECTION.toString(), HttpHeaderValues.KEEP_ALIVE.toString())
                .body("test"));
        exchange.getAttributes().put(Constants.CONTEXT, mock(ShenyuContext.class));
        exchange.getAttributes().put(Constants.HTTP_URL, "/test");

        StepVerifier.create(nettyHttpClientPlugin.execute(exchange, chain)).expectSubscription().verifyError();
    }

    /**
     * test case for NettyHttpClientPlugin {@link NettyHttpClientPlugin#skip(ServerWebExchange)}.
     */
    @Test
    public void testSkip() {
        ServerWebExchange exchangeNormal = generateServerWebExchange();
        assertTrue(nettyHttpClientPlugin.skip(exchangeNormal));

        ServerWebExchange exchangeHttp = generateServerWebExchange();
        when(((ShenyuContext) exchangeHttp.getAttributes().get(Constants.CONTEXT)).getRpcType())
                .thenReturn(RpcTypeEnum.HTTP.getName());
        assertFalse(nettyHttpClientPlugin.skip(exchangeHttp));

        ServerWebExchange exchangeSpringCloud = generateServerWebExchange();
        when(((ShenyuContext) exchangeSpringCloud.getAttributes().get(Constants.CONTEXT)).getRpcType())
                .thenReturn(RpcTypeEnum.SPRING_CLOUD.getName());
        assertFalse(nettyHttpClientPlugin.skip(exchangeSpringCloud));
    }

    /**
     * test case for NettyHttpClientPlugin {@link NettyHttpClientPlugin#getOrder()}.
     */
    @Test
    public void testGetOrder() {
        assertEquals(PluginEnum.NETTY_HTTP_CLIENT.getCode(), nettyHttpClientPlugin.getOrder());
    }

    /**
     * test case for NettyHttpClientPlugin {@link NettyHttpClientPlugin#named()}.
     */
    @Test
    public void testNamed() {
        assertEquals(PluginEnum.NETTY_HTTP_CLIENT.getName(), nettyHttpClientPlugin.named());
    }

    private ServerWebExchange generateServerWebExchange() {
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/test").build());
        exchange.getAttributes().put(Constants.CONTEXT, mock(ShenyuContext.class));
        exchange.getAttributes().put(Constants.HTTP_URL, "/test");
        return exchange;
    }
}
