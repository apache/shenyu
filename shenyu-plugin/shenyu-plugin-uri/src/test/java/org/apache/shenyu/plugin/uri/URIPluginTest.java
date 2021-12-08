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

package org.apache.shenyu.plugin.uri;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.net.InetSocketAddress;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

/**
 * The Test Case For {@link URIPlugin}.
 */
@RunWith(MockitoJUnitRunner.class)
public class URIPluginTest {

    private MockServerHttpRequest request;
    
    private URIPlugin uriPlugin;
    
    private ServerWebExchange exchange;
    
    private ShenyuPluginChain chain;
    
    private ShenyuContext shenyuContext;

    @Before
    public void setUp() {
        this.uriPlugin = new URIPlugin();
        this.chain = mock(ShenyuPluginChain.class);
        this.request = MockServerHttpRequest
                .get("localhost")
                .remoteAddress(new InetSocketAddress(8090))
                .header("X-source", "mock test")
                .queryParam("queryParam", "Hello,World")
                .build();
        this.exchange = spy(MockServerWebExchange.from(request));
        shenyuContext = mock(ShenyuContext.class);
        exchange.getAttributes().put(Constants.CONTEXT, shenyuContext);
    }

    @Test
    public void testDoExecute() {
        when(exchange.getAttribute(Constants.HTTP_DOMAIN)).thenReturn("http://localhost:8090");
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        StepVerifier.create(uriPlugin.execute(exchange, chain)).expectSubscription().verifyComplete();
        assertEquals(exchange.getAttributes().get(Constants.HTTP_URI).toString(), "http://localhost:8090?queryParam=Hello,World");
        // test https
        when(exchange.getAttribute(Constants.HTTP_DOMAIN)).thenReturn("https://localhost");
        when(shenyuContext.getRealUrl()).thenReturn("/test");
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        StepVerifier.create(uriPlugin.execute(exchange, chain)).expectSubscription().verifyComplete();
        assertEquals(exchange.getAttributes().get(Constants.HTTP_URI).toString(), "https://localhost/test?queryParam=Hello,World");
        // test realUrl
        when(exchange.getAttribute(Constants.HTTP_DOMAIN)).thenReturn("http://localhost");
        when(shenyuContext.getRealUrl()).thenReturn("/test");
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        StepVerifier.create(uriPlugin.execute(exchange, chain)).expectSubscription().verifyComplete();
        assertEquals(exchange.getAttributes().get(Constants.HTTP_URI).toString(), "http://localhost/test?queryParam=Hello,World");
        // test rewrite
        when(exchange.getAttribute(Constants.HTTP_DOMAIN)).thenReturn("http://localhost:8090");
        exchange.getAttributes().put(Constants.REWRITE_URI, "/rewrite");
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        StepVerifier.create(uriPlugin.execute(exchange, chain)).expectSubscription().verifyComplete();
        assertEquals(exchange.getAttributes().get(Constants.HTTP_URI).toString(), "http://localhost:8090/rewrite?queryParam=Hello,World");
        // test contains % in the row query
        request = MockServerHttpRequest
                .get("localhost")
                .remoteAddress(new InetSocketAddress(8090))
                .queryParam("queryParam", "Hello, World")
                .build();
        this.exchange = spy(MockServerWebExchange.from(request));
        shenyuContext = mock(ShenyuContext.class);
        exchange.getAttributes().put(Constants.CONTEXT, shenyuContext);
        when(exchange.getAttribute(Constants.HTTP_DOMAIN)).thenReturn("http://localhost:8090/query");
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        StepVerifier.create(uriPlugin.execute(exchange, chain)).expectSubscription().verifyComplete();
        assertEquals(exchange.getAttributes().get(Constants.HTTP_URI).toString(), "http://localhost:8090/query?queryParam=Hello,%20World");
    }

    @Test
    public void testGetOrder() {
        assertEquals(uriPlugin.getOrder(), PluginEnum.URI.getCode());
    }

    @Test
    public void tesNamed() {
        assertEquals(uriPlugin.named(), PluginEnum.URI.getName());
    }

    @Test
    public void testSkip() {
        when(shenyuContext.getRpcType()).thenReturn(RpcTypeEnum.HTTP.getName());
        Assert.assertFalse(uriPlugin.skip(exchange));
        when(shenyuContext.getRpcType()).thenReturn(RpcTypeEnum.SPRING_CLOUD.getName());
        Assert.assertFalse(uriPlugin.skip(exchange));
        when(shenyuContext.getRpcType()).thenReturn(RpcTypeEnum.DUBBO.getName());
        Assert.assertTrue(uriPlugin.skip(exchange));
    }
}
