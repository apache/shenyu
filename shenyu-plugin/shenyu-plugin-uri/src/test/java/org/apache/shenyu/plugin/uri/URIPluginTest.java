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

import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.net.InetSocketAddress;
import java.util.stream.Collectors;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

/**
 * The Test Case For {@link URIPlugin}.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class URIPluginTest {

    private MockServerHttpRequest request;
    
    private URIPlugin uriPlugin;
    
    private ServerWebExchange exchange;
    
    private ShenyuPluginChain chain;
    
    private ShenyuContext shenyuContext;

    @BeforeEach
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
        assertEquals("http://localhost:8090?queryParam=Hello,World", exchange.getAttributes().get(Constants.HTTP_URI).toString());
        // test https
        when(exchange.getAttribute(Constants.HTTP_DOMAIN)).thenReturn("https://localhost");
        when(shenyuContext.getRealUrl()).thenReturn("/test");
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        StepVerifier.create(uriPlugin.execute(exchange, chain)).expectSubscription().verifyComplete();
        assertEquals("https://localhost/test?queryParam=Hello,World", exchange.getAttributes().get(Constants.HTTP_URI).toString());
        // test realUrl
        when(exchange.getAttribute(Constants.HTTP_DOMAIN)).thenReturn("http://localhost");
        when(shenyuContext.getRealUrl()).thenReturn("/test");
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        StepVerifier.create(uriPlugin.execute(exchange, chain)).expectSubscription().verifyComplete();
        assertEquals("http://localhost/test?queryParam=Hello,World", exchange.getAttributes().get(Constants.HTTP_URI).toString());
        // test rewrite
        when(exchange.getAttribute(Constants.HTTP_DOMAIN)).thenReturn("http://localhost:8090");
        exchange.getAttributes().put(Constants.REWRITE_URI, "/rewrite");
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        StepVerifier.create(uriPlugin.execute(exchange, chain)).expectSubscription().verifyComplete();
        assertEquals("http://localhost:8090/rewrite?queryParam=Hello,World", exchange.getAttributes().get(Constants.HTTP_URI).toString());
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
        assertEquals("http://localhost:8090/query?queryParam=Hello%2C%20World", exchange.getAttributes().get(Constants.HTTP_URI).toString());
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
        assertFalse(uriPlugin.skip(exchange));
        when(shenyuContext.getRpcType()).thenReturn(RpcTypeEnum.SPRING_CLOUD.getName());
        assertFalse(uriPlugin.skip(exchange));
        when(shenyuContext.getRpcType()).thenReturn(RpcTypeEnum.DUBBO.getName());
        assertTrue(uriPlugin.skip(exchange));
    }

    @Test
    public void testRequestQueryCodec() {
        MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        queryParams.add("zcq", "01");
        queryParams.add("zcq", "03");
        queryParams.add("add", "bj");
        queryParams.add("name", "b j");
        queryParams.add("age", "['z','zc','zz']");
        assertEquals("zcq=01&zcq=03&add=bj&name=b%20j&age=['z','zc','zz']", getCodecQuery(queryParams));
    }

    /**
     * Gets codec query string.
     *
     * @param queryParams the queryParams
     * @return codec query string
     */
    private static String getCodecQuery(final MultiValueMap<String, String> queryParams) {
        return queryParams.keySet().stream()
                .map(key -> queryParams.get(key).stream()
                        .map(item -> String.join("=", key,
                                // https://www.w3.org/TR/html4/interact/forms.html#h-17.13.4.1
                                // https://www.ietf.org/rfc/rfc2396.txt
                                item.replaceAll(" ", "%20")))
                        .collect(Collectors.joining("&")))
                .collect(Collectors.joining("&")).trim();
    }
}
