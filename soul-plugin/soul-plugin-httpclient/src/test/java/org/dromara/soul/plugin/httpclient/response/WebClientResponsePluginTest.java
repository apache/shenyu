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

import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.plugin.api.SoulPluginChain;
import org.dromara.soul.plugin.api.context.SoulContext;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseCookie;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.reactive.function.client.ClientResponse;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * The test case for WebClientResponsePlugin.
 *
 * @author YuI
 **/
@RunWith(MockitoJUnitRunner.class)
public final class WebClientResponsePluginTest {

    private ServerWebExchange exchange;

    private SoulPluginChain chain;

    private WebClientResponsePlugin webClientResponsePlugin;

    @Before
    public void setup() {
        chain = mock(SoulPluginChain.class);
        exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/test").build());

        ClientResponse mockResponse = mock(ClientResponse.class);
        MultiValueMap<String, ResponseCookie> cookies = new LinkedMultiValueMap<>();
        cookies.add("id", mock(ResponseCookie.class));
        when(mockResponse.cookies()).thenReturn(cookies);
        ClientResponse.Headers headers = mock(ClientResponse.Headers.class);
        when(headers.asHttpHeaders()).thenReturn(mock(HttpHeaders.class));
        when(mockResponse.headers()).thenReturn(headers);
        exchange.getAttributes().put(Constants.CONTEXT, mock(SoulContext.class));
        exchange.getAttributes().put(Constants.HTTP_URL, "/test");
        exchange.getAttributes().put(Constants.CLIENT_RESPONSE_ATTR, mockResponse);

        webClientResponsePlugin = new WebClientResponsePlugin();
    }

    /**
     * test case for WebClientPlugin {@link WebClientResponsePlugin#execute(ServerWebExchange, SoulPluginChain)}.
     */
    @Test
    public void testExecuted() {
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        Mono<Void> mono = webClientResponsePlugin.execute(exchange, chain);
        StepVerifier.create(mono).expectSubscription().verifyError();
    }

    /**
     * test case for WebClientPlugin {@link WebClientResponsePlugin#skip(ServerWebExchange)}.
     */
    @Test
    public void testSkip() {
        Assert.assertTrue(webClientResponsePlugin.skip(exchange));
    }

    @Test
    public void testGetOrder() {
        assertEquals(PluginEnum.RESPONSE.getCode(), webClientResponsePlugin.getOrder());
    }

    @Test
    public void testNamed() {
        assertEquals(PluginEnum.RESPONSE.getName(), webClientResponsePlugin.named());
    }

}
