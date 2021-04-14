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
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.plugin.api.SoulPluginChain;
import org.dromara.soul.plugin.api.context.SoulContext;
import org.dromara.soul.plugin.api.result.SoulResult;
import org.dromara.soul.plugin.api.utils.SpringBeanUtils;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseCookie;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.reactive.function.client.ClientResponse;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.any;

/**
 * The test case for WebClientResponsePlugin.
 *
 * @author YuI
 **/
@RunWith(MockitoJUnitRunner.class)
public final class WebClientResponsePluginTest {

    private SoulPluginChain chain;

    private WebClientResponsePlugin webClientResponsePlugin;

    @Before
    public void setup() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setCfgContext(context);
        when(context.getBean(SoulResult.class)).thenReturn(mock(SoulResult.class));
        chain = mock(SoulPluginChain.class);
        webClientResponsePlugin = new WebClientResponsePlugin();
    }

    /**
     * test case for WebClientPlugin {@link WebClientResponsePlugin#execute(ServerWebExchange, SoulPluginChain)}.
     */
    @Test
    public void testExecuted() {
        ServerWebExchange exchangeNormal = generateServerWebExchange(true);
        reset(chain);
        when(chain.execute(exchangeNormal)).thenReturn(Mono.empty());
        Mono<Void> monoSuccess = webClientResponsePlugin.execute(exchangeNormal, chain);
        StepVerifier.create(monoSuccess).expectSubscription().verifyComplete();

        ServerWebExchange exchangeNullResponse = generateServerWebExchange(false);
        reset(chain);
        when(chain.execute(exchangeNullResponse)).thenReturn(Mono.empty());
        Mono<Void> monoNullResponse = webClientResponsePlugin.execute(exchangeNullResponse, chain);
        StepVerifier.create(monoNullResponse).expectSubscription().verifyComplete();

        ServerWebExchange exchangeInternalServerError = generateServerWebExchange(true);
        exchangeInternalServerError.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
        reset(chain);
        when(chain.execute(exchangeInternalServerError)).thenReturn(Mono.empty());
        Mono<Void> monoInternalServerError = webClientResponsePlugin.execute(exchangeInternalServerError, chain);
        StepVerifier.create(monoInternalServerError).expectSubscription().verifyComplete();

        ServerWebExchange exchangeBadGateway = generateServerWebExchange(true);
        exchangeBadGateway.getResponse().setStatusCode(HttpStatus.BAD_GATEWAY);
        reset(chain);
        when(chain.execute(exchangeBadGateway)).thenReturn(Mono.empty());
        Mono<Void> monoBadGateway = webClientResponsePlugin.execute(exchangeBadGateway, chain);
        StepVerifier.create(monoBadGateway).expectSubscription().verifyComplete();

        ServerWebExchange exchangeGatewayTimeout = generateServerWebExchange(true);
        exchangeGatewayTimeout.getResponse().setStatusCode(HttpStatus.GATEWAY_TIMEOUT);
        reset(chain);
        when(chain.execute(exchangeGatewayTimeout)).thenReturn(Mono.empty());
        Mono<Void> monoGatewayTimeout = webClientResponsePlugin.execute(exchangeGatewayTimeout, chain);
        StepVerifier.create(monoGatewayTimeout).expectSubscription().verifyComplete();
    }

    /**
     * test case for WebClientPlugin {@link WebClientResponsePlugin#skip(ServerWebExchange)}.
     */
    @Test
    public void testSkip() {
        ServerWebExchange exchangeNormal = generateServerWebExchange(true);
        assertTrue(webClientResponsePlugin.skip(exchangeNormal));

        ServerWebExchange exchangeHttp = generateServerWebExchange(true);
        when(((SoulContext) exchangeHttp.getAttributes().get(Constants.CONTEXT)).getRpcType())
                .thenReturn(RpcTypeEnum.HTTP.getName());
        assertFalse(webClientResponsePlugin.skip(exchangeHttp));

        ServerWebExchange exchangeSpringCloud = generateServerWebExchange(true);
        when(((SoulContext) exchangeSpringCloud.getAttributes().get(Constants.CONTEXT)).getRpcType())
                .thenReturn(RpcTypeEnum.SPRING_CLOUD.getName());
        assertFalse(webClientResponsePlugin.skip(exchangeSpringCloud));
    }

    @Test
    public void testGetOrder() {
        assertEquals(PluginEnum.RESPONSE.getCode(), webClientResponsePlugin.getOrder());
    }

    @Test
    public void testNamed() {
        assertEquals(PluginEnum.RESPONSE.getName(), webClientResponsePlugin.named());
    }

    private ServerWebExchange generateServerWebExchange(final boolean haveResponse) {
        ClientResponse mockResponse = mock(ClientResponse.class);
        MultiValueMap<String, ResponseCookie> cookies = new LinkedMultiValueMap<>();
        cookies.add("id", mock(ResponseCookie.class));
        when(mockResponse.cookies()).thenReturn(cookies);
        ClientResponse.Headers headers = mock(ClientResponse.Headers.class);
        when(headers.asHttpHeaders()).thenReturn(mock(HttpHeaders.class));
        when(mockResponse.headers()).thenReturn(headers);
        when(mockResponse.body(any())).thenReturn(Mono.empty());

        ServerWebExchange exchange = MockServerWebExchange
                .from(MockServerHttpRequest.get("/test").build());

        exchange.getAttributes().put(Constants.CONTEXT, mock(SoulContext.class));
        exchange.getAttributes().put(Constants.HTTP_URL, "/test");
        if (haveResponse) {
            exchange.getAttributes().put(Constants.CLIENT_RESPONSE_ATTR, mockResponse);
        }
        return exchange;
    }
}
