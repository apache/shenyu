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

package org.dromara.soul.plugin.httpclient;

import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.plugin.api.SoulPluginChain;
import org.dromara.soul.plugin.api.context.SoulContext;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.http.HttpStatus;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.reactive.function.client.ClientRequest;
import org.springframework.web.reactive.function.client.ClientResponse;
import org.springframework.web.reactive.function.client.ExchangeFunction;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.junit.Assert.assertEquals;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * The test case for WebClientPlugin.
 *
 * @author YuI
 **/
@RunWith(MockitoJUnitRunner.class)
public final class WebClientPluginTest {

    @Mock
    private ExchangeFunction exchangeFunction;

    @Captor
    private ArgumentCaptor<ClientRequest> captor;

    private WebClientPlugin webClientPlugin;

    private ServerWebExchange exchange;

    @Before
    public void setup() {
        exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/test").build());
        exchange.getAttributes().put(Constants.CONTEXT, mock(SoulContext.class));
        exchange.getAttributes().put(Constants.HTTP_URL, "/test");
        WebClient webClient = mockWebClientOK();

        webClientPlugin = new WebClientPlugin(webClient);
    }

    /**
     * test case for WebClientPlugin {@link WebClientPlugin#execute(ServerWebExchange, SoulPluginChain)}.
     */
    @Test
    public void testExecuted() {
        final SoulPluginChain chainOkTest = mock(SoulPluginChain.class);
        final WebClient webClientOkTest = mockWebClientOK();
        WebClientPlugin webClientPluginOkTest = new WebClientPlugin(webClientOkTest);
        Mono<Void> monoOkTest = webClientPluginOkTest.execute(exchange, chainOkTest);
        StepVerifier.create(monoOkTest).expectSubscription().verifyError();

        final SoulPluginChain chainErrorTest = mock(SoulPluginChain.class);
        final WebClient webClientErrorTest = mockWebClientError();
        WebClientPlugin webClientPluginErrorTest = new WebClientPlugin(webClientErrorTest);
        Mono<Void> monoErrorTest = webClientPluginErrorTest.execute(exchange, chainErrorTest);
        StepVerifier.create(monoErrorTest).expectSubscription().verifyError();
    }

    /**
     * test case for WebClientPlugin {@link WebClientPlugin#skip(ServerWebExchange)}.
     */
    @Test
    public void testSkip() {
        Assert.assertTrue(webClientPlugin.skip(exchange));
    }

    @Test
    public void testGetOrder() {
        assertEquals(PluginEnum.DIVIDE.getCode() + 1, webClientPlugin.getOrder());
    }

    @Test
    public void testNamed() {
        assertEquals("webClient", webClientPlugin.named());
    }

    private WebClient mockWebClientOK() {
        final ClientResponse mockResponse = mock(ClientResponse.class);
        when(mockResponse.statusCode()).thenReturn(HttpStatus.OK);
        given(this.exchangeFunction.exchange(this.captor.capture())).willReturn(Mono.just(mockResponse));
        return WebClient.builder().baseUrl("/test")
                .exchangeFunction(this.exchangeFunction)
                .apply(consumer -> consumer.defaultHeader("Accept", "application/json")
                        .defaultCookie("id", "test"))
                .build();
    }

    private WebClient mockWebClientError() {
        final ClientResponse mockResponse = mock(ClientResponse.class);
        when(mockResponse.statusCode()).thenReturn(HttpStatus.INTERNAL_SERVER_ERROR);
        given(this.exchangeFunction.exchange(this.captor.capture())).willReturn(Mono.just(mockResponse));
        return WebClient.builder().baseUrl("/test")
                .exchangeFunction(this.exchangeFunction)
                .apply(consumer -> consumer.defaultHeader("Accept", "application/json")
                        .defaultCookie("id", "test"))
                .build();
    }
}
