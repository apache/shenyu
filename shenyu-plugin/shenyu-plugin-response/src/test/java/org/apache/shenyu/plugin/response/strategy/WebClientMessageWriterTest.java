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

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
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

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.reset;

/**
 * The test case for {@link WebClientMessageWriter}.
 */
@RunWith(MockitoJUnitRunner.class)
public class WebClientMessageWriterTest {

    private ShenyuPluginChain chain;

    private WebClientMessageWriter webClientMessageWriter;

    @Before
    public void setup() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setApplicationContext(context);
        when(context.getBean(ShenyuResult.class)).thenReturn(mock(ShenyuResult.class));
        chain = mock(ShenyuPluginChain.class);
        webClientMessageWriter = new WebClientMessageWriter();
    }

    @Test
    public void testWriteWith() {
        ServerWebExchange exchangeNormal = generateServerWebExchange(true);
        reset(chain);
        when(chain.execute(exchangeNormal)).thenReturn(Mono.empty());
        Mono<Void> monoSuccess = webClientMessageWriter.writeWith(exchangeNormal, chain);
        StepVerifier.create(monoSuccess).expectSubscription().verifyComplete();

        ServerWebExchange exchangeNullResponse = generateServerWebExchange(false);
        reset(chain);
        when(chain.execute(exchangeNullResponse)).thenReturn(Mono.empty());
        Mono<Void> monoNullResponse = webClientMessageWriter.writeWith(exchangeNullResponse, chain);
        StepVerifier.create(monoNullResponse).expectSubscription().verifyComplete();

        ServerWebExchange exchangeInternalServerError = generateServerWebExchange(true);
        exchangeInternalServerError.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
        reset(chain);
        when(chain.execute(exchangeInternalServerError)).thenReturn(Mono.empty());
        Mono<Void> monoInternalServerError = webClientMessageWriter.writeWith(exchangeInternalServerError, chain);
        StepVerifier.create(monoInternalServerError).expectSubscription().verifyComplete();

        ServerWebExchange exchangeBadGateway = generateServerWebExchange(true);
        exchangeBadGateway.getResponse().setStatusCode(HttpStatus.BAD_GATEWAY);
        reset(chain);
        when(chain.execute(exchangeBadGateway)).thenReturn(Mono.empty());
        Mono<Void> monoBadGateway = webClientMessageWriter.writeWith(exchangeBadGateway, chain);
        StepVerifier.create(monoBadGateway).expectSubscription().verifyComplete();

        ServerWebExchange exchangeGatewayTimeout = generateServerWebExchange(true);
        exchangeGatewayTimeout.getResponse().setStatusCode(HttpStatus.GATEWAY_TIMEOUT);
        reset(chain);
        when(chain.execute(exchangeGatewayTimeout)).thenReturn(Mono.empty());
        Mono<Void> monoGatewayTimeout = webClientMessageWriter.writeWith(exchangeGatewayTimeout, chain);
        StepVerifier.create(monoGatewayTimeout).expectSubscription().verifyComplete();
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

        exchange.getAttributes().put(Constants.CONTEXT, mock(ShenyuContext.class));
        exchange.getAttributes().put(Constants.HTTP_URL, "/test");
        if (haveResponse) {
            exchange.getAttributes().put(Constants.CLIENT_RESPONSE_ATTR, mockResponse);
        }
        return exchange;
    }
}
