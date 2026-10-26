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

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.reactive.function.client.ClientRequest;
import org.springframework.web.reactive.function.client.ClientResponse;
import org.springframework.web.reactive.function.client.ExchangeFunction;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.net.URI;
import java.util.List;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * The test case for the trace context which the http client plugin injects into the outgoing request.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class WebClientPluginTraceContextTest {

    private static final String CLIENT_TRACEPARENT = "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01";

    @Mock
    private ExchangeFunction exchangeFunction;

    @Captor
    private ArgumentCaptor<ClientRequest> captor;

    @BeforeEach
    public void setUp() {
        TestTraceContextPropagator.reset();
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setApplicationContext(context);
        when(context.getBean(ShenyuResult.class)).thenReturn(mock(ShenyuResult.class));
    }

    @Test
    public void testTheUpstreamReceivesTheInjectedContextOnly() {
        mockOkResponse();
        ServerWebExchange exchange = createExchange();
        ShenyuPluginChain chain = mock(ShenyuPluginChain.class);
        when(chain.execute(any())).thenReturn(Mono.empty());
        WebClientPlugin plugin = new WebClientPlugin(WebClient.builder().exchangeFunction(exchangeFunction).build(),
                Constants.BYTES_PER_MB);
        StepVerifier.create(plugin.execute(exchange, chain)).expectSubscription().verifyComplete();
        HttpHeaders outboundHeaders = captor.getValue().headers();
        // the upstream has to receive exactly one traceparent, the injected one: the traceparent of
        // the client was copied to the outgoing request first and has to be replaced, not appended.
        assertEquals(List.of(TestTraceContextPropagator.INJECTED_TRACEPARENT),
                outboundHeaders.get("traceparent"));
        // the headers of the client are still forwarded, the injection only owns its own header
        assertEquals("keep-me", outboundHeaders.getFirst("x-client-header"));
        assertEquals(1, TestTraceContextPropagator.injections());
    }

    @Test
    public void testInjectsWhileTheClientHasNoTraceContext() {
        mockOkResponse();
        ServerWebExchange exchange = createExchange(null);
        ShenyuPluginChain chain = mock(ShenyuPluginChain.class);
        when(chain.execute(any())).thenReturn(Mono.empty());
        WebClientPlugin plugin = new WebClientPlugin(WebClient.builder().exchangeFunction(exchangeFunction).build(),
                Constants.BYTES_PER_MB);
        StepVerifier.create(plugin.execute(exchange, chain)).expectSubscription().verifyComplete();
        // a request which does not carry a trace context still starts one for the upstream call
        assertEquals(List.of(TestTraceContextPropagator.INJECTED_TRACEPARENT),
                captor.getValue().headers().get("traceparent"));
    }

    private ServerWebExchange createExchange() {
        return createExchange(CLIENT_TRACEPARENT);
    }

    /**
     * Creates an exchange, with the trace context of a client when one is given.
     *
     * @param clientTraceparent the traceparent of the client, null for a request without one
     * @return the exchange
     */
    private ServerWebExchange createExchange(final String clientTraceparent) {
        MockServerHttpRequest.BaseBuilder<?> request = MockServerHttpRequest.get("/http/order")
                .header("x-client-header", "keep-me");
        if (Objects.nonNull(clientTraceparent)) {
            request.header("traceparent", clientTraceparent);
        }
        ServerWebExchange exchange = MockServerWebExchange.from(request.build());
        ShenyuContext shenyuContext = new ShenyuContext();
        shenyuContext.setRpcType(RpcTypeEnum.HTTP.getName());
        exchange.getAttributes().put(Constants.CONTEXT, shenyuContext);
        exchange.getAttributes().put(Constants.HTTP_URI, URI.create("http://upstream:9195/http/order"));
        return exchange;
    }

    private void mockOkResponse() {
        ClientResponse.Headers headers = mock(ClientResponse.Headers.class);
        when(headers.asHttpHeaders()).thenReturn(new HttpHeaders());
        ClientResponse response = mock(ClientResponse.class);
        when(response.statusCode()).thenReturn(HttpStatus.OK);
        when(response.headers()).thenReturn(headers);
        when(response.bodyToFlux(DataBuffer.class)).thenReturn(Flux.empty());
        when(response.bodyToMono(byte[].class)).thenReturn(Mono.just(new byte[0]));
        when(response.releaseBody()).thenReturn(Mono.empty());
        given(exchangeFunction.exchange(captor.capture())).willReturn(Mono.just(response));
    }
}
