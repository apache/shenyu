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
import org.apache.shenyu.common.enums.RetryEnum;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.httpclient.exception.ShenyuUpstreamStatusException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.netty.DisposableServer;
import reactor.netty.http.client.HttpClient;
import reactor.netty.http.server.HttpServer;
import reactor.test.StepVerifier;

import java.net.URI;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import static org.mockito.Mockito.mock;

/**
 * The test case for the trace context which the netty http client plugin injects into the outgoing
 * request.
 */
public class NettyHttpClientPluginTraceContextTest {

    private static final String CLIENT_TRACEPARENT = "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01";

    private NettyHttpClientPlugin nettyHttpClientPlugin;

    @BeforeEach
    public void setUp() {
        TestTraceContextPropagator.reset();
        nettyHttpClientPlugin = new NettyHttpClientPlugin(HttpClient.create(), Constants.BYTES_PER_MB);
    }

    @Test
    public void testTheUpstreamReceivesTheInjectedContextOnly() {
        List<List<String>> received = new CopyOnWriteArrayList<>();
        DisposableServer server = startServer(received);
        try {
            ServerWebExchange exchange = createExchange(server.port());
            StepVerifier.create(nettyHttpClientPlugin.doRequest(exchange, "GET", uriOf(server.port()), Flux.empty()))
                    .expectError(ShenyuUpstreamStatusException.class)
                    .verify();
        } finally {
            server.disposeNow();
        }
        // the upstream receives exactly one traceparent, the injected one, and the client one was
        // replaced even though the headers of the request are read only
        Assertions.assertEquals(1, received.size());
        Assertions.assertEquals(List.of(TestTraceContextPropagator.INJECTED_TRACEPARENT), received.get(0));
        Assertions.assertEquals(1, TestTraceContextPropagator.injections());
    }

    @Test
    public void testTheContextIsNotAccumulatedByAResend() {
        List<List<String>> received = new CopyOnWriteArrayList<>();
        DisposableServer server = startServer(received);
        try {
            // a resend reuses the exchange of the request, the way the retry strategy resends it
            ServerWebExchange exchange = createExchange(server.port());
            for (int attempt = 0; attempt < 2; attempt++) {
                StepVerifier.create(nettyHttpClientPlugin.doRequest(exchange, "GET", uriOf(server.port()), Flux.empty()))
                        .expectError(ShenyuUpstreamStatusException.class)
                        .verify();
            }
        } finally {
            server.disposeNow();
        }
        // a resend builds its own headers, so every attempt carries exactly one traceparent and the
        // context of the previous attempt is not repeated
        Assertions.assertEquals(2, received.size());
        received.forEach(headers -> Assertions.assertEquals(
                List.of(TestTraceContextPropagator.INJECTED_TRACEPARENT), headers));
        Assertions.assertEquals(2, TestTraceContextPropagator.injections());
    }

    private DisposableServer startServer(final List<List<String>> received) {
        return HttpServer.create()
                .port(0)
                .handle((request, response) -> {
                    received.add(request.requestHeaders().getAll("traceparent"));
                    return response.status(500).sendString(Mono.just("server error"));
                })
                .bindNow();
    }

    private ServerWebExchange createExchange(final int port) {
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/test")
                .header("traceparent", CLIENT_TRACEPARENT)
                .build());
        exchange.getAttributes().put(Constants.CONTEXT, mock(ShenyuContext.class));
        exchange.getAttributes().put(Constants.HTTP_URI, uriOf(port));
        exchange.getAttributes().put(Constants.RETRY_STRATEGY, RetryEnum.FAILOVER.getName());
        return exchange;
    }

    private URI uriOf(final int port) {
        return URI.create("http://127.0.0.1:" + port + "/test");
    }
}
