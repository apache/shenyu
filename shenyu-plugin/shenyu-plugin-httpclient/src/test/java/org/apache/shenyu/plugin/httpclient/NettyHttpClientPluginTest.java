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
import org.apache.shenyu.common.enums.RetryEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.circuitbreaker.UpstreamCircuitBreaker;
import org.apache.shenyu.plugin.httpclient.exception.ShenyuUpstreamStatusException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.http.server.reactive.ServerHttpResponseDecorator;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.ServerWebExchangeDecorator;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Flux;
import reactor.netty.DisposableServer;
import reactor.netty.http.client.HttpClient;
import reactor.netty.http.server.HttpServer;
import reactor.test.StepVerifier;

import java.net.URI;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * The test case for NettyHttpClientPlugin.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class NettyHttpClientPluginTest {

    private NettyHttpClientPlugin nettyHttpClientPlugin;

    private ShenyuPluginChain chain;

    @BeforeEach
    public void setUp() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setApplicationContext(context);
        when(context.getBean(ShenyuResult.class)).thenReturn(mock(ShenyuResult.class));
        chain = mock(ShenyuPluginChain.class);
        when(chain.execute(any())).thenReturn(Mono.empty());
        HttpClient httpClient = HttpClient.create();
        nettyHttpClientPlugin = new NettyHttpClientPlugin(httpClient, Constants.BYTES_PER_MB);
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
        exchange.getAttributes().put(Constants.HTTP_URI, URI.create("/test"));

        StepVerifier.create(nettyHttpClientPlugin.execute(exchange, chain)).expectSubscription().verifyError();
    }

    @Test
    public void testRecoveryProbeMakesOneAttemptWhileOrdinaryRequestsRetainRetries() {
        AtomicInteger attempts = new AtomicInteger();
        NettyHttpClientPlugin failing = new NettyHttpClientPlugin(HttpClient.create(), Constants.BYTES_PER_MB) {
            @Override
            protected Mono<reactor.netty.http.client.HttpClientResponse> doRequest(final ServerWebExchange exchange, final String method,
                    final URI uri, final Flux<org.springframework.core.io.buffer.DataBuffer> body) {
                return Mono.defer(() -> {
                    attempts.incrementAndGet();
                    return Mono.error(new IllegalStateException("upstream down"));
                });
            }
        };
        for (boolean probe : new boolean[]{false, true}) {
            attempts.set(0);
            ServerWebExchange exchange = generateServerWebExchange();
            exchange.getAttributes().put(Constants.HTTP_URI, URI.create("http://localhost/test"));
            exchange.getAttributes().put(Constants.HTTP_RETRY, 2);
            exchange.getAttributes().put(Constants.RETRY_STRATEGY, RetryEnum.CURRENT.getName());
            exchange.getAttributes().put(UpstreamCircuitBreaker.HALF_OPEN_PROBE, probe);
            StepVerifier.create(failing.execute(exchange, chain)).expectError().verify();
            assertEquals(probe ? 1 : 3, attempts.get());
        }
    }

    @Test
    public void testServerErrorTriggersFailover() {
        final DisposableServer server = HttpServer.create()
                .port(0)
                .handle((request, response) -> response.status(500).sendString(Mono.just("server error")))
                .bindNow();
        try {
            final ServerWebExchange exchange = generateServerWebExchange();
            exchange.getAttributes().put(Constants.RETRY_STRATEGY, RetryEnum.FAILOVER.getName());
            final URI uri = URI.create("http://127.0.0.1:" + server.port() + "/test");

            StepVerifier.create(nettyHttpClientPlugin.doRequest(exchange, "GET", uri, Flux.empty()))
                    .expectError(ShenyuUpstreamStatusException.class)
                    .verify();
        } finally {
            server.disposeNow();
        }
    }

    /**
     * A non-standard upstream status code must be preserved even when the response is decorated.
     *
     * <p>Regression test for #7801. A {@link ServerHttpResponseDecorator} only implements the
     * {@link ServerHttpResponse} interface and does not extend Spring's
     * {@code AbstractServerHttpResponse}, so the raw-status fallback in the Netty client must not
     * depend on that concrete type.</p>
     */
    @Test
    public void testNonStandardStatusWithDecoratedResponse() {
        final DisposableServer server = HttpServer.create()
                .port(0)
                .handle((request, response) -> response.status(529).sendString(Mono.just("payload")))
                .bindNow();
        try {
            final ServerWebExchange baseExchange = generateServerWebExchange();
            final ServerHttpResponse decoratedResponse = new ServerHttpResponseDecorator(baseExchange.getResponse());
            final ServerWebExchange decoratedExchange = new ServerWebExchangeDecorator(baseExchange) {
                @Override
                public ServerHttpResponse getResponse() {
                    return decoratedResponse;
                }
            };
            final URI uri = URI.create("http://127.0.0.1:" + server.port() + "/test");

            StepVerifier.create(nettyHttpClientPlugin.doRequest(decoratedExchange, "GET", uri, Flux.empty()))
                    .expectNextCount(1)
                    .verifyComplete();

            assertEquals(529, baseExchange.getResponse().getStatusCode().value());
        } finally {
            server.disposeNow();
        }
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
        exchange.getAttributes().put(Constants.HTTP_URI, "/test");
        return exchange;
    }
}
