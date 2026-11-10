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
import org.apache.shenyu.common.enums.HeaderUniqueStrategyEnum;
import org.apache.shenyu.common.enums.UniqueHeaderEnum;
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
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.http.HttpHeaders;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.http.server.reactive.ServerHttpResponseDecorator;
import org.springframework.http.server.reactive.ReactorHttpHandlerAdapter;
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
import java.util.List;
import java.util.Objects;
import java.util.stream.Stream;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
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

    @ParameterizedTest
    @CsvSource(delimiter = '|', value = {
        "/plain | /plain", "/a%2Fb?x=a%2Bb&empty=&flag | /a%2Fb?x=a%2Bb&empty=&flag",
        "/你好 | /%E4%BD%A0%E5%A5%BD", "?key=value | /?key=value", "/a#fragment | /a", "/ | /"
    })
    public void testOutgoingUriPreservesEncoding(final String suffix, final String expected) {
        AtomicReference<String> received = new AtomicReference<>();
        DisposableServer server = HttpServer.create().host("127.0.0.1").port(0)
                .handle((request, response) -> {
                    received.set(request.uri());
                    return response.send();
                }).bindNow();
        try {
            ServerWebExchange exchange = generateServerWebExchange();
            URI uri = URI.create("http://127.0.0.1:" + server.port() + suffix);
            StepVerifier.create(nettyHttpClientPlugin.doRequest(exchange, "GET", uri, Flux.empty()))
                    .expectNextCount(1).verifyComplete();
            assertEquals(expected, received.get());
        } finally {
            server.disposeNow();
        }
    }

    static Stream<Arguments> responseHeaderPolicies() {
        return Stream.of(Arguments.of(null, List.of("one", "two", "one", "three"), List.of("a=1", "b=2", "a=1", "c=3")),
                Arguments.of("", List.of("one", "two", "one", "three"), List.of("a=1", "b=2", "a=1", "c=3")),
                Arguments.of("RETAIN_FIRST", List.of("one"), List.of("a=1")),
                Arguments.of("RETAIN_LAST", List.of("three"), List.of("c=3")),
                Arguments.of("RETAIN_UNIQUE", List.of("one", "two", "three"), List.of("a=1", "b=2", "c=3")));
    }

    @ParameterizedTest
    @MethodSource("responseHeaderPolicies")
    void testResponseHeadersPreserveValuesAndIsolation(final String policy, final List<String> expected, final List<String> cookies) {
        DisposableServer server = headerServer();
        try {
            ServerWebExchange exchange = generateServerWebExchange();
            exchange.getResponse().getHeaders().set("X-Multi", "gateway-old");
            exchange.getResponse().getHeaders().set("X-Gateway", "keep");
            if (Objects.nonNull(policy)) {
                exchange.getAttributes().put(UniqueHeaderEnum.RESP_UNIQUE_HEADER.getName(), policy.isEmpty() ? "" : "X-Multi;Set-Cookie");
                if (!policy.isEmpty()) {
                    exchange.getAttributes().put(UniqueHeaderEnum.RESP_UNIQUE_HEADER.getStrategy(), HeaderUniqueStrategyEnum.valueOf(policy));
                }
            }
            StepVerifier.create(nettyHttpClientPlugin.doRequest(exchange, "GET", URI.create("http://127.0.0.1:" + server.port() + "/test"), Flux.empty()))
                    .assertNext(response -> {
                        assertEquals(expected, exchange.getResponse().getHeaders().get("x-multi"));
                        assertEquals(cookies, exchange.getResponse().getHeaders().get(HttpHeaders.SET_COOKIE));
                        assertEquals("keep", exchange.getResponse().getHeaders().getFirst("X-Gateway"));
                        assertEquals("text/plain", exchange.getAttribute(Constants.ORIGINAL_RESPONSE_CONTENT_TYPE_ATTR));
                        assertEquals(List.of("one", "two", "one", "three"), response.responseHeaders().getAll("X-Multi"));
                        assertEquals(List.of("a=1", "b=2", "a=1", "c=3"), response.responseHeaders().getAll(HttpHeaders.SET_COOKIE));
                        exchange.getResponse().getHeaders().set("X-Multi", "changed-after-copy");
                        assertEquals(List.of("one", "two", "one", "three"), response.responseHeaders().getAll("X-Multi"));
                    }).verifyComplete();
        } finally {
            server.disposeNow();
        }
    }

    @Test
    void testCustomResponseHeaderHookKeepsMutableIndependentHeaders() {
        NettyHttpClientPlugin custom = new NettyHttpClientPlugin(HttpClient.create(), Constants.BYTES_PER_MB) {
            @Override
            protected void duplicateHeaders(final ServerWebExchange exchange, final HttpHeaders headers, final UniqueHeaderEnum type) {
                if (type == UniqueHeaderEnum.RESP_UNIQUE_HEADER) {
                    headers.set("X-Multi", "custom");
                } else {
                    super.duplicateHeaders(exchange, headers, type);
                }
            }
        };
        DisposableServer server = headerServer();
        try {
            ServerWebExchange exchange = generateServerWebExchange();
            StepVerifier.create(custom.doRequest(exchange, "GET", URI.create("http://127.0.0.1:" + server.port() + "/test"), Flux.empty()))
                    .assertNext(response -> {
                        assertEquals(List.of("custom"), exchange.getResponse().getHeaders().get("X-Multi"));
                        assertEquals(List.of("one", "two", "one", "three"), response.responseHeaders().getAll("X-Multi"));
                    }).verifyComplete();
        } finally {
            server.disposeNow();
        }
    }

    @Test
    void testReadOnlyResponseStillPreservesUpstreamResponse() {
        ServerWebExchange base = generateServerWebExchange();
        base.getResponse().getHeaders().set("X-Gateway", "keep");
        ServerHttpResponse decorated = new ServerHttpResponseDecorator(base.getResponse()) {
            @Override
            public HttpHeaders getHeaders() {
                return HttpHeaders.readOnlyHttpHeaders(super.getHeaders());
            }
        };
        ServerWebExchange exchange = new ServerWebExchangeDecorator(base) {
            @Override
            public ServerHttpResponse getResponse() {
                return decorated;
            }
        };
        DisposableServer server = headerServer();
        try {
            StepVerifier.create(nettyHttpClientPlugin.doRequest(exchange, "GET", URI.create("http://127.0.0.1:" + server.port() + "/test"), Flux.empty()))
                    .assertNext(response -> {
                        assertEquals(List.of("one", "two", "one", "three"), response.responseHeaders().getAll("X-Multi"));
                        assertEquals("keep", exchange.getResponse().getHeaders().getFirst("X-Gateway"));
                        assertEquals(200, exchange.getResponse().getStatusCode().value());
                        assertEquals("text/plain", exchange.getAttribute(Constants.ORIGINAL_RESPONSE_CONTENT_TYPE_ATTR));
                    }).verifyComplete();
        } finally {
            server.disposeNow();
        }
    }

    private DisposableServer headerServer() {
        return HttpServer.create().host("127.0.0.1").port(0).handle((request, response) -> {
            response.addHeader("X-Multi", "one").addHeader("x-multi", "two").addHeader("X-MULTI", "one").addHeader("X-Multi", "three");
            response.addHeader(HttpHeaders.SET_COOKIE, "a=1").addHeader(HttpHeaders.SET_COOKIE, "b=2").addHeader(HttpHeaders.SET_COOKIE, "a=1").addHeader(HttpHeaders.SET_COOKIE, "c=3");
            return response.header(HttpHeaders.CONTENT_TYPE, "text/plain").send();
        }).bindNow();
    }

    static Stream<Arguments> nativeResponseHeaderCases() {
        return responseHeaderPolicies().flatMap(args -> Stream.of(false, true).map(existing ->
                Arguments.of(args.get()[0], args.get()[1], args.get()[2], existing)));
    }

    @ParameterizedTest
    @MethodSource("nativeResponseHeaderCases")
    void testNativeResponseHeadersPreserveMergeAndPolicies(final String policy, final List<String> expected,
                                                          final List<String> cookies, final boolean existing) {
        DisposableServer upstream = headerServer();
        AtomicInteger assertions = new AtomicInteger();
        AtomicReference<Throwable> handlerFailure = new AtomicReference<>();
        DisposableServer gateway = HttpServer.create().host("127.0.0.1").port(0)
                .handle(new ReactorHttpHandlerAdapter((request, response) -> {
                    ServerWebExchange exchange = generateServerWebExchange().mutate().response(response).build();
                    if (existing) {
                        response.getHeaders().set("X-Multi", "gateway-old");
                        response.getHeaders().set("X-Gateway", "keep");
                    }
                    if (Objects.nonNull(policy)) {
                        exchange.getAttributes().put(UniqueHeaderEnum.RESP_UNIQUE_HEADER.getName(), policy.isEmpty() ? "" : "X-Multi;Set-Cookie");
                        if (!policy.isEmpty()) {
                            exchange.getAttributes().put(UniqueHeaderEnum.RESP_UNIQUE_HEADER.getStrategy(), HeaderUniqueStrategyEnum.valueOf(policy));
                        }
                    }
                    return nettyHttpClientPlugin.doRequest(exchange, "GET", URI.create("http://127.0.0.1:" + upstream.port() + "/test"), Flux.empty())
                            .doOnNext(raw -> {
                                assertEquals(expected, response.getHeaders().get("x-multi"),
                                        "target=" + response.getHeaders() + "; source=" + raw.responseHeaders());
                                assertEquals(cookies, response.getHeaders().get(HttpHeaders.SET_COOKIE));
                                assertEquals("text/plain", exchange.getAttribute(Constants.ORIGINAL_RESPONSE_CONTENT_TYPE_ATTR));
                                raw.responseHeaders().set("X-Multi", "upstream-mutated");
                                assertEquals(expected, response.getHeaders().get("x-multi"));
                                response.getHeaders().set("X-Multi", "gateway-mutated");
                                assertEquals(List.of("upstream-mutated"), raw.responseHeaders().getAll("X-Multi"));
                                response.getHeaders().put("X-Multi", expected);
                                assertions.incrementAndGet();
                            }).then(Mono.defer(response::setComplete)).doOnError(handlerFailure::set);
                })).bindNow();
        try {
            HttpClient.create().get().uri("http://127.0.0.1:" + gateway.port() + "/test")
                    .responseSingle((response, body) -> {
                        assertEquals(200, response.status().code(), String.valueOf(handlerFailure.get()));
                        assertEquals(expected, response.responseHeaders().getAll("X-Multi"));
                        assertEquals(cookies, response.responseHeaders().getAll(HttpHeaders.SET_COOKIE));
                        assertEquals(existing ? "keep" : null, response.responseHeaders().get("X-Gateway"));
                        return body.then();
                    }).block();
            assertEquals(1, assertions.get());
        } finally {
            gateway.disposeNow();
            upstream.disposeNow();
        }
    }

    @Test
    void testCommittedNativeResponseKeepsOriginalHeadersAndStatus() throws InterruptedException {
        DisposableServer upstream = headerServer();
        CountDownLatch completed = new CountDownLatch(1);
        AtomicReference<Throwable> failure = new AtomicReference<>();
        DisposableServer gateway = HttpServer.create().host("127.0.0.1").port(0)
                .handle(new ReactorHttpHandlerAdapter((request, response) -> {
                    response.getHeaders().set("X-Multi", "already-sent");
                    response.setStatusCode(org.springframework.http.HttpStatus.ACCEPTED);
                    ServerWebExchange exchange = generateServerWebExchange().mutate().response(response).build();
                    return response.setComplete().then(Mono.defer(() ->
                            nettyHttpClientPlugin.doRequest(exchange, "GET", URI.create("http://127.0.0.1:" + upstream.port() + "/test"), Flux.empty())))
                            .doOnNext(raw -> {
                                assertTrue(response.isCommitted());
                                assertEquals(List.of("already-sent"), response.getHeaders().get("X-Multi"));
                                assertEquals(202, response.getStatusCode().value());
                                assertEquals("text/plain", exchange.getAttribute(Constants.ORIGINAL_RESPONSE_CONTENT_TYPE_ATTR));
                                assertEquals(List.of("one", "two", "one", "three"), raw.responseHeaders().getAll("X-Multi"));
                            }).then().doOnError(failure::set).doFinally(signal -> completed.countDown());
                })).bindNow();
        try {
            HttpClient.create().get().uri("http://127.0.0.1:" + gateway.port() + "/test")
                    .responseSingle((response, body) -> {
                        assertEquals(202, response.status().code());
                        assertEquals(List.of("already-sent"), response.responseHeaders().getAll("X-Multi"));
                        return body.then();
                    }).block();
            assertTrue(completed.await(5, TimeUnit.SECONDS));
            assertNull(failure.get());
        } finally {
            gateway.disposeNow();
            upstream.disposeNow();
        }
    }

    @ParameterizedTest
    @CsvSource({"builtin", "subclass", "decorator", "read-only"})
    void testNativeResponseExtensionBoundaries(final String mode) {
        NettyHttpClientPlugin plugin = "subclass".equals(mode)
                ? new NettyHttpClientPlugin(HttpClient.create(), Constants.BYTES_PER_MB) {
                    @Override
                    protected void duplicateHeaders(final ServerWebExchange exchange, final HttpHeaders headers, final UniqueHeaderEnum type) {
                        if (type == UniqueHeaderEnum.RESP_UNIQUE_HEADER) {
                            headers.set("X-Multi", "custom");
                        } else {
                            super.duplicateHeaders(exchange, headers, type);
                        }
                    }
                } : nettyHttpClientPlugin;
        List<String> expected = "subclass".equals(mode) ? List.of("custom")
                : "read-only".equals(mode) ? List.of("gateway-old") : List.of("one", "two", "one", "three");
        AtomicReference<List<String>> atCommit = new AtomicReference<>();
        AtomicReference<Throwable> handlerFailure = new AtomicReference<>();
        DisposableServer upstream = headerServer();
        DisposableServer gateway = HttpServer.create().host("127.0.0.1").port(0)
                .handle(new ReactorHttpHandlerAdapter((request, response) -> {
                    response.getHeaders().set("X-Multi", "gateway-old");
                    response.getHeaders().set("X-Gateway", "keep");
                    response.getCookies().add("gateway", org.springframework.http.ResponseCookie.from("gateway", "yes").build());
                    response.beforeCommit(() -> {
                        atCommit.set(response.getHeaders().get("X-Multi"));
                        return Mono.empty();
                    });
                    ServerHttpResponse selected = response;
                    if ("decorator".equals(mode)) {
                        selected = new ServerHttpResponseDecorator(response);
                    } else if ("read-only".equals(mode)) {
                        selected = new ServerHttpResponseDecorator(response) {
                            @Override
                            public HttpHeaders getHeaders() {
                                return HttpHeaders.readOnlyHttpHeaders(super.getHeaders());
                            }
                        };
                    }
                    ServerWebExchange exchange = generateServerWebExchange().mutate().response(selected).build();
                    return plugin.doRequest(exchange, "GET", URI.create("http://127.0.0.1:" + upstream.port() + "/test"), Flux.empty())
                            .doOnNext(raw -> {
                                assertEquals(expected, response.getHeaders().get("X-Multi"));
                                assertEquals("keep", response.getHeaders().getFirst("X-Gateway"));
                                assertEquals("text/plain", exchange.getAttribute(Constants.ORIGINAL_RESPONSE_CONTENT_TYPE_ATTR));
                                assertEquals(List.of("one", "two", "one", "three"), raw.responseHeaders().getAll("X-Multi"));
                            }).then(Mono.defer(response::setComplete)).doOnError(handlerFailure::set);
                })).bindNow();
        try {
            HttpClient.create().get().uri("http://127.0.0.1:" + gateway.port() + "/test")
                    .responseSingle((response, body) -> {
                        assertEquals(200, response.status().code(), String.valueOf(handlerFailure.get()));
                        assertEquals(expected, response.responseHeaders().getAll("X-Multi"));
                        assertEquals("keep", response.responseHeaders().get("X-Gateway"));
                        List<String> cookies = response.responseHeaders().getAll(HttpHeaders.SET_COOKIE);
                        assertTrue(cookies.stream().anyMatch(value -> value.startsWith("gateway=yes")));
                        if (!"read-only".equals(mode)) {
                            assertTrue(cookies.containsAll(List.of("a=1", "b=2", "a=1", "c=3")));
                        }
                        return body.then();
                    }).block(java.time.Duration.ofSeconds(5));
            assertNull(handlerFailure.get());
            assertEquals(expected, atCommit.get());
        } finally {
            gateway.disposeNow();
            upstream.disposeNow();
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
