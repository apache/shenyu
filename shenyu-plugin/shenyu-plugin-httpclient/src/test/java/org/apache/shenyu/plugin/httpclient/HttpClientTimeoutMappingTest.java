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

import io.netty.channel.ConnectTimeoutException;
import io.netty.handler.timeout.ReadTimeoutException;
import io.netty.handler.timeout.WriteTimeoutException;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.client.reactive.ReactorClientHttpConnector;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.reactive.function.client.WebClientRequestException;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.netty.DisposableServer;
import reactor.netty.http.client.HttpClient;
import reactor.netty.http.server.HttpServer;
import reactor.test.StepVerifier;

import java.net.ConnectException;
import java.net.SocketTimeoutException;
import java.net.URI;
import java.time.Duration;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verifyNoInteractions;

/**
 * Transport timeout classification at the HTTP plugin boundary.
 */
public final class HttpClientTimeoutMappingTest {

    @ParameterizedTest
    @MethodSource("timeouts")
    public void testZeroRetryTimeout(final Throwable failure) {
        assertTimeoutMapping(failure, "default", 0, 1);
    }

    @ParameterizedTest
    @MethodSource("backoffTimeouts")
    public void testConfiguredBackoffTimeout(final String spec, final Throwable failure, final int expectedAttempts) {
        assertTimeoutMapping(failure, spec, 1, expectedAttempts);
    }

    @ParameterizedTest
    @ValueSource(ints = {400, 408, 503, 504})
    public void testExplicitResponseStatusIsPreserved(final int status) {
        ResponseStatusException failure = new ResponseStatusException(HttpStatus.valueOf(status), "existing status", ReadTimeoutException.INSTANCE);
        ShenyuPluginChain chain = mock(ShenyuPluginChain.class);
        StepVerifier.create(new FailingPlugin(failure, new AtomicInteger()).execute(exchange("default", 0), chain))
                .expectErrorSatisfies(error -> assertSame(failure, error))
                .verify();
        verifyNoInteractions(chain);
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    public void testConnectionRefusalIsNotTimeout(final boolean wrapped) {
        Throwable failure = new ConnectException("connection refused");
        if (wrapped) {
            failure = wrap(failure);
        }
        Throwable expected = failure;
        ShenyuPluginChain chain = mock(ShenyuPluginChain.class);
        StepVerifier.create(new FailingPlugin(failure, new AtomicInteger()).execute(exchange("default", 0), chain))
                .expectErrorSatisfies(error -> assertSame(expected, error))
                .verify();
        verifyNoInteractions(chain);
    }

    @Test
    public void testWrappedResponseStatusIsPreserved() {
        Throwable failure = wrap(new ResponseStatusException(HttpStatus.BAD_REQUEST, "existing status", ReadTimeoutException.INSTANCE));
        StepVerifier.create(new FailingPlugin(failure, new AtomicInteger()).execute(exchange("default", 0), mock(ShenyuPluginChain.class)))
                .expectErrorSatisfies(error -> assertSame(failure, error))
                .verify();
    }

    @Test
    public void testTimeoutReasonComesFromCause() {
        assertTimeoutMapping(new IllegalStateException("request failed", ReadTimeoutException.INSTANCE), "default", 0, 1);
    }

    @Test
    public void testCyclicCausesDoNotHang() {
        Throwable first = new IllegalStateException("first");
        Throwable second = new IllegalStateException("second");
        first.initCause(second);
        second.initCause(first);
        StepVerifier.create(new FailingPlugin(first, new AtomicInteger()).execute(exchange("default", 0), mock(ShenyuPluginChain.class)))
                .expectErrorSatisfies(error -> assertSame(first, error))
                .verify(Duration.ofSeconds(1));
    }

    @ParameterizedTest
    @ValueSource(strings = {"netty", "webClient"})
    public void testRealUpstreamReadTimeout(final String strategy) {
        AtomicInteger requests = new AtomicInteger();
        DisposableServer upstream = HttpServer.create().host("127.0.0.1").port(0)
                .handle((request, response) -> {
                    requests.incrementAndGet();
                    return Mono.never();
                }).bindNow();
        try {
            HttpClient client = HttpClient.newConnection().responseTimeout(Duration.ofMillis(100));
            ShenyuPlugin plugin = "netty".equals(strategy) ? new NettyHttpClientPlugin(client, Constants.BYTES_PER_MB)
                    : new WebClientPlugin(WebClient.builder().clientConnector(new ReactorClientHttpConnector(client)).build(), Constants.BYTES_PER_MB);
            ServerWebExchange exchange = exchange("default", 0);
            exchange.getAttributes().put(Constants.HTTP_URI, URI.create("http://127.0.0.1:" + upstream.port() + "/slow"));
            ShenyuPluginChain chain = mock(ShenyuPluginChain.class);
            StepVerifier.create(plugin.execute(exchange, chain))
                    .expectErrorSatisfies(error -> {
                        ResponseStatusException status = assertInstanceOf(ResponseStatusException.class, error);
                        assertEquals(HttpStatus.GATEWAY_TIMEOUT, status.getStatusCode());
                        Throwable transportError = status.getCause();
                        if ("webClient".equals(strategy)) {
                            transportError = assertInstanceOf(WebClientRequestException.class, transportError).getCause();
                        }
                        assertInstanceOf(ReadTimeoutException.class, transportError);
                    }).verify(Duration.ofSeconds(10));
            assertEquals(1, requests.get());
            verifyNoInteractions(chain);
        } finally {
            upstream.disposeNow();
        }
    }

    private void assertTimeoutMapping(final Throwable failure, final String spec, final int retries, final int expectedAttempts) {
        AtomicInteger attempts = new AtomicInteger();
        ShenyuPluginChain chain = mock(ShenyuPluginChain.class);
        StepVerifier.withVirtualTime(() -> new FailingPlugin(failure, attempts).execute(exchange(spec, retries), chain))
                .thenAwait(Duration.ofMinutes(1))
                .expectErrorSatisfies(error -> {
                    ResponseStatusException status = assertInstanceOf(ResponseStatusException.class, error);
                    assertEquals(HttpStatus.GATEWAY_TIMEOUT, status.getStatusCode());
                    assertSame(failure, status.getCause());
                    assertNotNull(status.getReason());
                    assertTrue(status.getReason().toLowerCase().contains("timeout") || status.getReason().toLowerCase().contains("timed out"));
                }).verify();
        assertEquals(expectedAttempts, attempts.get());
        verifyNoInteractions(chain);
    }

    private static Stream<Throwable> timeouts() {
        return Stream.of(new TimeoutException("request timeout"), new ConnectTimeoutException("connect timeout"), ReadTimeoutException.INSTANCE,
                WriteTimeoutException.INSTANCE, new SocketTimeoutException("socket timeout"))
                .flatMap(failure -> Stream.of(failure, wrap(failure)));
    }

    private static Stream<Arguments> backoffTimeouts() {
        // The existing backoff policies do not retry write timeouts.
        return Stream.of("fixed", "exponential").flatMap(spec -> timeouts().map(failure -> Arguments.of(spec, failure,
                failure instanceof WriteTimeoutException || failure.getCause() instanceof WriteTimeoutException ? 1 : 2)));
    }

    private static WebClientRequestException wrap(final Throwable failure) {
        return new WebClientRequestException(failure, HttpMethod.GET, URI.create("http://localhost/slow"), HttpHeaders.EMPTY);
    }

    private ServerWebExchange exchange(final String spec, final int retries) {
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/slow"));
        exchange.getAttributes().put(Constants.CONTEXT, new ShenyuContext());
        exchange.getAttributes().put(Constants.HTTP_URI, URI.create("http://localhost/slow"));
        exchange.getAttributes().put(Constants.HTTP_RETRY, retries);
        exchange.getAttributes().put(Constants.HTTP_RETRY_BACK_OFF_SPEC, spec);
        exchange.getAttributes().put(Constants.HTTP_TIME_OUT, 5000L);
        return exchange;
    }

    private static final class FailingPlugin extends AbstractHttpClientPlugin<String> {

        private final Throwable failure;

        private final AtomicInteger attempts;

        private FailingPlugin(final Throwable failure, final AtomicInteger attempts) {
            super(Constants.BYTES_PER_MB);
            this.failure = failure;
            this.attempts = attempts;
        }

        @Override
        protected Mono<String> doRequest(final ServerWebExchange exchange, final String httpMethod, final URI uri, final Flux<DataBuffer> body) {
            return Mono.defer(() -> {
                attempts.incrementAndGet();
                return Mono.error(failure);
            });
        }

        @Override
        public int getOrder() {
            return 0;
        }
    }
}
