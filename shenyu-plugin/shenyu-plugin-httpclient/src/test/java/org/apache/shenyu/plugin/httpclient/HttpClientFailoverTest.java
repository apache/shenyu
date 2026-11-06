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
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.httpclient.exception.ShenyuUpstreamStatusException;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.reactivestreams.Publisher;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.http.client.reactive.ReactorClientHttpConnector;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.netty.Connection;
import reactor.netty.DisposableServer;
import reactor.netty.http.client.HttpClient;
import reactor.netty.http.server.HttpServer;
import reactor.netty.http.server.HttpServerRequest;
import reactor.netty.http.server.HttpServerResponse;
import reactor.netty.resources.ConnectionProvider;

import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Exercises retry routing through the public client plugin entry point and real HTTP responses.
 */
class HttpClientFailoverTest {

    private static final Duration TEST_TIMEOUT = Duration.ofSeconds(10);

    private static final String REQUEST_PATH = "/test?mode=retry";

    @ParameterizedTest
    @CsvSource({
            "netty, SERVER_ERROR, fixed",
            "netty, SERVER_ERROR, exponential",
            "netty, DISCONNECT, fixed",
            "netty, DISCONNECT, exponential",
            "webclient, SERVER_ERROR, fixed",
            "webclient, SERVER_ERROR, exponential",
            "webclient, DISCONNECT, fixed",
            "webclient, DISCONNECT, exponential"
    })
    void failoverReachesStandbyAfterAnUpstreamFailure(final String client, final PrimaryBehavior behavior, final String backoff) {
        try (UpstreamFixture fixture = new UpstreamFixture(client, behavior).start()) {
            fixture.execute(RetryEnum.FAILOVER, backoff, 1).block(TEST_TIMEOUT);

            assertEquals("standby-success", fixture.payload.get());
            assertEquals(Collections.singletonList(REQUEST_PATH), fixture.primaryRequests);
            assertEquals(Collections.singletonList(REQUEST_PATH), fixture.standbyRequests);
            assertEquals(1, fixture.chainCalls.get());
            assertEquals(HttpStatus.OK, fixture.exchange.getResponse().getStatusCode());
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"netty", "webclient"})
    void currentRetriesTheOriginalUpstreamAfterADisconnect(final String client) {
        try (UpstreamFixture fixture = new UpstreamFixture(client, PrimaryBehavior.RECOVER_AFTER_DISCONNECT).start()) {
            fixture.execute(RetryEnum.CURRENT, "exponential", 1).block(TEST_TIMEOUT);

            assertEquals("primary-success", fixture.payload.get());
            assertEquals(List.of(REQUEST_PATH, REQUEST_PATH), fixture.primaryRequests);
            assertEquals(Collections.emptyList(), fixture.standbyRequests);
            assertEquals(1, fixture.chainCalls.get());
            assertEquals(HttpStatus.OK, fixture.exchange.getResponse().getStatusCode());
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"netty", "webclient"})
    void zeroRetriesDoNotContactStandby(final String client) {
        try (UpstreamFixture fixture = new UpstreamFixture(client, PrimaryBehavior.SERVER_ERROR).start()) {
            assertThrows(ShenyuUpstreamStatusException.class,
                    () -> fixture.execute(RetryEnum.FAILOVER, "exponential", 0).block(TEST_TIMEOUT));

            assertEquals(Collections.singletonList(REQUEST_PATH), fixture.primaryRequests);
            assertEquals(Collections.emptyList(), fixture.standbyRequests);
            assertEquals(0, fixture.chainCalls.get());
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"netty", "webclient"})
    void clientErrorsAreForwardedWithoutFailover(final String client) {
        try (UpstreamFixture fixture = new UpstreamFixture(client, PrimaryBehavior.CLIENT_ERROR).start()) {
            fixture.execute(RetryEnum.FAILOVER, "exponential", 1).block(TEST_TIMEOUT);

            assertEquals("primary-client-error", fixture.payload.get());
            assertEquals(Collections.singletonList(REQUEST_PATH), fixture.primaryRequests);
            assertEquals(Collections.emptyList(), fixture.standbyRequests);
            assertEquals(1, fixture.chainCalls.get());
            assertEquals(HttpStatus.BAD_REQUEST, fixture.exchange.getResponse().getStatusCode());
        }
    }

    private enum PrimaryBehavior {

        SERVER_ERROR,

        DISCONNECT,

        RECOVER_AFTER_DISCONNECT,

        CLIENT_ERROR
    }

    private static final class UpstreamFixture implements AutoCloseable {

        private final List<String> primaryRequests = new CopyOnWriteArrayList<>();

        private final List<String> standbyRequests = new CopyOnWriteArrayList<>();

        private final AtomicInteger chainCalls = new AtomicInteger();

        private final AtomicReference<String> payload = new AtomicReference<>();

        private final ConnectionProvider connectionProvider = ConnectionProvider.create("http-client-failover-test", 1);

        private final UpstreamCacheManager upstreamCache = UpstreamCacheManager.getInstance();

        private final PrimaryBehavior primaryBehavior;

        private final boolean netty;

        private final AbstractHttpClientPlugin<?> plugin;

        private DisposableServer primary;

        private DisposableServer standby;

        private String selectorId;

        private MockServerWebExchange exchange;

        private UpstreamFixture(final String client, final PrimaryBehavior primaryBehavior) {
            this.primaryBehavior = primaryBehavior;
            netty = "netty".equals(client);
            // Count only retries selected by ShenYu, not Reactor Netty's implicit retry on disconnect.
            HttpClient transport = HttpClient.create(connectionProvider).disableRetry(true);
            plugin = netty ? new NettyHttpClientPlugin(transport, 1024)
                    : new WebClientPlugin(WebClient.builder().clientConnector(new ReactorClientHttpConnector(transport)).build(), 1024);
        }

        private UpstreamFixture start() {
            try {
                primary = HttpServer.create().host("127.0.0.1").port(0).handle(this::respondPrimary).bindNow(TEST_TIMEOUT);
                standby = HttpServer.create().host("127.0.0.1").port(0).handle((request, response) -> {
                    standbyRequests.add(request.uri());
                    return response.sendString(Flux.just("standby-", "success"));
                }).bindNow(TEST_TIMEOUT);
                selectorId = "http-client-failover-" + primary.port() + "-" + standby.port();
                // Individual health checks are disabled without replacing the shared ShenYu configuration.
                upstreamCache.submit(selectorId, List.of(
                        Upstream.builder().url("127.0.0.1:" + primary.port()).healthCheckEnabled(false).build(),
                        Upstream.builder().url("127.0.0.1:" + standby.port()).healthCheckEnabled(false).build()));
                return this;
            } catch (RuntimeException error) {
                close();
                throw error;
            }
        }

        private Publisher<Void> respondPrimary(final HttpServerRequest request, final HttpServerResponse response) {
            primaryRequests.add(request.uri());
            if (primaryBehavior == PrimaryBehavior.DISCONNECT
                    || primaryBehavior == PrimaryBehavior.RECOVER_AFTER_DISCONNECT && primaryRequests.size() == 1) {
                // The request has arrived before closing, so the observed attempt count is deterministic.
                response.withConnection(Connection::dispose);
                return Mono.empty();
            }
            if (primaryBehavior == PrimaryBehavior.SERVER_ERROR) {
                return response.status(503).sendString(Mono.just("primary-unavailable"));
            }
            if (primaryBehavior == PrimaryBehavior.CLIENT_ERROR) {
                return response.status(400).sendString(Mono.just("primary-client-error"));
            }
            return response.sendString(Flux.just("primary-", "success"));
        }

        private Mono<Void> execute(final RetryEnum retryStrategy, final String backoff, final int retries) {
            exchange = MockServerWebExchange.from(MockServerHttpRequest.get(REQUEST_PATH).build());
            exchange.getAttributes().put(Constants.CONTEXT, new ShenyuContext());
            exchange.getAttributes().put(Constants.HTTP_URI, URI.create("http://127.0.0.1:" + primary.port() + REQUEST_PATH));
            exchange.getAttributes().put(Constants.REWRITE_URI, "/test");
            exchange.getAttributes().put(Constants.RETRY_STRATEGY, retryStrategy.getName());
            exchange.getAttributes().put(Constants.HTTP_RETRY_BACK_OFF_SPEC, backoff);
            exchange.getAttributes().put(Constants.HTTP_RETRY, retries);
            exchange.getAttributes().put(Constants.HTTP_TIME_OUT, 3000L);
            exchange.getAttributes().put(Constants.DIVIDE_SELECTOR_ID, selectorId);
            exchange.getAttributes().put(Constants.LOAD_BALANCE, "roundRobin");
            return plugin.execute(exchange, this::consumeResponse);
        }

        private Mono<Void> consumeResponse(final ServerWebExchange serverExchange) {
            chainCalls.incrementAndGet();
            if (netty) {
                Connection connection = Objects.requireNonNull(serverExchange.getAttribute(Constants.CLIENT_RESPONSE_CONN_ATTR));
                return connection.inbound().receive().aggregate().asString(StandardCharsets.UTF_8).doOnNext(payload::set).then();
            }
            ResponseEntity<Flux<DataBuffer>> response = Objects.requireNonNull(serverExchange.getAttribute(Constants.CLIENT_RESPONSE_ATTR));
            return DataBufferUtils.join(Objects.requireNonNull(response.getBody())).doOnNext(buffer -> {
                try {
                    payload.set(buffer.toString(StandardCharsets.UTF_8));
                } finally {
                    DataBufferUtils.release(buffer);
                }
            }).then();
        }

        @Override
        public void close() {
            try {
                if (Objects.nonNull(selectorId)) {
                    upstreamCache.removeByKey(selectorId);
                }
                connectionProvider.disposeLater().block(TEST_TIMEOUT);
            } finally {
                if (Objects.nonNull(primary)) {
                    primary.disposeNow(TEST_TIMEOUT);
                }
                if (Objects.nonNull(standby)) {
                    standby.disposeNow(TEST_TIMEOUT);
                }
            }
        }
    }
}
