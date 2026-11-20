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
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.httpclient.config.DefaultUpstreamPoolGroupRegistry;
import org.apache.shenyu.plugin.httpclient.config.UpstreamPoolGroupRegistry;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.reactivestreams.Publisher;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.HttpStatus;
import org.springframework.http.client.reactive.ReactorClientHttpConnector;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.server.ResponseStatusException;
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
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

/**
 * Loopback tests for named upstream connection-pool groups (issue #8194): two groups
 * against the same backend origin, where the bounded bulk group saturates while the
 * larger interactive group is unaffected on the very same origin, failover stays inside
 * the selected group, and an unknown group reference visibly fails.
 */
class UpstreamPoolGroupIsolationTest {

    private static final Duration TEST_TIMEOUT = Duration.ofSeconds(10);

    @ParameterizedTest
    @ValueSource(strings = {"netty", "webclient"})
    void saturatedBulkGroupDoesNotStarveTheInteractiveGroup(final String client) throws Exception {
        try (PoolGroupFixture fixture = new PoolGroupFixture(client).start()) {
            // one in-flight request holds the bulk group's single connection
            java.util.concurrent.CountDownLatch held = new java.util.concurrent.CountDownLatch(1);
            AtomicReference<String> heldPayload = new AtomicReference<>();
            fixture.execute("/slow", "bulk")
                    .doOnNext(heldPayload::set)
                    .doFinally(signal -> held.countDown()).subscribe();
            assertTrue(fixture.awaitPrimaryReceived(), "the slow request must reach the upstream before the second bulk request");

            // a second bulk request must fail on the bounded acquisition queue
            Mono<String> rejected = fixture.execute("/fast", "bulk");
            try {
                rejected.block(TEST_TIMEOUT);
                fail("the saturated bulk group must not serve a second concurrent request");
            } catch (RuntimeException expected) {
                assertTrue(String.valueOf(expected.getMessage()).contains("pending"),
                        "expected an acquisition timeout, got: " + expected);
            }

            // the interactive group on the same origin is unaffected
            assertEquals("slow-ok", fixture.execute("/slow", "interactive").block(TEST_TIMEOUT));
            assertEquals("fast-ok", fixture.execute("/fast", "interactive").block(TEST_TIMEOUT));

            // the held request still completes normally
            assertTrue(held.await(5, java.util.concurrent.TimeUnit.SECONDS), "the held request must complete");
            assertEquals("slow-ok", heldPayload.get());
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"netty", "webclient"})
    void unknownGroupReferenceFailsVisibly(final String client) {
        try (PoolGroupFixture fixture = new PoolGroupFixture(client).start()) {
            try {
                Mono<String> mono = fixture.execute("/fast", "ghost");
                mono.block(TEST_TIMEOUT);
                fail("an unknown pool group must visibly fail, not fall back to the default pool");
            } catch (ResponseStatusException expected) {
                assertEquals(HttpStatus.SERVICE_UNAVAILABLE, expected.getStatusCode());
                assertTrue(String.valueOf(expected.getReason()).contains("ghost"));
            }
            // the default path is untouched: no group attribute selects the shared client
            assertEquals("fast-ok", fixture.execute("/fast", null).block(TEST_TIMEOUT));
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"netty", "webclient"})
    void failoverStaysInsideTheSelectedGroup(final String client) {
        try (PoolGroupFixture fixture = new PoolGroupFixture(client).start()) {
            fixture.executeFailover();
            assertEquals("standby-ok", fixture.lastPayload());
            assertEquals(List.of("bulk", "bulk"), fixture.resolvedGroups(),
                    "the initial attempt and the failover resend must both resolve the selected group");
        }
    }

    /**
     * Recording facade that records resolved group names while delegating to the real registry.
     */
    private static final class RecordingRegistry implements UpstreamPoolGroupRegistry {

        private final UpstreamPoolGroupRegistry delegate;

        private final List<String> resolvedGroups = new CopyOnWriteArrayList<>();

        RecordingRegistry(final UpstreamPoolGroupRegistry delegate) {
            this.delegate = delegate;
        }

        List<String> resolvedGroups() {
            return List.copyOf(resolvedGroups);
        }

        @Override
        public boolean contains(final String name) {
            return delegate.contains(name);
        }

        @Override
        public HttpClient httpClientFor(final String name) {
            resolvedGroups.add(name);
            return delegate.httpClientFor(name);
        }

        @Override
        public WebClient webClientFor(final String name) {
            resolvedGroups.add(name);
            return delegate.webClientFor(name);
        }

        @Override
        public void close() {
            delegate.close();
        }
    }

    /**
     * One backend origin with a failing primary and a healthy standby, two bounded groups
     * and the shared default client.
     */
    private static final class PoolGroupFixture implements AutoCloseable {

        private final boolean netty;

        private final ConnectionProvider defaultProvider = ConnectionProvider.create("pg-test-default", 4);

        private final ConnectionProvider bulkProvider = ConnectionProvider.builder("pg-test-bulk")
                .maxConnections(1).pendingAcquireTimeout(Duration.ofMillis(300)).build();

        private final ConnectionProvider interactiveProvider = ConnectionProvider.builder("pg-test-interactive")
                .maxConnections(8).pendingAcquireTimeout(Duration.ofMillis(3000)).build();

        private final AtomicReference<String> payload = new AtomicReference<>();

        private final List<String> primaryRequests = new CopyOnWriteArrayList<>();

        private final List<String> standbyRequests = new CopyOnWriteArrayList<>();

        private final java.util.concurrent.CountDownLatch primaryReceived = new java.util.concurrent.CountDownLatch(1);

        private final UpstreamCacheManager upstreamCache = UpstreamCacheManager.getInstance();

        private DisposableServer primary;

        private DisposableServer standby;

        private AbstractHttpClientPlugin<?> plugin;

        private RecordingRegistry registry;

        private String selectorId;

        PoolGroupFixture(final String client) {
            this.netty = "netty".equals(client);
        }

        PoolGroupFixture start() {
            try {
                primary = HttpServer.create().host("127.0.0.1").port(0)
                        .handle(this::respondPrimary).bindNow(TEST_TIMEOUT);
                standby = HttpServer.create().host("127.0.0.1").port(0)
                        .handle(this::respondStandby).bindNow(TEST_TIMEOUT);
                selectorId = "pg-test-" + primary.port() + "-" + standby.port();
                upstreamCache.submit(selectorId, List.of(
                        Upstream.builder().url("127.0.0.1:" + primary.port()).healthCheckEnabled(false).build(),
                        Upstream.builder().url("127.0.0.1:" + standby.port()).healthCheckEnabled(false).build()));
                rebuildRegistry();
                return this;
            } catch (RuntimeException error) {
                close();
                throw error;
            }
        }

        private Publisher<Void> respondPrimary(final HttpServerRequest request, final HttpServerResponse response) {
            primaryRequests.add(request.uri());
            primaryReceived.countDown();
            if (request.uri().startsWith("/failover")) {
                return response.status(503).sendString(Mono.just("primary-unavailable"));
            }
            if (request.uri().startsWith("/slow")) {
                return response.sendString(Mono.delay(Duration.ofMillis(1500)).map(ignore -> "slow-ok"));
            }
            return response.sendString(Mono.just("fast-ok"));
        }

        private Publisher<Void> respondStandby(final HttpServerRequest request, final HttpServerResponse response) {
            standbyRequests.add(request.uri());
            return response.sendString(Mono.just("standby-ok"));
        }

        void rebuildRegistry() {
            final HttpClient bulk = HttpClient.create(bulkProvider).disableRetry(true);
            final HttpClient interactive = HttpClient.create(interactiveProvider).disableRetry(true);
            Map<String, HttpClient> clients = new LinkedHashMap<>();
            clients.put("bulk", bulk);
            clients.put("interactive", interactive);
            Map<String, WebClient> webClients = new LinkedHashMap<>();
            if (!netty) {
                webClients.put("bulk", WebClient.builder()
                        .clientConnector(new ReactorClientHttpConnector(bulk)).build());
                webClients.put("interactive", WebClient.builder()
                        .clientConnector(new ReactorClientHttpConnector(interactive)).build());
            }
            registry = new RecordingRegistry(new DefaultUpstreamPoolGroupRegistry(clients, webClients, List.of(bulkProvider)));
            final HttpClient defaultClient = HttpClient.create(defaultProvider).disableRetry(true);
            plugin = netty ? new NettyHttpClientPlugin(defaultClient, 1024, registry)
                    : new WebClientPlugin(WebClient.builder()
                            .clientConnector(new ReactorClientHttpConnector(defaultClient)).build(), 1024, registry);
        }

        Mono<String> execute(final String path, final String group) {
            MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get(path).build());
            exchange.getAttributes().put(Constants.CONTEXT, new ShenyuContext());
            exchange.getAttributes().put(Constants.HTTP_URI, URI.create("http://127.0.0.1:" + primary.port() + path));
            exchange.getAttributes().put(Constants.REWRITE_URI, path);
            exchange.getAttributes().put(Constants.HTTP_TIME_OUT, 5000L);
            exchange.getAttributes().put(Constants.HTTP_RETRY, 0);
            if (Objects.nonNull(group)) {
                exchange.getAttributes().put(Constants.HTTP_POOL_GROUP, group);
            }
            AtomicReference<String> reference = new AtomicReference<>();
            return plugin.execute(exchange, serverExchange -> consumeResponse(serverExchange, reference))
                    .then(Mono.fromSupplier(reference::get));
        }

        void executeFailover() {
            MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/failover").build());
            exchange.getAttributes().put(Constants.CONTEXT, new ShenyuContext());
            exchange.getAttributes().put(Constants.HTTP_URI, URI.create("http://127.0.0.1:" + primary.port() + "/failover"));
            exchange.getAttributes().put(Constants.REWRITE_URI, "/failover");
            exchange.getAttributes().put(Constants.HTTP_TIME_OUT, 5000L);
            exchange.getAttributes().put(Constants.HTTP_RETRY, 1);
            exchange.getAttributes().put(Constants.RETRY_STRATEGY, "failover");
            exchange.getAttributes().put(Constants.DIVIDE_SELECTOR_ID, selectorId);
            exchange.getAttributes().put(Constants.LOAD_BALANCE, "roundRobin");
            exchange.getAttributes().put(Constants.HTTP_POOL_GROUP, "bulk");
            AtomicReference<String> reference = new AtomicReference<>();
            plugin.execute(exchange, serverExchange -> consumeResponse(serverExchange, reference)).block(TEST_TIMEOUT);
            payload.set(reference.get());
        }

        private Mono<Void> consumeResponse(final ServerWebExchange serverExchange, final AtomicReference<String> reference) {
            if (netty) {
                Connection connection = Objects.requireNonNull(serverExchange.getAttribute(Constants.CLIENT_RESPONSE_CONN_ATTR));
                return connection.inbound().receive().aggregate().asString(StandardCharsets.UTF_8).doOnNext(reference::set).then();
            }
            @SuppressWarnings("unchecked")
            org.springframework.http.ResponseEntity<Flux<DataBuffer>> response =
                    Objects.requireNonNull(serverExchange.getAttribute(Constants.CLIENT_RESPONSE_ATTR));
            return DataBufferUtils.join(Objects.requireNonNull(response.getBody())).doOnNext(buffer -> {
                try {
                    reference.set(buffer.toString(StandardCharsets.UTF_8));
                } finally {
                    DataBufferUtils.release(buffer);
                }
            }).then();
        }

        String lastPayload() {
            return payload.get();
        }

        List<String> resolvedGroups() {
            return registry.resolvedGroups();
        }

        boolean awaitPrimaryReceived() {
            try {
                return primaryReceived.await(5, java.util.concurrent.TimeUnit.SECONDS);
            } catch (InterruptedException ex) {
                Thread.currentThread().interrupt();
                return false;
            }
        }

        @Override
        public void close() {
            try {
                if (Objects.nonNull(registry)) {
                    registry.close();
                }
                interactiveProvider.disposeLater().block(TEST_TIMEOUT);
                defaultProvider.disposeLater().block(TEST_TIMEOUT);
                upstreamCache.removeByKey(selectorId);
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
