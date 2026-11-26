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

import io.micrometer.core.instrument.FunctionCounter;
import io.micrometer.core.instrument.Metrics;
import io.micrometer.core.instrument.simple.SimpleMeterRegistry;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.base.circuitbreaker.UpstreamCircuitBreaker;
import org.apache.shenyu.plugin.base.circuitbreaker.UpstreamCircuitBreaker.HalfOpenProbe;
import org.junit.jupiter.api.Test;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.test.util.ReflectionTestUtils;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;
import reactor.netty.Connection;
import reactor.netty.DisposableServer;
import reactor.netty.http.client.HttpClient;
import reactor.netty.http.server.HttpServer;
import reactor.netty.internal.shaded.reactor.pool.PoolAcquirePendingLimitException;
import reactor.netty.resources.ConnectionPoolMetrics;
import reactor.netty.resources.ConnectionProvider;

import java.net.URI;
import java.time.Duration;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.BooleanSupplier;

import static java.util.concurrent.TimeUnit.SECONDS;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class HttpPoolCircuitBreakerTest {

    private static final Duration TEST_TIMEOUT = Duration.ofSeconds(10);

    @Test
    void poolRejectionsDoNotOpenBreakerAndRequestsRecoverWhenConnectionReturns() throws Exception {
        final SimpleMeterRegistry registry = new SimpleMeterRegistry();
        Metrics.addRegistry(registry);
        try (PoolFixture fixture = new PoolFixture()) {
            final String metricName = "shenyu.httpclient.pool.rejections";
            final FunctionCounter counter = registry.find(metricName).functionCounter();
            final double previousCount = Objects.isNull(counter) ? 0 : counter.count();
            final CompletableFuture<Void> active = fixture.forward("/hold").toFuture();
            final CompletableFuture<Void> pending = fixture.enqueue("/ok");
            for (int i = 0; i < 3; i++) {
                assertPoolRejection(fixture.forward("/ok"));
            }

            assertTrue(fixture.breaker.isRequestAllowed(fixture.key), "a full local queue must not open the upstream breaker");

            fixture.release.tryEmitEmpty();
            active.get(10, SECONDS);
            pending.get(10, SECONDS);
            fixture.forward("/ok").block(TEST_TIMEOUT);
            assertEquals(previousCount + 3, registry.get(metricName).functionCounter().count());
        } finally {
            Metrics.removeRegistry(registry);
            registry.close();
        }
    }

    @Test
    void upstreamDisconnectsStillOpenBreaker() {
        try (PoolFixture fixture = new PoolFixture()) {
            for (int i = 0; i < 3; i++) {
                assertThrows(RuntimeException.class, () -> fixture.forward("/disconnect").block(TEST_TIMEOUT));
            }

            await(() -> fixture.breaker.isBlocking(fixture.key));
        }
    }

    @Test
    void rejectedHalfOpenProbeReleasesItsPermit() throws Exception {
        try (PoolFixture fixture = new PoolFixture()) {
            final CompletableFuture<Void> active = fixture.send("/hold", false).toFuture();
            final CompletableFuture<Void> pending = fixture.enqueue("/ok");
            for (int i = 0; i < 3; i++) {
                fixture.breaker.recordFailure(fixture.key);
            }
            fixture.expireOpenWindow();
            HalfOpenProbe rejected = fixture.breaker.acquireHalfOpenProbe(fixture.key);
            assertNotNull(rejected);
            assertNull(fixture.breaker.acquireHalfOpenProbe(fixture.key));

            assertPoolRejection(rejected.recordOutcome(fixture.send("/ok", true)));
            fixture.release.tryEmitEmpty();
            active.get(10, SECONDS);
            pending.get(10, SECONDS);

            // Once the retry window expires a new probe can run, and only a real success closes the breaker.
            assertTrue(fixture.breaker.isBlocking(fixture.key));
            fixture.expireOpenWindow();
            HalfOpenProbe recovered = fixture.breaker.acquireHalfOpenProbe(fixture.key);
            assertNotNull(recovered);
            recovered.recordOutcome(fixture.send("/ok", true)).block(TEST_TIMEOUT);
            await(() -> fixture.breaker.isRequestAllowed(fixture.key));
        }
    }

    private static void assertPoolRejection(final Mono<Void> attempt) {
        RuntimeException error = assertThrows(RuntimeException.class, () -> attempt.block(TEST_TIMEOUT));
        assertTrue(ExceptionUtils.indexOfType(error, PoolAcquirePendingLimitException.class) >= 0, () -> "Expected a real pool rejection: " + error);
    }

    private static void await(final BooleanSupplier completed) {
        Mono.defer(() -> completed.getAsBoolean() ? Mono.just(Boolean.TRUE) : Mono.empty())
                .repeatWhen(complete -> complete.delayElements(Duration.ofMillis(5)))
                .next().block(TEST_TIMEOUT);
    }

    private static final class PoolFixture implements AutoCloseable {

        private final AtomicReference<ConnectionPoolMetrics> metrics = new AtomicReference<>();

        private final Sinks.One<Void> arrived = Sinks.one();

        private final Sinks.One<Void> release = Sinks.one();

        private final UpstreamCircuitBreaker breaker = UpstreamCircuitBreaker.getInstance();

        private final ConnectionProvider pool = ConnectionProvider.builder("small-http-pool")
                .maxConnections(1).pendingAcquireMaxCount(1).pendingAcquireTimeout(TEST_TIMEOUT)
                .metrics(true, () -> (name, id, address, measured) -> metrics.set(measured)).build();

        private final NettyHttpClientPlugin plugin = new NettyHttpClientPlugin(HttpClient.create(pool).disableRetry(true), 1024);

        private final DisposableServer server = HttpServer.create().host("127.0.0.1").port(0)
                .handle((request, response) -> {
                    if ("/disconnect".equals(request.uri())) {
                        response.withConnection(Connection::dispose);
                        return Mono.empty();
                    }
                    if ("/hold".equals(request.uri())) {
                        arrived.tryEmitEmpty();
                        return response.sendString(release.asMono().thenReturn("healthy"));
                    }
                    return response.sendString(Mono.just("healthy"));
                }).bindNow(TEST_TIMEOUT);

        private final String baseUrl = "http://127.0.0.1:" + server.port();

        private final String key = "small-http-pool:" + baseUrl;

        private Mono<Void> forward(final String path) {
            return UpstreamCircuitBreaker.recordOutcome(send(path, false), key);
        }

        private Mono<Void> send(final String path, final boolean probe) {
            MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get(path).build());
            exchange.getAttributes().put(Constants.CONTEXT, new ShenyuContext());
            exchange.getAttributes().put(Constants.HTTP_URI, URI.create(baseUrl + path));
            exchange.getAttributes().put(Constants.HTTP_TIME_OUT, 8000L);
            exchange.getAttributes().put(UpstreamCircuitBreaker.HALF_OPEN_PROBE, probe);
            return plugin.execute(exchange, response -> {
                Connection connection = Objects.requireNonNull(response.getAttribute(Constants.CLIENT_RESPONSE_CONN_ATTR));
                return connection.inbound().receive().then();
            });
        }

        private CompletableFuture<Void> enqueue(final String path) {
            arrived.asMono().block(TEST_TIMEOUT);
            CompletableFuture<Void> pending = send(path, false).toFuture();
            await(() -> Objects.nonNull(metrics.get()) && metrics.get().pendingAcquireSize() == 1);
            return pending;
        }

        private void expireOpenWindow() {
            Map<?, ?> states = (Map<?, ?>) ReflectionTestUtils.getField(breaker, "breakers");
            Object state = Objects.requireNonNull(states).get(key);
            ReflectionTestUtils.setField(Objects.requireNonNull(state), "openedAtMillis",
                    System.currentTimeMillis() - UpstreamCircuitBreaker.DEFAULT_HALF_OPEN_WAIT_MILLIS);
        }

        @Override
        public void close() {
            release.tryEmitEmpty();
            try {
                pool.disposeLater().block(TEST_TIMEOUT);
            } finally {
                server.disposeNow(TEST_TIMEOUT);
                breaker.reset(key);
            }
        }
    }
}
