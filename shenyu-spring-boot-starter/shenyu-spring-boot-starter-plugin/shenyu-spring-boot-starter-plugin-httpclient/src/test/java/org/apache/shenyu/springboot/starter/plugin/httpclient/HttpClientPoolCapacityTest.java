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

package org.apache.shenyu.springboot.starter.plugin.httpclient;

import io.micrometer.core.instrument.Metrics;
import io.micrometer.core.instrument.Timer;
import io.micrometer.core.instrument.simple.SimpleMeterRegistry;
import org.apache.shenyu.plugin.httpclient.config.HttpClientProperties;
import org.apache.shenyu.plugin.httpclient.config.HttpClientProperties.Pool.PoolType;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.boot.autoconfigure.web.ServerProperties;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;
import reactor.netty.DisposableServer;
import reactor.netty.http.HttpProtocol;
import io.netty.handler.timeout.ReadTimeoutException;
import reactor.netty.http.client.HttpClient;
import reactor.netty.http.server.HttpServer;

import java.time.Duration;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BooleanSupplier;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class HttpClientPoolCapacityTest {

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void providerRemainsUsableWithHttp2Multiplexing(final boolean metrics) {
        DisposableServer server = HttpServer.create().host("127.0.0.1").port(0).protocol(HttpProtocol.H2C)
                .handle((request, response) -> response.sendString(Mono.just("ok"))).bindNow();
        HttpClientProperties properties = properties(PoolType.FIXED, 1000L);
        properties.getPool().setMetrics(metrics);
        HttpClient client = new HttpClientFactory(properties, null, new ServerProperties()).createInstance().protocol(HttpProtocol.H2C);
        try {
            assertEquals(10, Flux.range(0, 10).flatMap(index -> request(client, server, "/"), 4)
                    .filter("ok"::equals).count().block(Duration.ofSeconds(5)));
        } finally {
            client.configuration().connectionProvider().disposeLater().block(Duration.ofSeconds(5));
            server.disposeNow();
        }
    }

    @ParameterizedTest
    @EnumSource(value = PoolType.class, names = {"FIXED", "ELASTIC"})
    void zeroAcquisitionTimeoutRetainsExplicitDisabledDeadline(final PoolType type) throws Exception {
        Sinks.One<Void> release = Sinks.one();
        CountDownLatch entered = new CountDownLatch(1);
        DisposableServer server = HttpServer.create().host("127.0.0.1").port(0).handle((request, response) -> {
            entered.countDown();
            return release.asMono().then(response.sendString(Mono.just("ok")).then());
        }).bindNow();
        HttpClient client = client(type, 0L);
        CompletableFuture<String> active = request(client, server, "/").toFuture();
        CompletableFuture<String> pending = null;
        try {
            assertTrue(entered.await(5, TimeUnit.SECONDS));
            pending = request(client, server, "/").toFuture();
            Thread.sleep(150);
            assertFalse(pending.isDone());
            release.tryEmitEmpty();
            assertEquals("ok", active.get(5, TimeUnit.SECONDS));
            assertEquals("ok", pending.get(5, TimeUnit.SECONDS));
        } finally {
            active.cancel(true);
            if (Objects.nonNull(pending)) {
                pending.cancel(true);
            }
            client.configuration().connectionProvider().disposeLater().block(Duration.ofSeconds(5));
            server.disposeNow();
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void cancellationReleasesQueueCapacityIndependentlyOfMetrics(final boolean metrics) throws Exception {
        Sinks.One<Void> release = Sinks.one();
        CountDownLatch entered = new CountDownLatch(1);
        DisposableServer server = HttpServer.create().host("127.0.0.1").port(0).handle((request, response) -> {
            entered.countDown();
            return release.asMono().then(response.sendString(Mono.just("ok")).then());
        }).bindNow();
        HttpClientProperties properties = properties(PoolType.FIXED, 3000L);
        properties.getPool().setMetrics(metrics);
        HttpClient client = new HttpClientFactory(properties, null, new ServerProperties()).createInstance();
        CompletableFuture<String> active = request(client, server, "/").toFuture();
        CompletableFuture<String> pending = null;
        try {
            assertTrue(entered.await(5, TimeUnit.SECONDS));
            pending = request(client, server, "/").toFuture();
            Throwable failure = assertThrows(Exception.class, () -> request(client, server, "/").block(Duration.ofSeconds(1)));
            assertTrue(hasCause(failure, "PoolAcquirePendingLimitException"));
            pending.cancel(true);
            final CompletableFuture<String> replacement = request(client, server, "/").toFuture();
            release.tryEmitEmpty();
            assertEquals("ok", active.get(1, TimeUnit.SECONDS));
            assertEquals("ok", replacement.get(1, TimeUnit.SECONDS));
        } finally {
            active.cancel(true);
            if (Objects.nonNull(pending)) {
                pending.cancel(true);
            }
            client.configuration().connectionProvider().disposeLater().block(Duration.ofSeconds(5));
            server.disposeNow();
        }
    }

    @ParameterizedTest
    @EnumSource(value = PoolType.class, names = {"FIXED", "ELASTIC"})
    void rejectsOverflowAndReleasesCancelledQueueSlots(final PoolType type) throws Exception {
        Sinks.One<Void> release = Sinks.one();
        CountDownLatch entered = new CountDownLatch(1);
        DisposableServer server = HttpServer.create().host("127.0.0.1").port(0).handle((request, response) -> {
            entered.countDown();
            return release.asMono().then(response.sendString(Mono.just("ok")).then());
        }).bindNow();
        SimpleMeterRegistry registry = new SimpleMeterRegistry();
        Metrics.addRegistry(registry);
        HttpClient client = client(type, 2000L);
        CompletableFuture<String> active = request(client, server, "/").toFuture();
        CompletableFuture<String> pending = null;
        try {
            assertTrue(entered.await(5, TimeUnit.SECONDS));
            pending = request(client, server, "/").toFuture();
            await(() -> pending(registry) == 1);
            Throwable failure = assertThrows(Exception.class, () -> request(client, server, "/").block(Duration.ofSeconds(5)));
            assertTrue(hasCause(failure, "PoolAcquirePendingLimitException"), failure.toString());
            assertEquals(1, pending(registry));
            assertEquals(1, timer(registry, "error").count());
            long cancelledAt = System.nanoTime();
            assertTrue(pending.cancel(true));
            await(() -> pending(registry) == 0 && timer(registry, "cancelled").count() == 1);
            assertTrue(System.nanoTime() - cancelledAt < TimeUnit.SECONDS.toNanos(1));
            final CompletableFuture<String> replacement = request(client, server, "/").toFuture();
            await(() -> pending(registry) == 1);
            release.tryEmitEmpty();
            assertEquals("ok", active.get(5, TimeUnit.SECONDS));
            assertEquals("ok", replacement.get(5, TimeUnit.SECONDS));
            assertEquals(2, timer(registry, "success").count());
            assertEquals(0, timer(registry, "timeout").count());
            registry.find("shenyu.httpclient.pool.acquire").timers().forEach(timer ->
                    assertEquals(2, timer.getId().getTags().size()));
            await(() -> pending(registry) == 0);
            assertEquals(1, registry.get("reactor.netty.connection.provider.max.connections").gauge().value());
            assertEquals(1, registry.get("reactor.netty.connection.provider.max.pending.connections").gauge().value());
        } finally {
            active.cancel(true);
            if (Objects.nonNull(pending)) {
                pending.cancel(true);
            }
            client.configuration().connectionProvider().disposeLater().block(Duration.ofSeconds(5));
            server.disposeNow();
            Metrics.removeRegistry(registry);
            registry.close();
        }
    }

    @ParameterizedTest
    @EnumSource(value = PoolType.class, names = {"FIXED", "ELASTIC"})
    void acquireTimeoutAndActiveCancellationRecoverCapacity(final PoolType type) throws Exception {
        CountDownLatch entered = new CountDownLatch(1);
        DisposableServer server = HttpServer.create().host("127.0.0.1").port(0).handle((request, response) -> {
            if (request.uri().equals("/hold")) {
                entered.countDown();
                return Mono.never();
            }
            return response.sendString(Mono.just("ok"));
        }).bindNow();
        SimpleMeterRegistry registry = new SimpleMeterRegistry();
        Metrics.addRegistry(registry);
        HttpClient client = client(type, 100L);
        CompletableFuture<String> active = request(client, server, "/hold").toFuture();
        try {
            assertTrue(entered.await(5, TimeUnit.SECONDS));
            Throwable failure = assertThrows(Exception.class, () -> request(client, server, "/").block(Duration.ofSeconds(5)));
            assertTrue(hasCause(failure, "PoolAcquireTimeoutException"), failure.toString());
            assertEquals(1, timer(registry, "timeout").count());
            active.cancel(true);
            assertEquals("ok", request(client, server, "/").block(Duration.ofSeconds(5)));
        } finally {
            active.cancel(true);
            client.configuration().connectionProvider().disposeLater().block(Duration.ofSeconds(5));
            server.disposeNow();
            Metrics.removeRegistry(registry);
            registry.close();
        }
    }

    @ParameterizedTest
    @EnumSource(value = PoolType.class, names = {"FIXED", "ELASTIC"})
    void responseTimeoutReleasesCapacityWithoutCountingAnAcquireTimeout(final PoolType type) {
        DisposableServer server = HttpServer.create().host("127.0.0.1").port(0).handle((request, response) ->
                request.uri().equals("/hold") ? Mono.never() : response.sendString(Mono.just("ok"))).bindNow();
        SimpleMeterRegistry registry = new SimpleMeterRegistry();
        Metrics.addRegistry(registry);
        HttpClientProperties properties = properties(type, 1000L);
        properties.setResponseTimeout(100L);
        HttpClient client = new HttpClientFactory(properties, null, new ServerProperties()).createInstance();
        try {
            assertThrows(ReadTimeoutException.class, () -> request(client, server, "/hold").block(Duration.ofSeconds(5)));
            assertEquals("ok", request(client, server, "/").block(Duration.ofSeconds(5)));
            assertEquals(0, timer(registry, "timeout").count());
            assertEquals(2, timer(registry, "success").count());
        } finally {
            client.configuration().connectionProvider().disposeLater().block(Duration.ofSeconds(5));
            server.disposeNow();
            Metrics.removeRegistry(registry);
            registry.close();
        }
    }

    @ParameterizedTest
    @EnumSource(value = PoolType.class, names = {"FIXED", "ELASTIC"})
    void streamingCancellationReleasesTheOccupiedConnection(final PoolType type) throws Exception {
        CountDownLatch firstContent = new CountDownLatch(1);
        DisposableServer server = HttpServer.create().host("127.0.0.1").port(0).handle((request, response) -> {
            if (request.uri().equals("/stream")) {
                return response.sendString(Flux.just("data: first\n\n").concatWith(Mono.never()));
            }
            return response.sendString(Mono.just("ok"));
        }).bindNow();
        HttpClient client = client(type, 100L);
        CompletableFuture<Void> stream = client.get().uri(url(server, "/stream")).responseContent()
                .doOnNext(ignored -> firstContent.countDown()).then().toFuture();
        try {
            assertTrue(firstContent.await(5, TimeUnit.SECONDS));
            Throwable failure = assertThrows(Exception.class, () -> request(client, server, "/").block(Duration.ofSeconds(5)));
            assertTrue(hasCause(failure, "PoolAcquireTimeoutException"), failure.toString());
            stream.cancel(true);
            assertEquals("ok", request(client, server, "/").block(Duration.ofSeconds(5)));
        } finally {
            stream.cancel(true);
            client.configuration().connectionProvider().disposeLater().block(Duration.ofSeconds(5));
            server.disposeNow();
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void keepAliveStillControlsReuseWithoutChangingItsDefault(final boolean keepAlive) {
        AtomicInteger connections = new AtomicInteger();
        DisposableServer server = HttpServer.create().host("127.0.0.1").port(0)
                .doOnChannelInit((observer, channel, address) -> connections.incrementAndGet())
                .handle((request, response) -> response.sendString(Mono.just("ok"))).bindNow();
        HttpClientProperties properties = properties(PoolType.FIXED, 1000L);
        assertFalse(new HttpClientProperties().isKeepAlive());
        properties.setKeepAlive(keepAlive);
        HttpClient client = new HttpClientFactory(properties, null, new ServerProperties()).createInstance();
        try {
            for (int index = 0; index < 10; index++) {
                assertEquals("ok", request(client, server, "/").block(Duration.ofSeconds(5)));
            }
            assertEquals(keepAlive ? 1 : 10, connections.get());
        } finally {
            client.configuration().connectionProvider().disposeLater().block(Duration.ofSeconds(5));
            server.disposeNow();
        }
    }

    private HttpClient client(final PoolType type, final long acquireTimeout) {
        return new HttpClientFactory(properties(type, acquireTimeout), null, new ServerProperties()).createInstance();
    }

    private HttpClientProperties properties(final PoolType type, final long acquireTimeout) {
        HttpClientProperties properties = new HttpClientProperties();
        properties.setKeepAlive(true);
        properties.setResponseTimeout(0L);
        properties.setReadTimeout(0);
        properties.getPool().setType(type);
        properties.getPool().setMaxConnections(1);
        properties.getPool().setPendingAcquireMaxCount(1);
        properties.getPool().setAcquireTimeout(acquireTimeout);
        properties.getPool().setMetrics(true);
        return properties;
    }

    private Mono<String> request(final HttpClient client, final DisposableServer server, final String path) {
        return client.get().uri(url(server, path)).responseSingle((response, body) -> body.asString());
    }

    private String url(final DisposableServer server, final String path) {
        return "http://127.0.0.1:" + server.port() + path;
    }

    private double pending(final SimpleMeterRegistry registry) {
        return registry.get("reactor.netty.connection.provider.pending.connections").gauge().value();
    }

    private Timer timer(final SimpleMeterRegistry registry, final String outcome) {
        return registry.get("shenyu.httpclient.pool.acquire").tag("name", "shenyu-proxy").tag("outcome", outcome).timer();
    }

    private boolean hasCause(final Throwable error, final String simpleName) {
        for (Throwable cause = error; Objects.nonNull(cause); cause = cause.getCause()) {
            if (cause.getClass().getSimpleName().equals(simpleName)) {
                return true;
            }
        }
        return false;
    }

    private void await(final BooleanSupplier condition) throws InterruptedException {
        long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5);
        while (!condition.getAsBoolean() && System.nanoTime() < deadline) {
            Thread.sleep(10);
        }
        assertTrue(condition.getAsBoolean());
    }
}
