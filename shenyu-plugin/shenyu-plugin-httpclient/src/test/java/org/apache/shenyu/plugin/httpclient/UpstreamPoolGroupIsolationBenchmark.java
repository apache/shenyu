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

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledIfSystemProperty;
import reactor.core.publisher.Mono;
import reactor.netty.DisposableServer;
import reactor.netty.http.client.HttpClient;
import reactor.netty.http.server.HttpServer;
import reactor.netty.resources.ConnectionProvider;

import java.time.Duration;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Opt-in fixed-workload isolation measurement for the connection-pool capacity behind
 * the named upstream pool groups. It compares one shared provider with two independent
 * providers against the same loopback origin. This measures the provider isolation, not
 * gateway routing overhead; it is intentionally not part of the default build.
 *
 * <p>Run with
 * {@code -Dtest=UpstreamPoolGroupIsolationBenchmark -Dshenyu.poolgroup.benchmark=true}.</p>
 */
final class UpstreamPoolGroupIsolationBenchmark {

    private static final int BULK_WORKERS = 8;

    private static final int INTERACTIVE_WORKERS = 2;

    private static final Duration WORKLOAD = Duration.ofSeconds(5);

    private static final Duration UPSTREAM_DELAY = Duration.ofMillis(50);

    @Test
    @EnabledIfSystemProperty(named = "shenyu.poolgroup.benchmark", matches = "true")
    void compareSharedPoolAndGroupPools() {
        DisposableServer server = HttpServer.create().host("127.0.0.1").port(0)
                .handle((request, response) -> request.uri().startsWith("/slow")
                        ? response.sendString(Mono.delay(UPSTREAM_DELAY).map(ignore -> "slow"))
                        : response.sendString(Mono.just("fast")))
                .bindNow();
        try {
            Result shared = run(server.port(), 4, 4, 4, false);
            Result groups = run(server.port(), 4, 4, 8, true);
            System.out.printf("scenario,bulk_ok,bulk_fail,interactive_ok,interactive_fail,interactive_p50_ms,interactive_p99_ms%n");
            print("shared", shared);
            print("groups", groups);
        } finally {
            server.disposeNow(Duration.ofSeconds(5));
        }
    }

    private static Result run(final int port, final int bulkConnections, final int bulkPending,
                              final int interactiveConnections, final boolean separateProviders) {
        ConnectionProvider bulkProvider = ConnectionProvider.builder("bench-bulk-" + System.nanoTime())
                .maxConnections(bulkConnections)
                .pendingAcquireMaxCount(bulkPending)
                .pendingAcquireTimeout(Duration.ofSeconds(1))
                .build();
        ConnectionProvider interactiveProvider = separateProviders
                ? ConnectionProvider.builder("bench-interactive-" + System.nanoTime())
                        .maxConnections(interactiveConnections)
                        .pendingAcquireTimeout(Duration.ofSeconds(1))
                        .build()
                : bulkProvider;
        HttpClient bulk = HttpClient.create(bulkProvider).disableRetry(true);
        HttpClient interactive = separateProviders ? HttpClient.create(interactiveProvider).disableRetry(true) : bulk;
        ExecutorService executor = Executors.newFixedThreadPool(BULK_WORKERS + INTERACTIVE_WORKERS);
        CountDownLatch start = new CountDownLatch(1);
        long deadline = System.nanoTime() + WORKLOAD.toNanos();
        AtomicLong bulkOk = new AtomicLong();
        AtomicLong bulkFail = new AtomicLong();
        AtomicLong interactiveFail = new AtomicLong();
        List<Long> interactiveLatency = Collections.synchronizedList(new ArrayList<>());
        List<Future<?>> futures = new ArrayList<>();
        try {
            for (int i = 0; i < BULK_WORKERS; i++) {
                futures.add(executor.submit(() -> runBulk(bulk, port, start, deadline, bulkOk, bulkFail)));
            }
            for (int i = 0; i < INTERACTIVE_WORKERS; i++) {
                futures.add(executor.submit(() -> runInteractive(interactive, port, start, deadline, interactiveLatency, interactiveFail)));
            }
            start.countDown();
            for (Future<?> future : futures) {
                future.get(30, TimeUnit.SECONDS);
            }
        } catch (Exception ex) {
            throw new IllegalStateException("benchmark worker failed", ex);
        } finally {
            executor.shutdownNow();
            bulkProvider.disposeLater().block(Duration.ofSeconds(5));
            if (separateProviders) {
                interactiveProvider.disposeLater().block(Duration.ofSeconds(5));
            }
        }
        return new Result(bulkOk.get(), bulkFail.get(), interactiveLatency.size(), interactiveFail.get(),
                percentile(interactiveLatency, 50), percentile(interactiveLatency, 99));
    }

    private static void runBulk(final HttpClient client, final int port, final CountDownLatch start,
                                final long deadline, final AtomicLong ok, final AtomicLong fail) {
        if (!awaitStart(start)) {
            return;
        }
        while (System.nanoTime() < deadline) {
            try {
                client.get().uri("http://127.0.0.1:" + port + "/slow")
                        .responseContent().aggregate().asString().block(Duration.ofSeconds(5));
                ok.incrementAndGet();
            } catch (RuntimeException ex) {
                fail.incrementAndGet();
            }
        }
    }

    private static void runInteractive(final HttpClient client, final int port, final CountDownLatch start,
                                       final long deadline, final List<Long> latencies, final AtomicLong failures) {
        if (!awaitStart(start)) {
            return;
        }
        while (System.nanoTime() < deadline) {
            long begin = System.nanoTime();
            try {
                client.get().uri("http://127.0.0.1:" + port + "/fast")
                        .responseContent().aggregate().asString().block(Duration.ofSeconds(5));
                latencies.add(TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - begin));
            } catch (RuntimeException ex) {
                failures.incrementAndGet();
            }
        }
    }

    private static boolean awaitStart(final CountDownLatch start) {
        try {
            start.await();
            return true;
        } catch (InterruptedException ex) {
            Thread.currentThread().interrupt();
            return false;
        }
    }

    private static long percentile(final List<Long> samples, final int percentile) {
        if (samples.isEmpty()) {
            return -1;
        }
        List<Long> sorted = new ArrayList<>(samples);
        Collections.sort(sorted);
        int index = (int) Math.min(sorted.size() - 1L, Math.ceil(sorted.size() * percentile / 100.0) - 1);
        return sorted.get(Math.max(index, 0));
    }

    private static void print(final String scenario, final Result result) {
        System.out.printf("%s,%d,%d,%d,%d,%d,%d%n", scenario, result.bulkOk(), result.bulkFailures(),
                result.interactiveOk(), result.interactiveFailures(), result.interactiveP50(), result.interactiveP99());
    }

    private static final class Result {

        private final long bulkOk;

        private final long bulkFailures;

        private final long interactiveOk;

        private final long interactiveFailures;

        private final long interactiveP50;

        private final long interactiveP99;

        private Result(final long bulkOk, final long bulkFailures, final long interactiveOk,
                       final long interactiveFailures, final long interactiveP50, final long interactiveP99) {
            this.bulkOk = bulkOk;
            this.bulkFailures = bulkFailures;
            this.interactiveOk = interactiveOk;
            this.interactiveFailures = interactiveFailures;
            this.interactiveP50 = interactiveP50;
            this.interactiveP99 = interactiveP99;
        }

        long bulkOk() {
            return bulkOk;
        }

        long bulkFailures() {
            return bulkFailures;
        }

        long interactiveOk() {
            return interactiveOk;
        }

        long interactiveFailures() {
            return interactiveFailures;
        }

        long interactiveP50() {
            return interactiveP50;
        }

        long interactiveP99() {
            return interactiveP99;
        }
    }
}
