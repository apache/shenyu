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
import org.springframework.http.HttpStatus;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

/**
 * The four behaviors issue #8433 asks to pin for the CURRENT retry strategy's
 * shared immutable backoff template: retry limits, concurrent template use,
 * per-sequence state isolation and cancellation.
 */
class DefaultRetryStrategyCurrentRetryTest {

    private DefaultRetryStrategy<String> strategy() {
        AbstractHttpClientPlugin<String> plugin = mock(AbstractHttpClientPlugin.class);
        return new DefaultRetryStrategy<>(plugin);
    }

    private ServerWebExchange exchange() {
        return MockServerWebExchange.from(MockServerHttpRequest.get("/current-retry"));
    }

    /**
     * A response that fails every attempt with a retryable transient failure.
     */
    private Mono<String> alwaysFailing(final AtomicInteger attempts) {
        return Mono.defer(() -> {
            attempts.incrementAndGet();
            return Mono.error(new IllegalStateException("retry", new TimeoutException("transient failure")));
        });
    }

    @Test
    void currentStrategyAttemptsExactlyRetryTimesPlusOne() {
        final AtomicInteger attempts = new AtomicInteger();
        StepVerifier.withVirtualTime(() -> strategy().execute(alwaysFailing(attempts),
                exchange(), Duration.ofMillis(100), 3))
                .thenAwait(Duration.ofMinutes(2))
                .expectError()
                .verify();
        assertEquals(4, attempts.get());
    }

    @Test
    void exhaustedRetriesMapToRequestTimeout() {
        final AtomicInteger attempts = new AtomicInteger();
        StepVerifier.withVirtualTime(() -> strategy().execute(alwaysFailing(attempts),
                exchange(), Duration.ofMillis(100), 2))
                .thenAwait(Duration.ofMinutes(2))
                .expectErrorSatisfies(error -> {
                    final ResponseStatusException status = (ResponseStatusException) error;
                    assertEquals(HttpStatus.REQUEST_TIMEOUT, status.getStatusCode());
                })
                .verify();
        assertEquals(3, attempts.get());
    }

    @Test
    void concurrentRequestsShareTheTemplateWithoutInterference() throws Exception {
        final DefaultRetryStrategy<String> shared = strategy();
        final int threads = 8;
        final int requestsPerThread = 30;
        final ExecutorService executor = Executors.newFixedThreadPool(threads);
        final CountDownLatch start = new CountDownLatch(1);
        final List<Future<Integer>> results = new ArrayList<>();
        for (int t = 0; t < threads; t++) {
            results.add(executor.submit(() -> {
                start.await();
                int minAttempts = Integer.MAX_VALUE;
                for (int i = 0; i < requestsPerThread; i++) {
                    final AtomicInteger attempts = new AtomicInteger();
                    // real short backoff: two retries of 20/40 ms per sequence
                    StepVerifier.create(shared.execute(alwaysFailing(attempts), exchange(),
                                    Duration.ofMillis(50), 2)).expectError().verify();
                    minAttempts = Math.min(minAttempts, attempts.get());
                    assertEquals(3, minAttempts, "retry count leaked across concurrent requests");
                }
                return minAttempts;
            }));
        }
        start.countDown();
        for (Future<Integer> result : results) {
            assertEquals(3, result.get(60, TimeUnit.SECONDS));
        }
        executor.shutdownNow();
        assertTrue(executor.awaitTermination(10, TimeUnit.SECONDS));
    }

    @Test
    void secondSequenceDoesNotInheritTemplateState() {
        final DefaultRetryStrategy<String> shared = strategy();
        final AtomicInteger first = new AtomicInteger();
        StepVerifier.create(shared.execute(alwaysFailing(first), exchange(), Duration.ofMillis(50), 2)).expectError().verify();
        final AtomicInteger second = new AtomicInteger();
        StepVerifier.create(shared.execute(alwaysFailing(second), exchange(), Duration.ofMillis(50), 2)).expectError().verify();
        assertEquals(3, first.get());
        assertEquals(3, second.get());
    }

    @Test
    void cancellationStopsTheRetrySequence() {
        final AtomicInteger attempts = new AtomicInteger();
        StepVerifier.withVirtualTime(() -> strategy().execute(Mono.defer(() -> {
            attempts.incrementAndGet();
            return Mono.error(new IllegalStateException("retry", new TimeoutException("transient failure")));
        }), exchange(), Duration.ofMillis(100), 10))
                .thenAwait(Duration.ofMillis(30))
                .thenCancel()
                .verify();
        assertTrue(attempts.get() < 10, "cancellation must stop further attempts");
    }
}
