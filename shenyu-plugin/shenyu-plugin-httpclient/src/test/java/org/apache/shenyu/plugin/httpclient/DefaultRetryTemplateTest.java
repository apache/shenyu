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
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.core.io.buffer.DataBufferLimitException;
import org.springframework.http.HttpStatus;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ResponseStatusException;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;
import reactor.util.context.Context;

import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;

/**
 * Request and subscription isolation for the shared default retry configuration.
 */
class DefaultRetryTemplateTest {

    @Test
    void concurrentRequestsKeepIndependentRetryLimitsAndContexts() {
        DefaultRetryStrategy<String> strategy = new DefaultRetryStrategy<>(mock(AbstractHttpClientPlugin.class));
        int[] limits = {0, 1, 3, 7};
        AtomicInteger[] attempts = {new AtomicInteger(), new AtomicInteger(), new AtomicInteger(), new AtomicInteger()};
        StepVerifier.withVirtualTime(() -> Flux.range(0, limits.length).flatMap(index -> {
            Mono<String> source = Mono.deferContextual(context -> {
                assertEquals(index, context.<Integer>get("request"));
                attempts[index].incrementAndGet();
                return Mono.error(new TimeoutException("upstream timeout"));
            });
            return strategy.execute(source, MockServerWebExchange.from(MockServerHttpRequest.get("/")), Duration.ofSeconds(5), limits[index])
                    .onErrorResume(ResponseStatusException.class, error -> {
                        assertEquals(HttpStatus.REQUEST_TIMEOUT, error.getStatusCode());
                        return Mono.just("exhausted");
                    }).contextWrite(Context.of("request", index));
        }))
                .thenAwait(Duration.ofMinutes(5))
                .expectNextCount(limits.length)
                .verifyComplete();
        for (int i = 0; i < limits.length; i++) {
            assertEquals(limits[i] + 1, attempts[i].get());
        }
    }

    @Test
    void repeatedSubscriptionsStartWithFreshRetryStateAndContext() {
        DefaultRetryStrategy<String> strategy = new DefaultRetryStrategy<>(mock(AbstractHttpClientPlugin.class));
        AtomicInteger attempts = new AtomicInteger();
        StepVerifier.withVirtualTime(() -> {
            Mono<String> source = Mono.deferContextual(context -> attempts.incrementAndGet() % 4 == 0
                    ? Mono.just(context.get("request")) : Mono.error(new TimeoutException("upstream timeout")));
            Mono<String> response = strategy.execute(source, MockServerWebExchange.from(MockServerHttpRequest.get("/")), Duration.ofSeconds(5), 3);
            return response.contextWrite(Context.of("request", "first"))
                    .concatWith(response.contextWrite(Context.of("request", "second")));
        })
                .thenAwait(Duration.ofMinutes(2))
                .expectNext("first", "second")
                .verifyComplete();
        assertEquals(8, attempts.get());
    }

    @Test
    void cancellationDuringBackoffDoesNotConsumeAnotherRequestsRetries() {
        DefaultRetryStrategy<String> strategy = new DefaultRetryStrategy<>(mock(AbstractHttpClientPlugin.class));
        AtomicInteger cancelledAttempts = new AtomicInteger();
        StepVerifier.withVirtualTime(() -> strategy.execute(Mono.defer(() -> {
            cancelledAttempts.incrementAndGet();
            return Mono.error(new TimeoutException("cancelled request"));
        }), MockServerWebExchange.from(MockServerHttpRequest.get("/cancel")), Duration.ofSeconds(5), 3))
                .thenAwait(Duration.ofMillis(1))
                .thenCancel()
                .verify();
        AtomicInteger subsequentAttempts = new AtomicInteger();
        StepVerifier.withVirtualTime(() -> strategy.execute(Mono.defer(() -> subsequentAttempts.incrementAndGet() == 4
                        ? Mono.just("success") : Mono.error(new TimeoutException("next request"))),
                MockServerWebExchange.from(MockServerHttpRequest.get("/next")), Duration.ofSeconds(5), 3))
                .thenAwait(Duration.ofMinutes(2))
                .expectNext("success")
                .verifyComplete();
        assertEquals(1, cancelledAttempts.get());
        assertEquals(4, subsequentAttempts.get());
    }

    @Test
    void parallelAssemblyKeepsRetryLimitsAndContextsIndependent() {
        DefaultRetryStrategy<String> strategy = new DefaultRetryStrategy<>(mock(AbstractHttpClientPlugin.class));
        int[] limits = {0, 1, 3, 5};
        AtomicInteger[] attempts = {new AtomicInteger(), new AtomicInteger(), new AtomicInteger(), new AtomicInteger()};
        ExecutorService assembly = Executors.newFixedThreadPool(limits.length);
        try {
            List<CompletableFuture<Mono<String>>> responses = new ArrayList<>();
            for (int i = 0; i < limits.length; i++) {
                final int index = i;
                responses.add(CompletableFuture.supplyAsync(() -> {
                    Mono<String> source = Mono.deferContextual(context -> {
                        assertEquals(index, context.<Integer>get("request"));
                        attempts[index].incrementAndGet();
                        return Mono.error(new TimeoutException("upstream timeout"));
                    });
                    return strategy.execute(source, MockServerWebExchange.from(MockServerHttpRequest.get("/parallel")),
                                    Duration.ofSeconds(5), limits[index])
                            .onErrorResume(ResponseStatusException.class, error -> {
                                assertEquals(HttpStatus.REQUEST_TIMEOUT, error.getStatusCode());
                                return Mono.just("exhausted-" + index);
                            }).contextWrite(Context.of("request", index));
                }, assembly));
            }
            StepVerifier.create(Flux.fromIterable(responses).flatMap(response -> Mono.fromFuture(response).flatMap(result -> result)))
                    .expectNextCount(limits.length)
                    .expectComplete()
                    .verify(Duration.ofSeconds(10));
            for (int i = 0; i < limits.length; i++) {
                assertEquals(limits[i] + 1, attempts[i].get());
            }
        } finally {
            assembly.shutdownNow();
        }
    }

    @ParameterizedTest
    @ValueSource(ints = {-1, 0, Integer.MAX_VALUE})
    void boundaryRetryCountsKeepSuccessAndPermanentErrorSemantics(final int limit) {
        DefaultRetryStrategy<String> strategy = new DefaultRetryStrategy<>(mock(AbstractHttpClientPlugin.class));
        StepVerifier.create(strategy.execute(Mono.just("success"), MockServerWebExchange.from(MockServerHttpRequest.get("/success")),
                        Duration.ofSeconds(5), limit))
                .expectNext("success")
                .verifyComplete();
        DataBufferLimitException failure = new DataBufferLimitException("permanent failure");
        StepVerifier.create(strategy.execute(Mono.error(failure), MockServerWebExchange.from(MockServerHttpRequest.get("/permanent")),
                        Duration.ofSeconds(5), limit))
                .expectErrorMatches(error -> error == failure)
                .verify();
    }

    @ParameterizedTest
    @ValueSource(ints = {-1, 0})
    void zeroAndNegativeRetryCountsKeepRetryableErrorMapping(final int limit) {
        DefaultRetryStrategy<String> strategy = new DefaultRetryStrategy<>(mock(AbstractHttpClientPlugin.class));
        AtomicInteger attempts = new AtomicInteger();
        Mono<String> source = Mono.defer(() -> {
            attempts.incrementAndGet();
            return Mono.error(new TimeoutException("upstream timeout"));
        });
        StepVerifier.create(strategy.execute(source, MockServerWebExchange.from(MockServerHttpRequest.get("/limit")),
                        Duration.ofSeconds(5), limit))
                .expectErrorSatisfies(error -> assertEquals(HttpStatus.REQUEST_TIMEOUT, ((ResponseStatusException) error).getStatusCode()))
                .verify();
        assertEquals(1, attempts.get());
    }
}
