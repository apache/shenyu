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
import org.junit.jupiter.api.Test;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.time.Duration;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;

/**
 * retry strategy test.
 *
 * @Date 2025/3/16 22:46
 */
public class RetryStrategyTest {

    @Test
    void testDefaultRetryBackoffExecute() {
        // Create a simulated AbstractHttpClientPlugin
        AbstractHttpClientPlugin<String> httpClientPlugin = mock(AbstractHttpClientPlugin.class);
        ExponentialRetryBackoffStrategy<String> strategy = new ExponentialRetryBackoffStrategy<>(httpClientPlugin);

        // Create a simulated ServerWebExchange
        ServerWebExchange exchange = mock(ServerWebExchange.class);
        Duration duration = Duration.ofSeconds(5);
        int retryTimes = 3;

        // Create a mock response Mono that throws an exception
        Mono<String> response = Mono.error(new RuntimeException("Test error"));

        // Execute retry policy
        Mono<String> result = strategy.execute(response, exchange, duration, retryTimes);

        // Use StepVerifier to verify results
        StepVerifier.create(result)
                .expectError(RuntimeException.class)
                .verify();
    }

    @Test
    void testDefaultRetryStrategyExecute() {
        //Create a simulated AbstractHttpClientPlugin
        AbstractHttpClientPlugin<String> httpClientPlugin = mock(AbstractHttpClientPlugin.class);
        DefaultRetryStrategy<String> strategy = new DefaultRetryStrategy<>(httpClientPlugin);

        // Create a simulated ServerWebExchange
        ServerWebExchange exchange = mock(ServerWebExchange.class);
        Duration duration = Duration.ofSeconds(5);
        int retryTimes = 3;

        // Create a mock response Mono that throws an exception
        Mono<String> response = Mono.error(new RuntimeException("Test error"));

        // Execute retry policy
        Mono<String> result = strategy.execute(response, exchange, duration, retryTimes);

        // Use StepVerifier to verify results
        StepVerifier.create(result)
                .expectError(RuntimeException.class)
                .verify();
    }

    @Test
    void testFixedRetryStrategyRetriesTransientGetFailures() {
        AbstractHttpClientPlugin<String> httpClientPlugin = mock(AbstractHttpClientPlugin.class);
        FixedRetryStrategy<String> strategy = new FixedRetryStrategy<>(httpClientPlugin);
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/").build());
        AtomicInteger attempts = new AtomicInteger();
        Mono<String> response = Mono.defer(() -> {
            attempts.incrementAndGet();
            return Mono.error(new ConnectTimeoutException("connection timed out"));
        });

        StepVerifier.withVirtualTime(() -> strategy.execute(response, exchange, Duration.ofSeconds(10), 2))
                .thenAwait(Duration.ofSeconds(4))
                .expectError(ConnectTimeoutException.class)
                .verify();

        assertEquals(3, attempts.get());
    }

    @Test
    void testFixedRetryStrategyDoesNotRetryPermanentFailures() {
        AbstractHttpClientPlugin<String> httpClientPlugin = mock(AbstractHttpClientPlugin.class);
        FixedRetryStrategy<String> strategy = new FixedRetryStrategy<>(httpClientPlugin);
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/").build());
        AtomicInteger attempts = new AtomicInteger();
        Mono<String> response = Mono.defer(() -> {
            attempts.incrementAndGet();
            return Mono.error(new IllegalArgumentException("permanent failure"));
        });

        StepVerifier.create(strategy.execute(response, exchange, Duration.ofSeconds(5), 3))
                .expectError(IllegalArgumentException.class)
                .verify();

        assertEquals(1, attempts.get());
    }

    @Test
    void testFixedRetryStrategyDoesNotRetryPostRequests() {
        AbstractHttpClientPlugin<String> httpClientPlugin = mock(AbstractHttpClientPlugin.class);
        FixedRetryStrategy<String> strategy = new FixedRetryStrategy<>(httpClientPlugin);
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/").build());
        AtomicInteger attempts = new AtomicInteger();
        Mono<String> response = Mono.defer(() -> {
            attempts.incrementAndGet();
            return Mono.error(new TimeoutException("request timed out"));
        });

        StepVerifier.create(strategy.execute(response, exchange, Duration.ofSeconds(5), 3))
                .expectError(TimeoutException.class)
                .verify();

        assertEquals(1, attempts.get());
    }
}
