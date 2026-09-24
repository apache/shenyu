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
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.time.Duration;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;

class RetryTimeoutBudgetTest {

    @ParameterizedTest
    @ValueSource(strings = {"fixed", "exponential", "current"})
    void allowsEveryConfiguredRetryDespiteBackoffExceedingAttemptTimeout(final String type) {
        AtomicInteger attempts = new AtomicInteger();
        StepVerifier.withVirtualTime(() -> strategy(type).execute(Mono.defer(() -> attempts.incrementAndGet() < 4
                        ? Mono.error(new IllegalStateException("retry")) : Mono.just("success")),
                MockServerWebExchange.from(MockServerHttpRequest.get("/")), Duration.ofMillis(100), 3))
                .thenAwait(Duration.ofMinutes(2))
                .expectNext("success")
                .verifyComplete();
        assertEquals(4, attempts.get());
    }

    @ParameterizedTest
    @ValueSource(strings = {"fixed", "exponential", "current"})
    void boundsTheEntireSequenceEvenIfSourceNeverCompletes(final String type) {
        StepVerifier.withVirtualTime(() -> strategy(type).execute(Mono.never(),
                MockServerWebExchange.from(MockServerHttpRequest.get("/")), Duration.ofMillis(100), 3))
                .thenAwait(Duration.ofMinutes(2))
                .expectErrorMatches(error -> error instanceof TimeoutException || error.getCause() instanceof TimeoutException)
                .verify();
    }

    @Test
    void calculatesBudgetAndSaturatesOverflow() {
        assertEquals(Duration.ofSeconds(18), RetryTimeoutUtils.totalTimeout(Duration.ofSeconds(3), 3, Duration.ofSeconds(2)));
        assertEquals(Duration.ofSeconds(3), RetryTimeoutUtils.totalTimeout(Duration.ofSeconds(3), 0, Duration.ofSeconds(20)));
        assertEquals(Duration.ofNanos(Long.MAX_VALUE), RetryTimeoutUtils.totalTimeout(Duration.ofSeconds(Long.MAX_VALUE), Integer.MAX_VALUE, Duration.ofSeconds(20)));
    }

    private RetryStrategy<String> strategy(final String type) {
        AbstractHttpClientPlugin<String> plugin = mock(AbstractHttpClientPlugin.class);
        if ("fixed".equals(type)) {
            return new FixedRetryStrategy<>(plugin);
        }
        if ("exponential".equals(type)) {
            return new ExponentialRetryBackoffStrategy<>(plugin);
        }
        return new DefaultRetryStrategy<>(plugin);
    }
}
