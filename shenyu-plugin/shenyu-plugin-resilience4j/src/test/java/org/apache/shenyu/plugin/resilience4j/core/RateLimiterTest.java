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

package org.apache.shenyu.plugin.resilience4j.core;

import io.github.resilience4j.ratelimiter.RateLimiter;
import io.github.resilience4j.ratelimiter.RequestNotPermitted;
import io.github.resilience4j.reactor.ratelimiter.operator.RateLimiterOperator;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.time.Duration;

import static org.mockito.Mockito.RETURNS_DEEP_STUBS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * RateLimiter test.
 */
@ExtendWith(MockitoExtension.class)
public final class RateLimiterTest {

    private RateLimiter rateLimiter;

    @BeforeEach
    public void setUp() {
        rateLimiter = mock(RateLimiter.class, RETURNS_DEEP_STUBS);
    }

    @Test
    public void normalTest() {
        when(rateLimiter.reservePermission()).thenReturn(Duration.ofSeconds(0).toNanos());
        StepVerifier.create(Mono.just("SHENYU")
                        .transformDeferred(RateLimiterOperator.of(rateLimiter)))
                .expectSubscription()
                .expectNext("SHENYU")
                .verifyComplete();
    }

    @Test
    public void errorTest() {
        when(rateLimiter.reservePermission()).thenReturn(Duration.ofSeconds(0).toNanos());
        StepVerifier.create(Mono.error(new RuntimeException("SHENYU"))
                        .transformDeferred(RateLimiterOperator.of(rateLimiter)))
                .expectSubscription()
                .expectError(RuntimeException.class)
                .verify();
    }

    @Test
    public void limitTest() {
        when(rateLimiter.reservePermission()).thenReturn(-1L);
        StepVerifier.create(Mono.just("SHENYU")
                        .transformDeferred(RateLimiterOperator.of(rateLimiter)))
                .expectError(RequestNotPermitted.class)
                .verify();
    }
}
