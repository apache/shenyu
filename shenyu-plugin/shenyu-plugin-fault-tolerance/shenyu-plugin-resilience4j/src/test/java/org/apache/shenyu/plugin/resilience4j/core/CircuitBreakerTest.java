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

import io.github.resilience4j.circuitbreaker.CallNotPermittedException;
import io.github.resilience4j.circuitbreaker.CircuitBreaker;
import io.github.resilience4j.reactor.circuitbreaker.operator.CircuitBreakerOperator;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.mockito.Mockito.RETURNS_DEEP_STUBS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * CircuitBreaker test.
 */
@ExtendWith(MockitoExtension.class)
public final class CircuitBreakerTest {

    private CircuitBreaker circuitBreaker;

    @BeforeEach
    public void setUp() {
        circuitBreaker = mock(CircuitBreaker.class, RETURNS_DEEP_STUBS);
    }

    @Test
    public void normalTest() {
        when(circuitBreaker.tryAcquirePermission()).thenReturn(true);
        StepVerifier.create(Mono.just("SHENYU")
                        .transformDeferred(CircuitBreakerOperator.of(circuitBreaker)))
                .expectNext("SHENYU")
                .verifyComplete();
    }

    @Test
    public void errorTest() {
        when(circuitBreaker.tryAcquirePermission()).thenReturn(true);
        StepVerifier.create(Mono.error(new RuntimeException("SHENYU"))
                        .transformDeferred(CircuitBreakerOperator.of(circuitBreaker)))
                .expectError(RuntimeException.class)
                .verify();
    }

    @Test
    public void circuitBreakerTest() {
        when(circuitBreaker.tryAcquirePermission()).thenReturn(false);
        StepVerifier.create((Mono.just("SHENYU"))
                        .transformDeferred(CircuitBreakerOperator.of(circuitBreaker)))
                .expectError(CallNotPermittedException.class)
                .verify();
    }
}
