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

package org.apache.shenyu.plugin.resilience4j.executor;

import io.github.resilience4j.circuitbreaker.CircuitBreakerConfig;
import io.github.resilience4j.ratelimiter.RateLimiterConfig;
import io.github.resilience4j.timelimiter.TimeLimiterConfig;
import org.apache.shenyu.plugin.resilience4j.conf.Resilience4JConf;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.MockedStatic;
import org.powermock.modules.junit4.PowerMockRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.mockito.Mockito.when;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.any;

/**
 * CombinedExecutor test.
 */
@RunWith(PowerMockRunner.class)
public final class CombinedExecutorTest {

    private static MockedStatic<LoggerFactory> loggerFactoryMockedStatic;

    private CombinedExecutor combinedExecutor;

    @BeforeClass
    public static void beforeClass() {
        loggerFactoryMockedStatic = mockStatic(LoggerFactory.class);
        loggerFactoryMockedStatic.when(() -> LoggerFactory.getLogger((Class<?>) any()))
                .thenReturn(mock(Logger.class));
    }

    @AfterClass
    public static void afterClass() {
        loggerFactoryMockedStatic.close();
    }

    @Before
    public void setUp() {
        combinedExecutor = new CombinedExecutor();
    }

    @Test
    public void normalTest() {
        Resilience4JConf conf = mock(Resilience4JConf.class);
        when(conf.getId()).thenReturn("SHENYU");
        when(conf.getRateLimiterConfig()).thenReturn(RateLimiterConfig.ofDefaults());
        when(conf.getTimeLimiterConfig()).thenReturn(TimeLimiterConfig.ofDefaults());
        when(conf.getCircuitBreakerConfig()).thenReturn(CircuitBreakerConfig.ofDefaults());
        Mono<String> mono = Mono.just("ERROR");
        StepVerifier.create(combinedExecutor.run(Mono.just("SHENYU"), throwable -> mono, conf))
                .expectSubscription()
                .expectNext("SHENYU")
                .verifyComplete();

    }

    @Test
    public void errorTest() {
        Resilience4JConf conf = mock(Resilience4JConf.class);
        when(conf.getId()).thenReturn("SHENYU");
        when(conf.getRateLimiterConfig()).thenReturn(RateLimiterConfig.ofDefaults());
        when(conf.getTimeLimiterConfig()).thenReturn(TimeLimiterConfig.ofDefaults());
        when(conf.getCircuitBreakerConfig()).thenReturn(CircuitBreakerConfig.ofDefaults());
        StepVerifier.create(combinedExecutor.run(Mono.error(new RuntimeException()), Mono::error, conf))
                .expectSubscription()
                .expectError(RuntimeException.class)
                .verify();
    }
}
