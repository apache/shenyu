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

package org.apache.shenyu.plugin.resilience4j.conf;

import io.github.resilience4j.circuitbreaker.CircuitBreakerConfig;
import io.github.resilience4j.core.IntervalFunction;
import io.github.resilience4j.ratelimiter.RateLimiterConfig;
import io.github.resilience4j.timelimiter.TimeLimiterConfig;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.time.Duration;

/**
 * The Test Case For Resilience4JConf.
 */
public final class Resilience4JConfTest {

    private Resilience4JConf resilience4JConf;

    private RateLimiterConfig rateLimiterConfig;

    private TimeLimiterConfig timeLimiterConfig;

    private CircuitBreakerConfig circuitBreakerConfig;

    @BeforeEach
    public void setUp() {
        this.resilience4JConf = new Resilience4JConf("1", "test", null, null, null);
    }

    @Test
    public void testId() {
        resilience4JConf.setId("2");
        Assertions.assertEquals(resilience4JConf.getId(), "2");
    }

    @Test
    public void testFallBackUri() {
        resilience4JConf.setFallBackUri("test");
        Assertions.assertEquals(resilience4JConf.getFallBackUri(), "test");
    }

    @Test
    public void testTimeLimiterConfig() {
        timeLimiterConfig = TimeLimiterConfig.custom()
                .timeoutDuration(Duration.ofMillis(30000)).build();
        resilience4JConf.setTimeLimiterConfig(timeLimiterConfig);
        Assertions.assertEquals(resilience4JConf.getTimeLimiterConfig().getClass(), TimeLimiterConfig.class);
    }

    @Test
    public void testCircuitBreakerConfig() {
        circuitBreakerConfig = CircuitBreakerConfig.custom()
                .recordExceptions(Throwable.class, Exception.class)
                .failureRateThreshold(50)
                .automaticTransitionFromOpenToHalfOpenEnabled(false)
                .slidingWindow(100, 100,
                        0 == 0
                                ? CircuitBreakerConfig.SlidingWindowType.COUNT_BASED
                                : CircuitBreakerConfig.SlidingWindowType.TIME_BASED).waitIntervalFunctionInOpenState(IntervalFunction
                        .of(Duration.ofMillis(60000)))
                .permittedNumberOfCallsInHalfOpenState(10).build();
        resilience4JConf.setCircuitBreakerConfig(circuitBreakerConfig);
        Assertions.assertEquals(resilience4JConf.getCircuitBreakerConfig().getClass(), CircuitBreakerConfig.class);
    }

    @Test
    public void testRateLimiterConfig() {
        rateLimiterConfig = RateLimiterConfig.custom()
                .timeoutDuration(Duration.ofMillis(30000)).build();
        resilience4JConf.setRateLimiterConfig(rateLimiterConfig);
        Assertions.assertEquals(resilience4JConf.getRateLimiterConfig().getClass(), RateLimiterConfig.class);
    }
}
