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

package org.dromara.soul.plugin.resilience4j.factory;

import io.github.resilience4j.circuitbreaker.CircuitBreaker;
import io.github.resilience4j.circuitbreaker.CircuitBreakerConfig;
import io.github.resilience4j.circuitbreaker.CircuitBreakerRegistry;
import io.github.resilience4j.ratelimiter.RateLimiter;
import io.github.resilience4j.ratelimiter.RateLimiterConfig;
import io.github.resilience4j.ratelimiter.RateLimiterRegistry;

/**
 * ResilienceRegistryFactory.
 *
 * @author zhanglei
 */
public class ResilienceRegistryFactory {


    /**
     * rateLimiterRegistry.
     */
    private static final RateLimiterRegistry RATE_LIMITER_REGISTRY = RateLimiterRegistry.ofDefaults();

    /**
     * circuitBreakerRegistry.
     */
    private static final CircuitBreakerRegistry CIRCUIT_BREAKER_REGISTRY = CircuitBreakerRegistry.ofDefaults();

    /**
     * circuitBreaker.
     *
     * @param id                   not null
     * @param circuitBreakerConfig not null
     * @return CircuitBreaker
     */
    public static CircuitBreaker circuitBreaker(final String id, final CircuitBreakerConfig circuitBreakerConfig) {
        return CIRCUIT_BREAKER_REGISTRY.circuitBreaker(id, circuitBreakerConfig);
    }

    /**
     * rateLimiter.
     *
     * @param id                not null
     * @param rateLimiterConfig not null
     * @return RateLimiter
     */
    public static RateLimiter rateLimiter(final String id, final RateLimiterConfig rateLimiterConfig) {
        return RATE_LIMITER_REGISTRY.rateLimiter(id, rateLimiterConfig);
    }

    /**
     * remove.
     *
     * @param id not null
     */
    public static void remove(final String id) {
        CIRCUIT_BREAKER_REGISTRY.remove(id);
        RATE_LIMITER_REGISTRY.remove(id);
    }

}
