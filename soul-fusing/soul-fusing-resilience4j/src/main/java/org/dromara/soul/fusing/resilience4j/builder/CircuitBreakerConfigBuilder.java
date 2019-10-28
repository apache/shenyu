/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.soul.fusing.resilience4j.builder;

import io.github.resilience4j.circuitbreaker.CircuitBreakerConfig;
import org.dromara.soul.fusing.api.config.FusingConfig;

import java.io.IOException;
import java.time.Duration;
import java.util.concurrent.TimeoutException;

/**
 * The type Circuit breaker config builder.
 *
 * @author xiaoyu
 */
public class CircuitBreakerConfigBuilder {

    /**
     * Build circuit breaker config.
     *
     * @param config the config
     * @return the circuit breaker config
     */
    public static CircuitBreakerConfig build(FusingConfig config) {
        return CircuitBreakerConfig.custom()
                .failureRateThreshold(config.getErrorThresholdPercentage())
                .waitDurationInOpenState(Duration.ofMillis(config.getSleepWindowInMilliseconds()))
                .permittedNumberOfCallsInHalfOpenState(2)
                .slidingWindowSize(2)
                .recordExceptions(IOException.class, TimeoutException.class)
                .build();
    }
}
