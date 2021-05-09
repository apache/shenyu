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
import io.github.resilience4j.ratelimiter.RateLimiterConfig;
import io.github.resilience4j.timelimiter.TimeLimiterConfig;
import lombok.Data;

/**
 * Resilience4J conf.
 */
@Data
public class Resilience4JConf {

    /**
     * id.
     */
    private String id;

    /**
     * fallBackUri.
     */
    private String fallBackUri;

    /**
     * timeLimiterConfig.
     */
    private TimeLimiterConfig timeLimiterConfig;

    /**
     * circuitBreakerConfig.
     */
    private CircuitBreakerConfig circuitBreakerConfig;

    /**
     * rateLimiterConfig.
     */
    private RateLimiterConfig rateLimiterConfig;

    public Resilience4JConf(final String id,
                            final String fallBackUri,
                            final RateLimiterConfig rateLimiterConfig,
                            final TimeLimiterConfig timeLimiterConfig,
                            final CircuitBreakerConfig circuitBreakerConfig) {
        this.id = id;
        this.fallBackUri = fallBackUri;
        this.rateLimiterConfig = rateLimiterConfig;
        this.timeLimiterConfig = timeLimiterConfig;
        this.circuitBreakerConfig = circuitBreakerConfig;
    }
}
