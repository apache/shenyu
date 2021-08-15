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

/**
 * Resilience4J conf.
 */
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

    /**
     * Instantiates a new Resilience4jConf.
     *
     * @param id                   the id
     * @param fallBackUri          the fall back uri
     * @param rateLimiterConfig    the rate limiter config
     * @param timeLimiterConfig    the time limiter config
     * @param circuitBreakerConfig the circuit breaker config
     */
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

    /**
     * Gets id.
     *
     * @return the id
     */
    public String getId() {
        return id;
    }

    /**
     * Sets id.
     *
     * @param id the id
     */
    public void setId(final String id) {
        this.id = id;
    }

    /**
     * Gets fall back uri.
     *
     * @return the fall back uri
     */
    public String getFallBackUri() {
        return fallBackUri;
    }

    /**
     * Sets fall back uri.
     *
     * @param fallBackUri the fall back uri
     */
    public void setFallBackUri(final String fallBackUri) {
        this.fallBackUri = fallBackUri;
    }

    /**
     * Gets time limiter config.
     *
     * @return the time limiter config
     */
    public TimeLimiterConfig getTimeLimiterConfig() {
        return timeLimiterConfig;
    }

    /**
     * Sets time limiter config.
     *
     * @param timeLimiterConfig the time limiter config
     */
    public void setTimeLimiterConfig(final TimeLimiterConfig timeLimiterConfig) {
        this.timeLimiterConfig = timeLimiterConfig;
    }

    /**
     * Gets circuit breaker config.
     *
     * @return the circuit breaker config
     */
    public CircuitBreakerConfig getCircuitBreakerConfig() {
        return circuitBreakerConfig;
    }

    /**
     * Sets circuit breaker config.
     *
     * @param circuitBreakerConfig the circuit breaker config
     */
    public void setCircuitBreakerConfig(final CircuitBreakerConfig circuitBreakerConfig) {
        this.circuitBreakerConfig = circuitBreakerConfig;
    }

    /**
     * Gets rate limiter config.
     *
     * @return the rate limiter config
     */
    public RateLimiterConfig getRateLimiterConfig() {
        return rateLimiterConfig;
    }

    /**
     * Sets rate limiter config.
     *
     * @param rateLimiterConfig the rate limiter config
     */
    public void setRateLimiterConfig(final RateLimiterConfig rateLimiterConfig) {
        this.rateLimiterConfig = rateLimiterConfig;
    }
}
