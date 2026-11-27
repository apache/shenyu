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

package org.apache.shenyu.springboot.plugin.divide;

import org.apache.shenyu.plugin.base.circuitbreaker.UpstreamCircuitBreaker;
import org.springframework.boot.context.properties.ConfigurationProperties;

import java.time.Duration;

/**
 * Startup settings for the Divide upstream circuit breaker.
 */
@ConfigurationProperties(prefix = DivideCircuitBreakerProperties.PREFIX)
public class DivideCircuitBreakerProperties {

    static final String PREFIX = "shenyu.upstream-circuit-breaker";

    static final String HALF_OPEN_WAIT_PROPERTY = PREFIX + ".half-open-wait";

    private static final Duration MIN_WAIT = Duration.ofMillis(1);

    private static final Duration MAX_WAIT = Duration.ofMillis(Long.MAX_VALUE);

    private int failureThreshold = UpstreamCircuitBreaker.DEFAULT_FAILURE_THRESHOLD;

    private Duration halfOpenWait = Duration.ofMillis(UpstreamCircuitBreaker.DEFAULT_HALF_OPEN_WAIT_MILLIS);

    /**
     * Get the consecutive failure threshold.
     *
     * @return failures that open the breaker
     */
    public int getFailureThreshold() {
        return failureThreshold;
    }

    /**
     * Set the consecutive failure threshold.
     *
     * @param failureThreshold positive number of failures
     */
    public void setFailureThreshold(final int failureThreshold) {
        if (failureThreshold < 1) {
            throw new IllegalArgumentException(PREFIX + ".failure-threshold must be a positive integer");
        }
        this.failureThreshold = failureThreshold;
    }

    /**
     * Get the configured wait before a recovery probe.
     *
     * @return half-open wait
     */
    public Duration getHalfOpenWait() {
        return halfOpenWait;
    }

    /**
     * Set the wait before a recovery probe.
     *
     * @param halfOpenWait duration in the supported millisecond range
     */
    public void setHalfOpenWait(final Duration halfOpenWait) {
        if (halfOpenWait.compareTo(MIN_WAIT) < 0 || halfOpenWait.compareTo(MAX_WAIT) > 0) {
            throw new IllegalArgumentException(HALF_OPEN_WAIT_PROPERTY + " must be between 1 ms and Long.MAX_VALUE milliseconds");
        }
        this.halfOpenWait = halfOpenWait;
    }

    /**
     * Convert to the breaker's clock resolution without shortening the configured wait.
     *
     * @return half-open wait in milliseconds
     */
    long halfOpenWaitMillis() {
        long millis = halfOpenWait.toMillis();
        return halfOpenWait.equals(Duration.ofMillis(millis)) ? millis : millis + 1;
    }
}
