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

package org.apache.shenyu.common.dto;

import java.time.Duration;
import java.util.Objects;

/** Immutable timing policy for managed remote catalog refresh. */
public record AgentGatewayRefreshPolicy(
        Duration refreshInterval,
        Duration initialBackoff,
        Duration maxBackoff,
        int maxRetries) {

    private static final Duration MIN_REFRESH_INTERVAL = Duration.ofSeconds(5);

    private static final Duration MAX_REFRESH_INTERVAL = Duration.ofHours(1);

    private static final Duration MIN_BACKOFF = Duration.ofSeconds(1);

    private static final int MAX_RETRIES = 8;

    private static final Duration DEFAULT_REFRESH_INTERVAL = Duration.ofSeconds(60);

    private static final Duration DEFAULT_INITIAL_BACKOFF = Duration.ofSeconds(1);

    private static final Duration DEFAULT_MAX_BACKOFF = Duration.ofSeconds(30);

    private static final int DEFAULT_MAX_RETRIES = 3;

    public AgentGatewayRefreshPolicy {
        if (Objects.isNull(refreshInterval)
                || Objects.isNull(initialBackoff)
                || Objects.isNull(maxBackoff)) {
            throw new IllegalArgumentException("Missing refresh timing policy");
        }
        if (refreshInterval.compareTo(MIN_REFRESH_INTERVAL) < 0
                || refreshInterval.compareTo(MAX_REFRESH_INTERVAL) > 0) {
            throw new IllegalArgumentException("Invalid refresh interval");
        }
        if (initialBackoff.compareTo(MIN_BACKOFF) < 0
                || maxBackoff.compareTo(initialBackoff) < 0
                || maxBackoff.compareTo(refreshInterval) > 0) {
            throw new IllegalArgumentException("Invalid refresh backoff");
        }
        if (maxRetries < 0 || maxRetries > MAX_RETRIES) {
            throw new IllegalArgumentException("Invalid refresh retry budget");
        }
    }

    /**
     * Create the conservative default timing policy.
     * @return immutable default policy
     */
    public static AgentGatewayRefreshPolicy defaults() {
        return new AgentGatewayRefreshPolicy(
                DEFAULT_REFRESH_INTERVAL,
                DEFAULT_INITIAL_BACKOFF,
                DEFAULT_MAX_BACKOFF,
                DEFAULT_MAX_RETRIES);
    }

    /**
     * Calculate the delay after a completed refresh attempt.
     * @param consecutiveFailures zero after success, positive after failures
     * @return delay before the next refresh attempt
     */
    public Duration nextDelay(final int consecutiveFailures) {
        if (consecutiveFailures < 0) {
            throw new IllegalArgumentException("Negative refresh failure count");
        }
        if (consecutiveFailures == 0 || consecutiveFailures > maxRetries) {
            return refreshInterval;
        }

        Duration delay = initialBackoff;
        for (int failure = 1; failure < consecutiveFailures; failure++) {
            delay = delay.multipliedBy(2);
            if (delay.compareTo(maxBackoff) >= 0) {
                return maxBackoff;
            }
        }
        return delay;
    }
}