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

package org.apache.shenyu.plugin.ai.proxy.enhanced.service;

import java.time.Duration;
import java.util.Objects;

/**
 * Retry properties and backoff calculation for AI upstream requests.
 */
public final class AiProxyRetryProperties {

    private static final int MAX_EXPONENTIAL_RETRIES = 30;

    private int maxRetries = 3;

    private int streamingMaxRetries = 1;

    private Duration initialBackoff = Duration.ofSeconds(1);

    /**
     * Create retry properties with defaults.
     */
    public AiProxyRetryProperties() {
    }

    /**
     * Create retry properties.
     *
     * @param maxRetries maximum retries for non-streaming requests
     * @param streamingMaxRetries maximum retries before a streaming response emits data
     * @param initialBackoff initial backoff for non-streaming retries
     */
    public AiProxyRetryProperties(final int maxRetries, final int streamingMaxRetries, final Duration initialBackoff) {
        setMaxRetries(maxRetries);
        setStreamingMaxRetries(streamingMaxRetries);
        setInitialBackoff(initialBackoff);
    }

    /**
     * Get maximum retries for non-streaming requests.
     *
     * @return maximum retries
     */
    public int getMaxRetries() {
        return maxRetries;
    }

    /**
     * Set maximum retries for non-streaming requests.
     *
     * @param maxRetries maximum retries
     */
    public void setMaxRetries(final int maxRetries) {
        this.maxRetries = requireRetryCount(maxRetries, "maxRetries");
    }

    /**
     * Get maximum retries before a streaming response emits data.
     *
     * @return maximum streaming retries
     */
    public int getStreamingMaxRetries() {
        return streamingMaxRetries;
    }

    /**
     * Set maximum retries before a streaming response emits data.
     *
     * @param streamingMaxRetries maximum streaming retries
     */
    public void setStreamingMaxRetries(final int streamingMaxRetries) {
        this.streamingMaxRetries = requireRetryCount(streamingMaxRetries, "streamingMaxRetries");
    }

    /**
     * Get initial non-streaming retry backoff.
     *
     * @return initial retry backoff
     */
    public Duration getInitialBackoff() {
        return initialBackoff;
    }

    /**
     * Set initial non-streaming retry backoff.
     *
     * @param initialBackoff initial retry backoff
     */
    public void setInitialBackoff(final Duration initialBackoff) {
        if (Objects.isNull(initialBackoff) || initialBackoff.isNegative()) {
            throw new IllegalArgumentException("initialBackoff must not be null or negative");
        }
        this.initialBackoff = initialBackoff;
    }

    int maxRetries(final boolean stream) {
        return stream ? streamingMaxRetries : maxRetries;
    }

    Duration retryDelay(final boolean stream, final int retryIndex) {
        if (retryIndex < 0 || retryIndex >= MAX_EXPONENTIAL_RETRIES) {
            throw new IllegalArgumentException(
                    "retryIndex must be between 0 and " + (MAX_EXPONENTIAL_RETRIES - 1));
        }
        if (stream) {
            return Duration.ZERO;
        }
        return initialBackoff.multipliedBy(1L << retryIndex);
    }

    private int requireRetryCount(final int value, final String name) {
        if (value < 0 || value > MAX_EXPONENTIAL_RETRIES) {
            throw new IllegalArgumentException(
                    name + " must be between 0 and " + MAX_EXPONENTIAL_RETRIES);
        }
        return value;
    }
}
