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

package org.apache.shenyu.plugin.ai.proxy.enhanced.transport;

import java.time.Duration;
import java.util.Objects;

/**
 * AI proxy transport properties.
 */
public class AiProxyTransportProperties {

    /**
     * Maximum duration allowed for establishing an upstream connection.
     */
    private Duration connectTimeout = Duration.ofSeconds(30);

    /**
     * Maximum duration allowed for receiving upstream response headers.
     */
    private Duration responseHeaderTimeout = Duration.ofSeconds(30);

    /**
     * Maximum idle duration allowed between upstream response body chunks.
     */
    private Duration streamingIdleTimeout = Duration.ofSeconds(60);

    /**
     * Maximum total duration allowed for an upstream response body stream.
     */
    private Duration totalStreamDuration = Duration.ofMinutes(10);

    /**
     * Maximum total bytes allowed for one upstream response. Zero means unlimited.
     */
    private long maxResponseBytes;

    /**
     * Get connect timeout.
     *
     * @return connect timeout
     */
    public Duration getConnectTimeout() {
        return connectTimeout;
    }

    /**
     * Set connect timeout.
     *
     * @param connectTimeout connect timeout
     */
    public void setConnectTimeout(final Duration connectTimeout) {
        this.connectTimeout = requirePositive(connectTimeout, "connectTimeout");
    }

    /**
     * Get response header timeout.
     *
     * @return response header timeout
     */
    public Duration getResponseHeaderTimeout() {
        return responseHeaderTimeout;
    }

    /**
     * Set response header timeout.
     *
     * @param responseHeaderTimeout response header timeout
     */
    public void setResponseHeaderTimeout(final Duration responseHeaderTimeout) {
        this.responseHeaderTimeout = requirePositive(responseHeaderTimeout, "responseHeaderTimeout");
    }

    /**
     * Get streaming idle timeout.
     *
     * @return streaming idle timeout
     */
    public Duration getStreamingIdleTimeout() {
        return streamingIdleTimeout;
    }

    /**
     * Set streaming idle timeout.
     *
     * @param streamingIdleTimeout streaming idle timeout
     */
    public void setStreamingIdleTimeout(final Duration streamingIdleTimeout) {
        this.streamingIdleTimeout = requirePositive(streamingIdleTimeout, "streamingIdleTimeout");
    }

    /**
     * Get total stream duration.
     *
     * @return total stream duration
     */
    public Duration getTotalStreamDuration() {
        return totalStreamDuration;
    }

    /**
     * Set total stream duration.
     *
     * @param totalStreamDuration total stream duration
     */
    public void setTotalStreamDuration(final Duration totalStreamDuration) {
        this.totalStreamDuration = requirePositive(totalStreamDuration, "totalStreamDuration");
    }

    /**
     * Get maximum upstream response bytes.
     *
     * @return maximum upstream response bytes, or zero when unlimited
     */
    public long getMaxResponseBytes() {
        return maxResponseBytes;
    }

    /**
     * Set maximum upstream response bytes.
     *
     * @param maxResponseBytes maximum upstream response bytes, or zero when unlimited
     */
    public void setMaxResponseBytes(final long maxResponseBytes) {
        this.maxResponseBytes = requireNonNegative(maxResponseBytes, "maxResponseBytes");
    }

    static Duration requirePositive(final Duration duration, final String name) {
        Objects.requireNonNull(duration, name + " must not be null");
        if (duration.isZero() || duration.isNegative()) {
            throw new IllegalArgumentException(name + " must be greater than zero");
        }
        return duration;
    }

    static long requireNonNegative(final long value, final String name) {
        if (value < 0) {
            throw new IllegalArgumentException(name + " must be greater than or equal to zero");
        }
        return value;
    }
}
