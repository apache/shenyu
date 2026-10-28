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

import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.time.ZonedDateTime;
import java.time.format.DateTimeParseException;
import java.time.format.DateTimeFormatter;
import java.util.LinkedHashMap;
import java.util.Objects;
import java.util.Map;

import org.apache.shenyu.plugin.ai.proxy.enhanced.model.ShenyuAiRequest;

/**
 * Retry properties and backoff calculation for AI upstream requests.
 */
public final class AiProxyRetryProperties {

    private static final int MAX_EXPONENTIAL_RETRIES = 30;

    private static final long MAX_RETRY_AFTER_SECONDS = 86_400L;

    private int maxRetries = 3;

    private int streamingMaxRetries = 1;

    private Duration initialBackoff = Duration.ofSeconds(1);

    private Duration maxBackoff = Duration.ofSeconds(30);

    private double jitterFraction = 0.2d;

    private Duration maxRetryAfter = Duration.ofSeconds(60);

    private Duration requestDeadline = Duration.ofMinutes(5);

    private ReplayProperties replay = new ReplayProperties();

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
     * Create retry properties for deterministic tests.
     *
     * @param maxRetries maximum retries for non-streaming requests
     * @param streamingMaxRetries maximum retries before a streaming response emits data
     * @param initialBackoff initial backoff for non-streaming retries
     * @param maxBackoff maximum backoff after exponential growth
     * @param jitterFraction maximum jitter as a fraction of the calculated delay
     * @param maxRetryAfter maximum accepted Retry-After delay
     * @param requestDeadline shared deadline for all attempts and delays
     */
    public AiProxyRetryProperties(final int maxRetries, final int streamingMaxRetries,
                                  final Duration initialBackoff, final Duration maxBackoff,
                                  final double jitterFraction, final Duration maxRetryAfter,
                                  final Duration requestDeadline) {
        this(maxRetries, streamingMaxRetries, initialBackoff);
        setMaxBackoff(maxBackoff);
        setJitterFraction(jitterFraction);
        setMaxRetryAfter(maxRetryAfter);
        setRequestDeadline(requestDeadline);
    }

    /**
     * Get replay-safety policy.
     *
     * @return replay policy
     */
    public ReplayProperties getReplay() {
        return replay;
    }

    /**
     * Set replay-safety policy.
     *
     * @param replay replay policy
     */
    public void setReplay(final ReplayProperties replay) {
        this.replay = Objects.requireNonNull(replay, "replay must not be null");
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

    /**
     * Get maximum retry backoff.
     *
     * @return maximum retry backoff
     */
    public Duration getMaxBackoff() {
        return maxBackoff;
    }

    /**
     * Set maximum retry backoff.
     *
     * @param maxBackoff maximum retry backoff
     */
    public void setMaxBackoff(final Duration maxBackoff) {
        if (Objects.isNull(maxBackoff) || maxBackoff.isNegative()) {
            throw new IllegalArgumentException("maxBackoff must not be null or negative");
        }
        this.maxBackoff = maxBackoff;
    }

    /**
     * Get maximum jitter fraction.
     *
     * @return maximum jitter fraction from zero to one
     */
    public double getJitterFraction() {
        return jitterFraction;
    }

    /**
     * Set maximum jitter fraction.
     *
     * @param jitterFraction fraction from zero to one
     */
    public void setJitterFraction(final double jitterFraction) {
        if (jitterFraction < 0d || jitterFraction > 1d || Double.isNaN(jitterFraction)) {
            throw new IllegalArgumentException("jitterFraction must be between 0 and 1");
        }
        this.jitterFraction = jitterFraction;
    }

    /**
     * Get maximum accepted Retry-After delay.
     *
     * @return maximum Retry-After delay
     */
    public Duration getMaxRetryAfter() {
        return maxRetryAfter;
    }

    /**
     * Set maximum accepted Retry-After delay.
     *
     * @param maxRetryAfter maximum Retry-After delay
     */
    public void setMaxRetryAfter(final Duration maxRetryAfter) {
        if (Objects.isNull(maxRetryAfter) || maxRetryAfter.isNegative()) {
            throw new IllegalArgumentException("maxRetryAfter must not be null or negative");
        }
        this.maxRetryAfter = maxRetryAfter;
    }

    /**
     * Get shared deadline for primary retries, delays and fallback.
     *
     * @return shared request deadline
     */
    public Duration getRequestDeadline() {
        return requestDeadline;
    }

    /**
     * Set shared deadline for primary retries, delays and fallback.
     *
     * @param requestDeadline shared request deadline
     */
    public void setRequestDeadline(final Duration requestDeadline) {
        if (Objects.isNull(requestDeadline) || requestDeadline.isZero()
                || requestDeadline.isNegative()) {
            throw new IllegalArgumentException("requestDeadline must be positive");
        }
        this.requestDeadline = requestDeadline;
    }

    int maxRetries(final boolean stream) {
        return stream ? streamingMaxRetries : maxRetries;
    }

    Duration retryDelay(final boolean stream, final int retryIndex, final long seed) {
        if (retryIndex < 0 || retryIndex >= MAX_EXPONENTIAL_RETRIES) {
            throw new IllegalArgumentException(
                    "retryIndex must be between 0 and " + (MAX_EXPONENTIAL_RETRIES - 1));
        }
        if (stream) {
            return Duration.ZERO;
        }
        Duration capped = initialBackoff;
        for (int power = 0; power < retryIndex && capped.compareTo(maxBackoff) < 0; power++) {
            capped = capped.multipliedBy(2);
        }
        return jitter(maxBackoff.compareTo(capped) < 0 ? maxBackoff : capped, seed);
    }

    Duration clampRetryAfter(final Duration delay) {
        if (Objects.isNull(delay) || delay.isNegative() || delay.compareTo(maxRetryAfter) > 0) {
            return null;
        }
        return delay;
    }

    boolean acceptsRetryAfter(final Duration delay) {
        return Objects.nonNull(delay) && !delay.isNegative() && delay.compareTo(maxRetryAfter) <= 0;
    }

    private Duration jitter(final Duration delay, final long seed) {
        if (jitterFraction == 0d || delay.isZero()) {
            return delay;
        }
        double fraction = randomFraction(seed);
        double offset = delay.toNanos() * jitterFraction * fraction;
        return delay.plusNanos(Math.round(offset));
    }

    private int requireRetryCount(final int value, final String name) {
        if (value < 0 || value > MAX_EXPONENTIAL_RETRIES) {
            throw new IllegalArgumentException(
                    name + " must be between 0 and " + MAX_EXPONENTIAL_RETRIES);
        }
        return value;
    }

    private double randomFraction(final long seed) {
        long mixed = seed * 0x9E3779B97F4A7C15L;
        mixed ^= mixed >>> 32;
        return ((mixed & 0x7fffffffL) + 1d) / 0x80000000L;
    }

    long parseRetryAfter(final String value, final Clock clock) {
        if (Objects.isNull(value) || value.isBlank()) {
            return -1L;
        }
        String trimmed = value.trim();
        try {
            long seconds = Long.parseLong(trimmed);
            if (seconds < 0 || seconds > MAX_RETRY_AFTER_SECONDS) {
                return -1L;
            }
            return seconds;
        } catch (NumberFormatException ignored) {
            return parseHttpDateRetryAfter(trimmed, clock);
        }
    }

    private long parseHttpDateRetryAfter(final String value, final Clock clock) {
        try {
            Instant retryAt = value.endsWith("Z")
                    ? Instant.parse(value)
                    : ZonedDateTime.parse(value, DateTimeFormatter.RFC_1123_DATE_TIME).toInstant();
            long seconds = Duration.between(clock.instant(), retryAt).toSeconds();
            return seconds >= 0 && seconds <= MAX_RETRY_AFTER_SECONDS ? seconds : -1L;
        } catch (DateTimeParseException ignored) {
            return -1L;
        }
    }

    /**
     * Replay-safety controls. ShenYu remains conservative unless a provider explicitly
     * declares an operation replay-safe.
     */
    public static class ReplayProperties {

        private ReplayMode responseStatus = ReplayMode.CONSERVATIVE;

        private ReplayMode definiteTransport = ReplayMode.CONSERVATIVE;

        private ReplayMode ambiguousTransport = ReplayMode.CONSERVATIVE;

        private Map<String, ReplayMode> operations = new LinkedHashMap<>();

        public ReplayMode getResponseStatus() {
            return responseStatus;
        }

        public void setResponseStatus(final ReplayMode responseStatus) {
            this.responseStatus = Objects.requireNonNull(responseStatus, "responseStatus must not be null");
        }

        public ReplayMode getDefiniteTransport() {
            return definiteTransport;
        }

        public void setDefiniteTransport(final ReplayMode definiteTransport) {
            this.definiteTransport = Objects.requireNonNull(definiteTransport, "definiteTransport must not be null");
        }

        public ReplayMode getAmbiguousTransport() {
            return ambiguousTransport;
        }

        public void setAmbiguousTransport(final ReplayMode ambiguousTransport) {
            this.ambiguousTransport = Objects.requireNonNull(ambiguousTransport, "ambiguousTransport must not be null");
        }

        public Map<String, ReplayMode> getOperations() {
            return operations;
        }

        public void setOperations(final Map<String, ReplayMode> operations) {
            Objects.requireNonNull(operations, "operations must not be null");
            operations.forEach((operation, mode) -> {
                if (Objects.isNull(operation) || operation.isBlank()) {
                    throw new IllegalArgumentException("operation must not be null or blank");
                }
                if (Objects.isNull(mode)) {
                    throw new IllegalArgumentException("operations value must not be null");
                }
            });
            this.operations = new LinkedHashMap<>(operations);
        }

        ReplayMode mode(final ShenyuAiRequest.Operation operation, final boolean executionAmbiguous,
                        final boolean transportFailure) {
            ReplayMode configured = operations.get(operation.name());
            if (Objects.isNull(configured)) {
                configured = operations.get(operation.name()
                        .replace('_', '-')
                        .toLowerCase(java.util.Locale.ROOT));
            }
            if (Objects.nonNull(configured)) {
                return configured;
            }
            if (transportFailure) {
                return executionAmbiguous ? ambiguousTransport : definiteTransport;
            }
            return responseStatus;
        }
    }

    /**
     * How a failed attempt may be replayed.
     */
    public enum ReplayMode {
        NEVER,
        CONSERVATIVE,
        PROVIDER_DECLARED
    }
}
