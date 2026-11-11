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

import org.apache.shenyu.plugin.ai.proxy.enhanced.model.ShenyuAiRequest;

import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;

/**
 * Internal retry calculations derived from public retry configuration.
 */
final class AiRetrySettings {

    private static final int MAX_EXPONENTIAL_RETRIES = 30;

    private static final long MAX_RETRY_AFTER_SECONDS = 86_400L;

    private final AiProxyRetryProperties properties;

    AiRetrySettings(final AiProxyRetryProperties properties) {
        this.properties = Objects.requireNonNull(properties, "properties must not be null");
    }

    int maxRetries(final boolean stream) {
        return stream ? properties.getStreamingMaxRetries() : properties.getMaxRetries();
    }

    Duration retryDelay(final boolean stream, final int retryIndex, final long seed) {
        if (retryIndex < 0 || retryIndex >= MAX_EXPONENTIAL_RETRIES) {
            throw new IllegalArgumentException(
                    "retryIndex must be between 0 and " + (MAX_EXPONENTIAL_RETRIES - 1));
        }
        if (stream) {
            return Duration.ZERO;
        }
        Duration capped = properties.getInitialBackoff();
        for (int power = 0; power < retryIndex && capped.compareTo(properties.getMaxBackoff()) < 0; power++) {
            capped = capped.multipliedBy(2);
        }
        Duration bounded = properties.getMaxBackoff().compareTo(capped) < 0
                ? properties.getMaxBackoff() : capped;
        return jitter(bounded, seed);
    }

    Duration clampRetryAfter(final Duration delay) {
        if (Objects.isNull(delay) || delay.isNegative()
                || delay.compareTo(properties.getMaxRetryAfter()) > 0) {
            return null;
        }
        return delay;
    }

    boolean acceptsRetryAfter(final Duration delay) {
        return Objects.nonNull(delay) && !delay.isNegative()
                && delay.compareTo(properties.getMaxRetryAfter()) <= 0;
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

    AiProxyRetryProperties.ReplayMode replayMode(final ShenyuAiRequest.Operation operation,
                                                  final boolean executionAmbiguous,
                                                  final boolean transportFailure) {
        AiProxyRetryProperties.ReplayProperties replay = properties.getReplay();
        Map<String, AiProxyRetryProperties.ReplayMode> operations = replay.getOperations();
        AiProxyRetryProperties.ReplayMode configured = operations.get(operation.name());
        if (Objects.isNull(configured)) {
            configured = operations.get(operation.name().replace('_', '-').toLowerCase(Locale.ROOT));
        }
        if (Objects.nonNull(configured)) {
            return configured;
        }
        if (transportFailure) {
            return executionAmbiguous ? replay.getAmbiguousTransport() : replay.getDefiniteTransport();
        }
        return replay.getResponseStatus();
    }

    private Duration jitter(final Duration delay, final long seed) {
        if (properties.getJitterFraction() == 0d || delay.isZero()) {
            return delay;
        }
        double fraction = randomFraction(seed);
        double offset = delay.toNanos() * properties.getJitterFraction() * fraction;
        return delay.plusNanos(Math.round(offset));
    }

    private double randomFraction(final long seed) {
        long mixed = seed * 0x9E3779B97F4A7C15L;
        mixed ^= mixed >>> 32;
        return ((mixed & 0x7fffffffL) + 1d) / 0x80000000L;
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
}
