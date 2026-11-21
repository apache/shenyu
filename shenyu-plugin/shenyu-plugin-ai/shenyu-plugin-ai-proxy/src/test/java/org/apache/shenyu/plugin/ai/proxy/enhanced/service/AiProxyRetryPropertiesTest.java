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

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.ShenyuAiRequest;

import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for {@link AiProxyRetryProperties}.
 */
class AiProxyRetryPropertiesTest {

    private static final Clock CLOCK = Clock.fixed(Instant.parse("2026-01-01T00:00:00Z"), ZoneOffset.UTC);

    @Test
    void shouldProvideCompatibleDefaults() {
        final AiProxyRetryProperties properties = new AiProxyRetryProperties();

        assertEquals(3, properties.maxRetries(false));
        assertEquals(1, properties.maxRetries(true));
        assertEquals(Duration.ofSeconds(1), properties.getInitialBackoff());
        assertEquals(Duration.ofSeconds(30), properties.getMaxBackoff());
        assertEquals(0.2d, properties.getJitterFraction());
        assertEquals(Duration.ofSeconds(60), properties.getMaxRetryAfter());
        assertEquals(Duration.ofMinutes(5), properties.getRequestDeadline());
    }

    @ParameterizedTest
    @CsvSource({
        "0, 1000",
        "1, 2000",
        "2, 4000"
    })
    void shouldCalculateExponentialBackoff(final int retryIndex, final long expectedMillis) {
        final AiProxyRetryProperties properties = new AiProxyRetryProperties(3, 1, Duration.ofSeconds(1));

        assertEquals(Duration.ofMillis(expectedMillis), properties.retryDelay(false, retryIndex, 0L));
    }

    @Test
    void shouldCapExponentialBackoff() {
        AiProxyRetryProperties properties = new AiProxyRetryProperties(3, 1, Duration.ofSeconds(10));
        properties.setMaxBackoff(Duration.ofSeconds(15));
        properties.setJitterFraction(0d);

        assertEquals(Duration.ofSeconds(15), properties.retryDelay(false, 5, 0L));
    }

    @Test
    void shouldBoundDeterministicJitter() {
        AiProxyRetryProperties properties = new AiProxyRetryProperties(3, 1, Duration.ofSeconds(10));
        properties.setJitterFraction(1d);

        for (int seed = -10; seed <= 10; seed++) {
            Duration delay = properties.retryDelay(false, 0, seed);
            assertTrue(!delay.minus(Duration.ofSeconds(10)).isNegative()
                    && !Duration.ofSeconds(20).minus(delay).isNegative());
        }
    }

    @Test
    void shouldParseRetryAfterSecondsAndUtcDates() {
        AiProxyRetryProperties properties = new AiProxyRetryProperties();

        assertEquals(12L, properties.parseRetryAfter("12", CLOCK));
        assertEquals(5L, properties.parseRetryAfter("2026-01-01T00:00:05Z", CLOCK));
        assertEquals(5L, properties.parseRetryAfter("Thu, 1 Jan 2026 00:00:05 GMT", CLOCK));
        assertEquals(-1L, properties.parseRetryAfter("-1", CLOCK));
        assertEquals(-1L, properties.parseRetryAfter("not-a-date", CLOCK));
    }

    @Test
    void shouldClampRetryAfter() {
        AiProxyRetryProperties properties = new AiProxyRetryProperties();
        properties.setMaxRetryAfter(Duration.ofSeconds(3));

        assertNull(properties.clampRetryAfter(Duration.ofSeconds(9)));
        assertEquals(Duration.ofSeconds(1), properties.clampRetryAfter(Duration.ofSeconds(1)));
    }

    @ParameterizedTest
    @CsvSource({"0", "1", "29"})
    void shouldRetryStreamingRequestsImmediately(final int retryIndex) {
        final AiProxyRetryProperties properties = new AiProxyRetryProperties(3, 1, Duration.ofSeconds(1));

        assertEquals(Duration.ZERO, properties.retryDelay(true, retryIndex, 0L));
    }

    @ParameterizedTest
    @CsvSource({
        "-1, 1",
        "31, 1",
        "3, -1",
        "3, 31"
    })
    void shouldRejectRetryCountsOutsideTheSupportedRange(final int maxRetries, final int streamingMaxRetries) {
        assertThrows(IllegalArgumentException.class,
                () -> new AiProxyRetryProperties(maxRetries, streamingMaxRetries, Duration.ZERO));
    }

    @Test
    void shouldRejectInvalidRetryBounds() {
        assertThrows(IllegalArgumentException.class, () -> new AiProxyRetryProperties(3, 1, null));
        assertThrows(IllegalArgumentException.class,
                () -> new AiProxyRetryProperties(3, 1, Duration.ofMillis(-1)));
        assertThrows(IllegalArgumentException.class,
                () -> new AiProxyRetryProperties(3, 1, Duration.ZERO, null, 0d,
                        Duration.ZERO, Duration.ofSeconds(1)));
        assertThrows(IllegalArgumentException.class,
                () -> new AiProxyRetryProperties(3, 1, Duration.ZERO, Duration.ZERO, 1.1d,
                        Duration.ZERO, Duration.ofSeconds(1)));
        assertThrows(IllegalArgumentException.class,
                () -> new AiProxyRetryProperties(3, 1, Duration.ZERO, Duration.ZERO, 0d,
                        null, Duration.ofSeconds(1)));
        assertThrows(IllegalArgumentException.class,
                () -> new AiProxyRetryProperties(3, 1, Duration.ZERO, Duration.ZERO, 0d,
                        Duration.ZERO, Duration.ZERO));
    }

    @Test
    void shouldRejectMalformedOperationReplayConfiguration() {
        final AiProxyRetryProperties properties = new AiProxyRetryProperties();
        Map<String, AiProxyRetryProperties.ReplayMode> operations = new HashMap<>();
        operations.put(" ", AiProxyRetryProperties.ReplayMode.NEVER);
        assertThrows(IllegalArgumentException.class, () -> properties.getReplay().setOperations(operations));
    }

    @Test
    void shouldResolveRelaxedSpringOperationNames() {
        final AiProxyRetryProperties properties = new AiProxyRetryProperties();
        properties.getReplay().setOperations(java.util.Map.of(
                "chat-completion", AiProxyRetryProperties.ReplayMode.NEVER));
        assertEquals(AiProxyRetryProperties.ReplayMode.NEVER, properties.getReplay()
                .mode(ShenyuAiRequest.Operation.CHAT_COMPLETION, false, true));
    }

    @ParameterizedTest
    @CsvSource({"-1", "30"})
    void shouldRejectRetryIndexOutsideTheSupportedRange(final int retryIndex) {
        final AiProxyRetryProperties properties = new AiProxyRetryProperties();

        assertThrows(IllegalArgumentException.class, () -> properties.retryDelay(false, retryIndex, 0L));
    }

}
