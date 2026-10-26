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

import java.time.Duration;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Test cases for {@link AiProxyRetryProperties}.
 */
class AiProxyRetryPropertiesTest {

    @Test
    void shouldProvideCompatibleDefaults() {
        final AiProxyRetryProperties properties = new AiProxyRetryProperties();

        assertEquals(3, properties.maxRetries(false));
        assertEquals(1, properties.maxRetries(true));
        assertEquals(Duration.ofSeconds(1), properties.getInitialBackoff());
    }

    @ParameterizedTest
    @CsvSource({
        "0, 1000",
        "1, 2000",
        "2, 4000"
    })
    void shouldCalculateExponentialBackoff(final int retryIndex, final long expectedMillis) {
        final AiProxyRetryProperties properties = new AiProxyRetryProperties(3, 1, Duration.ofSeconds(1));

        assertEquals(Duration.ofMillis(expectedMillis), properties.retryDelay(false, retryIndex));
    }

    @ParameterizedTest
    @CsvSource({"0", "1", "29"})
    void shouldRetryStreamingRequestsImmediately(final int retryIndex) {
        final AiProxyRetryProperties properties = new AiProxyRetryProperties(3, 1, Duration.ofSeconds(1));

        assertEquals(Duration.ZERO, properties.retryDelay(true, retryIndex));
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
    void shouldRejectInvalidBackoff() {
        assertThrows(IllegalArgumentException.class, () -> new AiProxyRetryProperties(3, 1, null));
        assertThrows(IllegalArgumentException.class,
                () -> new AiProxyRetryProperties(3, 1, Duration.ofMillis(-1)));
    }

    @ParameterizedTest
    @CsvSource({"-1", "30"})
    void shouldRejectRetryIndexOutsideTheSupportedRange(final int retryIndex) {
        final AiProxyRetryProperties properties = new AiProxyRetryProperties();

        assertThrows(IllegalArgumentException.class, () -> properties.retryDelay(false, retryIndex));
    }
}
