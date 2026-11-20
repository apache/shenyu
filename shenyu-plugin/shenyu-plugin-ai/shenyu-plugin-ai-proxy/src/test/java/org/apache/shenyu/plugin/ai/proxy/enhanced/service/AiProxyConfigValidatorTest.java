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

import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

class AiProxyConfigValidatorTest {

    @Test
    void testOptionalFieldsDoNotRequireACompleteConfiguration() {
        AiCommonConfig config = new AiCommonConfig();
        AiCommonConfig original = new AiCommonConfig(config);
        assertDoesNotThrow(() -> AiProxyConfigValidator.validate(config));
        assertEquals(original, config);
    }

    @ParameterizedTest
    @ValueSource(ints = {1, Integer.MAX_VALUE})
    void testPositiveMaxTokens(final int value) {
        AiCommonConfig config = new AiCommonConfig();
        config.setMaxTokens(value);
        assertDoesNotThrow(() -> AiProxyConfigValidator.validate(config));
        assertEquals(value, config.getMaxTokens());
    }

    @ParameterizedTest
    @ValueSource(ints = {0, -1, Integer.MIN_VALUE})
    void testNonPositiveMaxTokens(final int value) {
        AiCommonConfig config = new AiCommonConfig();
        config.setMaxTokens(value);
        assertEquals("maxTokens.nonPositive", assertThrows(IllegalArgumentException.class,
                () -> AiProxyConfigValidator.validate(config)).getMessage());
    }

    @ParameterizedTest
    @ValueSource(doubles = {0, 1.5, 3, Double.MAX_VALUE})
    void testFiniteNonNegativeTemperatureHasNoCommonUpperBound(final double value) {
        AiCommonConfig config = new AiCommonConfig();
        config.setTemperature(value);
        assertDoesNotThrow(() -> AiProxyConfigValidator.validate(config));
        assertEquals(value, config.getTemperature());
    }

    @ParameterizedTest
    @ValueSource(doubles = {-0.1, Double.NaN, Double.POSITIVE_INFINITY, Double.NEGATIVE_INFINITY})
    void testInvalidTemperature(final double value) {
        AiCommonConfig config = new AiCommonConfig();
        config.setTemperature(value);
        assertEquals(Double.isFinite(value) ? "temperature.negative" : "temperature.notFinite",
                assertThrows(IllegalArgumentException.class, () -> AiProxyConfigValidator.validate(config)).getMessage());
    }

    @ParameterizedTest
    @NullAndEmptySource
    @ValueSource(strings = {" ", "https://example.org/a%20b", "HTTP://example.org:1/v1", "https://example.org:65535", "http://127.0.0.1:80", "https://[::1]:443/v1"})
    void testOptionalHttpEndpointsAreAcceptedWithoutChangingValues(final String value) {
        AiCommonConfig config = new AiCommonConfig();
        config.setBaseUrl(value);
        assertDoesNotThrow(() -> AiProxyConfigValidator.validate(config));
        assertEquals(value, config.getBaseUrl());
    }

    @ParameterizedTest
    @ValueSource(strings = {"abc", "ftp://example.org", "https:///v1", "https://test-user:test-secret@example.org",
            "https://example.org/?key=test-secret", "https://example.org/#test-secret", "https://example.org:0", "https://example.org:65536"})
    void testInvalidEndpointDoesNotExposeItsValue(final String value) {
        AiCommonConfig config = new AiCommonConfig();
        config.setBaseUrl(value);
        IllegalArgumentException error = assertThrows(IllegalArgumentException.class, () -> AiProxyConfigValidator.validate(config));
        assertEquals("baseUrl.invalidEndpoint", error.getMessage());
        assertNull(error.getCause());
    }

    @ParameterizedTest
    @ValueSource(strings = {"https://example.org/%ZZ", "https://test-user:test-secret@example.org/bad path"})
    void testMalformedUriDoesNotExposeItsValue(final String value) {
        AiCommonConfig config = new AiCommonConfig();
        config.setBaseUrl(value);
        IllegalArgumentException error = assertThrows(IllegalArgumentException.class, () -> AiProxyConfigValidator.validate(config));
        assertEquals("baseUrl.invalidSyntax", error.getMessage());
        assertNull(error.getCause());
    }
}
