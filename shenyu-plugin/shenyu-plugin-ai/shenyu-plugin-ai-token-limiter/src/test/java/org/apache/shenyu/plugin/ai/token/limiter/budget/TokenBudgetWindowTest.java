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

package org.apache.shenyu.plugin.ai.token.limiter.budget;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;

import java.time.Duration;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Fixed window boundary, scope isolation and exact integer tests.
 */
class TokenBudgetWindowTest {

    @ParameterizedTest
    @CsvSource({"0,0,60000", "59999,0,60000", "60000,60000,120000", "119999,60000,120000", "120000,120000,180000"})
    void testStartIsInclusiveAndEndIsExclusive(final long now, final long expectedStart, final long expectedEnd) {
        final TokenBudgetWindow window = TokenBudgetWindow.at("scope", now, 60_000L);

        assertEquals(expectedStart, window.getStartMillis());
        assertEquals(expectedEnd, window.getEndMillis());
        assertEquals(TokenBudgetWindow.at("scope", expectedStart, 60_000L).getKey(), window.getKey());
    }

    @Test
    void testScopeAndWindowSizeArePartOfKeyAndBracesCannotChangeHashTag() {
        final String scope = "rule:{attacker}:用户";
        final String key = TokenBudgetWindow.at(scope, 1L, 60_000L).getKey();

        assertEquals(key, TokenBudgetWindow.at(scope, 59_999L, 60_000L).getKey());
        assertNotEquals(key, TokenBudgetWindow.at(scope + "other", 1L, 60_000L).getKey());
        assertNotEquals(key, TokenBudgetWindow.at(scope, 1L, 120_000L).getKey());
        assertNotEquals(key, TokenBudgetWindow.at(scope, 60_000L, 60_000L).getKey());
        assertFalse(key.contains("attacker"));
        assertEquals(64, key.substring(key.indexOf('{') + 1, key.indexOf('}')).length());
    }

    @ParameterizedTest
    @ValueSource(longs = {0L, -1L, 9_007_199_254_740_992L})
    void testRejectsInvalidWindowSize(final long millis) {
        assertThrows(IllegalArgumentException.class, () -> TokenBudgetWindow.validateWindowSize("scope", Duration.ofMillis(millis)));
    }

    @Test
    void testRejectsFractionalMillisecondsAndDurationOverflow() {
        assertThrows(IllegalArgumentException.class, () -> TokenBudgetWindow.validateWindowSize("scope", Duration.ofNanos(1L)));
        assertThrows(IllegalArgumentException.class, () -> TokenBudgetWindow.validateWindowSize("scope", Duration.ofMillis(1L).plusNanos(1L)));
        assertThrows(IllegalArgumentException.class, () -> TokenBudgetWindow.validateWindowSize("scope", Duration.ofSeconds(Long.MAX_VALUE)));
    }

    @Test
    void testRejectsInvalidClockAndWindowEndOverflow() {
        assertThrows(IllegalArgumentException.class, () -> TokenBudgetWindow.at("scope", -1L, 60_000L));
        assertThrows(IllegalArgumentException.class, () -> TokenBudgetWindow.at("scope", TokenBudgetWindow.MAX_EXACT_INTEGER + 1L, 60_000L));
        assertThrows(IllegalArgumentException.class, () -> TokenBudgetWindow.at("scope", TokenBudgetWindow.MAX_EXACT_INTEGER - 1L, 2L));
        assertEquals(TokenBudgetWindow.MAX_EXACT_INTEGER, TokenBudgetWindow.at("scope", 0L, TokenBudgetWindow.MAX_EXACT_INTEGER).getEndMillis());
    }
}
