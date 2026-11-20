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

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;

import java.time.Duration;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class AgentGatewayRefreshPolicyTest {

    @ParameterizedTest
    @CsvSource({"0, 60", "1, 1", "2, 2", "3, 4", "4, 60", "5, 60", "2147483647, 60"})
    void defaultsBoundFastRetriesAndKeepSlowRecoveryProbes(final int failures, final long seconds) {
        assertEquals(Duration.ofSeconds(seconds), AgentGatewayRefreshPolicy.defaults().nextDelay(failures));
    }

    @ParameterizedTest
    @CsvSource({"1, 3", "2, 6", "3, 10", "4, 10", "5, 10", "6, 10", "7, 10", "8, 10", "9, 60"})
    void exponentialBackoffStopsAtConfiguredCeiling(final int failures, final long seconds) {
        AgentGatewayRefreshPolicy policy = new AgentGatewayRefreshPolicy(
                Duration.ofSeconds(60), Duration.ofSeconds(3), Duration.ofSeconds(10), 8);
        assertEquals(Duration.ofSeconds(seconds), policy.nextDelay(failures));
    }

    @Test
    void zeroRetriesUsesNormalIntervalAfterFailure() {
        AgentGatewayRefreshPolicy policy = new AgentGatewayRefreshPolicy(
                Duration.ofSeconds(60), Duration.ofSeconds(1), Duration.ofSeconds(30), 0);
        assertEquals(Duration.ofSeconds(60), policy.nextDelay(1));
        assertEquals(Duration.ofSeconds(60), policy.nextDelay(Integer.MAX_VALUE));
    }

    @ParameterizedTest
    @ValueSource(longs = {-1, 0, 4999, 3600001})
    void rejectsIntervalsOutsideBounds(final long millis) {
        assertThrows(IllegalArgumentException.class, () -> new AgentGatewayRefreshPolicy(
                Duration.ofMillis(millis), Duration.ofSeconds(1), Duration.ofSeconds(2), 3));
    }

    @ParameterizedTest
    @ValueSource(longs = {5, 3600})
    void acceptsIntervalBoundaries(final long seconds) {
        AgentGatewayRefreshPolicy policy = new AgentGatewayRefreshPolicy(
                Duration.ofSeconds(seconds), Duration.ofSeconds(1), Duration.ofSeconds(seconds), 3);
        assertEquals(Duration.ofSeconds(seconds), policy.nextDelay(0));
    }

    @ParameterizedTest
    @CsvSource({"0, 1", "-1, 1", "3, 2", "1, 61"})
    void rejectsInvalidBackoffBounds(final long initial, final long maximum) {
        assertThrows(IllegalArgumentException.class, () -> new AgentGatewayRefreshPolicy(
                Duration.ofSeconds(60), Duration.ofSeconds(initial), Duration.ofSeconds(maximum), 3));
    }

    @Test
    void acceptsEqualBackoffAndInterval() {
        AgentGatewayRefreshPolicy policy = new AgentGatewayRefreshPolicy(
                Duration.ofSeconds(5), Duration.ofSeconds(5), Duration.ofSeconds(5), 8);
        assertEquals(Duration.ofSeconds(5), policy.nextDelay(1));
        assertEquals(Duration.ofSeconds(5), policy.nextDelay(8));
    }

    @ParameterizedTest
    @ValueSource(ints = {-1, 9, Integer.MAX_VALUE})
    void rejectsInvalidRetryBudget(final int retries) {
        assertThrows(IllegalArgumentException.class, () -> new AgentGatewayRefreshPolicy(
                Duration.ofSeconds(60), Duration.ofSeconds(1), Duration.ofSeconds(30), retries));
    }

    @Test
    void rejectsMissingDurations() {
        assertThrows(IllegalArgumentException.class, () -> new AgentGatewayRefreshPolicy(
                null, Duration.ofSeconds(1), Duration.ofSeconds(30), 3));
        assertThrows(IllegalArgumentException.class, () -> new AgentGatewayRefreshPolicy(
                Duration.ofSeconds(60), null, Duration.ofSeconds(30), 3));
        assertThrows(IllegalArgumentException.class, () -> new AgentGatewayRefreshPolicy(
                Duration.ofSeconds(60), Duration.ofSeconds(1), null, 3));
    }

    @ParameterizedTest
    @ValueSource(ints = {-1, Integer.MIN_VALUE})
    void rejectsNegativeFailureCount(final int failures) {
        assertThrows(IllegalArgumentException.class, () -> AgentGatewayRefreshPolicy.defaults().nextDelay(failures));
    }
}
