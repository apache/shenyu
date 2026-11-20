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

package org.apache.shenyu.plugin.mcp.server.transport;

import org.junit.jupiter.api.Test;

import java.time.Duration;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Tests for the documented defaults and validation of {@link SseSessionPolicy}.
 */
class SseSessionPolicyTest {

    @Test
    void defaultsAreConservativeAndDocumented() {
        SseSessionPolicy policy = SseSessionPolicy.defaults();
        assertEquals(SseSessionPolicy.DEFAULT_MAX_PENDING_EVENTS, policy.getMaxPendingEvents());
        assertEquals(SseSessionPolicy.DEFAULT_MAX_PENDING_BYTES, policy.getMaxPendingBytes());
        assertEquals(SseSessionPolicy.DEFAULT_STALL_TIMEOUT, policy.getStallTimeout());
    }

    @Test
    void builderProducesConfiguredLimits() {
        SseSessionPolicy policy = SseSessionPolicy.builder()
                .maxPendingEvents(7)
                .maxPendingBytes(2048)
                .stallTimeout(Duration.ofMillis(500))
                .build();
        assertEquals(7, policy.getMaxPendingEvents());
        assertEquals(2048, policy.getMaxPendingBytes());
        assertEquals(Duration.ofMillis(500), policy.getStallTimeout());
    }

    @Test
    void nonPositiveEventLimitIsRejected() {
        assertThrows(IllegalArgumentException.class, () -> SseSessionPolicy.builder()
                .maxPendingEvents(0).build());
    }

    @Test
    void nonPositiveByteLimitIsRejected() {
        assertThrows(IllegalArgumentException.class, () -> SseSessionPolicy.builder()
                .maxPendingBytes(-1).build());
    }

    @Test
    void nonPositiveStallTimeoutIsRejected() {
        assertThrows(IllegalArgumentException.class, () -> SseSessionPolicy.builder()
                .stallTimeout(Duration.ZERO).build());
        assertThrows(IllegalArgumentException.class, () -> SseSessionPolicy.builder()
                .stallTimeout(null).build());
        assertNotNull(SseSessionPolicy.builder().stallTimeout(Duration.ofSeconds(1)).build());
    }
}
