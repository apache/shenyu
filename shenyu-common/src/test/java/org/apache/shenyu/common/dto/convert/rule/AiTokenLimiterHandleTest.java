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

package org.apache.shenyu.common.dto.convert.rule;

import org.apache.shenyu.common.utils.GsonUtils;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Reservation configuration compatibility and serialization tests.
 */
class AiTokenLimiterHandleTest {

    @Test
    void testLegacyJsonKeepsReservationDisabled() {
        final AiTokenLimiterHandle handle = GsonUtils.getInstance().fromJson(
                "{\"aiTokenLimitType\":\"uri\",\"timeWindowSeconds\":60,\"tokenLimit\":100,\"keyName\":\"default\"}", AiTokenLimiterHandle.class);

        assertFalse(handle.isReservationEnabled());
        assertNull(handle.getReservationTokens());
        assertFalse(AiTokenLimiterHandle.newDefaultInstance().isReservationEnabled());
    }

    @Test
    void testExplicitReservationConfigurationSurvivesJsonRoundTrip() {
        final AiTokenLimiterHandle handle = AiTokenLimiterHandle.newDefaultInstance();
        handle.setReservationEnabled(true);
        handle.setReservationTokens(20L);

        final AiTokenLimiterHandle decoded = GsonUtils.getInstance().fromJson(GsonUtils.getInstance().toJson(handle), AiTokenLimiterHandle.class);

        assertTrue(decoded.isReservationEnabled());
        assertEquals(Long.valueOf(20L), decoded.getReservationTokens());
        assertEquals(handle, decoded);
        assertEquals(handle.hashCode(), decoded.hashCode());
    }

    @Test
    void testReservationPolicyParticipatesInEquality() {
        final AiTokenLimiterHandle first = AiTokenLimiterHandle.newDefaultInstance();
        final AiTokenLimiterHandle second = AiTokenLimiterHandle.newDefaultInstance();
        assertEquals(first, second);
        first.setReservationEnabled(true);
        assertNotEquals(first, second);
        second.setReservationEnabled(true);
        first.setReservationTokens(10L);
        assertNotEquals(first, second);
        second.setReservationTokens(10L);
        assertEquals(first, second);
        assertEquals(first.hashCode(), second.hashCode());
    }
}
