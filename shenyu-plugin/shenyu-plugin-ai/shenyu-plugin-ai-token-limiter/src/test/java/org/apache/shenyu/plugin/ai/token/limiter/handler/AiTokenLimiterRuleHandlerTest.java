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

package org.apache.shenyu.plugin.ai.token.limiter.handler;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.convert.rule.AiTokenLimiterHandle;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Reservation rule defaults, validation and preservation of the last valid rule.
 */
class AiTokenLimiterRuleHandlerTest {

    private final RuleData rule = RuleData.builder().selectorId("reservation-config-selector").id("reservation-config-rule").build();

    private final AiTokenLimiterPluginHandler handler = new AiTokenLimiterPluginHandler();

    @AfterEach
    void removeOwnedRule() {
        AiTokenLimiterPluginHandler.CACHED_HANDLE.get().removeHandle(CacheKeyUtils.INST.getKey(rule));
    }

    @Test
    void testLegacyAndDisabledRulesDoNotRequireReservationAmount() {
        rule.setHandle("{\"aiTokenLimitType\":\"uri\"}");
        handler.handlerRule(rule);
        assertFalse(cached().isReservationEnabled());
        assertEquals(Long.valueOf(100L), cached().getTokenLimit());

        rule.setHandle("{\"reservationEnabled\":false,\"reservationTokens\":-1}");
        handler.handlerRule(rule);
        assertFalse(cached().isReservationEnabled());
    }

    @Test
    void testEnabledRuleRequiresExplicitReservationButUsesLegacyDefaults() {
        rule.setHandle("{\"reservationEnabled\":true,\"reservationTokens\":20}");
        handler.handlerRule(rule);

        assertTrue(cached().isReservationEnabled());
        assertEquals(Long.valueOf(20L), cached().getReservationTokens());
        assertEquals(Long.valueOf(60L), cached().getTimeWindowSeconds());
        assertEquals(Long.valueOf(100L), cached().getTokenLimit());
        assertNotNull(cached().getAiTokenLimitType());
    }

    @Test
    void testMissingReservationAmountCannotEnableMode() {
        rule.setHandle("{\"reservationEnabled\":true}");
        assertThrows(IllegalArgumentException.class, () -> handler.handlerRule(rule));
    }

    @Test
    void testBooleanDictionaryStringEnablesReservationMode() {
        rule.setHandle("{\"reservationEnabled\":\"true\",\"reservationTokens\":20}");
        handler.handlerRule(rule);

        assertTrue(cached().isReservationEnabled());
        assertEquals(Long.valueOf(20L), cached().getReservationTokens());
    }

    @ParameterizedTest
    @CsvSource({"0,10,60", "-1,10,60", "9007199254740992,10,60", "100,0,60", "100,-1,60", "100,101,60",
        "100,10,0", "100,10,-1", "100,10,9007199254741"})
    void testInvalidReservationUpdatePreservesLastValidRule(final long limit, final long amount, final long seconds) {
        rule.setHandle("{\"reservationEnabled\":true,\"reservationTokens\":10}");
        handler.handlerRule(rule);
        final AiTokenLimiterHandle previous = cached();
        rule.setHandle("{\"reservationEnabled\":true,\"reservationTokens\":" + amount + ",\"tokenLimit\":" + limit + ",\"timeWindowSeconds\":" + seconds + "}");

        assertThrows(IllegalArgumentException.class, () -> handler.handlerRule(rule));
        assertSame(previous, cached());
    }

    private AiTokenLimiterHandle cached() {
        return AiTokenLimiterPluginHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(rule));
    }
}
