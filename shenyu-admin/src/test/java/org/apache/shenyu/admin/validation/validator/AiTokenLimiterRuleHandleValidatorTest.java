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

package org.apache.shenyu.admin.validation.validator;

import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Admin-side validation uses the same optional reservation settings as gateways.
 */
class AiTokenLimiterRuleHandleValidatorTest {

    @ParameterizedTest
    @ValueSource(strings = {"{}", "{\"reservationEnabled\":false,\"reservationTokens\":-1}",
        "{\"reservationEnabled\":true,\"reservationTokens\":20}", "{\"reservationEnabled\":\"true\",\"reservationTokens\":20}",
        "{\"reservationEnabled\":true,\"reservationTokens\":20,\"tokenLimit\":null,\"timeWindowSeconds\":null}"})
    void testCompatibleAndValidRulesAreAccepted(final String handle) {
        assertDoesNotThrow(() -> AiTokenLimiterRuleHandleValidator.validate(handle));
    }

    @ParameterizedTest
    @ValueSource(strings = {"{\"reservationEnabled\":true}", "{\"reservationEnabled\":true,\"reservationTokens\":0}",
        "{\"reservationEnabled\":true,\"reservationTokens\":-1}", "{\"reservationEnabled\":true,\"reservationTokens\":101}",
        "{\"reservationEnabled\":true,\"reservationTokens\":10,\"tokenLimit\":9007199254740992}",
        "{\"reservationEnabled\":true,\"reservationTokens\":10,\"timeWindowSeconds\":0}",
        "{\"reservationEnabled\":true,\"reservationTokens\":10,\"timeWindowSeconds\":9007199254741}", "not-json"})
    void testInvalidSettingsAreRejected(final String handle) {
        assertThrows(ShenyuAdminException.class, () -> AiTokenLimiterRuleHandleValidator.validate(handle));
    }

    @Test
    void testMissingLegacyHandlesRemainCompatible() {
        assertDoesNotThrow(() -> AiTokenLimiterRuleHandleValidator.validate(null));
        assertDoesNotThrow(() -> AiTokenLimiterRuleHandleValidator.validate(" "));
    }
}
