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
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests for websocket rule handle validation at the Admin write boundary.
 */
public final class WebSocketRuleHandleValidatorTest {

    @Test
    public void testAcceptedHandles() {
        assertDoesNotThrow(() -> WebSocketRuleHandleValidator.validate(null));
        assertDoesNotThrow(() -> WebSocketRuleHandleValidator.validate(""));
        assertDoesNotThrow(() -> WebSocketRuleHandleValidator.validate("{}"));
        assertDoesNotThrow(() -> WebSocketRuleHandleValidator.validate("{\"timeout\":3000,\"idleTimeout\":0}"));
        assertDoesNotThrow(() -> WebSocketRuleHandleValidator.validate("{\"timeout\":0,\"idleTimeout\":60000}"));
        assertDoesNotThrow(() -> WebSocketRuleHandleValidator.validate("{\"loadBalance\":\"random\",\"retry\":2}"));
    }

    @ParameterizedTest
    @ValueSource(strings = {
        "not-json",
        "[]",
        "\"test\"",
        "{\"timeout\":-1}",
        "{\"timeout\":3000,\"idleTimeout\":-5}",
        "{\"idleTimeout\":-30000}"
    })
    public void testRejectedHandles(final String handle) {
        assertThrows(ShenyuAdminException.class, () -> WebSocketRuleHandleValidator.validate(handle));
    }

    @Test
    public void testMessageNamesTheRejectedField() {
        final ShenyuAdminException timeoutError = assertThrows(ShenyuAdminException.class,
            () -> WebSocketRuleHandleValidator.validate("{\"timeout\":-1}"));
        assertTrue(timeoutError.getMessage().contains("timeout"));
        final ShenyuAdminException idleError = assertThrows(ShenyuAdminException.class,
            () -> WebSocketRuleHandleValidator.validate("{\"idleTimeout\":-1}"));
        assertTrue(idleError.getMessage().contains("idleTimeout"));
    }
}
