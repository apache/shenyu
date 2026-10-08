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
 * Tests for agent gateway rule handle validation at the Admin write boundary.
 */
public final class AgentGatewayRuleHandleValidatorTest {

    @Test
    public void testValidHandles() {
        assertDoesNotThrow(() -> AgentGatewayRuleHandleValidator.validate("{\"trafficType\":\"LLM\"}"));
        assertDoesNotThrow(() -> AgentGatewayRuleHandleValidator.validate("{\"trafficType\":\"LLM\",\"responseRequestId\":false}"));
    }

    @ParameterizedTest
    @ValueSource(strings = {
        "",
        "not-json",
        "[]",
        "{}",
        "{\"trafficType\":\"MCP\"}",
        "{\"trafficType\":null}",
        "{\"trafficType\":true}",
        "{\"trafficType\":\"LLM\",\"responseRequestId\":\"true\"}",
        "{\"trafficType\":\"LLM\",\"responseRequestId\":null}",
        "{\"trafficType\":\"LLM\",\"extra\":42}"
    })
    public void testInvalidHandles(final String handle) {
        assertThrows(ShenyuAdminException.class, () -> AgentGatewayRuleHandleValidator.validate(handle));
    }

    @Test
    public void testNullHandle() {
        assertThrows(ShenyuAdminException.class, () -> AgentGatewayRuleHandleValidator.validate(null));
    }
}
