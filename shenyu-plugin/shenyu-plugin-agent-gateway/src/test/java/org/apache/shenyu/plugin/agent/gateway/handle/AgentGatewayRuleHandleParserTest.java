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

package org.apache.shenyu.plugin.agent.gateway.handle;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AgentGatewayRuleHandleParserTest {

    private final AgentGatewayRuleHandleParser parser = new AgentGatewayRuleHandleParser();

    @Test
    void shouldParseValidLlmHandle() {
        AgentGatewayRuleHandle handle = parser.parse("{\"trafficType\":\"LLM\",\"responseRequestId\":true}");

        assertTrue(handle.isValid());
        assertEquals("LLM", handle.getTrafficType());
        assertTrue(handle.isResponseRequestId());
    }

    @Test
    void shouldUseFalseAsDefaultForResponseRequestId() {
        AgentGatewayRuleHandle handle = parser.parse("{\"trafficType\":\"LLM\"}");

        assertTrue(handle.isValid());
        assertFalse(handle.isResponseRequestId());
    }

    @Test
    void shouldRejectNonCanonicalTrafficType() {
        AgentGatewayRuleHandle handle = parser.parse("{\"trafficType\":\"llm\"}");

        assertFalse(handle.isValid());
        assertEquals("trafficType must be LLM", handle.getErrorMessage());
    }

    @Test
    void shouldRejectEmptyHandle() {
        AgentGatewayRuleHandle handle = parser.parse(" ");

        assertFalse(handle.isValid());
        assertEquals("rule handle must not be empty", handle.getErrorMessage());
    }

    @Test
    void shouldIgnoreUnknownField() {
        AgentGatewayRuleHandle handle = parser.parse("{\"trafficType\":\"LLM\",\"target\":\"x\"}");

        assertTrue(handle.isValid());
        assertEquals("LLM", handle.getTrafficType());
    }

    @Test
    void shouldRejectNonLlmTraffic() {
        AgentGatewayRuleHandle handle = parser.parse("{\"trafficType\":\"MCP\"}");

        assertFalse(handle.isValid());
        assertEquals("trafficType must be LLM", handle.getErrorMessage());
    }

    @Test
    void shouldRejectWrongBooleanType() {
        AgentGatewayRuleHandle handle = parser.parse("{\"trafficType\":\"LLM\",\"responseRequestId\":\"true\"}");

        assertFalse(handle.isValid());
        assertEquals("responseRequestId must be a boolean", handle.getErrorMessage());
    }
}
