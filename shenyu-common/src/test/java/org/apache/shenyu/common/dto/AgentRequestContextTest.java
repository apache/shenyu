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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;

class AgentRequestContextTest {

    @Test
    void snapshotsPreserveCorrelationWithoutMutatingPreviousStages() {
        AgentRequestContext initial = new AgentRequestContext("request", "mcp", "selector", "rule");
        AgentRequestContext identity = initial.withIdentity("tenant", "subject");
        final AgentRequestContext execution = identity.withConfigurationVersion(7).withTarget("server.tool", "tools/call", "2025-06-18");
        assertNotSame(identity, initial);
        assertNull(initial.getTenant());
        assertNull(initial.getSubject());
        assertNull(identity.getTarget());
        assertNull(identity.getConfigurationVersion());
        assertEquals("request", execution.getRequestId());
        assertEquals("tenant", execution.getTenant());
        assertEquals("subject", execution.getSubject());
        assertEquals("server.tool", execution.getTarget());
        assertEquals("tools/call", execution.getOperation());
        assertEquals("2025-06-18", execution.getProtocolVersion());
        assertEquals(7L, execution.getConfigurationVersion());
        assertNull(execution.getSessionId());
    }

    @Test
    void unassignedIdentityAndSessionRemainUnknown() {
        AgentRequestContext context = new AgentRequestContext("request", "LLM", null, null)
                .withIdentity(null, null).withTarget(null, null, null);
        assertNull(context.getSubject());
        assertNull(context.getTenant());
        assertNull(context.getSessionId());
        assertNull(context.getProtocolVersion());
        assertNull(context.getConfigurationVersion());
    }
}
