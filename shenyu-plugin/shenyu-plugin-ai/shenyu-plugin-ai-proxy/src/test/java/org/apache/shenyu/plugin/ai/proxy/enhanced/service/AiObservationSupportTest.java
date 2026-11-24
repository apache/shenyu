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

package org.apache.shenyu.plugin.ai.proxy.enhanced.service;

import org.apache.shenyu.common.dto.AgentRequestContext;
import org.junit.jupiter.api.Test;
import reactor.util.context.Context;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AiObservationSupportTest {

    @Test
    void invalidIdReplacementPreservesAttributionFacts() {
        AgentRequestContext existing = new AgentRequestContext("bad id", "llm", "old-selector", "old-rule")
                .withIdentity("tenant", "subject")
                .withTarget("target", "operation", "v1")
                .withConfigurationVersion(7L)
                .withApiKeyId("api-key");

        AgentRequestContext replaced = AiObservationSupport.context(
                Context.of(AgentRequestContext.REACTOR_KEY, existing), "proxy-selector", "proxy-rule");

        assertNotSame(existing, replaced);
        assertNotEquals("bad id", replaced.getRequestId());
        assertTrue(AgentRequestContext.isReusableRequestId(replaced.getRequestId()));
        assertEquals("proxy-selector", replaced.getSelectorId());
        assertEquals("proxy-rule", replaced.getRuleId());
        assertEquals("tenant", replaced.getTenant());
        assertEquals("subject", replaced.getSubject());
        assertEquals("target", replaced.getTarget());
        assertEquals("operation", replaced.getOperation());
        assertEquals("v1", replaced.getProtocolVersion());
        assertEquals(Long.valueOf(7L), replaced.getConfigurationVersion());
        assertEquals("api-key", replaced.getApiKeyId());
    }

    @Test
    void invalidIdReplacementWithoutOwnerKeepsExistingRouteIdentity() {
        AgentRequestContext existing = new AgentRequestContext("bad id", "llm", "old-selector", "old-rule")
                .withIdentity("tenant", "subject");

        AgentRequestContext replaced = AiObservationSupport.context(
                Context.of(AgentRequestContext.REACTOR_KEY, existing), null, null);

        assertNotEquals("bad id", replaced.getRequestId());
        assertEquals("old-selector", replaced.getSelectorId());
        assertEquals("old-rule", replaced.getRuleId());
        assertEquals("tenant", replaced.getTenant());
        assertEquals("subject", replaced.getSubject());
    }

    @Test
    void validIdRefreshesRouteIdentityAndPreservesAttribution() {
        AgentRequestContext existing = new AgentRequestContext("existing", "llm", "old-selector", "old-rule")
                .withApiKeyId("api-key");

        AgentRequestContext refreshed = AiObservationSupport.context(
                Context.of(AgentRequestContext.REACTOR_KEY, existing), "proxy-selector", "proxy-rule");

        assertEquals("existing", refreshed.getRequestId());
        assertEquals("proxy-selector", refreshed.getSelectorId());
        assertEquals("proxy-rule", refreshed.getRuleId());
        assertEquals("api-key", refreshed.getApiKeyId());
    }
}
