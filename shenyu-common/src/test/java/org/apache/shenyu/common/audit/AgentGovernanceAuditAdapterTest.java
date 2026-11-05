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

package org.apache.shenyu.common.audit;

import org.apache.shenyu.common.dto.AgentRequestContext;
import org.apache.shenyu.common.governance.AgentGovernanceDecision;
import org.junit.jupiter.api.Test;

import java.time.Instant;

import static org.junit.jupiter.api.Assertions.assertEquals;

final class AgentGovernanceAuditAdapterTest {

    @Test
    void shadowWouldDenyRemainsAllowedInAudit() {
        AgentRequestContext context = new AgentRequestContext("request-1", "mcp", "selector-1", "rule-1")
                .withConfigurationVersion(17L).withIdentity("tenant-1", "subject-1")
                .withTarget("server.tool", "tools/call", "2025-06-18").withApiKeyId("key-1");
        AgentInvocationAuditEvent event = AgentGovernanceAuditAdapter.toEvent(context, decision(
                AgentGovernanceDecision.Mode.SHADOW, AgentGovernanceDecision.Result.WOULD_DENY,
                AgentGovernanceDecision.Reason.MCP_INVOCATION_LIMIT));
        assertEquals(AgentInvocationAuditEvent.Decision.ALLOWED, event.getDecision());
        assertEquals("SUCCESS", event.getOutcome());
        assertEquals("SHADOW_MCP_INVOCATION_LIMIT", event.getReason());
    }

    @Test
    void enforceDenyIsAuditedAsRejection() {
        AgentRequestContext context = new AgentRequestContext("request-1", "llm", "selector-1", "rule-1")
                .withTarget("model-a", null, null);
        AgentInvocationAuditEvent event = AgentGovernanceAuditAdapter.toEvent(context, decision(
                AgentGovernanceDecision.Mode.ENFORCE, AgentGovernanceDecision.Result.DENY,
                AgentGovernanceDecision.Reason.COST_BUDGET_EXCEEDED));
        assertEquals(AgentInvocationAuditEvent.Decision.REJECTED, event.getDecision());
        assertEquals("REJECTED", event.getOutcome());
        assertEquals("COST_BUDGET_EXCEEDED", event.getReason());
    }

    private AgentGovernanceDecision decision(final AgentGovernanceDecision.Mode mode,
                                             final AgentGovernanceDecision.Result result,
                                             final AgentGovernanceDecision.Reason reason) {
        return AgentGovernanceDecision.builder(AgentGovernanceDecision.Consumer.LLM_BUDGET, mode)
                .policyVersion("budget-v1").configurationVersion("configuration-1")
                .correlationId("request-1").ledgerGeneration("generation-1")
                .effectiveTime(Instant.parse("2026-10-26T10:00:00Z"))
                .result(result).reason(reason).build();
    }
}
