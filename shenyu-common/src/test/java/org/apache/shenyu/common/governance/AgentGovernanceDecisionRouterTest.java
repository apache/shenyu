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

package org.apache.shenyu.common.governance;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

import java.time.Instant;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

final class AgentGovernanceDecisionRouterTest {

    private final Instant effectiveTime = Instant.parse("2026-10-26T10:00:00Z");

    @Test
    void shadowPolicyDenialIsReportedWithoutTrafficDenial() {
        AgentGovernanceDecision decision = router(AgentGovernanceDecision.Mode.SHADOW)
                .deny("request-1", effectiveTime, AgentGovernanceDecision.Reason.MCP_CONCURRENCY_LIMIT);
        assertEquals(AgentGovernanceDecision.Result.WOULD_DENY, decision.getResult());
        assertFalse(decision.deniesTraffic());
    }

    @Test
    void enforcePolicyDenialCanRejectTraffic() {
        AgentGovernanceDecision decision = router(AgentGovernanceDecision.Mode.ENFORCE)
                .deny("request-1", effectiveTime, AgentGovernanceDecision.Reason.TOKEN_BUDGET_EXCEEDED);
        assertEquals(AgentGovernanceDecision.Result.DENY, decision.getResult());
        assertTrue(decision.deniesTraffic());
    }

    @Test
    void llmBudgetUsesTheSameDecisionSemantics() {
        AgentGovernanceDecisionRouter router = new AgentGovernanceDecisionRouter(
                AgentGovernanceDecision.Consumer.LLM_BUDGET, AgentGovernanceDecision.Mode.ENFORCE,
                "token-budget-v1", "configuration-1", "enforce-generation-1");
        AgentGovernanceDecision decision = router.deny("request-1", effectiveTime,
                AgentGovernanceDecision.Reason.COST_BUDGET_EXCEEDED);
        assertEquals(AgentGovernanceDecision.Consumer.LLM_BUDGET, decision.getConsumer());
        assertEquals(AgentGovernanceDecision.Result.DENY, decision.getResult());
    }

    @Test
    void mandatoryRejectionStaysEnforcedInShadowMode() {
        AgentGovernanceDecision decision = router(AgentGovernanceDecision.Mode.SHADOW)
                .mandatoryDeny("request-1", effectiveTime, AgentGovernanceDecision.Reason.AUTHORIZATION_REJECTED);
        assertEquals(AgentGovernanceDecision.Mode.ENFORCE, decision.getMode());
        assertTrue(decision.deniesTraffic());
    }

    @ParameterizedTest
    @EnumSource(value = AgentGovernanceDecision.Reason.class, names = {
            "AUTHENTICATION_REJECTED", "AUTHORIZATION_REJECTED", "RESOURCE_LIMIT_REJECTED"})
    void mandatorySecurityAndResourceReasonsCannotBeShadowed(final AgentGovernanceDecision.Reason reason) {
        AgentGovernanceDecision decision = router(AgentGovernanceDecision.Mode.SHADOW)
                .mandatoryDeny("request-1", effectiveTime, reason);
        assertEquals(AgentGovernanceDecision.Mode.ENFORCE, decision.getMode());
        assertEquals(AgentGovernanceDecision.Result.DENY, decision.getResult());
        assertTrue(decision.deniesTraffic());
    }

    @Test
    void shadowAndEnforceOutcomesHaveExplicitComparisonSemantics() {
        AgentGovernanceDecisionRouter router = new AgentGovernanceDecisionRouter(
                AgentGovernanceDecision.Consumer.MCP_QUOTA, AgentGovernanceDecision.Mode.SHADOW,
                "mcp-quota-v1", "configuration-1", "generation-1");
        AgentGovernanceDecision shadowWouldDeny = AgentGovernanceDecision.builder(
                AgentGovernanceDecision.Consumer.MCP_QUOTA, AgentGovernanceDecision.Mode.SHADOW)
                .policyVersion("mcp-quota-v1").configurationVersion("configuration-1")
                .correlationId("request-1").ledgerGeneration("generation-1").effectiveTime(effectiveTime)
                .result(AgentGovernanceDecision.Result.WOULD_DENY)
                .reason(AgentGovernanceDecision.Reason.MCP_CONCURRENCY_LIMIT).build();
        AgentGovernanceDecision enforceAllow = AgentGovernanceDecision.builder(
                AgentGovernanceDecision.Consumer.MCP_QUOTA, AgentGovernanceDecision.Mode.ENFORCE)
                .policyVersion("mcp-quota-v1").configurationVersion("configuration-1")
                .correlationId("request-1").ledgerGeneration("generation-1").effectiveTime(effectiveTime)
                .result(AgentGovernanceDecision.Result.ALLOW).reason(AgentGovernanceDecision.Reason.ALLOWED).build();
        assertEquals(AgentGovernanceDecisionRouter.Comparison.SHADOW_STRICTER,
                router.compare(shadowWouldDeny, enforceAllow));
    }

    @Test
    void rejectInvalidDecisionCombinations() {
        assertThrows(IllegalArgumentException.class, () -> AgentGovernanceDecision.builder(
                AgentGovernanceDecision.Consumer.MCP_QUOTA, AgentGovernanceDecision.Mode.SHADOW)
                .correlationId("request-1").result(AgentGovernanceDecision.Result.DENY)
                .reason(AgentGovernanceDecision.Reason.MCP_INVOCATION_LIMIT).build());
        assertThrows(IllegalArgumentException.class, () -> AgentGovernanceDecision.builder(
                AgentGovernanceDecision.Consumer.LLM_BUDGET, AgentGovernanceDecision.Mode.ENFORCE)
                .correlationId("request-1").result(AgentGovernanceDecision.Result.WOULD_DENY)
                .reason(AgentGovernanceDecision.Reason.COST_BUDGET_EXCEEDED).build());
        assertThrows(IllegalArgumentException.class, () -> AgentGovernanceDecision.builder(
                AgentGovernanceDecision.Consumer.LLM_BUDGET, AgentGovernanceDecision.Mode.SHADOW)
                .correlationId("request-1").result(AgentGovernanceDecision.Result.ALLOW)
                .reason(AgentGovernanceDecision.Reason.RESERVATION_EXPIRED).build());
    }

    private AgentGovernanceDecisionRouter router(final AgentGovernanceDecision.Mode mode) {
        return new AgentGovernanceDecisionRouter(AgentGovernanceDecision.Consumer.MCP_QUOTA, mode,
                "mcp-quota-v1", "configuration-1", "shadow-generation-1");
    }
}
