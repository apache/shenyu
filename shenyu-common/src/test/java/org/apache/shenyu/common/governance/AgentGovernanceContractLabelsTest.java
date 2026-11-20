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

import java.time.Instant;

import static org.junit.jupiter.api.Assertions.assertEquals;

final class AgentGovernanceContractLabelsTest {

    @Test
    void exposesOnlyBoundedContractLabels() {
        AgentGovernanceDecision decision = AgentGovernanceDecision.builder(
                        AgentGovernanceDecision.Consumer.LLM_BUDGET, AgentGovernanceDecision.Mode.SHADOW)
                .policyVersion("token-budget-v1").configurationVersion("configuration-1")
                .correlationId("request-with-tenant-and-trace").ledgerGeneration("generation-1")
                .effectiveTime(Instant.parse("2026-10-26T10:00:00Z"))
                .result(AgentGovernanceDecision.Result.WOULD_DENY).reason(AgentGovernanceDecision.Reason.TOKEN_BUDGET_EXCEEDED).build();
        AgentGovernanceContractLabels labels = AgentGovernanceContractLabels.from(decision);
        assertEquals("LLM_BUDGET", labels.getConsumer());
        assertEquals("SHADOW", labels.getMode());
        assertEquals("WOULD_DENY", labels.getResult());
        assertEquals("TOKEN_BUDGET_EXCEEDED", labels.getReason());
    }
}
