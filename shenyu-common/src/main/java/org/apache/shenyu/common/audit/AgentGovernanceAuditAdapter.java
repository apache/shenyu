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

import java.util.Objects;

/**
 * Adapts a governance decision into the existing metadata-only audit contract.
 * Shadow would-deny remains an allowed invocation in the audit terminal outcome.
 */
public final class AgentGovernanceAuditAdapter {

    private AgentGovernanceAuditAdapter() {
    }

    public static AgentInvocationAuditEvent toEvent(final AgentRequestContext context, final AgentGovernanceDecision decision) {
        Objects.requireNonNull(context, "context");
        Objects.requireNonNull(decision, "decision");
        boolean denied = decision.deniesTraffic();
        return AgentInvocationAuditEvent.logical()
                .requestId(context.getRequestId())
                .tenant(context.getTenant()).subject(context.getSubject()).apiKeyId(context.getApiKeyId())
                .selectorId(context.getSelectorId()).ruleId(context.getRuleId())
                .configurationVersion(Objects.isNull(context.getConfigurationVersion()) ? "unknown" : context.getConfigurationVersion().toString())
                .policyVersion(decision.getPolicyVersion())
                .decision(denied ? AgentInvocationAuditEvent.Decision.REJECTED : AgentInvocationAuditEvent.Decision.ALLOWED)
                .target(context.getTarget())
                .outcome(denied ? "REJECTED" : "SUCCESS")
                .reason(denied ? decision.getReason().name() : "SHADOW_" + decision.getReason().name())
                .build();
    }
}
