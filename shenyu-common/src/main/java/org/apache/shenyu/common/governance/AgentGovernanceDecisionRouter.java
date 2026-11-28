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

import java.time.Instant;
import java.util.Objects;

/**
 * Normalizes a policy evaluation without hiding mandatory security or resource failures.
 * Shadow mode changes only the reported result; callers must not debit production state.
 */
public final class AgentGovernanceDecisionRouter {

    private final AgentGovernanceDecision.Consumer consumer;

    private final AgentGovernanceDecision.Mode mode;

    private final String policyVersion;

    private final String configurationVersion;

    private final String ledgerGeneration;

    public AgentGovernanceDecisionRouter(final AgentGovernanceDecision.Consumer consumer, final AgentGovernanceDecision.Mode mode,
                                         final String policyVersion, final String configurationVersion,
                                         final String ledgerGeneration) {
        this.consumer = Objects.requireNonNull(consumer, "consumer");
        this.mode = Objects.requireNonNull(mode, "mode");
        this.policyVersion = Objects.requireNonNull(policyVersion, "policyVersion");
        this.configurationVersion = Objects.requireNonNull(configurationVersion, "configurationVersion");
        this.ledgerGeneration = Objects.requireNonNull(ledgerGeneration, "ledgerGeneration");
    }

    public AgentGovernanceDecision allow(final String correlationId, final Instant effectiveTime) {
        return decision(correlationId, effectiveTime, AgentGovernanceDecision.Result.ALLOW, AgentGovernanceDecision.Reason.ALLOWED);
    }

    /**
     * Compares two policy outcomes without treating shadow evaluation as an execution decision.
     * A shadow WOULD_DENY differs from the enforce expectation, while ALLOW agrees across modes.
     *
     * @param shadowDecision the shadow observation to compare
     * @param enforceDecision the enforce expectation with the same correlation id
     * @return the explicit shadow/enforce comparison outcome
     */
    public Comparison compare(final AgentGovernanceDecision shadowDecision,
                              final AgentGovernanceDecision enforceDecision) {
        Objects.requireNonNull(shadowDecision, "shadowDecision");
        Objects.requireNonNull(enforceDecision, "enforceDecision");
        requireOwned(shadowDecision, AgentGovernanceDecision.Mode.SHADOW);
        requireOwned(enforceDecision, AgentGovernanceDecision.Mode.ENFORCE);
        if (Objects.isNull(shadowDecision.getCorrelationId())
                || !shadowDecision.getCorrelationId().equals(enforceDecision.getCorrelationId())) {
            throw new IllegalArgumentException("decisions must use the same correlation id");
        }
        boolean shadowWouldDeny = shadowDecision.getResult() == AgentGovernanceDecision.Result.WOULD_DENY;
        boolean enforceDeny = enforceDecision.getResult() == AgentGovernanceDecision.Result.DENY;
        if (shadowWouldDeny && !enforceDeny) {
            return Comparison.SHADOW_STRICTER;
        }
        if (!shadowWouldDeny && enforceDeny) {
            return Comparison.ENFORCE_STRICTER;
        }
        return Comparison.SAME_EFFECT;
    }

    public AgentGovernanceDecision deny(final String correlationId, final Instant effectiveTime,
                                        final AgentGovernanceDecision.Reason reason) {
        Objects.requireNonNull(reason, "reason");
        AgentGovernanceDecision.Result result = mode == AgentGovernanceDecision.Mode.SHADOW
                ? AgentGovernanceDecision.Result.WOULD_DENY : AgentGovernanceDecision.Result.DENY;
        if (reason == AgentGovernanceDecision.Reason.ALLOWED) {
            throw new IllegalArgumentException("ALLOWED reason cannot deny");
        }
        return decision(correlationId, effectiveTime, result, reason);
    }

    public AgentGovernanceDecision mandatoryDeny(final String correlationId, final Instant effectiveTime,
                                                 final AgentGovernanceDecision.Reason reason) {
        AgentGovernanceDecision.Reason mandatory = mandatoryReason(reason);
        return AgentGovernanceDecision.builder(consumer, AgentGovernanceDecision.Mode.ENFORCE)
                .policyVersion(policyVersion).configurationVersion(configurationVersion)
                .correlationId(correlationId).ledgerGeneration(ledgerGeneration).effectiveTime(effectiveTime)
                .result(AgentGovernanceDecision.Result.DENY).reason(mandatory).build();
    }

    private AgentGovernanceDecision decision(final String correlationId, final Instant effectiveTime,
                                             final AgentGovernanceDecision.Result result,
                                             final AgentGovernanceDecision.Reason reason) {
        return AgentGovernanceDecision.builder(consumer, mode)
                .policyVersion(policyVersion).configurationVersion(configurationVersion)
                .correlationId(correlationId).ledgerGeneration(ledgerGeneration).effectiveTime(effectiveTime)
                .result(result).reason(reason).build();
    }

    private AgentGovernanceDecision.Reason mandatoryReason(final AgentGovernanceDecision.Reason reason) {
        if (reason != AgentGovernanceDecision.Reason.AUTHENTICATION_REJECTED
                && reason != AgentGovernanceDecision.Reason.AUTHORIZATION_REJECTED
                && reason != AgentGovernanceDecision.Reason.RESOURCE_LIMIT_REJECTED) {
            throw new IllegalArgumentException("reason is not mandatory");
        }
        return reason;
    }

    private void requireOwned(final AgentGovernanceDecision decision, final AgentGovernanceDecision.Mode expectedMode) {
        if (decision.getConsumer() != consumer || decision.getMode() != expectedMode
                || !decision.getPolicyVersion().equals(policyVersion)
                || !decision.getConfigurationVersion().equals(configurationVersion)
                || !decision.getLedgerGeneration().equals(ledgerGeneration)) {
            throw new IllegalArgumentException("decision does not belong to this router");
        }
    }

    public enum Comparison {
        SAME_EFFECT,
        SHADOW_STRICTER,
        ENFORCE_STRICTER
    }
}
