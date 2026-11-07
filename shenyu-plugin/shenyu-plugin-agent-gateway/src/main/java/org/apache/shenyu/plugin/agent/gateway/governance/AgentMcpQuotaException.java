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

package org.apache.shenyu.plugin.agent.gateway.governance;

import org.apache.shenyu.common.governance.AgentGovernanceDecision;

/** Typed quota rejection carrying only closed decision metadata. */
public final class AgentMcpQuotaException extends RuntimeException {

    private final AgentGovernanceDecision decision;

    public AgentMcpQuotaException(final AgentGovernanceDecision decision) {
        super(message(decision));
        this.decision = decision;
    }

    public AgentMcpQuotaException(final AgentGovernanceDecision decision, final Throwable cause) {
        super(message(decision), cause);
        this.decision = decision;
    }

    public AgentGovernanceDecision getDecision() {
        return decision;
    }

    private static String message(final AgentGovernanceDecision decision) {
        return decision.getReason() == AgentGovernanceDecision.Reason.POLICY_ERROR
                ? "MCP quota policy is unavailable"
                : decision.getReason() == AgentGovernanceDecision.Reason.MCP_CONCURRENCY_LIMIT
                ? "MCP concurrency quota exceeded" : "MCP invocation quota exceeded";
    }
}
