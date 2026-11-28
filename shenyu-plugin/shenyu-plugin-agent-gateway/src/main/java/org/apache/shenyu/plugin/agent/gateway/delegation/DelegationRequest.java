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

package org.apache.shenyu.plugin.agent.gateway.delegation;

import org.apache.shenyu.common.dto.AgentGatewayHttpToolConfig.IdentityDelegation;
import org.apache.shenyu.common.dto.AgentGatewayHttpToolConfig.Tool;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolInvocation;

import java.time.Instant;
import java.util.Optional;

/** Identity and policy snapshot supplied to a delegator, omitting caller arguments and capabilities. */
public final class DelegationRequest {

    private final Tool tool;

    private final IdentityDelegation policy;

    private final String subject;

    private final String tenant;

    private final Instant deadline;

    private final String policyVersion;

    public DelegationRequest(final Tool tool, final AgentToolInvocation invocation) {
        this.tool = tool;
        policy = tool.getIdentityDelegation().orElseThrow(() -> new AgentIdentityDelegationException(AgentIdentityDelegationException.Reason.UNCONFIGURED));
        subject = invocation.getSubject();
        tenant = invocation.getTenant().orElse(null);
        deadline = invocation.getDeadline();
        policyVersion = invocation.getRuleId() + ":" + invocation.getConfigurationVersion();
    }

    public Tool getTool() {
        return tool;
    }

    public IdentityDelegation getPolicy() {
        return policy;
    }

    public String getSubject() {
        return subject;
    }

    public Optional<String> getTenant() {
        return Optional.ofNullable(tenant);
    }

    public Instant getDeadline() {
        return deadline;
    }

    public String getPolicyVersion() {
        return policyVersion;
    }
}
