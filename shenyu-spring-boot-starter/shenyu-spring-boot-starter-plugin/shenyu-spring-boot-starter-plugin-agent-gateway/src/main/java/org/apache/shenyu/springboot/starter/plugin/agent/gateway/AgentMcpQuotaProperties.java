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

package org.apache.shenyu.springboot.starter.plugin.agent.gateway;

import org.apache.shenyu.plugin.agent.gateway.governance.AgentMcpQuotaConfig;
import org.apache.shenyu.common.governance.AgentGovernanceText;
import org.springframework.boot.context.properties.ConfigurationProperties;

import java.time.Duration;

/**
 * Deployment-owned distributed MCP quota policy. Redis connectivity and lifecycle
 * are owned by the application; this namespace only describes admission behavior.
 */
@ConfigurationProperties(prefix = "shenyu.plugins.agent.gateway.mcp-quota", ignoreUnknownFields = false)
public class AgentMcpQuotaProperties {

    private boolean enabled;

    private AgentMcpQuotaConfig.Mode mode = AgentMcpQuotaConfig.Mode.SHADOW;

    private long invocationLimit = 1000;

    private Duration invocationWindow = Duration.ofMinutes(1);

    private int concurrencyLimit = 100;

    private Duration leaseTtl = Duration.ofMinutes(1);

    private boolean failOpen;

    private String policyVersion = "unknown";

    public boolean isEnabled() {
        return enabled;
    }

    public void setEnabled(final boolean enabled) {
        this.enabled = enabled;
    }

    public AgentMcpQuotaConfig.Mode getMode() {
        return mode;
    }

    public void setMode(final AgentMcpQuotaConfig.Mode mode) {
        this.mode = mode;
    }

    public long getInvocationLimit() {
        return invocationLimit;
    }

    public void setInvocationLimit(final long invocationLimit) {
        this.invocationLimit = invocationLimit;
    }

    public Duration getInvocationWindow() {
        return invocationWindow;
    }

    public void setInvocationWindow(final Duration invocationWindow) {
        this.invocationWindow = invocationWindow;
    }

    public int getConcurrencyLimit() {
        return concurrencyLimit;
    }

    public void setConcurrencyLimit(final int concurrencyLimit) {
        this.concurrencyLimit = concurrencyLimit;
    }

    public Duration getLeaseTtl() {
        return leaseTtl;
    }

    public void setLeaseTtl(final Duration leaseTtl) {
        this.leaseTtl = leaseTtl;
    }

    public boolean isFailOpen() {
        return failOpen;
    }

    public void setFailOpen(final boolean failOpen) {
        this.failOpen = failOpen;
    }

    public String getPolicyVersion() {
        return policyVersion;
    }

    public void setPolicyVersion(final String policyVersion) {
        this.policyVersion = policyVersion;
    }

    /**
     * Build the immutable admission policy after Spring's relaxed binding has populated it.
     *
     * @return validated quota policy
     */
    public AgentMcpQuotaConfig toConfig() {
        return AgentMcpQuotaConfig.of(mode, invocationLimit,
                invocationWindow, concurrencyLimit, leaseTtl, failOpen);
    }

    public String trustedPolicyVersion() {
        return AgentGovernanceText.require(policyVersion, "policyVersion", 128);
    }

}
