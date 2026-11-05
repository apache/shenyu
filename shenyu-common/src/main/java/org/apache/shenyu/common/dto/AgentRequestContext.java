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

import java.util.Objects;

/**
 * Immutable subscription-local correlation snapshot shared by AI and MCP.
 * Identity must come from the configured authentication adapter, never wire metadata.
 * Nullable facts are unknown; neither an RPC id nor a trace id is an identity or session.
 * This is diagnostic context, not an authorization grant or a set of metric labels.
 */
public final class AgentRequestContext {

    public static final String REACTOR_KEY = AgentRequestContext.class.getName();

    public static final String EXCHANGE_ATTRIBUTE = REACTOR_KEY;

    private final String requestId;

    private final String trafficType;

    private final String selectorId;

    private final String ruleId;

    private final Long configurationVersion;

    private final String tenant;

    private final String subject;

    private final String target;

    private final String operation;

    private final String protocolVersion;

    private final String sessionId;

    private final String apiKeyId;

    public AgentRequestContext(final String requestId, final String trafficType, final String selectorId, final String ruleId) {
        this(requestId, trafficType, selectorId, ruleId, null, null, null, null, null, null, null);
    }

    public AgentRequestContext(final String requestId, final String trafficType, final String selectorId, final String ruleId,
                               final Long configurationVersion, final String tenant, final String subject, final String target,
                               final String operation, final String protocolVersion, final String sessionId) {
        this(requestId, trafficType, selectorId, ruleId, configurationVersion, tenant, subject, target, operation, protocolVersion, sessionId, null);
    }

    private AgentRequestContext(final String requestId, final String trafficType, final String selectorId, final String ruleId,
                                final Long configurationVersion, final String tenant, final String subject, final String target,
                                final String operation, final String protocolVersion, final String sessionId, final String apiKeyId) {
        this.requestId = Objects.requireNonNull(requestId, "requestId");
        this.trafficType = Objects.requireNonNull(trafficType, "trafficType");
        this.selectorId = selectorId;
        this.ruleId = ruleId;
        this.configurationVersion = configurationVersion;
        this.tenant = tenant;
        this.subject = subject;
        this.target = target;
        this.operation = operation;
        this.protocolVersion = protocolVersion;
        this.sessionId = sessionId;
        this.apiKeyId = apiKeyId;
    }

    public AgentRequestContext withIdentity(final String trustedTenant, final String trustedSubject) {
        return new AgentRequestContext(requestId, trafficType, selectorId, ruleId, configurationVersion, trustedTenant, trustedSubject,
                target, operation, protocolVersion, sessionId, apiKeyId);
    }

    public AgentRequestContext withTarget(final String resolvedTarget, final String resolvedOperation, final String resolvedProtocolVersion) {
        return new AgentRequestContext(requestId, trafficType, selectorId, ruleId, configurationVersion, tenant, subject,
                resolvedTarget, resolvedOperation, resolvedProtocolVersion, sessionId, apiKeyId);
    }

    public AgentRequestContext withConfigurationVersion(final long version) {
        return new AgentRequestContext(requestId, trafficType, selectorId, ruleId, version, tenant, subject, target, operation, protocolVersion, sessionId, apiKeyId);
    }

    public AgentRequestContext withApiKeyId(final String internalId) {
        return new AgentRequestContext(requestId, trafficType, selectorId, ruleId, configurationVersion, tenant, subject,
                target, operation, protocolVersion, sessionId, internalId);
    }

    public String getApiKeyId() {
        return apiKeyId;
    }

    public String getRequestId() {
        return requestId;
    }

    public String getTrafficType() {
        return trafficType;
    }

    public String getSelectorId() {
        return selectorId;
    }

    public String getRuleId() {
        return ruleId;
    }

    public Long getConfigurationVersion() {
        return configurationVersion;
    }

    public String getTenant() {
        return tenant;
    }

    public String getSubject() {
        return subject;
    }

    public String getTarget() {
        return target;
    }

    public String getOperation() {
        return operation;
    }

    public String getProtocolVersion() {
        return protocolVersion;
    }

    public String getSessionId() {
        return sessionId;
    }
}
