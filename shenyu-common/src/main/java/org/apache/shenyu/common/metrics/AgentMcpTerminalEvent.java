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

package org.apache.shenyu.common.metrics;

import org.apache.shenyu.common.dto.AgentRequestContext;

import java.util.Objects;

/**
 * Immutable terminal snapshot for one matched MCP request subscription.
 * Nullable metadata was unavailable or unvalidated, not inferred from headers.
 * Requested tools do not prove authorization or execution; assigned HTTP status
 * does not prove delivery. Identifiers must not become unbounded metric labels.
 */
public final class AgentMcpTerminalEvent {

    private final AgentRequestContext context;

    private final AgentMcpCallObserver.Outcome outcome;

    private final long elapsedMillis;

    private final Integer httpStatus;

    public AgentMcpTerminalEvent(final String requestId, final String selectorId, final String ruleId, final long configurationVersion,
                                 final String subject, final String method, final String requestedTool, final String protocolVersion,
                                 final AgentMcpCallObserver.Outcome outcome, final long elapsedMillis, final Integer httpStatus) {
        this(new AgentRequestContext(requestId, "mcp", selectorId, ruleId, configurationVersion, null, subject, requestedTool, method, protocolVersion, null),
                outcome, elapsedMillis, httpStatus);
    }

    public AgentMcpTerminalEvent(final AgentRequestContext context, final AgentMcpCallObserver.Outcome outcome, final long elapsedMillis, final Integer httpStatus) {
        this.context = Objects.requireNonNull(context, "context");
        Objects.requireNonNull(context.getConfigurationVersion(), "configurationVersion");
        this.outcome = outcome;
        this.elapsedMillis = elapsedMillis;
        this.httpStatus = httpStatus;
    }

    public AgentRequestContext getContext() {
        return context;
    }

    public String getTenant() {
        return context.getTenant();
    }

    public String getSessionId() {
        return context.getSessionId();
    }

    public String getRequestId() {
        return context.getRequestId();
    }

    public String getSelectorId() {
        return context.getSelectorId();
    }

    public String getRuleId() {
        return context.getRuleId();
    }

    public long getConfigurationVersion() {
        return context.getConfigurationVersion();
    }

    public String getSubject() {
        return context.getSubject();
    }

    public String getMethod() {
        return context.getOperation();
    }

    public String getRequestedTool() {
        return context.getTarget();
    }

    public String getProtocolVersion() {
        return context.getProtocolVersion();
    }

    public AgentMcpCallObserver.Outcome getOutcome() {
        return outcome;
    }

    public long getElapsedMillis() {
        return elapsedMillis;
    }

    public Integer getHttpStatus() {
        return httpStatus;
    }
}
