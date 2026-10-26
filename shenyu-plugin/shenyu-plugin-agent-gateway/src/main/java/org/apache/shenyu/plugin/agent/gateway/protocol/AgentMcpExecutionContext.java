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

package org.apache.shenyu.plugin.agent.gateway.protocol;

import org.apache.shenyu.common.dto.AgentGatewayResultFilterConfig;
import org.apache.shenyu.common.dto.AgentGatewayHttpToolConfig;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentHttpToolInvoker;

import java.util.HashSet;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.time.Instant;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Immutable authorization input supplied exclusively by a trusted server-side adapter.
 * Creating this object does not authenticate its caller.
 */
public final class AgentMcpExecutionContext {

    private final String requestId;

    private final String subject;

    private final String tenant;

    private final String ruleId;

    private final long configurationVersion;

    private final Set<String> ruleTools;

    private final Set<String> allowedTools;

    private final Set<String> ruleResources;

    private final Set<String> allowedResources;

    private final Instant deadline;

    private final AgentGatewayResultFilterConfig resultFilters;

    private final AgentGatewayHttpToolConfig httpTools;

    private final AgentHttpToolInvoker httpInvoker;

    private final Runnable attemptListener;

    private final AtomicBoolean attemptRecorded = new AtomicBoolean();

    public AgentMcpExecutionContext(final String requestId, final String subject, final String ruleId, final long configurationVersion,
                                    final Set<String> ruleTools, final Set<String> securityGrants) {
        this(requestId, subject, ruleId, configurationVersion, ruleTools, securityGrants, Instant.MAX);
    }

    public AgentMcpExecutionContext(final String requestId, final String subject, final String ruleId, final long configurationVersion,
                                    final Set<String> ruleTools, final Set<String> securityGrants, final Instant deadline) {
        this(requestId, subject, ruleId, configurationVersion, ruleTools, securityGrants, deadline, AgentGatewayResultFilterConfig.empty());
    }

    public AgentMcpExecutionContext(final String requestId, final String subject, final String ruleId, final long configurationVersion,
                                    final Set<String> ruleTools, final Set<String> securityGrants, final Instant deadline,
                                    final AgentGatewayResultFilterConfig resultFilters) {
        this(requestId, subject, ruleId, configurationVersion, ruleTools, securityGrants, deadline, resultFilters,
                Set.of(), Set.of(), AgentGatewayHttpToolConfig.empty(), null, null, null);
    }

    public AgentMcpExecutionContext(final String requestId, final String subject, final String ruleId, final long configurationVersion,
                                    final Set<String> ruleTools, final Set<String> securityGrants, final Instant deadline,
                                    final AgentGatewayResultFilterConfig resultFilters, final Set<String> ruleResources,
                                    final Set<String> resourceGrants) {
        this(requestId, subject, ruleId, configurationVersion, ruleTools, securityGrants, deadline, resultFilters,
                ruleResources, resourceGrants, AgentGatewayHttpToolConfig.empty(), null, null, null);
    }

    public AgentMcpExecutionContext(final String requestId, final String subject, final String ruleId, final long configurationVersion,
                                    final Set<String> ruleTools, final Set<String> securityGrants, final Instant deadline,
                                    final AgentGatewayResultFilterConfig resultFilters, final String tenant) {
        this(requestId, subject, ruleId, configurationVersion, ruleTools, securityGrants, deadline, resultFilters,
                Set.of(), Set.of(), AgentGatewayHttpToolConfig.empty(), null, tenant, null);
    }

    public AgentMcpExecutionContext(final String requestId, final String subject, final String ruleId, final long configurationVersion,
                                    final Set<String> ruleTools, final Set<String> securityGrants, final Instant deadline,
                                    final AgentGatewayResultFilterConfig resultFilters, final Set<String> ruleResources,
                                    final Set<String> resourceGrants, final String tenant) {
        this(requestId, subject, ruleId, configurationVersion, ruleTools, securityGrants, deadline, resultFilters,
                ruleResources, resourceGrants, AgentGatewayHttpToolConfig.empty(), null, tenant, null);
    }

    public AgentMcpExecutionContext(final String requestId, final String subject, final String ruleId, final long configurationVersion,
                                    final Set<String> ruleTools, final Set<String> securityGrants, final Instant deadline,
                                    final AgentGatewayResultFilterConfig resultFilters, final AgentGatewayHttpToolConfig httpTools,
                                    final AgentHttpToolInvoker httpInvoker) {
        this(requestId, subject, ruleId, configurationVersion, ruleTools, securityGrants, deadline, resultFilters,
                Set.of(), Set.of(), httpTools, httpInvoker, null, null);
    }

    public AgentMcpExecutionContext(final String requestId, final String subject, final String ruleId, final long configurationVersion,
                                    final Set<String> ruleTools, final Set<String> securityGrants, final Instant deadline,
                                    final AgentGatewayResultFilterConfig resultFilters, final AgentGatewayHttpToolConfig httpTools,
                                    final AgentHttpToolInvoker httpInvoker, final String tenant) {
        this(requestId, subject, ruleId, configurationVersion, ruleTools, securityGrants, deadline, resultFilters,
                Set.of(), Set.of(), httpTools, httpInvoker, tenant, null);
    }

    /**
     * Request-local context with HTTP tools and one terminal attempt callback.
     * @param requestId trusted request identity
     * @param subject trusted authenticated subject
     * @param ruleId matched rule identity
     * @param configurationVersion rule snapshot generation
     * @param ruleTools configured tool allow-list
     * @param securityGrants identity tool grants
     * @param deadline request deadline
     * @param resultFilters configured result filters
     * @param httpTools request-owned HTTP tool definitions
     * @param httpInvoker internal gateway HTTP executor
     * @param tenant trusted tenant identity
     * @param attemptListener internal attempt callback
     */
    AgentMcpExecutionContext(final String requestId, final String subject, final String ruleId, final long configurationVersion,
                            final Set<String> ruleTools, final Set<String> securityGrants, final Instant deadline,
                            final AgentGatewayResultFilterConfig resultFilters, final AgentGatewayHttpToolConfig httpTools,
                            final AgentHttpToolInvoker httpInvoker, final String tenant, final Runnable attemptListener) {
        this(requestId, subject, ruleId, configurationVersion, ruleTools, securityGrants, deadline, resultFilters,
                Set.of(), Set.of(), httpTools, httpInvoker, tenant, attemptListener);
    }

    /**
     * Fully populated request-local context.
     * @param requestId trusted request identity
     * @param subject trusted authenticated subject
     * @param ruleId matched rule identity
     * @param configurationVersion rule snapshot generation
     * @param ruleTools configured tool allow-list
     * @param securityGrants identity tool grants
     * @param deadline request deadline
     * @param resultFilters configured result filters
     * @param ruleResources configured resource allow-list
     * @param resourceGrants identity resource grants
     * @param httpTools request-owned HTTP tool definitions
     * @param httpInvoker internal gateway HTTP executor
     * @param tenant trusted tenant identity
     * @param attemptListener internal attempt callback
     */
    AgentMcpExecutionContext(final String requestId, final String subject, final String ruleId, final long configurationVersion,
                            final Set<String> ruleTools, final Set<String> securityGrants, final Instant deadline,
                            final AgentGatewayResultFilterConfig resultFilters, final Set<String> ruleResources,
                            final Set<String> resourceGrants, final AgentGatewayHttpToolConfig httpTools,
                            final AgentHttpToolInvoker httpInvoker, final String tenant, final Runnable attemptListener) {
        this.tenant = Objects.isNull(tenant) ? null : requireText(tenant, "tenant");
        this.deadline = Objects.requireNonNull(deadline, "deadline");
        this.httpTools = Objects.requireNonNull(httpTools, "httpTools");
        this.httpInvoker = httpInvoker;
        this.attemptListener = attemptListener;
        this.resultFilters = Objects.requireNonNull(resultFilters, "resultFilters");
        this.requestId = requireText(requestId, "requestId");
        this.subject = requireText(subject, "subject");
        this.ruleId = requireText(ruleId, "ruleId");
        if (configurationVersion < 0) {
            throw new IllegalArgumentException("configurationVersion must not be negative");
        }
        this.configurationVersion = configurationVersion;
        this.ruleTools = Set.copyOf(ruleTools);
        Set<String> intersection = new HashSet<>(this.ruleTools);
        intersection.retainAll(Set.copyOf(securityGrants));
        allowedTools = Set.copyOf(intersection);
        this.ruleResources = Set.copyOf(ruleResources);
        Set<String> resources = new HashSet<>(this.ruleResources);
        resources.retainAll(Set.copyOf(resourceGrants));
        allowedResources = Set.copyOf(resources);
    }

    public String getRequestId() {
        return requestId;
    }

    public Instant getDeadline() {
        return deadline;
    }

    public String getSubject() {
        return subject;
    }

    public Optional<String> getTenant() {
        return Optional.ofNullable(tenant);
    }

    public String getRuleId() {
        return ruleId;
    }

    public long getConfigurationVersion() {
        return configurationVersion;
    }

    public Set<String> getRuleTools() {
        return ruleTools;
    }

    public Set<String> getAllowedTools() {
        return allowedTools;
    }

    public Set<String> getRuleResources() {
        return ruleResources;
    }

    public Set<String> getAllowedResources() {
        return allowedResources;
    }

    public AgentGatewayResultFilterConfig getResultFilters() {
        return resultFilters;
    }

    public AgentGatewayHttpToolConfig getHttpTools() {
        return httpTools;
    }

    public AgentHttpToolInvoker getHttpInvoker() {
        return httpInvoker;
    }

    void notifyAttempt() {
        if (Objects.nonNull(attemptListener) && attemptRecorded.compareAndSet(false, true)) {
            attemptListener.run();
        }
    }

    private String requireText(final String value, final String name) {
        if (Objects.requireNonNull(value, name).isBlank()) {
            throw new IllegalArgumentException(name + " must not be blank");
        }
        return value;
    }
}
