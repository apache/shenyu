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
import org.apache.shenyu.common.audit.AgentGovernanceAuditAdapter;
import org.apache.shenyu.common.audit.AgentInvocationAuditObserver;
import org.apache.shenyu.common.dto.AgentRequestContext;
import org.apache.shenyu.common.metrics.AgentRuntimeMetrics;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import reactor.core.publisher.Mono;
import reactor.util.context.ContextView;

import java.time.Instant;
import java.util.Objects;
import java.util.UUID;
import java.util.Map;

/** Applies one immutable MCP quota policy at the actual tool execution boundary. */
public final class AgentMcpQuotaGovernor {

    private static final Logger LOG = LoggerFactory.getLogger(AgentMcpQuotaGovernor.class);

    private final AgentMcpQuotaConfig config;

    private final AgentMcpQuotaStore store;

    private final AgentRuntimeMetrics metrics;

    private final AgentInvocationAuditObserver audit;

    private final String policyVersion;

    private final String ledgerGeneration;

    public AgentMcpQuotaGovernor(final AgentMcpQuotaConfig config, final AgentMcpQuotaStore store,
                                 final AgentRuntimeMetrics metrics, final String policyVersion) {
        this(config, store, metrics, policyVersion, null);
    }

    /** Create a governor with an optional metadata-only decision audit sink.
     *
     * @param config immutable policy snapshot
     * @param store distributed atomic admission backend
     * @param metrics fallback closed-label metrics port
     * @param policyVersion deployment-owned policy version
     * @param audit optional application-owned decision audit sink
     */
    public AgentMcpQuotaGovernor(final AgentMcpQuotaConfig config, final AgentMcpQuotaStore store,
                                 final AgentRuntimeMetrics metrics, final String policyVersion,
                                 final AgentInvocationAuditObserver audit) {
        this.config = Objects.requireNonNull(config, "config");
        this.store = Objects.requireNonNull(store, "store");
        this.metrics = Objects.isNull(metrics) ? AgentRuntimeMetrics.NOOP : metrics;
        this.audit = audit;
        this.policyVersion = bounded(policyVersion, "policyVersion");
        this.ledgerGeneration = config.newGeneration();
    }

    /** Wrap an execution with atomic admission and idempotent lease release.
     *
     * @param <T> execution result
     * @param tenant trusted tenant
     * @param subject trusted subject
     * @param correlationId request correlation id
     * @param configurationVersion immutable rule generation
     * @param execution actual tool execution
     * @return governed result
     */
    public <T> Mono<T> withQuota(final String tenant, final String subject, final String correlationId,
                                 final long configurationVersion, final Mono<T> execution) {
        Objects.requireNonNull(execution, "execution");
        String normalizedTenant = bounded(tenant, "tenant");
        String normalizedSubject = bounded(subject, "subject");
        String normalizedCorrelation = bounded(correlationId, "correlationId");
        Instant effectiveTime = Instant.now();
        String configuration = Long.toString(configurationVersion);
        return Mono.deferContextual(context -> Mono.just(new GovernorContext(context)))
                .flatMap(governorContext -> {
                    AgentRuntimeMetrics selectedMetrics = governorContext.metrics == AgentRuntimeMetrics.NOOP
                            ? metrics : governorContext.metrics;
                    AgentRequestContext requestContext = governorContext.requestContext;
                    return store.admit(config, normalizedTenant, normalizedSubject, normalizedCorrelation, effectiveTime)
                            .map(admission -> Map.entry(admission, false))
                            .onErrorResume(error -> policyUnavailable(error, selectedMetrics, requestContext,
                                    normalizedCorrelation, configuration, effectiveTime))
                            .flatMap(admission -> {
                                boolean policyUnavailable = admission.getValue();
                                AgentGovernanceDecision decision = decision(admission.getKey(), policyUnavailable,
                                        normalizedCorrelation, configuration, effectiveTime);
                                AgentRuntimeMetrics.safely(() -> selectedMetrics.governanceDecision(decision));
                                if (Objects.nonNull(requestContext) && Objects.nonNull(audit)
                                        && decision.getReason() != AgentGovernanceDecision.Reason.ALLOWED) {
                                    AgentRuntimeMetrics.safely(() -> audit.record(AgentGovernanceAuditAdapter.toEvent(requestContext, decision)));
                                }
                                if (decision.deniesTraffic()) {
                                    return Mono.error(new AgentMcpQuotaException(decision));
                                }
                                if (policyUnavailable) {
                                    return execution;
                                }
                                return Mono.usingWhen(Mono.just(admission.getKey()),
                                        ignored -> execution,
                                        ignored -> releaseOnce(normalizedTenant, normalizedSubject, normalizedCorrelation, admission.getKey().lease()),
                                        (ignored, error) -> releaseOnce(normalizedTenant, normalizedSubject, normalizedCorrelation, admission.getKey().lease()),
                                        ignored -> releaseOnce(normalizedTenant, normalizedSubject, normalizedCorrelation, admission.getKey().lease()));
                            });
                });
    }

    private Mono<Map.Entry<AgentMcpQuotaStore.Admission, Boolean>> policyUnavailable(final Throwable error,
                                                                                     final AgentRuntimeMetrics selectedMetrics,
                                                                                     final AgentRequestContext requestContext,
                                                                                     final String correlationId,
                                                                                     final String configurationVersion,
                                                                                     final Instant effectiveTime) {
        if (config.isFailOpen()) {
            return Mono.just(Map.entry(new AgentMcpQuotaStore.Admission(true, false, false,
                    UUID.randomUUID().toString()), true));
        }
        AgentGovernanceDecision decision = AgentGovernanceDecision.builder(
                        AgentGovernanceDecision.Consumer.MCP_QUOTA, AgentGovernanceDecision.Mode.ENFORCE)
                .policyVersion(policyVersion).configurationVersion("unknown")
                .result(AgentGovernanceDecision.Result.DENY).reason(AgentGovernanceDecision.Reason.POLICY_ERROR)
                .correlationId(correlationId).ledgerGeneration(ledgerGeneration)
                .effectiveTime(effectiveTime).build();
        AgentRuntimeMetrics.safely(() -> selectedMetrics.governanceDecision(decision));
        if (Objects.nonNull(requestContext) && Objects.nonNull(audit)) {
            AgentRuntimeMetrics.safely(() -> audit.record(AgentGovernanceAuditAdapter.toEvent(requestContext, decision)));
        }
        return Mono.error(new AgentMcpQuotaException(decision, error));
    }

    private Mono<Boolean> releaseOnce(final String tenant, final String subject, final String correlationId, final String lease) {
        return store.release(config, tenant, subject, correlationId, lease)
                .onErrorResume(error -> store.release(config, tenant, subject, correlationId, lease)
                        .doOnError(nextError -> LOG.warn("MCP quota lease release failed twice; lease will expire by TTL, correlationId={}",
                                correlationId, nextError))
                        .onErrorComplete());
    }

    private AgentGovernanceDecision decision(final AgentMcpQuotaStore.Admission admission, final boolean policyUnavailable,
                                             final String correlationId, final String configurationVersion,
                                             final Instant effectiveTime) {
        AgentGovernanceDecision.Reason reason = policyUnavailable ? AgentGovernanceDecision.Reason.POLICY_ERROR
                : admission.invocationLimited()
                ? AgentGovernanceDecision.Reason.MCP_INVOCATION_LIMIT
                : admission.concurrencyLimited() ? AgentGovernanceDecision.Reason.MCP_CONCURRENCY_LIMIT
                : AgentGovernanceDecision.Reason.ALLOWED;
        AgentGovernanceDecision.Result result = policyUnavailable && config.isFailOpen() ? AgentGovernanceDecision.Result.ALLOW
                : admission.allowed() ? AgentGovernanceDecision.Result.ALLOW
                : config.getMode() == AgentMcpQuotaConfig.Mode.SHADOW
                ? AgentGovernanceDecision.Result.WOULD_DENY : AgentGovernanceDecision.Result.DENY;
        return AgentGovernanceDecision.builder(AgentGovernanceDecision.Consumer.MCP_QUOTA,
                        config.getMode() == AgentMcpQuotaConfig.Mode.SHADOW
                                ? AgentGovernanceDecision.Mode.SHADOW : AgentGovernanceDecision.Mode.ENFORCE)
                .policyVersion(policyVersion).configurationVersion(configurationVersion)
                .result(result).reason(reason).correlationId(correlationId)
                .ledgerGeneration(ledgerGeneration).effectiveTime(effectiveTime).build();
    }

    private static String bounded(final String value, final String name) {
        if (Objects.isNull(value) || value.isBlank() || value.length() > 256
                || !value.equals(value.strip()) || value.chars().anyMatch(Character::isISOControl)) {
            throw new IllegalArgumentException(name + " must be bounded nonblank text");
        }
        return value;
    }

    private static final class GovernorContext {

        private final AgentRuntimeMetrics metrics;

        private final AgentRequestContext requestContext;

        private GovernorContext(final ContextView context) {
            AgentRuntimeMetrics requestMetrics = context.getOrDefault(AgentRuntimeMetrics.KEY, AgentRuntimeMetrics.NOOP);
            metrics = requestMetrics == AgentRuntimeMetrics.NOOP ? AgentRuntimeMetrics.NOOP : requestMetrics;
            requestContext = context.getOrDefault(AgentRequestContext.REACTOR_KEY, null);
        }
    }
}
