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
import org.apache.shenyu.common.metrics.AgentMcpTerminalEvent;
import org.apache.shenyu.common.metrics.AgentMcpTerminalObserver;
import org.apache.shenyu.common.metrics.AiAttemptEvent;
import org.apache.shenyu.common.metrics.AiCallObserver;
import org.apache.shenyu.common.metrics.AiTerminalEvent;
import org.apache.shenyu.common.metrics.AiUsage;
import org.apache.shenyu.common.metrics.AiUsageSummary;

import java.util.Objects;
import java.util.concurrent.atomic.LongAdder;

/** Maps existing terminal owners without another request registry or completion state machine. */
public final class AgentInvocationAuditBridge {

    private static final LongAdder OMITTED_METADATA = new LongAdder();

    private static final LongAdder DELIVERY_FAILURES = new LongAdder();

    private AgentInvocationAuditBridge() {
    }

    /**
     * Count omitted invalid optional fields without retaining their values.
     * @return process-wide omitted field count
     */
    public static long omittedMetadataCount() {
        return OMITTED_METADATA.sum();
    }

    /**
     * Count failed observer callbacks without retaining exception data.
     * @return process-wide callback failure count
     */
    public static long deliveryFailureCount() {
        return DELIVERY_FAILURES.sum();
    }

    /**
     * Preserve the application observer and add independent audit delivery.
     * @param existing existing observer
     * @param audit optional audit sink
     * @return combined observer, or the existing observer when disabled
     */
    public static AiCallObserver ai(final AiCallObserver existing, final AgentInvocationAuditObserver audit) {
        if (Objects.isNull(audit)) {
            return existing;
        }
        return new AiCallObserver() {
            @Override
            public void record(final AiTerminalEvent event) {
                safely(() -> emit(audit, event.context(), event.outcome().name(), event.httpStatus(), event.elapsedMillis(),
                        null, null, null, null, null, null, event.usage(), event.usageTotals()));
                if (Objects.nonNull(existing)) {
                    safely(() -> existing.record(event));
                }
            }

            @Override
            public void recordAttempt(final AiAttemptEvent event) {
                safely(() -> emit(audit, event.context(), event.outcome().name(), event.httpStatus(), event.elapsedMillis(),
                        event.attemptNumber(), event.provider(), event.clientProtocol(), event.targetProtocol(),
                        event.requestedModel(), event.resolvedModel(), event.usage(), null));
                if (Objects.nonNull(existing)) {
                    safely(() -> existing.recordAttempt(event));
                }
            }
        };
    }

    /**
     * Adapt MCP logical and actual execution snapshots without copying payloads.
     * @param existing existing observer
     * @param audit optional audit sink
     * @return combined observer
     */
    public static AgentMcpTerminalObserver mcp(final AgentMcpTerminalObserver existing, final AgentInvocationAuditObserver audit) {
        if (Objects.isNull(audit)) {
            return existing;
        }
        return new AgentMcpTerminalObserver() {
            @Override
            public void record(final AgentMcpTerminalEvent event) {
                safely(() -> emit(audit, event.getContext(), event.getOutcome().name(), event.getHttpStatus(), event.getElapsedMillis(),
                        null, null, null, null, null, null, null, null));
                if (Objects.nonNull(existing)) {
                    safely(() -> existing.record(event));
                }
            }

            @Override
            public void recordAttempt(final AgentMcpTerminalEvent event) {
                safely(() -> emit(audit, event.getContext(), event.getOutcome().name(), event.getHttpStatus(), event.getElapsedMillis(),
                        1, null, null, null, null, null, null, null));
                if (Objects.nonNull(existing)) {
                    safely(() -> existing.recordAttempt(event));
                }
            }
        };
    }

    private static void emit(final AgentInvocationAuditObserver sink, final AgentRequestContext context, final String outcome,
                             final Integer status, final long elapsed, final Integer attempt, final String provider,
                             final String clientProtocol, final String targetProtocol, final String requestedModel,
                             final String resolvedModel, final AiUsage usage, final AiUsageSummary usageTotals) {
        AgentInvocationAuditEvent.Builder builder = Objects.isNull(attempt) ? AgentInvocationAuditEvent.logical() : AgentInvocationAuditEvent.attempt(attempt);
        sink.record(builder.requestId(context.getRequestId()).tenant(optionalMetadata(context.getTenant())).subject(optionalMetadata(context.getSubject()))
                .apiKeyId(optionalMetadata(context.getApiKeyId())).selectorId(optionalMetadata(context.getSelectorId()))
                .ruleId(optionalMetadata(context.getRuleId())).target(optionalMetadata(context.getTarget()))
                .configurationVersion(Objects.isNull(context.getConfigurationVersion()) ? "unknown" : context.getConfigurationVersion().toString())
                .policyVersion("mcp".equals(context.getTrafficType()) ? "tool-grant-intersection-v1" : "agent-ai-admission-v1")
                .decision("REJECTED".equals(outcome) || "RATE_LIMITED".equals(outcome)
                        ? AgentInvocationAuditEvent.Decision.REJECTED : AgentInvocationAuditEvent.Decision.ALLOWED)
                .outcome("ERROR".equals(outcome) ? "SERVER_ERROR" : outcome)
                .reason("CANCELLED".equals(outcome) ? "DOWNSTREAM_CANCELLED" : "TIMEOUT".equals(outcome) ? "DEADLINE_EXCEEDED"
                        : "TOOL_ERROR".equals(outcome) ? "TOOL_REPORTED_ERROR" : Objects.isNull(status) ? "NO_RESPONSE" : "HTTP_" + status)
                .httpStatus(status).elapsedMillis(elapsed).provider(optionalMetadata(provider)).clientProtocol(optionalMetadata(clientProtocol))
                .targetProtocol(optionalMetadata(targetProtocol)).requestedModel(optionalMetadata(requestedModel)).resolvedModel(optionalMetadata(resolvedModel))
                .usage(usage).usageTotals(usageTotals).build());
    }

    private static String optionalMetadata(final String value) {
        // Omit invalid metadata, never truncate it into a different identity.
        try {
            return AgentInvocationAuditEvent.nullableText(value, "optional metadata");
        } catch (IllegalArgumentException ignored) {
            OMITTED_METADATA.increment();
            return null;
        }
    }

    private static void safely(final Runnable action) {
        try {
            action.run();
        } catch (RuntimeException ignored) {
            DELIVERY_FAILURES.increment();
            // Sink failures must not alter traffic or suppress another observer.
        }
    }
}
