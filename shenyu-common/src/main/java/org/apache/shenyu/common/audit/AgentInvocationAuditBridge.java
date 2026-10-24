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

import java.util.Objects;

/** Maps existing terminal owners without another request registry or completion state machine. */
public final class AgentInvocationAuditBridge {

    private AgentInvocationAuditBridge() {
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
                safely(() -> emit(audit, event.context(), event.outcome().name(), event.httpStatus(), event.elapsedMillis(), null));
                if (Objects.nonNull(existing)) {
                    safely(() -> existing.record(event));
                }
            }

            @Override
            public void recordAttempt(final AiAttemptEvent event) {
                safely(() -> emit(audit, event.context(), event.outcome().name(), event.httpStatus(), event.elapsedMillis(), event.attemptNumber()));
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
                safely(() -> emit(audit, event.getContext(), event.getOutcome().name(), event.getHttpStatus(), event.getElapsedMillis(), null));
                if (Objects.nonNull(existing)) {
                    safely(() -> existing.record(event));
                }
            }

            @Override
            public void recordAttempt(final AgentMcpTerminalEvent event) {
                safely(() -> emit(audit, event.getContext(), event.getOutcome().name(), event.getHttpStatus(), event.getElapsedMillis(), 1));
                if (Objects.nonNull(existing)) {
                    safely(() -> existing.recordAttempt(event));
                }
            }
        };
    }

    private static void emit(final AgentInvocationAuditObserver sink, final AgentRequestContext context, final String outcome,
                             final Integer status, final long elapsed, final Integer attempt) {
        AgentInvocationAuditEvent.Builder builder = Objects.isNull(attempt) ? AgentInvocationAuditEvent.logical() : AgentInvocationAuditEvent.attempt(attempt);
        sink.record(builder.requestId(context.getRequestId()).tenant(context.getTenant()).subject(context.getSubject()).apiKeyId(context.getApiKeyId())
                .selectorId(context.getSelectorId()).ruleId(context.getRuleId()).target(context.getTarget())
                .configurationVersion(Objects.isNull(context.getConfigurationVersion()) ? "unknown" : context.getConfigurationVersion().toString())
                .policyVersion("mcp".equals(context.getTrafficType()) ? "tool-grant-intersection-v1" : "agent-ai-admission-v1")
                .decision("REJECTED".equals(outcome) || "RATE_LIMITED".equals(outcome)
                        ? AgentInvocationAuditEvent.Decision.REJECTED : AgentInvocationAuditEvent.Decision.ALLOWED)
                .outcome("ERROR".equals(outcome) ? "SERVER_ERROR" : outcome)
                .reason("CANCELLED".equals(outcome) ? "DOWNSTREAM_CANCELLED" : "TIMEOUT".equals(outcome) ? "DEADLINE_EXCEEDED"
                        : "TOOL_ERROR".equals(outcome) ? "TOOL_REPORTED_ERROR" : Objects.isNull(status) ? "NO_RESPONSE" : "HTTP_" + status)
                .httpStatus(status).elapsedMillis(elapsed).build());
    }

    private static void safely(final Runnable action) {
        try {
            action.run();
        } catch (RuntimeException ignored) {
            // Sink failures must not alter traffic or suppress another observer.
        }
    }
}
