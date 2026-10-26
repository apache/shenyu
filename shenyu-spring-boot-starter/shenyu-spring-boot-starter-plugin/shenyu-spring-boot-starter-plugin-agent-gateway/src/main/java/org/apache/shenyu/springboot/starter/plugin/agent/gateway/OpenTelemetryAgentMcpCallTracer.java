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

import io.opentelemetry.api.OpenTelemetry;
import io.opentelemetry.api.trace.Span;
import io.opentelemetry.api.trace.SpanKind;
import io.opentelemetry.api.trace.StatusCode;
import io.opentelemetry.api.trace.Tracer;
import org.apache.shenyu.common.metrics.AgentMcpCallObserver.Outcome;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpCallTracer;
import reactor.util.context.Context;
import reactor.util.context.ContextView;

import java.util.Locale;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Bounded MCP spans using explicitly propagated Reactor context, without thread-local scopes.
 */
final class OpenTelemetryAgentMcpCallTracer implements AgentMcpCallTracer {

    private final Tracer tracer;

    OpenTelemetryAgentMcpCallTracer(final OpenTelemetry telemetry) {
        tracer = telemetry.getTracer("org.apache.shenyu.agent.gateway");
    }

    @Override
    public Trace start(final ContextView parentContext) {
        Object value = parentContext.getOrDefault(io.opentelemetry.context.Context.class, null);
        io.opentelemetry.context.Context parent = io.opentelemetry.context.Context.root();
        if (value instanceof io.opentelemetry.context.Context context) {
            // Keep only the parent span identity, never baggage or unrelated application data.
            parent = parent.with(Span.wrap(Span.fromContext(context).getSpanContext()));
        }
        Span span = tracer.spanBuilder("shenyu.agent_gateway.tools.call").setSpanKind(SpanKind.INTERNAL).setParent(parent).startSpan();
        io.opentelemetry.context.Context execution = io.opentelemetry.context.Context.root().with(span);
        AtomicBoolean ended = new AtomicBoolean();
        return new Trace() {
            @Override
            public Context attach(final Context context) {
                return context.put(io.opentelemetry.context.Context.class, execution);
            }

            @Override
            public void finish(final Outcome outcome) {
                if (!ended.compareAndSet(false, true)) {
                    return;
                }
                try {
                    span.setAttribute("shenyu.mcp.outcome", outcome.name().toLowerCase(Locale.ROOT));
                    if (outcome == Outcome.TOOL_ERROR || outcome == Outcome.SERVER_ERROR || outcome == Outcome.TIMEOUT) {
                        span.setStatus(StatusCode.ERROR);
                    }
                } finally {
                    span.end();
                }
            }
        };
    }
}
