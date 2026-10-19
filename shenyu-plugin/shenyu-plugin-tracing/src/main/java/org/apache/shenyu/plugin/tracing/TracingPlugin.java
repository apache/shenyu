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

package org.apache.shenyu.plugin.tracing;

import io.opentelemetry.api.trace.Span;
import io.opentelemetry.api.trace.SpanKind;
import io.opentelemetry.api.trace.Tracer;
import io.opentelemetry.context.Context;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.tracing.context.ReactorTracingContext;
import org.apache.shenyu.plugin.tracing.extract.TraceContextExtractor;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.concurrent.atomic.AtomicReference;

/**
 * The tracing plugin.
 *
 * <p>The plugin continues the trace of an inbound request, or starts a new trace when the request
 * carries no valid context, and starts a ShenYu server span which covers the remaining plugin
 * chain. The resulting trace id is exposed through the {@code shenyu-agent-trace-id} exchange
 * attribute, which the logging plugins already read, and the span is ended once the reactive
 * request completes, fails or is cancelled.</p>
 *
 * <p>What the span records is built by {@link SpanAttributesBuilder}, and the recorded attributes and
 * the span name are bounded values.</p>
 *
 * <p>The span is created for every subscription, so the span of an attempt is never ended twice.
 * The gateway chain itself is subscribed once per request, the guarantee matters for callers which
 * subscribe the returned {@code Mono} more than once.</p>
 *
 * <p>A deployment which already instruments the gateway, for example with a java agent, can disable
 * the span creation: the plugin then only propagates the incoming context, so that the request is not
 * traced twice.</p>
 */
public class TracingPlugin implements ShenyuPlugin {

    private final Tracer tracer;

    private final TraceContextExtractor extractor;

    private final boolean createSpan;

    /**
     * Instantiates a new tracing plugin.
     *
     * @param tracer the tracer which creates the gateway spans
     */
    public TracingPlugin(final Tracer tracer) {
        this(tracer, new TraceContextExtractor(), true);
    }

    /**
     * Instantiates a new tracing plugin.
     *
     * @param tracer the tracer which creates the gateway spans
     * @param extractor the extractor of the inbound trace context
     */
    public TracingPlugin(final Tracer tracer, final TraceContextExtractor extractor) {
        this(tracer, extractor, true);
    }

    /**
     * Instantiates a new tracing plugin.
     *
     * @param tracer the tracer which creates the gateway spans
     * @param extractor the extractor of the inbound trace context
     * @param createSpan whether the plugin creates a span of its own, false for a deployment which
     *                   instruments the gateway already, for example with a java agent, where the
     *                   request is only propagated
     */
    public TracingPlugin(final Tracer tracer, final TraceContextExtractor extractor, final boolean createSpan) {
        this.tracer = tracer;
        this.extractor = extractor;
        this.createSpan = createSpan;
    }

    @Override
    public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        // the remaining chain is invoked here, while the plugin chain is assembled, which is how every
        // ShenYu plugin behaves, while the work it describes only runs once it is subscribed. The span
        // is bound to the subscription, so it is created once per subscription and ended exactly once;
        // the gateway subscribes the chain of a request a single time.
        final Mono<Void> chainResult = chain.execute(exchange);
        return Mono.defer(() -> withSpan(exchange, chainResult));
    }

    @Override
    public int getOrder() {
        return PluginEnum.TRACING.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.TRACING.getName();
    }

    /**
     * Starts a span for one subscription of the request and ends it with that subscription.
     *
     * @param exchange the current exchange
     * @param chainResult the assembled remaining plugin chain
     * @return the chain wrapped by the span lifecycle
     */
    private Mono<Void> withSpan(final ServerWebExchange exchange, final Mono<Void> chainResult) {
        final Context parent = parentContext(exchange);
        if (!createSpan) {
            // the deployment instruments the gateway already, so the request is propagated without
            // creating a second span for it: the active context of the instrumentation is the
            // current context, which the access log and the outgoing call see as well.
            ReactorTracingContext.store(exchange, parent);
            exposeTraceId(exchange, Span.fromContext(parent));
            return chainResult
                    .contextWrite(context -> context.put(TracingConstants.REACTOR_CONTEXT_KEY, parent));
        }
        final Span span = tracer.spanBuilder(SpanAttributesBuilder.spanName(exchange))
                .setSpanKind(SpanKind.SERVER)
                .setParent(parent)
                .startSpan();
        SpanAttributesBuilder.recordRequest(exchange, span);
        final Context tracingContext = parent.with(span);
        ReactorTracingContext.store(exchange, tracingContext);
        exposeTraceId(exchange, span);
        // the failure is captured rather than handled in doOnError, so that the outcome of the
        // request is written to the span in one place and its status is never set twice.
        final AtomicReference<Throwable> failure = new AtomicReference<>();
        return chainResult
                .contextWrite(context -> context.put(TracingConstants.REACTOR_CONTEXT_KEY, tracingContext))
                .doOnError(failure::set)
                .doFinally(signalType -> {
                    // the span is ended whatever happens while the outcome is recorded, otherwise a
                    // failure there would leave the span open.
                    try {
                        SpanAttributesBuilder.recordOutcome(exchange, span, failure.get(), signalType);
                    } finally {
                        span.end();
                    }
                });
    }

    /**
     * Acquires the context which the request continues.
     *
     * <p>A deployment which instruments the gateway itself, for example with a java agent, has an
     * active context of its own for the request: continuing it keeps the gateway span of the agent in
     * the trace instead of skipping it. The context of the headers of the client is used while no
     * instrumentation is active, which is the normal case of the gateway.</p>
     *
     * @param exchange the current exchange
     * @return the context of the request
     */
    private Context parentContext(final ServerWebExchange exchange) {
        final Context active = Context.current();
        return Span.fromContext(active).getSpanContext().isValid() ? active : extractor.extract(exchange);
    }

    /**
     * Exposes the trace id of the span through the exchange attribute which the logging plugins read.
     *
     * @param exchange the current exchange
     * @param span the gateway span of the request
     */
    private void exposeTraceId(final ServerWebExchange exchange, final Span span) {
        if (!span.getSpanContext().isValid()) {
            return;
        }
        exchange.getAttributes().put(Constants.SHENYU_AGENT_TRACE_ID, span.getSpanContext().getTraceId());
    }
}
