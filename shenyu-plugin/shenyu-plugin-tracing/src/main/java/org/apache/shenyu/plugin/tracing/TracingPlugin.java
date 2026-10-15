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
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.tracing.context.ReactorTracingContext;
import org.apache.shenyu.plugin.tracing.extract.TraceContextExtractor;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Objects;

/**
 * The tracing plugin.
 *
 * <p>The plugin continues the trace of an inbound request, or starts a new trace when the request
 * carries no valid context, and starts a ShenYu server span which covers the remaining plugin
 * chain. The resulting trace id is exposed through the {@code shenyu-agent-trace-id} exchange
 * attribute, which the logging plugins already read, and the span is ended once the reactive
 * request completes, fails or is cancelled.</p>
 *
 * <p>The span is created for every subscription, so the span of an attempt is never ended twice.
 * The gateway chain itself is subscribed once per request, the guarantee matters for callers which
 * subscribe the returned {@code Mono} more than once.</p>
 */
public class TracingPlugin implements ShenyuPlugin {

    private final Tracer tracer;

    private final TraceContextExtractor extractor;

    /**
     * Instantiates a new tracing plugin.
     *
     * @param tracer the tracer which creates the gateway spans
     */
    public TracingPlugin(final Tracer tracer) {
        this(tracer, new TraceContextExtractor());
    }

    /**
     * Instantiates a new tracing plugin.
     *
     * @param tracer the tracer which creates the gateway spans
     * @param extractor the extractor of the inbound trace context
     */
    public TracingPlugin(final Tracer tracer, final TraceContextExtractor extractor) {
        this.tracer = tracer;
        this.extractor = extractor;
    }

    @Override
    public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        // the remaining chain is assembled eagerly, which is how every ShenYu plugin behaves. The
        // span is bound to the subscription instead, so that it is created once per subscription and
        // ended exactly once; the gateway subscribes the chain of a request a single time.
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
        final Context parent = extractor.extract(exchange);
        final Span span = tracer.spanBuilder(spanName(exchange))
                .setSpanKind(SpanKind.SERVER)
                .setParent(parent)
                .startSpan();
        final Context tracingContext = parent.with(span);
        ReactorTracingContext.store(exchange, tracingContext);
        exposeTraceId(exchange, span);
        return chainResult
                .contextWrite(context -> context.put(TracingConstants.REACTOR_CONTEXT_KEY, tracingContext))
                .doFinally(signalType -> span.end());
    }

    /**
     * Builds the span name of the request from the http method and the request path.
     *
     * @param exchange the current exchange
     * @return the span name
     */
    private String spanName(final ServerWebExchange exchange) {
        final ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        if (Objects.isNull(shenyuContext)) {
            return TracingConstants.DEFAULT_SPAN_NAME;
        }
        final String method = shenyuContext.getHttpMethod();
        final String path = shenyuContext.getPath();
        if (StringUtils.isBlank(path)) {
            return StringUtils.isBlank(method) ? TracingConstants.DEFAULT_SPAN_NAME : method;
        }
        return StringUtils.isBlank(method) ? path : method + " " + path;
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
