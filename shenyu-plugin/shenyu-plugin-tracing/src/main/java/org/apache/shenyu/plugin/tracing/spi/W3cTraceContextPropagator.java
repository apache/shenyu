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

package org.apache.shenyu.plugin.tracing.spi;

import io.opentelemetry.api.baggage.propagation.W3CBaggagePropagator;
import io.opentelemetry.api.trace.Span;
import io.opentelemetry.api.trace.propagation.W3CTraceContextPropagator;
import io.opentelemetry.context.Context;
import io.opentelemetry.context.propagation.TextMapPropagator;
import io.opentelemetry.context.propagation.TextMapSetter;
import org.apache.shenyu.plugin.api.tracing.TraceContextCarrier;
import org.apache.shenyu.plugin.api.tracing.TraceContextPropagator;
import org.apache.shenyu.plugin.tracing.TracingConstants;
import org.apache.shenyu.plugin.tracing.context.ReactorTracingContext;
import org.apache.shenyu.spi.Join;
import org.springframework.web.server.ServerWebExchange;

/**
 * Injects the trace context of the gateway span with the W3C trace context and the W3C baggage,
 * delegating the encoding to the OpenTelemetry propagators.
 *
 * <p>The injected context is the context of the gateway span, so the upstream call becomes a child of
 * the gateway span, in the same trace the client started. When the request has no tracing context,
 * for example because tracing is disabled, nothing is injected and the context of the client keeps
 * being forwarded as it was.</p>
 */
@Join
public class W3cTraceContextPropagator implements TraceContextPropagator {

    private static final TextMapPropagator PROPAGATOR = TextMapPropagator.composite(
            W3CTraceContextPropagator.getInstance(), W3CBaggagePropagator.getInstance());

    private static final TextMapSetter<TraceContextCarrier> SETTER = TraceContextCarrier::set;

    @Override
    public void inject(final ServerWebExchange exchange, final TraceContextCarrier carrier) {
        final Context context = ReactorTracingContext.current(exchange);
        if (!Span.fromContext(context).getSpanContext().isValid()) {
            return;
        }
        // the headers of the client are copied to the outgoing request, so the context of the client
        // is dropped before the context of the gateway span is written: the upstream must receive
        // exactly one trace context and it has to belong to the gateway span. The trace state is
        // dropped as well, because a trace state of the client which does not belong to the context
        // we inject would otherwise stay on the outgoing request.
        carrier.remove(TracingConstants.TRACEPARENT);
        carrier.remove(TracingConstants.TRACESTATE);
        PROPAGATOR.inject(context, carrier, SETTER);
    }
}
