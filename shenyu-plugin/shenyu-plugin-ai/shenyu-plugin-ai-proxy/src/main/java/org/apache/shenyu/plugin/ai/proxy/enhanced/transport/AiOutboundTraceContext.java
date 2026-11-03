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

package org.apache.shenyu.plugin.ai.proxy.enhanced.transport;

import io.opentelemetry.api.trace.Span;
import io.opentelemetry.api.trace.propagation.W3CTraceContextPropagator;
import io.opentelemetry.context.Context;
import org.apache.shenyu.common.tracing.AgentTraceContext;
import reactor.util.context.ContextView;

import java.util.HashMap;
import java.util.Map;

/**
 * Optional OTel API bridge using explicit Reactor parents only, never ambient context.
 */
final class AiOutboundTraceContext {

    private AiOutboundTraceContext() {
    }

    static Map<String, String> headers(final ContextView view) {
        try {
            Object parent = view.getOrDefault(AgentTraceContext.GATEWAY_REACTOR_CONTEXT_KEY, null);
            if (!(parent instanceof Context context) || !Span.fromContext(context).getSpanContext().isValid()) {
                parent = view.getOrDefault(Context.class, null);
            }
            if (!(parent instanceof Context context) || !Span.fromContext(context).getSpanContext().isValid()) {
                return Map.of();
            }
            Context isolated = Context.root().with(Span.wrap(Span.fromContext(context).getSpanContext()));
            Map<String, String> carrier = new HashMap<>();
            W3CTraceContextPropagator.getInstance().inject(isolated, carrier, Map::put);
            return AgentTraceContext.headers(carrier);
        } catch (RuntimeException ignored) {
            // Invalid diagnostic context cannot fail the business request.
            return Map.of();
        }
    }
}
