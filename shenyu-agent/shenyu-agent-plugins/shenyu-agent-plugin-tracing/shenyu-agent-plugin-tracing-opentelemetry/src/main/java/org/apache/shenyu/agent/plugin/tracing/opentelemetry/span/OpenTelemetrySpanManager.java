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

package org.apache.shenyu.agent.plugin.tracing.opentelemetry.span;

import io.opentelemetry.api.GlobalOpenTelemetry;
import io.opentelemetry.api.trace.Span;
import io.opentelemetry.api.trace.SpanBuilder;
import io.opentelemetry.api.trace.SpanKind;
import io.opentelemetry.context.Context;
import org.apache.shenyu.agent.plugin.tracing.common.constant.TracingConstants;
import org.springframework.web.server.ServerWebExchange;

import java.util.Map;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;

public class OpenTelemetrySpanManager {

    private volatile Span lastSpan;

    private final AtomicInteger count;

    public OpenTelemetrySpanManager() {
        this.count = new AtomicInteger(0);
    }

    /**
     * Start and record a span.
     *
     * @param spanName span name
     * @param attributesMap attributes
     * @return {@linkplain Span}
     */
    public Span startAndRecord(final String spanName, final Map<String, String> attributesMap) {
        SpanBuilder spanBuilder = GlobalOpenTelemetry.getTracer(TracingConstants.SHENYU_AGENT)
                .spanBuilder(spanName)
                .setSpanKind(SpanKind.INTERNAL);
        Optional.ofNullable(attributesMap).ifPresent(attributes -> attributes.forEach(spanBuilder::setAttribute));
        Optional.ofNullable(lastSpan).ifPresent(parentSpan -> spanBuilder.setParent(Context.current().with(parentSpan)));
        Span span = spanBuilder.startSpan();

        count.incrementAndGet();
        lastSpan = span;

        return span;
    }

    /**
     * Finish span.
     *
     * @param span {@linkplain Span}
     * @param exchange webflux server object
     */
    public void finish(final Span span, final ServerWebExchange exchange) {
        span.end();
        if (count.decrementAndGet() == 0) {
            exchange.getAttributes().remove(TracingConstants.SHENYU_AGENT);
        }
    }
}
