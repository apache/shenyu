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

package org.apache.shenyu.agent.plugin.tracing.jaeger.span;

import io.opentracing.Scope;
import io.opentracing.Span;
import io.opentracing.Tracer;
import io.opentracing.util.GlobalTracer;
import org.apache.shenyu.agent.plugin.tracing.jaeger.constant.JaegerConstants;
import org.springframework.web.server.ServerWebExchange;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;

public class JaegerSpanManager {

    private final List<Scope> scopeList;

    private volatile Span lastSpan;

    private final AtomicInteger count;

    /**
     * construct to init.
     */
    public JaegerSpanManager() {
        scopeList = new LinkedList<>();
        count = new AtomicInteger(0);
    }

    /**
     * add span message to create new span.
     *
     * @param buildSpan span name
     * @param tagMap span tag
     * @return {@linkplain Span}
     */
    public Span add(final String buildSpan, final Map<String, String> tagMap) {
        Tracer tracer = GlobalTracer.get();

        Tracer.SpanBuilder spanBuilder = tracer.buildSpan(buildSpan);
        if (Objects.nonNull(lastSpan)) {
            spanBuilder.asChildOf(lastSpan);
        } else {
            spanBuilder.ignoreActiveSpan();
        }

        tagMap.forEach(spanBuilder::withTag);
        Span span = spanBuilder.start();
        Scope scope = tracer.scopeManager().activate(span);

        lastSpan = span;
        count.incrementAndGet();
        scopeList.add(scope);

        return lastSpan;
    }

    /**
     * finish span record.
     *
     * @param span {@linkplain Span}
     * @param exchange webflux server object
     */
    public void finish(final Span span, final ServerWebExchange exchange) {
        span.finish();
        if (count.decrementAndGet() == 0) {
            scopeList.forEach(Scope::close);
            exchange.getAttributes().remove(JaegerConstants.RESPONSE_SPAN);
        }
    }

    /**
     * record error.
     *
     * @param span {@linkplain Span}
     * @param exchange webflux server object
     * @param throwable error
     */
    public void error(final Span span, final ServerWebExchange exchange, final Throwable throwable) {
        JaegerErrorSpan.setError(span, throwable);
        finish(span, exchange);
    }

}
