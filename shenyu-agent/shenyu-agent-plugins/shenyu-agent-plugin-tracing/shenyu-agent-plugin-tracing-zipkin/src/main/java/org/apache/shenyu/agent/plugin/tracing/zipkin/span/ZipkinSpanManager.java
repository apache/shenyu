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

package org.apache.shenyu.agent.plugin.tracing.zipkin.span;

import brave.Span;
import brave.Tracing;
import org.apache.shenyu.agent.plugin.tracing.common.constant.TracingConstants;
import org.springframework.web.server.ServerWebExchange;

import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;

public class ZipkinSpanManager {

    private volatile Span lastSpan;

    private final AtomicInteger count;

    public ZipkinSpanManager() {
        this.count = new AtomicInteger(0);
    }

    /**
     * Start span.
     *
     * @param spanName span name
     * @param tagMap  tag
     * @return {@linkplain Span}
     */
    public Span start(final String spanName, final Map<String, String> tagMap) {
        Span span;
        if (Objects.isNull(lastSpan)) {
            span = Tracing.currentTracer().nextSpan().name(spanName);
        } else {
            span = Tracing.currentTracer().newChild(lastSpan.context()).name(spanName);
        }

        tagMap.forEach(span::tag);
        span.start();

        count.incrementAndGet();
        lastSpan = span;

        return span;
    }

    /**
     * Finish span.
     *
     * @param span     {@linkplain Span}
     * @param exchange webflux server object
     */
    public void finish(final Span span, final ServerWebExchange exchange) {
        span.finish();
        if (count.decrementAndGet() == 0) {
            exchange.getAttributes().remove(TracingConstants.SHENYU_AGENT);
        }
    }
}
