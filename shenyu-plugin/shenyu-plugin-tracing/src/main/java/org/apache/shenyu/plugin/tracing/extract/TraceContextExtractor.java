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

package org.apache.shenyu.plugin.tracing.extract;

import io.opentelemetry.api.baggage.propagation.W3CBaggagePropagator;
import io.opentelemetry.api.trace.propagation.W3CTraceContextPropagator;
import io.opentelemetry.context.Context;
import io.opentelemetry.context.propagation.TextMapGetter;
import io.opentelemetry.context.propagation.TextMapPropagator;
import org.apache.shenyu.plugin.tracing.TracingConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.web.server.ServerWebExchange;

import java.util.List;
import java.util.Objects;

/**
 * Extracts the W3C trace context and the W3C baggage of an inbound request.
 */
public class TraceContextExtractor {

    private static final Logger LOG = LoggerFactory.getLogger(TraceContextExtractor.class);

    private static final TextMapPropagator PROPAGATOR = TextMapPropagator.composite(
            W3CTraceContextPropagator.getInstance(), W3CBaggagePropagator.getInstance());

    private static final TextMapGetter<HttpHeaders> HEADERS_GETTER = new HttpHeadersGetter();

    /**
     * Extracts the tracing context of the given request headers.
     *
     * <p>An absent or malformed context is not an error, the root context is returned in that case
     * and the caller starts a new trace.</p>
     *
     * @param headers the request headers
     * @return the extracted context, the root context when the request carries no valid context
     */
    public Context extract(final HttpHeaders headers) {
        if (Objects.isNull(headers)) {
            return Context.root();
        }
        try {
            return PROPAGATOR.extract(Context.root(), headers, HEADERS_GETTER);
        } catch (RuntimeException ex) {
            // tracing must never break request forwarding, a broken header only means a new trace.
            LOG.warn("shenyu tracing could not extract the trace context, a new trace will be started", ex);
            return Context.root();
        }
    }

    /**
     * Extracts the tracing context of the given exchange.
     *
     * @param exchange the current exchange
     * @return the extracted context, the root context when the request carries no valid context
     */
    public Context extract(final ServerWebExchange exchange) {
        return extract(exchange.getRequest().getHeaders());
    }

    /**
     * Reads the tracing headers of a request.
     */
    private static final class HttpHeadersGetter implements TextMapGetter<HttpHeaders> {

        @Override
        public Iterable<String> keys(final HttpHeaders carrier) {
            return carrier.keySet();
        }

        @Override
        public String get(final HttpHeaders carrier, final String key) {
            if (Objects.isNull(carrier) || Objects.isNull(key)) {
                return null;
            }
            List<String> values = carrier.get(key);
            if (Objects.isNull(values) || values.isEmpty()) {
                return null;
            }
            if (values.size() > 1 && TracingConstants.TRACEPARENT.equalsIgnoreCase(key)) {
                // the W3C trace context rejects a request which repeats the traceparent header,
                // because a repeated header is a known trace context injection vector.
                return null;
            }
            if (values.size() > 1) {
                // the W3C propagation headers may be split over several header lines, a repeated
                // header is equivalent to a single comma separated value.
                return String.join(",", values);
            }
            return values.get(0);
        }
    }
}
