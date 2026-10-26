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

import io.opentelemetry.api.baggage.Baggage;
import io.opentelemetry.api.trace.Span;
import io.opentelemetry.api.trace.SpanContext;
import io.opentelemetry.api.trace.Tracer;
import io.opentelemetry.context.Context;
import io.opentelemetry.sdk.trace.SdkTracerProvider;
import org.apache.shenyu.plugin.api.tracing.TraceContextCarrier;
import org.apache.shenyu.plugin.api.tracing.TraceContextPropagator;
import org.apache.shenyu.plugin.tracing.TracingConstants;
import org.apache.shenyu.plugin.tracing.context.ReactorTracingContext;
import org.apache.shenyu.plugin.tracing.extract.TraceContextExtractor;
import org.apache.shenyu.spi.ExtensionLoader;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpHeaders;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;

import java.util.List;

/**
 * The Test Case For the W3C propagator which injects the gateway context into an outgoing call.
 */
public class W3cTraceContextPropagatorTest {

    private static final String CLIENT_TRACE_ID = "4bf92f3577b34da6a3ce929d0e0e4736";

    private static final String CLIENT_SPAN_ID = "00f067aa0ba902b7";

    private static final String STALE_TRACEPARENT = "00-" + CLIENT_TRACE_ID + "-" + CLIENT_SPAN_ID + "-01";

    private final TraceContextExtractor extractor = new TraceContextExtractor();

    private final TraceContextPropagator propagator = new W3cTraceContextPropagator();

    private SdkTracerProvider tracerProvider;

    private Tracer tracer;

    @BeforeEach
    public void setUp() {
        this.tracerProvider = SdkTracerProvider.builder().build();
        this.tracer = tracerProvider.get(TracingConstants.INSTRUMENTATION_NAME);
    }

    @AfterEach
    public void tearDown() {
        tracerProvider.close();
    }

    @Test
    public void testInjectsTheContextOfTheGatewaySpan() {
        final ServerWebExchange exchange = createExchange();
        Span gatewaySpan = tracer.spanBuilder("GET /http/order").startSpan();
        ReactorTracingContext.store(exchange, Context.root().with(gatewaySpan));
        HeadersTraceContextCarrier carrier = new HeadersTraceContextCarrier();
        // the headers of the client are copied to the outgoing request before the propagator runs
        carrier.set(TracingConstants.TRACEPARENT, STALE_TRACEPARENT);
        propagator.inject(exchange, carrier);
        // the upstream has to receive exactly one traceparent, and it has to be the gateway span, in
        // the trace the gateway continues from the client.
        List<String> injected = carrier.getHeaders().get(TracingConstants.TRACEPARENT);
        Assertions.assertEquals(1, injected.size());
        SpanContext upstreamParent = Span.fromContext(extractor.extract(carrier.getHeaders())).getSpanContext();
        Assertions.assertTrue(upstreamParent.isValid());
        Assertions.assertTrue(upstreamParent.isRemote());
        Assertions.assertEquals(gatewaySpan.getSpanContext().getSpanId(), upstreamParent.getSpanId());
        Assertions.assertEquals(gatewaySpan.getSpanContext().getTraceId(), upstreamParent.getTraceId());
        Assertions.assertTrue(upstreamParent.isSampled());
        gatewaySpan.end();
    }

    @Test
    public void testContinuesTheTraceAndTheBaggageOfTheClient() {
        final ServerWebExchange exchange = createExchange();
        HttpHeaders clientHeaders = new HttpHeaders();
        clientHeaders.add(TracingConstants.TRACEPARENT, STALE_TRACEPARENT);
        clientHeaders.add("tracestate", "vendor=value");
        clientHeaders.add("baggage", "userId=alice");
        Context clientContext = extractor.extract(clientHeaders);
        // the gateway span continues the trace of the client, exactly like the tracing plugin does
        Span gatewaySpan = tracer.spanBuilder("GET /http/order").setParent(clientContext).startSpan();
        ReactorTracingContext.store(exchange, clientContext.with(gatewaySpan));
        HeadersTraceContextCarrier carrier = new HeadersTraceContextCarrier();
        propagator.inject(exchange, carrier);
        SpanContext upstreamParent = Span.fromContext(extractor.extract(carrier.getHeaders())).getSpanContext();
        Assertions.assertEquals(CLIENT_TRACE_ID, upstreamParent.getTraceId());
        Assertions.assertEquals(gatewaySpan.getSpanContext().getSpanId(), upstreamParent.getSpanId());
        Assertions.assertEquals("value", upstreamParent.getTraceState().get("vendor"));
        Context upstreamContext = extractor.extract(carrier.getHeaders());
        Assertions.assertEquals("alice", Baggage.fromContext(upstreamContext).getEntryValue("userId"));
        gatewaySpan.end();
    }

    @Test
    public void testKeepsTheContextOfTheClientWhileTracingIsNotActive() {
        final ServerWebExchange exchange = createExchange();
        HeadersTraceContextCarrier carrier = new HeadersTraceContextCarrier();
        carrier.set(TracingConstants.TRACEPARENT, STALE_TRACEPARENT);
        propagator.inject(exchange, carrier);
        // tracing is not active for this request, so the client context is forwarded untouched
        Assertions.assertEquals(List.of(STALE_TRACEPARENT), carrier.getHeaders().get(TracingConstants.TRACEPARENT));
    }

    @Test
    public void testClearsTheTraceStateOfTheClientWhichIsNotContinued() {
        final ServerWebExchange exchange = createExchange();
        Span gatewaySpan = tracer.spanBuilder("GET /http/order").startSpan();
        ReactorTracingContext.store(exchange, Context.root().with(gatewaySpan));
        HeadersTraceContextCarrier carrier = new HeadersTraceContextCarrier();
        // the client sent a trace state which does not belong to the context the gateway injects
        carrier.set(TracingConstants.TRACESTATE, "stale=value");
        propagator.inject(exchange, carrier);
        List<String> injected = carrier.getHeaders().get(TracingConstants.TRACEPARENT);
        Assertions.assertEquals(1, injected.size());
        Assertions.assertNull(carrier.getHeaders().getFirst(TracingConstants.TRACESTATE));
        gatewaySpan.end();
    }

    @Test
    public void testPropagatesATraceWhichTheClientDidNotSample() {
        final ServerWebExchange exchange = createExchange();
        HttpHeaders clientHeaders = new HttpHeaders();
        clientHeaders.add(TracingConstants.TRACEPARENT, "00-" + CLIENT_TRACE_ID + "-" + CLIENT_SPAN_ID + "-00");
        Context clientContext = extractor.extract(clientHeaders);
        Span gatewaySpan = tracer.spanBuilder("GET /http/order").setParent(clientContext).startSpan();
        ReactorTracingContext.store(exchange, clientContext.with(gatewaySpan));
        HeadersTraceContextCarrier carrier = new HeadersTraceContextCarrier();
        propagator.inject(exchange, carrier);
        SpanContext upstreamParent = Span.fromContext(extractor.extract(carrier.getHeaders())).getSpanContext();
        // the decision of the client is kept: the upstream continues the trace and does not record it
        Assertions.assertEquals(CLIENT_TRACE_ID, upstreamParent.getTraceId());
        Assertions.assertEquals(gatewaySpan.getSpanContext().getSpanId(), upstreamParent.getSpanId());
        Assertions.assertFalse(upstreamParent.isSampled());
        gatewaySpan.end();
    }

    @Test
    public void testPropagatorIsRegisteredForTheHttpProtocol() {
        TraceContextPropagator registered = ExtensionLoader.getExtensionLoader(TraceContextPropagator.class)
                .getJoin(TraceContextPropagator.HTTP);
        Assertions.assertInstanceOf(W3cTraceContextPropagator.class, registered);
    }

    private ServerWebExchange createExchange() {
        return MockServerWebExchange.from(MockServerHttpRequest.get("/http/order").build());
    }

    /**
     * The carrier of an outgoing http request, which is the headers of the request in production.
     */
    private static final class HeadersTraceContextCarrier implements TraceContextCarrier {

        private final HttpHeaders headers = new HttpHeaders();

        @Override
        public void set(final String name, final String value) {
            headers.set(name, value);
        }

        @Override
        public void remove(final String name) {
            headers.remove(name);
        }

        private HttpHeaders getHeaders() {
            return headers;
        }
    }
}
