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

import io.opentelemetry.api.baggage.Baggage;
import io.opentelemetry.api.trace.Span;
import io.opentelemetry.api.trace.SpanContext;
import io.opentelemetry.api.trace.TraceFlags;
import io.opentelemetry.context.Context;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.http.HttpHeaders;

/**
 * The Test Case For TraceContextExtractor.
 */
public final class TraceContextExtractorTest {

    private static final String TRACE_ID = "4bf92f3577b34da6a3ce929d0e0e4736";

    private static final String PARENT_SPAN_ID = "00f067aa0ba902b7";

    private static final String NOT_SAMPLED = "00";

    private final TraceContextExtractor extractor = new TraceContextExtractor();

    @Test
    public void testExtractTraceContext() {
        SpanContext spanContext = extract(createHeaders("traceparent", "00-" + TRACE_ID + "-" + PARENT_SPAN_ID + "-01"));
        Assertions.assertTrue(spanContext.isValid());
        Assertions.assertTrue(spanContext.isRemote());
        Assertions.assertEquals(TRACE_ID, spanContext.getTraceId());
        Assertions.assertEquals(PARENT_SPAN_ID, spanContext.getSpanId());
        Assertions.assertEquals(TraceFlags.getSampled(), spanContext.getTraceFlags());
    }

    @Test
    public void testExtractNotSampledTraceContext() {
        SpanContext spanContext = extract(createHeaders("traceparent", "00-" + TRACE_ID + "-" + PARENT_SPAN_ID + "-" + NOT_SAMPLED));
        Assertions.assertTrue(spanContext.isValid());
        Assertions.assertEquals(TraceFlags.getDefault(), spanContext.getTraceFlags());
    }

    @Test
    public void testExtractBaggage() {
        Context context = extractor.extract(createHeaders("baggage", "userId=alice"));
        Assertions.assertEquals("alice", Baggage.fromContext(context).getEntryValue("userId"));
    }

    @Test
    public void testExtractTraceState() {
        HttpHeaders headers = createHeaders("traceparent", "00-" + TRACE_ID + "-" + PARENT_SPAN_ID + "-01");
        headers.add("tracestate", "vendor1=value1,vendor2=value2");
        SpanContext spanContext = extract(headers);
        Assertions.assertTrue(spanContext.isValid());
        Assertions.assertEquals("value1", spanContext.getTraceState().get("vendor1"));
        Assertions.assertEquals("value2", spanContext.getTraceState().get("vendor2"));
    }

    @Test
    public void testRepeatedTraceStateAndBaggageAreCombined() {
        // the propagation headers may be split over several header lines on the wire, the receiver
        // has to combine them instead of dropping everything after the first one.
        HttpHeaders headers = createHeaders("traceparent", "00-" + TRACE_ID + "-" + PARENT_SPAN_ID + "-01");
        headers.add("tracestate", "vendor1=value1");
        headers.add("tracestate", "vendor2=value2");
        headers.add("baggage", "userId=alice");
        headers.add("baggage", "tenantId=acme");
        Context context = extractor.extract(headers);
        SpanContext spanContext = Span.fromContext(context).getSpanContext();
        Assertions.assertTrue(spanContext.isValid());
        Assertions.assertEquals("value1", spanContext.getTraceState().get("vendor1"));
        Assertions.assertEquals("value2", spanContext.getTraceState().get("vendor2"));
        Assertions.assertEquals("alice", Baggage.fromContext(context).getEntryValue("userId"));
        Assertions.assertEquals("acme", Baggage.fromContext(context).getEntryValue("tenantId"));
    }

    @Test
    public void testDuplicatedTraceparentIsRejected() {
        HttpHeaders headers = createHeaders("traceparent", "00-" + TRACE_ID + "-" + PARENT_SPAN_ID + "-01");
        headers.add("traceparent", "00-" + TRACE_ID + "-" + PARENT_SPAN_ID + "-01");
        Assertions.assertFalse(extract(headers).isValid());
    }

    @Test
    public void testReservedTraceFlagsAreAcceptedAndIgnored() {
        // version 00 only defines the sampled bit, the remaining flag bits are reserved and a
        // receiver must ignore them instead of rejecting the trace context.
        SpanContext reservedFlag = extract(createHeaders("traceparent", "00-" + TRACE_ID + "-" + PARENT_SPAN_ID + "-02"));
        Assertions.assertTrue(reservedFlag.isValid());
        Assertions.assertFalse(reservedFlag.isSampled());
        SpanContext everyFlag = extract(createHeaders("traceparent", "00-" + TRACE_ID + "-" + PARENT_SPAN_ID + "-ff"));
        Assertions.assertTrue(everyFlag.isValid());
        Assertions.assertTrue(everyFlag.isSampled());
    }

    @Test
    public void testExtractWithoutHeadersReturnsRootContext() {
        Assertions.assertFalse(extract(new HttpHeaders()).isValid());
        Assertions.assertFalse(Span.fromContext(extractor.extract((HttpHeaders) null)).getSpanContext().isValid());
    }

    /**
     * A malformed trace context must never be treated as a valid parent, the gateway starts a new
     * trace instead and the request keeps flowing.
     *
     * @param traceparent a malformed traceparent header value
     */
    @ParameterizedTest
    @ValueSource(strings = {
            "",
            "00",
            "00-" + TRACE_ID + "-" + PARENT_SPAN_ID,
            "00-00000000000000000000000000000000-" + PARENT_SPAN_ID + "-01",
            "00-" + TRACE_ID + "-0000000000000000-01",
            "00-" + TRACE_ID + "-" + PARENT_SPAN_ID + "-zz",
            "ff-" + TRACE_ID + "-" + PARENT_SPAN_ID + "-01",
            "00-" + TRACE_ID + PARENT_SPAN_ID + "-01",
            "00-4BF92F3577B34DA6A3CE929D0E0E4736-" + PARENT_SPAN_ID + "-01",
            "00-" + TRACE_ID + "-00F067AA0BA902B7-01"
    })
    public void testExtractMalformedTraceContext(final String traceparent) {
        SpanContext spanContext = extract(createHeaders("traceparent", traceparent));
        Assertions.assertFalse(spanContext.isValid());
    }

    private HttpHeaders createHeaders(final String name, final String value) {
        HttpHeaders headers = new HttpHeaders();
        headers.add(name, value);
        return headers;
    }

    private SpanContext extract(final HttpHeaders headers) {
        return Span.fromContext(extractor.extract(headers)).getSpanContext();
    }
}
