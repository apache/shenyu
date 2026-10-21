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
import io.opentelemetry.api.trace.TracerProvider;
import io.opentelemetry.context.Context;
import io.opentelemetry.context.Scope;
import io.opentelemetry.sdk.testing.exporter.InMemorySpanExporter;
import io.opentelemetry.sdk.trace.SdkTracerProvider;
import io.opentelemetry.sdk.trace.data.SpanData;
import io.opentelemetry.sdk.trace.export.SimpleSpanProcessor;
import io.opentelemetry.sdk.trace.samplers.Sampler;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.tracing.context.ReactorTracingContext;
import org.apache.shenyu.plugin.tracing.extract.TraceContextExtractor;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.ArgumentMatchers;
import org.mockito.Mockito;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;
import reactor.test.StepVerifier;

import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;

/**
 * The Test Case For TracingPlugin.
 */
public class TracingPluginTest {

    private static final String TRACE_ID = "4bf92f3577b34da6a3ce929d0e0e4736";

    private static final String PARENT_SPAN_ID = "00f067aa0ba902b7";

    private static final String PATH = "/http/order/findById";

    private InMemorySpanExporter spanExporter;

    private SdkTracerProvider tracerProvider;

    private TracingPlugin tracingPlugin;

    private ShenyuPluginChain chain;

    @BeforeEach
    public void setUp() {
        this.spanExporter = InMemorySpanExporter.create();
        // the same sampler as the tracing starter configures
        this.tracerProvider = SdkTracerProvider.builder()
                .addSpanProcessor(SimpleSpanProcessor.create(spanExporter))
                .setSampler(Sampler.parentBased(Sampler.alwaysOn()))
                .build();
        this.tracingPlugin = new TracingPlugin(tracerProvider.get(TracingConstants.INSTRUMENTATION_NAME));
        this.chain = Mockito.mock(ShenyuPluginChain.class);
    }

    @AfterEach
    public void tearDown() {
        tracerProvider.close();
    }

    @Test
    public void testContinuesInboundTrace() {
        Mockito.when(chain.execute(ArgumentMatchers.any())).thenReturn(Mono.empty());
        ServerWebExchange exchange = createExchange("00-" + TRACE_ID + "-" + PARENT_SPAN_ID + "-01");
        StepVerifier.create(tracingPlugin.execute(exchange, chain)).expectSubscription().verifyComplete();
        List<SpanData> spans = spanExporter.getFinishedSpanItems();
        Assertions.assertEquals(1, spans.size());
        SpanData span = spans.get(0);
        Assertions.assertEquals(TRACE_ID, span.getTraceId());
        Assertions.assertEquals(PARENT_SPAN_ID, span.getParentSpanId());
        Assertions.assertTrue(span.getParentSpanContext().isRemote());
        Assertions.assertEquals(SpanKind.SERVER, span.getKind());
        // the name follows the http conventions: the method and the matched route, here no api is
        // registered, so the request is named by its method alone.
        Assertions.assertEquals("GET", span.getName());
        Assertions.assertEquals(TRACE_ID, exchange.getAttribute(Constants.SHENYU_AGENT_TRACE_ID));
    }

    @Test
    public void testStartsNewTraceWithoutInboundContext() {
        Mockito.when(chain.execute(ArgumentMatchers.any())).thenReturn(Mono.empty());
        ServerWebExchange exchange = createExchange(null);
        StepVerifier.create(tracingPlugin.execute(exchange, chain)).expectSubscription().verifyComplete();
        List<SpanData> spans = spanExporter.getFinishedSpanItems();
        Assertions.assertEquals(1, spans.size());
        SpanData span = spans.get(0);
        Assertions.assertTrue(span.getSpanContext().isValid());
        Assertions.assertNotEquals(TRACE_ID, span.getTraceId());
        Assertions.assertFalse(span.getParentSpanContext().isValid());
        Assertions.assertEquals(span.getTraceId(), exchange.getAttribute(Constants.SHENYU_AGENT_TRACE_ID));
    }

    /**
     * A malformed inbound context must not be trusted as a parent, tracing starts a new trace and
     * the request keeps flowing.
     *
     * @param traceparent a malformed traceparent header value
     */
    @ParameterizedTest
    @ValueSource(strings = {
            "",
            "00",
            "00-" + TRACE_ID + "-" + PARENT_SPAN_ID,
            "00-00000000000000000000000000000000-" + PARENT_SPAN_ID + "-01",
            "00-" + TRACE_ID + "-0000000000000000-01"
    })
    public void testMalformedInboundContextStartsNewTrace(final String traceparent) {
        Mockito.when(chain.execute(ArgumentMatchers.any())).thenReturn(Mono.empty());
        ServerWebExchange exchange = createExchange(traceparent);
        StepVerifier.create(tracingPlugin.execute(exchange, chain)).expectSubscription().verifyComplete();
        List<SpanData> spans = spanExporter.getFinishedSpanItems();
        Assertions.assertEquals(1, spans.size());
        SpanData span = spans.get(0);
        Assertions.assertTrue(span.getSpanContext().isValid());
        Assertions.assertFalse(span.getParentSpanContext().isValid());
        Assertions.assertEquals(span.getTraceId(), exchange.getAttribute(Constants.SHENYU_AGENT_TRACE_ID));
    }

    @Test
    public void testSpanEndsOnErrorSignal() {
        Mockito.when(chain.execute(ArgumentMatchers.any()))
                .thenReturn(Mono.error(new IllegalStateException("upstream failed")));
        ServerWebExchange exchange = createExchange(null);
        StepVerifier.create(tracingPlugin.execute(exchange, chain))
                .expectSubscription()
                .expectError(IllegalStateException.class)
                .verify();
        Assertions.assertEquals(1, spanExporter.getFinishedSpanItems().size());
    }

    @Test
    public void testSpanEndsOnCancellation() {
        Mockito.when(chain.execute(ArgumentMatchers.any())).thenReturn(Mono.never());
        ServerWebExchange exchange = createExchange(null);
        StepVerifier.create(tracingPlugin.execute(exchange, chain)).expectSubscription().thenCancel().verify();
        Assertions.assertEquals(1, spanExporter.getFinishedSpanItems().size());
    }

    @Test
    public void testSpanIsCreatedLazilyOnSubscription() {
        Mockito.when(chain.execute(ArgumentMatchers.any())).thenReturn(Mono.empty());
        ServerWebExchange exchange = createExchange(null);
        Mono<Void> result = tracingPlugin.execute(exchange, chain);
        // the remaining chain is assembled eagerly, but no span exists before the request is subscribed
        Assertions.assertTrue(spanExporter.getFinishedSpanItems().isEmpty());
        Assertions.assertNull(exchange.getAttribute(Constants.SHENYU_AGENT_TRACE_ID));
        StepVerifier.create(result).expectSubscription().verifyComplete();
        Assertions.assertEquals(1, spanExporter.getFinishedSpanItems().size());
    }

    /**
     * The gateway subscribes the chain of a request once, but the returned {@code Mono} is part of the
     * plugin contract: every subscription has to own its own span instead of reusing and ending one
     * span twice.
     */
    @Test
    public void testEverySubscriptionOwnsItsSpan() {
        Mockito.when(chain.execute(ArgumentMatchers.any())).thenReturn(Mono.empty());
        ServerWebExchange exchange = createExchange("00-" + TRACE_ID + "-" + PARENT_SPAN_ID + "-01");
        Mono<Void> result = tracingPlugin.execute(exchange, chain);
        StepVerifier.create(result).expectSubscription().verifyComplete();
        StepVerifier.create(result).expectSubscription().verifyComplete();
        List<SpanData> spans = spanExporter.getFinishedSpanItems();
        // exactly one span per subscription, each of them ended exactly once
        Assertions.assertEquals(2, spans.size());
        Assertions.assertNotEquals(spans.get(0).getSpanId(), spans.get(1).getSpanId());
        spans.forEach(span -> Assertions.assertEquals(TRACE_ID, span.getTraceId()));
    }

    @Test
    public void testChainAssemblyFailureCreatesNoSpan() {
        Mockito.when(chain.execute(ArgumentMatchers.any()))
                .thenThrow(new IllegalStateException("the plugin chain could not be assembled"));
        ServerWebExchange exchange = createExchange(null);
        Assertions.assertThrows(IllegalStateException.class, () -> tracingPlugin.execute(exchange, chain));
        Assertions.assertTrue(spanExporter.getFinishedSpanItems().isEmpty());
    }

    @Test
    public void testTracingContextSurvivesReactorThreadSwitch() {
        AtomicReference<Context> captured = new AtomicReference<>();
        Mockito.when(chain.execute(ArgumentMatchers.any())).thenReturn(
                Mono.deferContextual(view -> Mono.fromSupplier(() -> ReactorTracingContext.current(view)))
                        .subscribeOn(Schedulers.boundedElastic())
                        .doOnNext(captured::set)
                        .then());
        ServerWebExchange exchange = createExchange("00-" + TRACE_ID + "-" + PARENT_SPAN_ID + "-01");
        StepVerifier.create(tracingPlugin.execute(exchange, chain)).expectSubscription().verifyComplete();
        Assertions.assertEquals(TRACE_ID, Span.fromContext(captured.get()).getSpanContext().getTraceId());
    }

    @Test
    public void testNoopTracerKeepsForwardingWithoutTraceId() {
        TracingPlugin plugin = new TracingPlugin(TracerProvider.noop().get(TracingConstants.INSTRUMENTATION_NAME));
        Mockito.when(chain.execute(ArgumentMatchers.any())).thenReturn(Mono.empty());
        ServerWebExchange exchange = createExchange(null);
        StepVerifier.create(plugin.execute(exchange, chain)).expectSubscription().verifyComplete();
        Assertions.assertNull(exchange.getAttribute(Constants.SHENYU_AGENT_TRACE_ID));
        Assertions.assertFalse(Span.fromContext(ReactorTracingContext.current(exchange)).getSpanContext().isValid());
        Assertions.assertTrue(spanExporter.getFinishedSpanItems().isEmpty());
    }

    @Test
    public void testNotSampledInboundTraceIsContinuedWithoutRecording() {
        Mockito.when(chain.execute(ArgumentMatchers.any())).thenReturn(Mono.empty());
        ServerWebExchange exchange = createExchange("00-" + TRACE_ID + "-" + PARENT_SPAN_ID + "-00");
        StepVerifier.create(tracingPlugin.execute(exchange, chain)).expectSubscription().verifyComplete();
        // the client decided not to sample: no span is recorded or exported, but the trace context is
        // still continued and exposed, which is what the access log correlation relies on.
        Assertions.assertTrue(spanExporter.getFinishedSpanItems().isEmpty());
        Assertions.assertEquals(TRACE_ID, exchange.getAttribute(Constants.SHENYU_AGENT_TRACE_ID));
    }

    @Test
    public void testTraceContextStaysReadableAfterTheSpanEnded() {
        Mockito.when(chain.execute(ArgumentMatchers.any())).thenReturn(Mono.empty());
        ServerWebExchange exchange = createExchange("00-" + TRACE_ID + "-" + PARENT_SPAN_ID + "-01");
        StepVerifier.create(tracingPlugin.execute(exchange, chain)).expectSubscription().verifyComplete();
        // readers which run after the chain has terminated, for example the error log of a failed
        // request, still have to see the trace of that request.
        Assertions.assertEquals(TRACE_ID, exchange.getAttribute(Constants.SHENYU_AGENT_TRACE_ID));
        Assertions.assertEquals(TRACE_ID,
                Span.fromContext(ReactorTracingContext.current(exchange)).getSpanContext().getTraceId());
    }

    @Test
    public void testSpanNameFallsBackWithoutShenyuContext() {
        Mockito.when(chain.execute(ArgumentMatchers.any())).thenReturn(Mono.empty());
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("http://localhost/health").build());
        StepVerifier.create(tracingPlugin.execute(exchange, chain)).expectSubscription().verifyComplete();
        Assertions.assertEquals(TracingConstants.DEFAULT_SPAN_NAME, spanExporter.getFinishedSpanItems().get(0).getName());
    }

    @Test
    public void testSpanCreationCanBeDisabled() {
        TracingPlugin plugin = new TracingPlugin(tracerProvider.get(TracingConstants.INSTRUMENTATION_NAME),
                new TraceContextExtractor(), false);
        Mockito.when(chain.execute(ArgumentMatchers.any())).thenReturn(Mono.empty());
        ServerWebExchange exchange = createExchange("00-" + TRACE_ID + "-" + PARENT_SPAN_ID + "-01");
        StepVerifier.create(plugin.execute(exchange, chain)).expectSubscription().verifyComplete();
        // the deployment instruments the gateway itself, so no span is created while the incoming
        // trace is still propagated to the access log and to the upstream call
        Assertions.assertTrue(spanExporter.getFinishedSpanItems().isEmpty());
        Assertions.assertEquals(TRACE_ID, exchange.getAttribute(Constants.SHENYU_AGENT_TRACE_ID));
    }

    @Test
    public void testSpanCreationDisabledWithoutAnIncomingTrace() {
        TracingPlugin plugin = new TracingPlugin(tracerProvider.get(TracingConstants.INSTRUMENTATION_NAME),
                new TraceContextExtractor(), false);
        Mockito.when(chain.execute(ArgumentMatchers.any())).thenReturn(Mono.empty());
        ServerWebExchange exchange = createExchange(null);
        StepVerifier.create(plugin.execute(exchange, chain)).expectSubscription().verifyComplete();
        // a request without an incoming trace has nothing to propagate either
        Assertions.assertTrue(spanExporter.getFinishedSpanItems().isEmpty());
        Assertions.assertNull(exchange.getAttribute(Constants.SHENYU_AGENT_TRACE_ID));
    }

    @Test
    public void testPropagatesTheActiveContextOfAnInstrumentation() {
        TracingPlugin plugin = new TracingPlugin(tracerProvider.get(TracingConstants.INSTRUMENTATION_NAME),
                new TraceContextExtractor(), false);
        Mockito.when(chain.execute(ArgumentMatchers.any())).thenReturn(Mono.empty());
        ServerWebExchange exchange = createExchange("00-" + TRACE_ID + "-" + PARENT_SPAN_ID + "-01");
        Span agentSpan = tracerProvider.get(TracingConstants.INSTRUMENTATION_NAME)
                .spanBuilder("agent-server").startSpan();
        try (Scope scope = Context.root().with(agentSpan).makeCurrent()) {
            StepVerifier.create(plugin.execute(exchange, chain)).expectSubscription().verifyComplete();
        }
        // the context of the instrumentation which runs in this process wins over the headers of the
        // client, so that the access log and the outgoing call carry the trace of the agent
        Assertions.assertEquals(agentSpan.getSpanContext().getTraceId(),
                exchange.getAttribute(Constants.SHENYU_AGENT_TRACE_ID));
        Assertions.assertNotEquals(TRACE_ID, exchange.getAttribute(Constants.SHENYU_AGENT_TRACE_ID));
        Assertions.assertTrue(spanExporter.getFinishedSpanItems().isEmpty());
        agentSpan.end();
    }

    @Test
    public void testTheGatewaySpanContinuesTheActiveContext() {
        Mockito.when(chain.execute(ArgumentMatchers.any())).thenReturn(Mono.empty());
        ServerWebExchange exchange = createExchange("00-" + TRACE_ID + "-" + PARENT_SPAN_ID + "-01");
        Span agentSpan = tracerProvider.get(TracingConstants.INSTRUMENTATION_NAME)
                .spanBuilder("agent-server").startSpan();
        try (Scope scope = Context.root().with(agentSpan).makeCurrent()) {
            StepVerifier.create(tracingPlugin.execute(exchange, chain)).expectSubscription().verifyComplete();
        }
        // the gateway span is a child of the span of the instrumentation instead of a child of the
        // remote parent, so the agent span does not drop out of the trace
        SpanData gatewaySpan = spanExporter.getFinishedSpanItems().stream()
                .filter(span -> "GET".equals(span.getName()))
                .findFirst()
                .orElseThrow();
        Assertions.assertEquals(agentSpan.getSpanContext().getSpanId(), gatewaySpan.getParentSpanId());
        Assertions.assertEquals(agentSpan.getSpanContext().getTraceId(), gatewaySpan.getTraceId());
        agentSpan.end();
    }

    @Test
    public void testOrderAndName() {
        Assertions.assertEquals(PluginEnum.TRACING.getCode(), tracingPlugin.getOrder());
        Assertions.assertEquals(PluginEnum.TRACING.getName(), tracingPlugin.named());
    }

    private ServerWebExchange createExchange(final String traceparent) {
        MockServerHttpRequest.BaseBuilder<?> builder = MockServerHttpRequest.get("http://localhost" + PATH);
        if (Objects.nonNull(traceparent)) {
            builder.header("traceparent", traceparent);
        }
        ServerWebExchange exchange = MockServerWebExchange.from(builder.build());
        ShenyuContext shenyuContext = new ShenyuContext();
        shenyuContext.setHttpMethod("GET");
        shenyuContext.setPath(PATH);
        exchange.getAttributes().put(Constants.CONTEXT, shenyuContext);
        return exchange;
    }
}
