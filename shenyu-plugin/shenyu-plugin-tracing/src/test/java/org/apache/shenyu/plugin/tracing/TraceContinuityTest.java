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

import com.sun.net.httpserver.HttpServer;
import io.opentelemetry.api.trace.Span;
import io.opentelemetry.api.trace.SpanContext;
import io.opentelemetry.sdk.testing.exporter.InMemorySpanExporter;
import io.opentelemetry.sdk.trace.SdkTracerProvider;
import io.opentelemetry.sdk.trace.data.SpanData;
import io.opentelemetry.sdk.trace.export.SimpleSpanProcessor;
import io.opentelemetry.sdk.trace.samplers.Sampler;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.httpclient.WebClientPlugin;
import org.apache.shenyu.plugin.tracing.extract.TraceContextExtractor;
import org.awaitility.Awaitility;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpHeaders;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.io.IOException;
import java.io.InputStream;
import java.net.InetSocketAddress;
import java.net.URI;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

/**
 * The Test Case For the continuity of a trace across the gateway.
 *
 * <p>The real tracing plugin and the real http client plugin are wired together against a local
 * upstream, so the whole path of a request is covered: the span of the gateway, the context which is
 * injected into the outgoing call, and the parent relationship the upstream receives.</p>
 */
public class TraceContinuityTest {

    private static final String PATH = "/http/order";

    private static final String CLIENT_TRACE_ID = "4bf92f3577b34da6a3ce929d0e0e4736";

    private static final String CLIENT_SPAN_ID = "00f067aa0ba902b7";

    private static final String BAGGAGE = "baggage";

    private final TraceContextExtractor extractor = new TraceContextExtractor();

    private final AtomicReference<List<String>> receivedTraceParents = new AtomicReference<>();

    private final AtomicReference<String> receivedTraceState = new AtomicReference<>();

    private final AtomicReference<String> receivedBaggage = new AtomicReference<>();

    private InMemorySpanExporter spanExporter;

    private SdkTracerProvider tracerProvider;

    private HttpServer upstream;

    @BeforeEach
    public void setUp() throws IOException {
        this.spanExporter = InMemorySpanExporter.create();
        this.tracerProvider = SdkTracerProvider.builder()
                .addSpanProcessor(SimpleSpanProcessor.create(spanExporter))
                .setSampler(Sampler.parentBased(Sampler.alwaysOn()))
                .build();
        this.upstream = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        this.upstream.createContext(PATH, received -> {
            receivedTraceParents.set(received.getRequestHeaders().get(TracingConstants.TRACEPARENT));
            receivedTraceState.set(received.getRequestHeaders().getFirst(TracingConstants.TRACESTATE));
            receivedBaggage.set(received.getRequestHeaders().getFirst(BAGGAGE));
            try (InputStream body = received.getRequestBody()) {
                body.readAllBytes();
            }
            received.sendResponseHeaders(200, -1);
            received.close();
        });
        this.upstream.start();
    }

    @AfterEach
    public void tearDown() {
        upstream.stop(0);
        tracerProvider.close();
    }

    @Test
    public void testTheUpstreamContinuesTheTraceOfTheClient() {
        ServerWebExchange exchange = createExchange("00-" + CLIENT_TRACE_ID + "-" + CLIENT_SPAN_ID + "-01", null, null);
        route(exchange);
        // the whole path carries one trace: the client started it, the gateway continued it, and the
        // upstream call became a child of the gateway span
        List<String> traceParents = receivedTraceParents.get();
        Assertions.assertEquals(1, traceParents.size());
        SpanContext upstreamParent = upstreamParent(traceParents.get(0));
        Assertions.assertEquals(CLIENT_TRACE_ID, upstreamParent.getTraceId());
        SpanData gatewaySpan = onlyFinishedSpan();
        Assertions.assertEquals(gatewaySpan.getSpanId(), upstreamParent.getSpanId());
        Assertions.assertEquals(CLIENT_SPAN_ID, gatewaySpan.getParentSpanId());
        Assertions.assertEquals(CLIENT_TRACE_ID, gatewaySpan.getTraceId());
    }

    @Test
    public void testTheGatewayStartsTheTraceWithoutAnIncomingContext() {
        ServerWebExchange exchange = createExchange(null, null, null);
        route(exchange);
        List<String> traceParents = receivedTraceParents.get();
        Assertions.assertEquals(1, traceParents.size());
        SpanData gatewaySpan = onlyFinishedSpan();
        Assertions.assertEquals(gatewaySpan.getTraceId(), upstreamParent(traceParents.get(0)).getTraceId());
        Assertions.assertFalse(gatewaySpan.getParentSpanContext().isValid());
    }

    @Test
    public void testTheTraceStateAndTheBaggageAreForwardedToTheUpstream() {
        ServerWebExchange exchange = createExchange("00-" + CLIENT_TRACE_ID + "-" + CLIENT_SPAN_ID + "-01",
                "vendor=value", "userId=alice");
        route(exchange);
        // the propagation entries of the client are forwarded, while the traceparent is rebuilt from
        // the context of the gateway span
        Assertions.assertEquals("vendor=value", receivedTraceState.get());
        Assertions.assertEquals("userId=alice", receivedBaggage.get());
        Assertions.assertEquals(1, receivedTraceParents.get().size());
    }

    /**
     * Runs the request through the tracing plugin and the http client plugin, ending at the upstream.
     *
     * @param exchange the current exchange
     */
    private void route(final ServerWebExchange exchange) {
        final ShenyuPluginChain resource = finalExchange -> Mono.empty();
        final WebClientPlugin webClientPlugin = new WebClientPlugin(WebClient.builder().build(), Constants.BYTES_PER_MB);
        // the plugin chain of the gateway invokes a plugin when the request is subscribed, which is
        // after the tracing plugin has created its span, so the chain of this test is lazy as well
        final ShenyuPluginChain client = finalExchange -> Mono.defer(
                () -> webClientPlugin.execute(finalExchange, resource));
        final TracingPlugin tracingPlugin = new TracingPlugin(
                tracerProvider.get(TracingConstants.INSTRUMENTATION_NAME));
        StepVerifier.create(tracingPlugin.execute(exchange, client)).expectSubscription().verifyComplete();
    }

    private ServerWebExchange createExchange(final String traceparent, final String traceState,
                                             final String baggage) {
        MockServerHttpRequest.BaseBuilder<?> request = MockServerHttpRequest.get(PATH);
        if (Objects.nonNull(traceparent)) {
            request.header(TracingConstants.TRACEPARENT, traceparent);
        }
        if (Objects.nonNull(traceState)) {
            request.header(TracingConstants.TRACESTATE, traceState);
        }
        if (Objects.nonNull(baggage)) {
            request.header(BAGGAGE, baggage);
        }
        ServerWebExchange exchange = MockServerWebExchange.from(request.build());
        ShenyuContext shenyuContext = new ShenyuContext();
        shenyuContext.setRpcType(RpcTypeEnum.HTTP.getName());
        exchange.getAttributes().put(Constants.CONTEXT, shenyuContext);
        exchange.getAttributes().put(Constants.HTTP_URI,
                URI.create("http://127.0.0.1:" + upstream.getAddress().getPort() + PATH));
        return exchange;
    }

    /**
     * Parses the context the upstream received, the way the upstream service would continue it.
     *
     * @param traceparent the received traceparent
     * @return the parent context of the upstream call
     */
    private SpanContext upstreamParent(final String traceparent) {
        HttpHeaders headers = new HttpHeaders();
        headers.add(TracingConstants.TRACEPARENT, traceparent);
        return Span.fromContext(extractor.extract(headers)).getSpanContext();
    }

    private SpanData onlyFinishedSpan() {
        // doFinally can export the span after StepVerifier observes the request's completion.
        Awaitility.await().atMost(5, TimeUnit.SECONDS).until(() -> !spanExporter.getFinishedSpanItems().isEmpty());
        List<SpanData> spans = spanExporter.getFinishedSpanItems();
        Assertions.assertEquals(1, spans.size());
        return spans.get(0);
    }
}
