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

package org.apache.shenyu.springboot.starter.plugin.agent.gateway;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import io.opentelemetry.api.baggage.Baggage;
import io.opentelemetry.api.common.AttributeKey;
import io.opentelemetry.api.trace.Span;
import io.opentelemetry.api.trace.SpanKind;
import io.opentelemetry.api.trace.StatusCode;
import io.opentelemetry.context.Context;
import io.opentelemetry.context.Scope;
import io.opentelemetry.sdk.OpenTelemetrySdk;
import io.opentelemetry.sdk.testing.exporter.InMemorySpanExporter;
import io.opentelemetry.sdk.trace.SdkTracerProvider;
import io.opentelemetry.sdk.trace.data.SpanData;
import io.opentelemetry.sdk.trace.export.SimpleSpanProcessor;
import org.apache.shenyu.common.dto.AgentGatewayMcpConfig;
import org.apache.shenyu.common.metrics.AgentMcpCallObserver.Outcome;
import org.apache.shenyu.plugin.agent.gateway.AgentTrafficContext;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpCallTracer;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpDispatcher;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpHttpHandler;
import org.apache.shenyu.plugin.agent.gateway.security.AgentMcpIdentity;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolExecutionException;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolProvider;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolRegistry;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;
import reactor.test.StepVerifier;

import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.awaitility.Awaitility.await;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class OpenTelemetryAgentMcpCallTracerTest {

    @ParameterizedTest
    @CsvSource({"json,SUCCESS", "sse,SUCCESS", "json,TOOL_ERROR", "sse,TOOL_ERROR", "json,SERVER_ERROR", "sse,SERVER_ERROR", "json,REJECTED", "sse,REJECTED"})
    void exportsOnlyBoundedTerminalData(final String mode, final Outcome expected) {
        try (Fixture fixture = new Fixture()) {
            Mono<JsonObject> action = switch (expected) {
                case TOOL_ERROR -> Mono.error(new AgentToolExecutionException("private-business-error"));
                case SERVER_ERROR -> Mono.error(new IllegalStateException("private-server-error"));
                default -> Mono.just(JsonParser.parseString("{\"private-result\":\"secret\"}").getAsJsonObject());
            };
            MockServerWebExchange exchange = exchange("tools/call");
            StepVerifier.create(fixture.handler(action, expected == Outcome.REJECTED ? Set.of() : Set.of("read"))
                    .handle(exchange, config(mode, 30000), traffic(), 1)).verifyComplete();
            SpanData span = fixture.single(expected);
            assertEquals(SpanKind.INTERNAL, span.getKind());
            assertEquals(expected == Outcome.TOOL_ERROR || expected == Outcome.SERVER_ERROR ? StatusCode.ERROR : StatusCode.UNSET, span.getStatus().getStatusCode());
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void deadlineEndsSpanOnceAndCancelsItsExecution(final String mode) {
        try (Fixture fixture = new Fixture()) {
            AtomicBoolean cancelled = new AtomicBoolean();
            MockServerWebExchange exchange = exchange("tools/call");
            AgentMcpHttpHandler handler = fixture.handler(Mono.<JsonObject>never().doOnCancel(() -> cancelled.set(true)), Set.of("read"));
            StepVerifier.withVirtualTime(() -> handler.handle(exchange, config(mode, 100), traffic(), 1))
                    .thenAwait(Duration.ofMillis(101)).verifyComplete();
            assertEquals(504, exchange.getResponse().getStatusCode().value());
            assertTrue(cancelled.get());
            assertEquals(StatusCode.ERROR, fixture.single(Outcome.TIMEOUT).getStatus().getStatusCode());
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void responseWritingMustTerminateBeforeSpanEnds(final String mode) {
        try (Fixture fixture = new Fixture()) {
            MockServerWebExchange exchange = exchange("tools/call");
            exchange.getResponse().setWriteHandler(buffers -> buffers.doOnNext(DataBufferUtils::release).then(Mono.never()));
            StepVerifier.create(fixture.handler(Mono.just(new JsonObject()), Set.of("read")).handle(exchange, config(mode, 30000), traffic(), 1))
                    .then(() -> assertTrue(fixture.exporter.getFinishedSpanItems().isEmpty())).thenCancel().verify();
            fixture.single(Outcome.CANCELLED);
        }
    }

    @Test
    void reactorParentAndChildStayPrivateAcrossThirtyTwoSameIdCalls() {
        try (Fixture fixture = new Fixture()) {
            List<String> parents = new CopyOnWriteArrayList<>();
            Map<String, String> children = new ConcurrentHashMap<>();
            Mono<JsonObject> action = Mono.deferContextual(context -> {
                Context execution = context.get(Context.class);
                assertTrue(Baggage.fromContext(execution).isEmpty());
                children.put(Span.fromContext(execution).getSpanContext().getSpanId(), context.get("expected-parent"));
                return Mono.just(new JsonObject());
            }).subscribeOn(Schedulers.parallel());
            AgentMcpHttpHandler handler = fixture.handler(action, Set.of("read"));
            StepVerifier.create(Flux.range(0, 32).flatMap(index -> {
                Span parent = fixture.sdk.getTracer("test-parent").spanBuilder("parent").startSpan();
                parents.add(parent.getSpanContext().getSpanId());
                Context context = Baggage.builder().put("private-subject", "secret").build().storeInContext(Context.root().with(parent));
                return handler.handle(exchange("tools/call"), config("json", 30000), traffic(), index)
                        .contextWrite(value -> value.put(Context.class, context).put("expected-parent", parent.getSpanContext().getSpanId()))
                        .doFinally(signal -> parent.end());
            }, 32)).verifyComplete();
            await().atMost(Duration.ofSeconds(3)).until(() -> fixture.exporter.getFinishedSpanItems().size() == 64);
            List<SpanData> calls = fixture.exporter.getFinishedSpanItems().stream().filter(span -> "shenyu.agent_gateway.tools.call".equals(span.getName())).toList();
            assertEquals(32, calls.size());
            assertEquals(32, children.size());
            assertEquals(Set.copyOf(parents), calls.stream().map(SpanData::getParentSpanId).collect(java.util.stream.Collectors.toSet()));
            assertEquals(children.keySet(), calls.stream().map(SpanData::getSpanId).collect(java.util.stream.Collectors.toSet()));
            calls.forEach(span -> assertEquals(children.get(span.getSpanId()), span.getParentSpanId()));
            assertFalse(Span.current().getSpanContext().isValid());
        }
    }

    @Test
    void doesNotInheritThreadLocalParentOrEndIt() {
        try (Fixture fixture = new Fixture()) {
            Span unrelated = fixture.sdk.getTracer("unrelated").spanBuilder("unrelated").startSpan();
            try (Scope ignored = unrelated.makeCurrent()) {
                AgentMcpCallTracer.Trace trace = fixture.tracer.start(reactor.util.context.Context.empty());
                trace.finish(Outcome.SUCCESS);
                trace.finish(Outcome.TIMEOUT);
                assertFalse(fixture.single(Outcome.SUCCESS).getParentSpanContext().isValid());
                assertEquals(unrelated.getSpanContext(), Span.current().getSpanContext());
            } finally {
                unrelated.end();
            }
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"tools/list", "server/discover"})
    void discoveryDoesNotExportLogicalCallSpans(final String method) {
        try (Fixture fixture = new Fixture()) {
            StepVerifier.create(fixture.handler(Mono.just(new JsonObject()), Set.of("read"))
                    .handle(exchange(method), config("json", 30000), traffic(), 1)).verifyComplete();
            assertTrue(fixture.exporter.getFinishedSpanItems().isEmpty());
        }
    }

    @Test
    void cancellingBeforeParsingDoesNotCreateASpan() {
        try (Fixture fixture = new Fixture()) {
            StepVerifier.create(fixture.handler(Mono.never(), Set.of("read"))
                    .handle(exchange("tools/call"), config("json", 30000), traffic(), 1)).thenCancel().verify();
            assertTrue(fixture.exporter.getFinishedSpanItems().isEmpty());
        }
    }

    private static MockServerWebExchange exchange(final String method) {
        MockServerHttpRequest.BodyBuilder builder = MockServerHttpRequest.post("/agent").header("Content-Type", "application/json")
                .header("Accept", "application/json, text/event-stream").header("MCP-Protocol-Version", "2026-07-28").header("Mcp-Method", method);
        if ("tools/call".equals(method)) {
            builder.header("Mcp-Name", "read");
        }
        return MockServerWebExchange.from(builder.body("{\"jsonrpc\":\"2.0\",\"id\":\"same-id\",\"method\":\"" + method + "\","
                + "\"params\":{\"name\":\"read\",\"_meta\":{\"io.modelcontextprotocol/protocolVersion\":\"2026-07-28\","
                + "\"io.modelcontextprotocol/clientCapabilities\":{}}}}"));
    }

    private static AgentGatewayMcpConfig config(final String mode, final int timeout) {
        return AgentGatewayMcpConfig.parse(JsonParser.parseString("{\"allowedTools\":[\"read\"],\"responseMode\":\"" + mode
                + "\",\"timeoutMs\":" + timeout + "}").getAsJsonObject(), true);
    }

    private static AgentTrafficContext traffic() {
        return new AgentTrafficContext("private-id", "mcp", "private-selector", "private-rule");
    }

    private static final class Fixture implements AutoCloseable {

        private final InMemorySpanExporter exporter = InMemorySpanExporter.create();

        private final SdkTracerProvider provider = SdkTracerProvider.builder().addSpanProcessor(SimpleSpanProcessor.create(exporter)).build();

        private final OpenTelemetrySdk sdk = OpenTelemetrySdk.builder().setTracerProvider(provider).build();

        private final OpenTelemetryAgentMcpCallTracer tracer = new OpenTelemetryAgentMcpCallTracer(sdk);

        private AgentMcpHttpHandler handler(final Mono<JsonObject> action, final Set<String> grants) {
            AgentToolProvider tool = mock(AgentToolProvider.class);
            when(tool.getName()).thenReturn("read");
            when(tool.getDescription()).thenReturn("private-description");
            when(tool.getInputSchema()).thenReturn(JsonParser.parseString("{\"type\":\"object\"}").getAsJsonObject());
            when(tool.getRequiredClientCapabilities()).thenReturn(new JsonObject());
            when(tool.invoke(any())).thenReturn(action);
            return new AgentMcpHttpHandler(new AgentMcpDispatcher(new AgentToolRegistry(List.of(tool)), "test", "1"),
                    ignored -> Mono.just(new AgentMcpIdentity("private-subject", grants)), tracer);
        }

        private SpanData single(final Outcome outcome) {
            List<SpanData> spans = exporter.getFinishedSpanItems();
            assertEquals(1, spans.size());
            SpanData span = spans.get(0);
            assertEquals("shenyu.agent_gateway.tools.call", span.getName());
            assertEquals(1, span.getAttributes().size());
            assertEquals(outcome.name().toLowerCase(java.util.Locale.ROOT), span.getAttributes().get(AttributeKey.stringKey("shenyu.mcp.outcome")));
            assertTrue(span.getEvents().isEmpty());
            assertTrue(span.getLinks().isEmpty());
            assertTrue(span.getEndEpochNanos() >= span.getStartEpochNanos());
            return span;
        }

        @Override
        public void close() {
            sdk.close();
        }
    }
}
