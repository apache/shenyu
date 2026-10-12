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

package org.apache.shenyu.plugin.agent.gateway.protocol;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.AgentGatewayMcpConfig;
import org.apache.shenyu.common.metrics.AgentMcpCallObserver;
import org.apache.shenyu.common.metrics.AgentMcpCallObserver.Outcome;
import org.apache.shenyu.plugin.agent.gateway.AgentTrafficContext;
import org.apache.shenyu.plugin.agent.gateway.security.AgentMcpIdentity;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolExecutionException;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolProvider;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolRegistry;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.SignalType;
import reactor.core.publisher.Sinks;
import reactor.core.scheduler.Schedulers;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.awaitility.Awaitility.await;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Logical call observation through execution, encoding, writing and cancellation.
 */
class AgentMcpCallObservationTest {

    private final AgentTrafficContext traffic = new AgentTrafficContext("private-id", "mcp", "selector", "rule");

    @ParameterizedTest
    @CsvSource({"json,SUCCESS", "sse,SUCCESS", "json,TOOL_ERROR", "sse,TOOL_ERROR"})
    void recordsBusinessOutcomeNotOnlyHttpStatus(final String mode, final Outcome expected) {
        AgentToolProvider tool = tool(expected == Outcome.TOOL_ERROR ? Mono.error(new AgentToolExecutionException("private-diagnostic")) : Mono.just(new JsonObject()));
        Recording observer = new Recording();
        MockServerWebExchange exchange = exchange("tools/call", observer);
        StepVerifier.create(handler(tool, Set.of("read")).handle(exchange, config(mode, 30000), traffic, 1)).verifyComplete();
        observer.assertOnly(expected);
        assertEquals(200, exchange.getResponse().getStatusCode().value());
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void recordsRejectedCallWithoutInvokingProvider(final String mode) {
        AtomicInteger invocations = new AtomicInteger();
        AgentToolProvider tool = tool(Mono.defer(() -> {
            invocations.incrementAndGet();
            return Mono.just(new JsonObject());
        }));
        Recording observer = new Recording();
        MockServerWebExchange exchange = exchange("tools/call", observer);
        StepVerifier.create(handler(tool, Set.of()).handle(exchange, config(mode, 30000), traffic, 1)).verifyComplete();
        observer.assertOnly(Outcome.REJECTED);
        assertEquals(0, invocations.get());
        assertEquals(403, exchange.getResponse().getStatusCode().value());
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void recordsInternalErrorNotRecoveredHttpCompletion(final String mode) {
        Recording observer = new Recording();
        MockServerWebExchange exchange = exchange("tools/call", observer);
        StepVerifier.create(handler(tool(Mono.error(new IllegalStateException("private-error"))), Set.of("read"))
                .handle(exchange, config(mode, 30000), traffic, 1)).verifyComplete();
        observer.assertOnly(Outcome.SERVER_ERROR);
        assertEquals(500, exchange.getResponse().getStatusCode().value());
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void recordsDeadlineOnceInsteadOfProviderCancellation(final String mode) {
        Recording observer = new Recording();
        AtomicBoolean cancelled = new AtomicBoolean();
        MockServerWebExchange exchange = exchange("tools/call", observer);
        AgentMcpHttpHandler handler = handler(tool(Mono.<JsonObject>never().doOnCancel(() -> cancelled.set(true))), Set.of("read"));
        StepVerifier.withVirtualTime(() -> handler.handle(exchange, config(mode, 100), traffic, 1))
                .thenAwait(Duration.ofMillis(101)).verifyComplete();
        observer.assertOnly(Outcome.TIMEOUT);
        assertTrue(cancelled.get());
        assertEquals(504, exchange.getResponse().getStatusCode().value());
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void recordsDownstreamCancellationDuringExecution(final String mode) {
        Recording observer = new Recording();
        AtomicBoolean started = new AtomicBoolean();
        AtomicBoolean cancelled = new AtomicBoolean();
        MockServerWebExchange exchange = exchange("tools/call", observer);
        AgentMcpHttpHandler handler = handler(tool(Mono.<JsonObject>never().doOnSubscribe(subscription -> started.set(true))
                .doOnCancel(() -> cancelled.set(true))), Set.of("read"));
        StepVerifier.create(handler.handle(exchange, config(mode, 30000), traffic, 1)).then(() -> assertTrue(started.get())).thenCancel().verify();
        observer.assertOnly(Outcome.CANCELLED);
        assertTrue(cancelled.get());
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void doesNotCountComputedResultAsSuccessDuringStalledWriting(final String mode) {
        Recording observer = new Recording();
        MockServerWebExchange exchange = exchange("tools/call", observer);
        exchange.getResponse().setWriteHandler(buffers -> buffers.doOnNext(DataBufferUtils::release).then(Mono.never()));
        StepVerifier.create(handler(tool(Mono.just(new JsonObject())), Set.of("read")).handle(exchange, config(mode, 30000), traffic, 1))
                .then(() -> assertTrue(observer.outcomes.isEmpty())).thenCancel().verify();
        observer.assertOnly(Outcome.CANCELLED);
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void recordsResponseWriteErrorNotComputedSuccess(final String mode) {
        Recording observer = new Recording();
        MockServerWebExchange exchange = exchange("tools/call", observer);
        exchange.getResponse().setWriteHandler(buffers -> buffers.doOnNext(DataBufferUtils::release)
                .then(Mono.error(new IllegalStateException("private-write-error"))));
        StepVerifier.create(handler(tool(Mono.just(new JsonObject())), Set.of("read")).handle(exchange, config(mode, 30000), traffic, 1))
                .expectError(IllegalStateException.class).verify();
        observer.assertOnly(Outcome.SERVER_ERROR);
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void errorResponseWriteFailureOverridesOriginalRejection(final String mode) {
        Recording observer = new Recording();
        AtomicInteger invocations = new AtomicInteger();
        MockServerWebExchange exchange = exchange("tools/call", observer);
        exchange.getResponse().setWriteHandler(buffers -> buffers.doOnNext(DataBufferUtils::release)
                .then(Mono.error(new IllegalStateException("private-error-response-write"))));
        AgentMcpHttpHandler handler = handler(tool(Mono.defer(() -> {
            invocations.incrementAndGet();
            return Mono.just(new JsonObject());
        })), Set.of());
        StepVerifier.create(handler.handle(exchange, config(mode, 30000), traffic, 1))
                .expectError(IllegalStateException.class).verify();
        observer.assertOnly(Outcome.SERVER_ERROR);
        assertEquals(0, invocations.get());
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void errorResponseWriteTimeoutOverridesOriginalRejection(final String mode) {
        Recording observer = new Recording();
        MockServerWebExchange exchange = exchange("tools/call", observer);
        exchange.getResponse().setWriteHandler(buffers -> buffers.doOnNext(DataBufferUtils::release).then(Mono.never()));
        AgentMcpHttpHandler handler = handler(tool(Mono.just(new JsonObject())), Set.of());
        StepVerifier.withVirtualTime(() -> handler.handle(exchange, config(mode, 100), traffic, 1))
                .then(() -> assertTrue(observer.outcomes.isEmpty()))
                .thenAwait(Duration.ofMillis(101)).expectError(TimeoutException.class).verify();
        observer.assertOnly(Outcome.TIMEOUT);
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void cancellingErrorResponseWriteOverridesOriginalRejection(final String mode) {
        Recording observer = new Recording();
        MockServerWebExchange exchange = exchange("tools/call", observer);
        exchange.getResponse().setWriteHandler(buffers -> buffers.doOnNext(DataBufferUtils::release).then(Mono.never()));
        StepVerifier.create(handler(tool(Mono.just(new JsonObject())), Set.of()).handle(exchange, config(mode, 30000), traffic, 1))
                .then(() -> assertTrue(observer.outcomes.isEmpty())).thenCancel().verify();
        observer.assertOnly(Outcome.CANCELLED);
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void recordsResponseLimitFailureWithoutLeakingSuccess(final String mode) {
        JsonObject result = new JsonObject();
        result.addProperty("large", "x".repeat(2000));
        Recording observer = new Recording();
        MockServerWebExchange exchange = exchange("tools/call", observer);
        JsonObject settings = JsonParser.parseString("{\"allowedTools\":[\"read\"],\"maxResponseBytes\":1024,\"responseMode\":\"" + mode + "\"}").getAsJsonObject();
        StepVerifier.create(handler(tool(Mono.just(result)), Set.of("read")).handle(exchange, AgentGatewayMcpConfig.parse(settings, true), traffic, 1)).verifyComplete();
        observer.assertOnly(Outcome.SERVER_ERROR);
        assertEquals(500, exchange.getResponse().getStatusCode().value());
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void timeoutWhileWritingIsNotSuccessfulToolCompletion(final String mode) {
        Recording observer = new Recording();
        MockServerWebExchange exchange = exchange("tools/call", observer);
        exchange.getResponse().setWriteHandler(buffers -> buffers.doOnNext(DataBufferUtils::release).then(Mono.never()));
        AgentMcpHttpHandler handler = handler(tool(Mono.just(new JsonObject())), Set.of("read"));
        StepVerifier.withVirtualTime(() -> handler.handle(exchange, config(mode, 100), traffic, 1))
                .thenAwait(Duration.ofMillis(101)).expectError(TimeoutException.class).verify();
        observer.assertOnly(Outcome.TIMEOUT);
    }

    @ParameterizedTest
    @ValueSource(strings = {"server/discover", "tools/list"})
    void excludesDiscoveryAndListing(final String method) {
        Recording observer = new Recording();
        StepVerifier.create(handler(tool(Mono.just(new JsonObject())), Set.of("read"))
                .handle(exchange(method, observer), config("json", 30000), traffic, 1)).verifyComplete();
        assertTrue(observer.outcomes.isEmpty());
    }

    @Test
    void excludesUnparsedAndUnauthenticatedRequests() {
        Recording observer = new Recording();
        MockServerWebExchange malformed = MockServerWebExchange.from(MockServerHttpRequest.post("/agent")
                .contentType(MediaType.APPLICATION_JSON).header(HttpHeaders.ACCEPT, "application/json, text/event-stream").body("{}"));
        malformed.getAttributes().put(Constants.METRICS_AGENT_MCP_CALL, observer);
        StepVerifier.create(handler(tool(Mono.just(new JsonObject())), Set.of("read"))
                .handle(malformed, config("json", 30000), traffic, 1)).verifyComplete();
        AgentMcpHttpHandler unauthenticated = new AgentMcpHttpHandler(new AgentMcpDispatcher(new AgentToolRegistry(List.of()), "test", "1"), ignored -> Mono.empty());
        StepVerifier.create(unauthenticated.handle(exchange("tools/call", observer), config("json", 30000), traffic, 1)).verifyComplete();
        assertTrue(observer.outcomes.isEmpty());
    }

    @Test
    void callbackFailuresDoNotChangeSuccessfulResponse() {
        AtomicInteger records = new AtomicInteger();
        AgentMcpCallObserver observer = (outcome, millis) -> {
            records.incrementAndGet();
            throw new IllegalStateException("private-observer-error");
        };
        MockServerWebExchange exchange = exchange("tools/call", observer);
        StepVerifier.create(handler(tool(Mono.just(new JsonObject())), Set.of("read"))
                .handle(exchange, config("json", 30000), traffic, 1)).verifyComplete();
        assertEquals(1, records.get());
        assertEquals(200, exchange.getResponse().getStatusCode().value());
    }

    @Test
    void concurrentSameRpcIdsHaveIndependentTerminalRecords() {
        Recording observer = new Recording();
        AgentMcpHttpHandler handler = handler(tool(Mono.just(new JsonObject()).publishOn(Schedulers.parallel())), Set.of("read"));
        StepVerifier.create(Flux.range(0, 32).flatMap(index -> handler.handle(exchange("tools/call", observer), config("json", 30000), traffic, index), 32))
                .verifyComplete();
        await().atMost(Duration.ofSeconds(3)).until(() -> observer.outcomes.size() == 32);
        assertEquals(32, observer.outcomes.size());
        assertTrue(observer.outcomes.stream().allMatch(outcome -> outcome == Outcome.SUCCESS));
    }

    @Test
    void aFreshSubscriptionDoesNotReuseThePreviousTerminalRecord() {
        Recording observer = new Recording();
        AgentMcpHttpHandler handler = handler(tool(Mono.just(new JsonObject())), Set.of("read"));
        Mono<Void> call = Mono.defer(() -> handler.handle(exchange("tools/call", observer), config("json", 30000), traffic, 1));
        StepVerifier.create(call).verifyComplete();
        StepVerifier.create(call).verifyComplete();
        assertEquals(List.of(Outcome.SUCCESS, Outcome.SUCCESS), List.copyOf(observer.outcomes));
    }

    @Test
    void cancellingOneCallDoesNotFinalizeAnotherWithTheSameRpcId() {
        Recording cancelled = new Recording();
        Recording successful = new Recording();
        AtomicBoolean started = new AtomicBoolean();
        Sinks.One<JsonObject> pending = Sinks.one();
        StepVerifier.create(handler(tool(pending.asMono()), Set.of("read"))
                .handle(exchange("tools/call", successful), config("json", 30000), traffic, 1))
                .then(() -> {
                    StepVerifier.create(handler(tool(Mono.<JsonObject>never().doOnSubscribe(subscription -> started.set(true))), Set.of("read"))
                            .handle(exchange("tools/call", cancelled), config("json", 30000), traffic, 1))
                            .then(() -> assertTrue(started.get())).thenCancel().verify();
                    cancelled.assertOnly(Outcome.CANCELLED);
                    assertTrue(successful.outcomes.isEmpty());
                    assertEquals(Sinks.EmitResult.OK, pending.tryEmitValue(new JsonObject()));
                }).verifyComplete();
        successful.assertOnly(Outcome.SUCCESS);
    }

    @Test
    void lateSignalsCannotRecordATerminalTwice() {
        Recording observer = new Recording();
        AgentMcpCallObservation observation = new AgentMcpCallObservation(observer);
        AgentMcpRequest request = new AgentMcpRequestParser().parse(body("tools/call").getBytes(StandardCharsets.UTF_8),
                exchange("tools/call", observer).getRequest().getHeaders(), 262144);
        observation.parsed(request);
        observation.response(new ObjectMapper().createObjectNode());
        observation.finish(SignalType.CANCEL);
        observation.finish(SignalType.ON_COMPLETE);
        observation.failure(new IllegalStateException("late-error"));
        observation.finish(SignalType.ON_ERROR);
        observer.assertOnly(Outcome.CANCELLED);
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void absentOrWrongTypeCallbackDoesNotRequireMetricsPlugin(final boolean wrongType) {
        MockServerWebExchange exchange = exchange("tools/call", null);
        if (wrongType) {
            exchange.getAttributes().put(Constants.METRICS_AGENT_MCP_CALL, "unusable-callback");
        }
        StepVerifier.create(handler(tool(Mono.just(new JsonObject())), Set.of("read"))
                .handle(exchange, config("json", 30000), traffic, 1)).verifyComplete();
        assertEquals(200, exchange.getResponse().getStatusCode().value());
    }

    @Test
    void cancellationBeforeParsingIsNotAToolCall() {
        Recording observer = new Recording();
        AgentMcpHttpHandler handler = handler(tool(Mono.never()), Set.of("read"));
        StepVerifier.create(handler.handle(exchange("tools/call", observer), config("json", 30000), traffic, 1)).thenCancel().verify();
        assertTrue(observer.outcomes.isEmpty());
    }

    private AgentMcpHttpHandler handler(final AgentToolProvider tool, final Set<String> grants) {
        return new AgentMcpHttpHandler(new AgentMcpDispatcher(new AgentToolRegistry(List.of(tool)), "test", "1"),
                ignored -> Mono.just(new AgentMcpIdentity("private-subject", grants)));
    }

    private AgentToolProvider tool(final Mono<JsonObject> action) {
        AgentToolProvider tool = mock(AgentToolProvider.class);
        when(tool.getName()).thenReturn("read");
        when(tool.getDescription()).thenReturn("Read test data");
        when(tool.getInputSchema()).thenReturn(JsonParser.parseString("{\"type\":\"object\"}").getAsJsonObject());
        when(tool.getRequiredClientCapabilities()).thenReturn(new JsonObject());
        when(tool.invoke(any())).thenReturn(action);
        return tool;
    }

    private AgentGatewayMcpConfig config(final String mode, final int timeout) {
        return AgentGatewayMcpConfig.parse(JsonParser.parseString("{\"allowedTools\":[\"read\"],\"responseMode\":\"" + mode
                + "\",\"timeoutMs\":" + timeout + "}").getAsJsonObject(), true);
    }

    private MockServerWebExchange exchange(final String method, final AgentMcpCallObserver observer) {
        MockServerHttpRequest.BodyBuilder builder = MockServerHttpRequest.post("/agent").contentType(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.ACCEPT, "application/json, text/event-stream").header("MCP-Protocol-Version", AgentMcpRequestParser.VERSION).header("Mcp-Method", method);
        if ("tools/call".equals(method)) {
            builder.header("Mcp-Name", "read");
        }
        MockServerWebExchange exchange = MockServerWebExchange.from(builder.body(body(method)));
        if (Objects.nonNull(observer)) {
            exchange.getAttributes().put(Constants.METRICS_AGENT_MCP_CALL, observer);
        }
        return exchange;
    }

    private String body(final String method) {
        return "{\"jsonrpc\":\"2.0\",\"id\":\"same-id\",\"method\":\"" + method + "\",\"params\":{\"name\":\"read\",\"_meta\":{"
                + "\"io.modelcontextprotocol/protocolVersion\":\"2026-07-28\",\"io.modelcontextprotocol/clientCapabilities\":{}}}}";
    }

    private static final class Recording implements AgentMcpCallObserver {

        private final ConcurrentLinkedQueue<Outcome> outcomes = new ConcurrentLinkedQueue<>();

        @Override
        public void record(final Outcome outcome, final long millis) {
            assertTrue(millis >= 0);
            outcomes.add(outcome);
        }

        private void assertOnly(final Outcome outcome) {
            assertEquals(List.of(outcome), List.copyOf(outcomes));
        }
    }
}
