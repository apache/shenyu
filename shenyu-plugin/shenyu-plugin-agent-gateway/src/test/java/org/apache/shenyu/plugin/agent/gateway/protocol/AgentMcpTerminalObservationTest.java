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

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.apache.shenyu.common.audit.AgentInvocationAuditBridge;
import org.apache.shenyu.common.audit.AgentInvocationAuditEvent;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.AgentGatewayMcpConfig;
import org.apache.shenyu.common.dto.AgentRequestContext;
import org.apache.shenyu.common.metrics.AgentMcpCallObserver;
import org.apache.shenyu.common.metrics.AgentMcpCallObserver.Outcome;
import org.apache.shenyu.common.metrics.AgentMcpTerminalEvent;
import org.apache.shenyu.common.metrics.AgentMcpTerminalObserver;
import org.apache.shenyu.common.metrics.AgentRuntimeMetrics;
import org.apache.shenyu.plugin.agent.gateway.AgentTrafficContext;
import org.apache.shenyu.plugin.agent.gateway.security.AgentMcpIdentity;
import org.apache.shenyu.plugin.agent.gateway.security.AgentMcpSecurityResolver;
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
import org.springframework.http.server.reactive.ReactorHttpHandlerAdapter;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.adapter.WebHttpHandlerBuilder;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.SignalType;
import reactor.core.publisher.Sinks;
import reactor.netty.DisposableServer;
import reactor.netty.http.server.HttpServer;
import reactor.test.StepVerifier;

import java.net.Socket;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class AgentMcpTerminalObservationTest {

    private final AgentTrafficContext traffic = new AgentTrafficContext("gateway-id", "mcp", "selector", "rule");

    @Test
    void auditUsesActualAttemptAndTerminalOwner() {
        List<AgentInvocationAuditEvent> events = new java.util.concurrent.CopyOnWriteArrayList<>();
        AgentMcpTerminalObserver observer = AgentInvocationAuditBridge.mcp(null, events::add);
        StepVerifier.create(handler(Mono.just(new JsonObject()), ignored -> Mono.just(new AgentMcpIdentity("tenant", "subject", Set.of("read"), Set.of())), observer)
                .handle(exchange("tools/call"), config("json", 30000), traffic, 7)).verifyComplete();
        assertEquals(2, events.size());
        assertEquals(AgentInvocationAuditEvent.RecordType.ATTEMPT, events.get(0).getRecordType());
        assertEquals(AgentInvocationAuditEvent.RecordType.LOGICAL, events.get(1).getRecordType());
        assertEquals("tenant", events.get(1).getTenant().orElseThrow());
        events.clear();
        StepVerifier.create(handler(Mono.just(new JsonObject()), ignored -> Mono.empty(), observer)
                .handle(exchange("tools/call"), config("json", 30000), traffic, 7)).verifyComplete();
        assertEquals(1, events.size());
        assertEquals("REJECTED", events.get(0).getOutcome());
        assertTrue(events.get(0).getSubject().isEmpty());
    }

    @ParameterizedTest
    @ValueSource(strings = {"missing", "long", " padded", "padded ", "control\nname", "delete-control"})
    void deniedToolCallsPreserveLogicalAuditForInvalidTargetMetadata(final String input) {
        String target = "long".equals(input) ? "x".repeat(257) : "delete-control".equals(input) ? Character.toString(127) : input;
        List<AgentInvocationAuditEvent> events = new java.util.concurrent.CopyOnWriteArrayList<>();
        AtomicInteger calls = new AtomicInteger();
        AgentMcpTerminalObserver observer = AgentInvocationAuditBridge.mcp(null, events::add);
        JsonObject request = JsonParser.parseString(body("tools/call")).getAsJsonObject();
        request.getAsJsonObject("params").addProperty("name", target);
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/agent")
                .contentType(MediaType.APPLICATION_JSON).header(HttpHeaders.ACCEPT, "application/json, text/event-stream")
                .header("MCP-Protocol-Version", AgentMcpRequestParser.VERSION)
                .header("Mcp-Method", "tools/call").header("Mcp-Name", target).body(request.toString()));
        StepVerifier.create(handler(Mono.fromSupplier(() -> {
            calls.incrementAndGet();
            return new JsonObject();
        }), ignored -> Mono.just(new AgentMcpIdentity("tenant", "subject", Set.of("read"), Set.of())), observer)
                .handle(exchange, config("json", 30000), traffic, 7)).verifyComplete();
        boolean parsed = "missing".equals(input) || "long".equals(input);
        int status = parsed ? 403 : 400;
        assertEquals(status, exchange.getResponse().getStatusCode().value());
        assertEquals(0, calls.get());
        assertEquals(1, events.size());
        AgentInvocationAuditEvent event = events.get(0);
        assertEquals(AgentInvocationAuditEvent.RecordType.LOGICAL, event.getRecordType());
        assertEquals(AgentInvocationAuditEvent.Decision.REJECTED, event.getDecision());
        assertEquals("REJECTED", event.getOutcome());
        assertEquals("gateway-id", event.getRequestId());
        assertEquals("tenant", event.getTenant().orElseThrow());
        assertEquals("subject", event.getSubject().orElseThrow());
        assertEquals(status, event.getHttpStatus().orElseThrow());
        assertEquals("missing".equals(input) ? java.util.Optional.of(target) : java.util.Optional.empty(), event.getTarget());
    }

    @Test
    void runtimeMetricsWorkWithoutAnApplicationObserverBeforeParsing() {
        AgentRuntimeMetrics metrics = mock(AgentRuntimeMetrics.class);
        MockServerWebExchange exchange = exchange("tools/call");
        exchange.getAttributes().put(AgentRuntimeMetrics.KEY, metrics);
        StepVerifier.create(handler(Mono.just(new JsonObject()), ignored -> Mono.empty(), null)
                .handle(exchange, config("json", 30000), traffic, 1)).verifyComplete();
        verify(metrics).started(AgentRuntimeMetrics.Traffic.MCP);
        verify(metrics).finished(org.mockito.ArgumentMatchers.eq(AgentRuntimeMetrics.Traffic.MCP),
                org.mockito.ArgumentMatchers.eq(AgentRuntimeMetrics.Outcome.REJECTED),
                org.mockito.ArgumentMatchers.eq(AgentRuntimeMetrics.Reason.AUTHENTICATION), org.mockito.ArgumentMatchers.anyLong());
    }

    @ParameterizedTest
    @CsvSource({"success,SUCCESS,2", "tool-error,TOOL_ERROR,2", "timeout,TIMEOUT,2", "rejected,REJECTED,1"})
    void realHttpAuditPreservesResponseAndCorrelatesExecution(final String scenario, final String outcome, final int count) throws Exception {
        List<AgentInvocationAuditEvent> audits = new java.util.concurrent.CopyOnWriteArrayList<>();
        Mono<JsonObject> action = "timeout".equals(scenario) ? Mono.never()
                : "tool-error".equals(scenario) ? Mono.error(new AgentToolExecutionException("secret"))
                : Mono.just(new JsonObject());
        AgentMcpSecurityResolver resolver = "rejected".equals(scenario) ? ignored -> Mono.empty() : identity();
        try (TerminalHttpFixture fixture = new TerminalHttpFixture(
                handler(action, resolver, AgentInvocationAuditBridge.mcp(null, audits::add)), config("json", 500), traffic, 7)) {
            HttpResponse<String> response = send(fixture);
            assertEquals("timeout".equals(scenario) ? 504 : "rejected".equals(scenario) ? 401 : 200, response.statusCode());
            await().atMost(Duration.ofSeconds(5)).untilAsserted(() -> assertEquals(count, audits.size()));
            assertTrue(audits.stream().allMatch(event -> outcome.equals(event.getOutcome()) && "gateway-id".equals(event.getRequestId())));
            assertEquals(1, audits.stream().filter(event -> event.getRecordType() == AgentInvocationAuditEvent.RecordType.LOGICAL).count());
            assertFalse(response.body().contains("secret"));
        }
    }

    @Test
    void runtimeMetricsFinishOnceOnCancellationEvenIfApplicationObserverThrows() {
        AgentRuntimeMetrics metrics = mock(AgentRuntimeMetrics.class);
        AgentMcpTerminalObservation observation = new AgentMcpTerminalObservation(event -> {
            throw new IllegalStateException("private");
        }, traffic, 1, metrics);
        observation.finish(SignalType.CANCEL, null);
        observation.finish(SignalType.ON_COMPLETE, 200);
        verify(metrics).started(AgentRuntimeMetrics.Traffic.MCP);
        verify(metrics).finished(org.mockito.ArgumentMatchers.eq(AgentRuntimeMetrics.Traffic.MCP),
                org.mockito.ArgumentMatchers.eq(AgentRuntimeMetrics.Outcome.CANCELLED),
                org.mockito.ArgumentMatchers.eq(AgentRuntimeMetrics.Reason.NONE), org.mockito.ArgumentMatchers.anyLong());
        org.mockito.Mockito.verifyNoMoreInteractions(metrics);
    }

    @ParameterizedTest
    @CsvSource({"401,AUTHENTICATION", "403,PROTOCOL", "413,RESOURCE_LIMIT", "429,RESOURCE_LIMIT", "400,PROTOCOL", "500,OTHER"})
    void runtimeMetricsUseBoundedFailureReasons(final int status, final AgentRuntimeMetrics.Reason reason) {
        AgentRuntimeMetrics metrics = mock(AgentRuntimeMetrics.class);
        AgentMcpTerminalObservation observation = new AgentMcpTerminalObservation(null, traffic, 1, metrics);
        observation.failure(new IllegalStateException("private"), status);
        observation.finish(SignalType.ON_COMPLETE, status);
        verify(metrics).finished(org.mockito.ArgumentMatchers.eq(AgentRuntimeMetrics.Traffic.MCP), org.mockito.ArgumentMatchers.any(),
                org.mockito.ArgumentMatchers.eq(reason), org.mockito.ArgumentMatchers.anyLong());
    }

    @Test
    void metricsFailureCannotChangeTheMcpResult() {
        AgentRuntimeMetrics metrics = new AgentRuntimeMetrics() {
            @Override
            public void started(final Traffic trafficType) {
                throw new IllegalStateException("private");
            }

            @Override
            public void finished(final Traffic trafficType, final AgentRuntimeMetrics.Outcome outcome, final Reason reason, final long millis) {
                throw new IllegalStateException("private");
            }
        };
        MockServerWebExchange exchange = exchange("tools/call");
        exchange.getAttributes().put(AgentRuntimeMetrics.KEY, metrics);
        StepVerifier.create(handler(Mono.just(new JsonObject()), identity(), null).handle(exchange, config("json", 30000), traffic, 1)).verifyComplete();
        assertEquals(200, exchange.getResponse().getStatusCode().value());
    }

    @Test
    void trustedTenantAndCommonContextReachProviderAndTerminalWithoutSessionFabrication() {
        Recording observer = new Recording();
        AtomicReference<AgentRequestContext> captured = new AtomicReference<>();
        Mono<JsonObject> action = Mono.deferContextual(context -> {
            captured.set(context.get(AgentRequestContext.REACTOR_KEY));
            return Mono.just(new JsonObject());
        });
        StepVerifier.create(handler(action, ignored -> Mono.just(new AgentMcpIdentity("tenant", "trusted", Set.of("read"), Set.of())), observer)
                .handle(exchange("tools/call"), config("json", 30000), traffic, 7)).verifyComplete();
        AgentMcpTerminalEvent event = observer.only(Outcome.SUCCESS);
        assertEquals("tenant", event.getTenant());
        assertEquals("tenant", captured.get().getTenant());
        assertEquals("trusted", captured.get().getSubject());
        assertEquals(event.getRequestId(), captured.get().getRequestId());
        assertEquals("read", captured.get().getTarget());
        assertEquals("tools/call", captured.get().getOperation());
        assertEquals(AgentMcpRequestParser.VERSION, captured.get().getProtocolVersion());
        assertNull(event.getSessionId());
        assertNull(captured.get().getSessionId());
    }

    @ParameterizedTest
    @CsvSource({"json,SUCCESS", "sse,SUCCESS", "json,TOOL_ERROR", "sse,TOOL_ERROR"})
    void recordsBusinessResultAfterWriting(final String mode, final Outcome outcome) {
        Recording observer = new Recording();
        Mono<JsonObject> action = outcome == Outcome.TOOL_ERROR ? Mono.error(new AgentToolExecutionException("private")) : Mono.just(new JsonObject());
        MockServerWebExchange exchange = exchange("tools/call");
        StepVerifier.create(handler(action, identity(), observer).handle(exchange, config(mode, 30000), traffic, 7)).verifyComplete();
        AgentMcpTerminalEvent event = observer.only(outcome);
        assertEquals("gateway-id", event.getRequestId());
        assertEquals("selector", event.getSelectorId());
        assertEquals("rule", event.getRuleId());
        assertEquals(7, event.getConfigurationVersion());
        assertEquals("trusted", event.getSubject());
        assertEquals("tools/call", event.getMethod());
        assertEquals("read", event.getRequestedTool());
        assertEquals(AgentMcpRequestParser.VERSION, event.getProtocolVersion());
        assertEquals(200, event.getHttpStatus());
        assertTrue(event.getElapsedMillis() >= 0);
    }

    @ParameterizedTest
    @ValueSource(strings = {"tools/list", "server/discover"})
    void nonCallMethodsHaveEventsButDoNotBecomeToolCalls(final String method) {
        Recording observer = new Recording();
        AtomicInteger metricCalls = new AtomicInteger();
        MockServerWebExchange exchange = exchange(method);
        exchange.getAttributes().put(Constants.METRICS_AGENT_MCP_CALL, (AgentMcpCallObserver) (outcome, elapsed) -> metricCalls.incrementAndGet());
        StepVerifier.create(handler(Mono.just(new JsonObject()), identity(), observer).handle(exchange, config("json", 30000), traffic, 1)).verifyComplete();
        AgentMcpTerminalEvent event = observer.only(Outcome.SUCCESS);
        assertEquals(method, event.getMethod());
        assertNull(event.getRequestedTool());
        assertEquals(0, metricCalls.get());
    }

    @Test
    void unauthenticatedRequestDoesNotConsumeBodyOrTrustMirroredMetadata() {
        Recording observer = new Recording();
        AtomicBoolean bodyRead = new AtomicBoolean();
        MockServerWebExchange exchange = MockServerWebExchange.from(builder("tools/call").body(Flux.defer(() -> {
            bodyRead.set(true);
            return Flux.empty();
        })));
        StepVerifier.create(handler(Mono.never(), ignored -> Mono.empty(), observer).handle(exchange, config("json", 30000), traffic, 1)).verifyComplete();
        AgentMcpTerminalEvent event = observer.only(Outcome.REJECTED);
        assertEquals(401, event.getHttpStatus());
        assertNull(event.getSubject());
        assertNull(event.getMethod());
        assertNull(event.getRequestedTool());
        assertNull(event.getProtocolVersion());
        assertFalse(bodyRead.get());
    }

    @Test
    void rejectedPreflightNeverResolvesIdentity() {
        Recording observer = new Recording();
        AtomicBoolean resolved = new AtomicBoolean();
        MockServerWebExchange exchange = MockServerWebExchange.from(builder("tools/call").header(HttpHeaders.ORIGIN, "https://untrusted.example").body(body("tools/call")));
        StepVerifier.create(handler(Mono.never(), ignored -> {
            resolved.set(true);
            return identity().resolve(exchange);
        }, observer).handle(exchange, config("json", 30000), traffic, 1)).verifyComplete();
        assertEquals(403, observer.only(Outcome.REJECTED).getHttpStatus());
        assertFalse(resolved.get());
    }

    @Test
    void malformedBodyRetainsOnlyTrustedSubject() {
        Recording observer = new Recording();
        MockServerWebExchange exchange = MockServerWebExchange.from(builder("tools/call").body("{broken"));
        StepVerifier.create(handler(Mono.never(), identity(), observer).handle(exchange, config("json", 30000), traffic, 1)).verifyComplete();
        AgentMcpTerminalEvent event = observer.only(Outcome.REJECTED);
        assertEquals(400, event.getHttpStatus());
        assertEquals("trusted", event.getSubject());
        assertNull(event.getMethod());
        assertNull(event.getRequestedTool());
        assertNull(event.getProtocolVersion());
    }

    @Test
    void authorizationRejectionDoesNotImplyToolExecution() {
        Recording observer = new Recording();
        AtomicBoolean invoked = new AtomicBoolean();
        StepVerifier.create(handler(Mono.defer(() -> {
            invoked.set(true);
            return Mono.just(new JsonObject());
        }), ignored -> Mono.just(new AgentMcpIdentity("trusted", Set.of())), observer)
                .handle(exchange("tools/call"), config("json", 30000), traffic, 1)).verifyComplete();
        AgentMcpTerminalEvent event = observer.only(Outcome.REJECTED);
        assertEquals(403, event.getHttpStatus());
        assertEquals("read", event.getRequestedTool());
        assertFalse(invoked.get());
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void deadlinesDuringIdentityResolutionAreTerminalTimeouts(final String mode) {
        Recording observer = new Recording();
        AtomicBoolean cancelled = new AtomicBoolean();
        StepVerifier.withVirtualTime(() -> handler(Mono.never(), ignored -> Mono.<AgentMcpIdentity>never().doOnCancel(() -> cancelled.set(true)), observer)
                .handle(exchange("tools/call"), config(mode, 1000), traffic, 1)).thenAwait(Duration.ofSeconds(2)).verifyComplete();
        AgentMcpTerminalEvent event = observer.only(Outcome.TIMEOUT);
        assertEquals(504, event.getHttpStatus());
        assertNull(event.getSubject());
        assertNull(event.getMethod());
        assertTrue(cancelled.get());
    }

    @ParameterizedTest
    @CsvSource({"json,true", "sse,true", "json,false", "sse,false"})
    void recoveredErrorsKeepTheirOriginalClassification(final String mode, final boolean timeout) {
        Recording observer = new Recording();
        Throwable error = timeout ? new TimeoutException("private") : new IllegalStateException("private");
        StepVerifier.create(handler(Mono.error(error), identity(), observer).handle(exchange("tools/call"), config(mode, 30000), traffic, 1)).verifyComplete();
        assertEquals(timeout ? 504 : 500, observer.only(timeout ? Outcome.TIMEOUT : Outcome.SERVER_ERROR).getHttpStatus());
    }

    @Test
    void explicit504IsClassifiedAsTimeout() {
        Recording observer = new Recording();
        StepVerifier.create(handler(Mono.error(new AgentMcpProtocolException(504, -32001, "deadline", null, null)), identity(), observer)
                .handle(exchange("tools/call"), config("json", 30000), traffic, 1)).verifyComplete();
        observer.only(Outcome.TIMEOUT);
    }

    @Test
    void failedRecoveryWriteDoesNotOverwriteTheOriginalClassification() {
        Recording observer = new Recording();
        MockServerWebExchange exchange = exchange("tools/call");
        exchange.getResponse().setWriteHandler(buffers -> buffers.doOnNext(DataBufferUtils::release).then(Mono.error(new IllegalStateException("write"))));
        StepVerifier.create(handler(Mono.error(new AgentMcpProtocolException(403, -32001, "denied", null, null)), identity(), observer)
                .handle(exchange, config("json", 30000), traffic, 1)).expectError().verify();
        AgentMcpTerminalEvent event = observer.only(Outcome.REJECTED);
        assertEquals(403, event.getHttpStatus());
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void cancellingAStalledWriteIsNotSuccess(final String mode) {
        Recording observer = new Recording();
        AtomicBoolean writeStarted = new AtomicBoolean();
        MockServerWebExchange exchange = exchange("tools/call");
        exchange.getResponse().setWriteHandler(buffers -> buffers.doOnNext(DataBufferUtils::release).then(Mono.<Void>never().doOnSubscribe(ignored -> writeStarted.set(true))));
        StepVerifier.create(handler(Mono.just(new JsonObject()), identity(), observer).handle(exchange, config(mode, 30000), traffic, 1))
                .then(() -> {
                    assertTrue(writeStarted.get());
                    assertTrue(observer.events.isEmpty());
                }).thenCancel().verify();
        assertEquals(200, observer.only(Outcome.CANCELLED).getHttpStatus());
    }

    @Test
    void cancellingAuthenticationHasNoInventedMetadataOrStatus() {
        Recording observer = new Recording();
        AtomicBoolean resolving = new AtomicBoolean();
        StepVerifier.create(handler(Mono.never(), ignored -> Mono.<AgentMcpIdentity>never().doOnSubscribe(subscription -> resolving.set(true)), observer)
                .handle(exchange("tools/call"), config("json", 30000), traffic, 1))
                .then(() -> assertTrue(resolving.get())).thenCancel().verify();
        AgentMcpTerminalEvent event = observer.only(Outcome.CANCELLED);
        assertNull(event.getSubject());
        assertNull(event.getMethod());
        assertNull(event.getHttpStatus());
    }

    @Test
    void failedResponseWritingIsNotSuccess() {
        Recording observer = new Recording();
        MockServerWebExchange exchange = exchange("tools/call");
        exchange.getResponse().setWriteHandler(buffers -> buffers.doOnNext(DataBufferUtils::release).then(Mono.error(new IllegalStateException("write"))));
        StepVerifier.create(handler(Mono.just(new JsonObject()), identity(), observer).handle(exchange, config("json", 30000), traffic, 1)).expectError().verify();
        observer.only(Outcome.SERVER_ERROR);
    }

    @Test
    void aThrowingSinkDoesNotBreakResponseMetricsOrTracing() {
        AtomicInteger metrics = new AtomicInteger();
        AtomicInteger events = new AtomicInteger();
        AgentMcpCallTracer.Trace trace = mock(AgentMcpCallTracer.Trace.class);
        when(trace.attach(any())).thenAnswer(invocation -> invocation.getArgument(0));
        MockServerWebExchange exchange = exchange("tools/call");
        exchange.getAttributes().put(Constants.METRICS_AGENT_MCP_CALL, (AgentMcpCallObserver) (outcome, elapsed) -> metrics.incrementAndGet());
        AgentMcpHttpHandler handler = new AgentMcpHttpHandler(dispatcher(Mono.just(new JsonObject())), identity(), ignored -> trace, event -> {
            events.incrementAndGet();
            throw new IllegalStateException("sensitive callback exception");
        });
        StepVerifier.create(handler.handle(exchange, config("json", 30000), traffic, 1)).verifyComplete();
        assertEquals(200, exchange.getResponse().getStatusCode().value());
        assertEquals(1, metrics.get());
        assertEquals(1, events.get());
        verify(trace).finish(Outcome.SUCCESS);
    }

    @Test
    void concurrentRequestsWithIdenticalRpcIdsDoNotShareMetadata() {
        Recording observer = new Recording();
        AgentMcpHttpHandler handler = handler(Mono.just(new JsonObject()).delayElement(Duration.ofMillis(1)),
                exchange -> Mono.just(new AgentMcpIdentity(exchange.getRequest().getPath().value(), Set.of("read"))), observer);
        StepVerifier.create(Flux.range(0, 32).flatMap(index -> handler.handle(MockServerWebExchange.from(MockServerHttpRequest.post("/agent/" + index)
                .headers(builder("tools/call").body(body("tools/call")).getHeaders()).body(body("tools/call"))), config("json", 30000),
                new AgentTrafficContext("request-" + index, "mcp", "selector", "rule-" + index), index), 32)).verifyComplete();
        await().atMost(Duration.ofSeconds(3)).until(() -> observer.events.size() == 32);
        assertEquals(32, observer.events.stream().map(AgentMcpTerminalEvent::getRequestId).distinct().count());
        for (AgentMcpTerminalEvent event : observer.events) {
            long index = event.getConfigurationVersion();
            assertEquals("request-" + index, event.getRequestId());
            assertEquals("rule-" + index, event.getRuleId());
            assertEquals("/agent/" + index, event.getSubject());
            assertEquals(Outcome.SUCCESS, event.getOutcome());
        }
    }

    @Test
    void cancellingOneSubscriptionDoesNotFinishItsSibling() {
        Recording cancelled = new Recording();
        Recording successful = new Recording();
        Sinks.One<JsonObject> pending = Sinks.one();
        StepVerifier.create(handler(pending.asMono(), identity(), successful).handle(exchange("tools/call"), config("json", 30000), traffic, 1))
                .then(() -> {
                    AtomicBoolean started = new AtomicBoolean();
                    StepVerifier.create(handler(Mono.<JsonObject>never().doOnSubscribe(subscription -> started.set(true)), identity(), cancelled)
                            .handle(exchange("tools/call"), config("json", 30000), traffic, 1)).then(() -> assertTrue(started.get())).thenCancel().verify();
                    cancelled.only(Outcome.CANCELLED);
                    assertTrue(successful.events.isEmpty());
                    pending.tryEmitValue(new JsonObject());
                }).verifyComplete();
        successful.only(Outcome.SUCCESS);
    }

    @Test
    void eachSubscriptionStartsAFreshObservation() {
        Recording observer = new Recording();
        AgentMcpHttpHandler handler = handler(Mono.just(new JsonObject()), identity(), observer);
        Mono<Void> request = Mono.defer(() -> handler.handle(exchange("tools/call"), config("json", 30000), traffic, 1));
        StepVerifier.create(request).verifyComplete();
        StepVerifier.create(request).verifyComplete();
        assertEquals(2, observer.events.size());
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void realHttpSuccessEmitsOneBodyFreeTerminalEvent(final String mode) throws Exception {
        Recording observer = new Recording();
        AgentGatewayMcpConfig config = config(mode, 30000);
        AgentTrafficContext requestTraffic = new AgentTrafficContext("http-request", "mcp", "selector", "rule");
        try (TerminalHttpFixture fixture = new TerminalHttpFixture(handler(Mono.just(new JsonObject()), identity(), observer), config, requestTraffic, 1)) {
            HttpResponse<String> response = send(fixture);
            assertEquals(200, response.statusCode());
            assertTrue(response.body().contains("same-id"));
            assertTrue(response.headers().firstValue("Content-Type").orElse("").startsWith(
                    "sse".equals(mode) ? "text/event-stream" : "application/json"));
            AgentMcpTerminalEvent event = observer.only(Outcome.SUCCESS);
            assertEquals("http-request", event.getRequestId());
            assertEquals("tools/call", event.getMethod());
            assertEquals("read", event.getRequestedTool());
            assertEquals(200, event.getHttpStatus());
        }
    }

    @Test
    void realSocketCancellationIsNotSuccess() throws Exception {
        Recording cancelled = new Recording();
        AtomicBoolean subscribed = new AtomicBoolean();
        AtomicBoolean toolCancelled = new AtomicBoolean();
        Mono<JsonObject> pending = Mono.<JsonObject>never().doOnSubscribe(subscription -> subscribed.set(true)).doOnCancel(() -> toolCancelled.set(true));
        AgentGatewayMcpConfig config = config("json", 30000);
        AgentTrafficContext cancelledTraffic = new AgentTrafficContext("cancelled", "mcp", "selector", "rule");
        try (TerminalHttpFixture fixture = new TerminalHttpFixture(handler(pending, identity(), cancelled), config, cancelledTraffic, 1)) {
            try (Socket socket = new Socket()) {
                socket.connect(fixture.address(), 3000);
                socket.setSoTimeout(3000);
                socket.setSoLinger(true, 0);
                String requestBody = body("tools/call");
                socket.getOutputStream().write(("POST /agent HTTP/1.1\r\nHost: localhost\r\nContent-Type: application/json\r\n"
                        + "Accept: application/json, text/event-stream\r\nMCP-Protocol-Version: " + AgentMcpRequestParser.VERSION
                        + "\r\nMcp-Method: tools/call\r\nMcp-Name: read\r\nContent-Length: "
                        + requestBody.getBytes(StandardCharsets.UTF_8).length + "\r\n\r\n" + requestBody).getBytes(StandardCharsets.UTF_8));
                socket.getOutputStream().flush();
                await().atMost(Duration.ofSeconds(5)).untilTrue(subscribed);
            }
            AgentMcpTerminalEvent event = cancelled.only(Outcome.CANCELLED);
            assertEquals("cancelled", event.getRequestId());
            assertEquals("trusted", event.getSubject());
            assertEquals("read", event.getRequestedTool());
            await().atMost(Duration.ofSeconds(3)).untilTrue(toolCancelled);
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void realHttpIdentityFailureRetainsNoClientMetadata(final boolean timeout) throws Exception {
        Recording observer = new Recording();
        AgentMcpSecurityResolver resolver = ignored -> timeout ? Mono.never() : Mono.empty();
        try (TerminalHttpFixture fixture = new TerminalHttpFixture(handler(Mono.just(new JsonObject()), resolver, observer), config("json", 200), traffic, 1)) {
            HttpResponse<String> response = send(fixture);
            assertEquals(timeout ? 504 : 401, response.statusCode());
            AgentMcpTerminalEvent event = observer.only(timeout ? Outcome.TIMEOUT : Outcome.REJECTED);
            assertEquals(response.statusCode(), event.getHttpStatus());
            assertNull(event.getSubject());
            assertNull(event.getMethod());
            assertNull(event.getRequestedTool());
            assertNull(event.getProtocolVersion());
        }
    }

    @Test
    void lateSignalsCannotAlterOrRepeatAnEvent() {
        Recording observer = new Recording();
        AgentMcpTerminalObservation observation = new AgentMcpTerminalObservation(observer, traffic, 1);
        observation.identified("original");
        observation.finish(SignalType.CANCEL, null);
        observation.identified("late");
        observation.response(false);
        observation.failure(new IllegalStateException(), null);
        observation.finish(SignalType.ON_COMPLETE, 200);
        assertEquals("original", observer.only(Outcome.CANCELLED).getSubject());
    }

    private AgentMcpHttpHandler handler(final Mono<JsonObject> action, final AgentMcpSecurityResolver resolver, final AgentMcpTerminalObserver observer) {
        return new AgentMcpHttpHandler(dispatcher(action), resolver, null,
                observer instanceof Recording recording ? AgentInvocationAuditBridge.mcp(observer, recording.audits::add) : observer);
    }

    private AgentMcpDispatcher dispatcher(final Mono<JsonObject> action) {
        AgentToolProvider tool = mock(AgentToolProvider.class);
        when(tool.getName()).thenReturn("read");
        when(tool.getDescription()).thenReturn("Read test data");
        when(tool.getInputSchema()).thenReturn(JsonParser.parseString("{\"type\":\"object\"}").getAsJsonObject());
        when(tool.getRequiredClientCapabilities()).thenReturn(new JsonObject());
        when(tool.invoke(any())).thenReturn(action);
        return new AgentMcpDispatcher(new AgentToolRegistry(List.of(tool)), "test", "1");
    }

    private AgentMcpSecurityResolver identity() {
        return ignored -> Mono.just(new AgentMcpIdentity("trusted", Set.of("read")));
    }

    private AgentGatewayMcpConfig config(final String mode, final int timeout) {
        return AgentGatewayMcpConfig.parse(JsonParser.parseString("{\"allowedTools\":[\"read\"],\"responseMode\":\"" + mode
                + "\",\"timeoutMs\":" + timeout + "}").getAsJsonObject(), true);
    }

    private MockServerHttpRequest.BodyBuilder builder(final String method) {
        MockServerHttpRequest.BodyBuilder builder = MockServerHttpRequest.post("/agent").contentType(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.ACCEPT, "application/json, text/event-stream").header("MCP-Protocol-Version", AgentMcpRequestParser.VERSION).header("Mcp-Method", method);
        if ("tools/call".equals(method)) {
            builder.header("Mcp-Name", "read");
        }
        return builder;
    }

    private MockServerWebExchange exchange(final String method) {
        return MockServerWebExchange.from(builder(method).body(body(method)));
    }

    private String body(final String method) {
        return "{\"jsonrpc\":\"2.0\",\"id\":\"same-id\",\"method\":\"" + method + "\",\"params\":{\"name\":\"read\",\"_meta\":{"
                + "\"io.modelcontextprotocol/protocolVersion\":\"2026-07-28\",\"io.modelcontextprotocol/clientCapabilities\":{}}}}";
    }

    private HttpResponse<String> send(final TerminalHttpFixture fixture) throws Exception {
        HttpRequest request = HttpRequest.newBuilder(fixture.uri()).timeout(Duration.ofSeconds(10))
                .header("Content-Type", "application/json")
                .header("Accept", "application/json, text/event-stream")
                .header("MCP-Protocol-Version", AgentMcpRequestParser.VERSION)
                .header("Mcp-Method", "tools/call").header("Mcp-Name", "read")
                .POST(HttpRequest.BodyPublishers.ofString(body("tools/call"), StandardCharsets.UTF_8)).build();
        return HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(5)).build().send(request, HttpResponse.BodyHandlers.ofString());
    }

    private static final class TerminalHttpFixture implements AutoCloseable {

        private final DisposableServer server;

        private TerminalHttpFixture(final AgentMcpHttpHandler handler, final AgentGatewayMcpConfig config,
                                    final AgentTrafficContext traffic, final long configurationVersion) {
            ReactorHttpHandlerAdapter adapter = new ReactorHttpHandlerAdapter(WebHttpHandlerBuilder
                    .webHandler(exchange -> handler.handle(exchange, config, traffic, configurationVersion)).build());
            this.server = HttpServer.create().host("127.0.0.1").port(0)
                    .handle(adapter).bindNow(Duration.ofSeconds(10));
        }

        private java.net.InetSocketAddress address() {
            return new java.net.InetSocketAddress("127.0.0.1", server.port());
        }

        private URI uri() {
            return URI.create("http://127.0.0.1:" + server.port() + "/agent");
        }

        @Override
        public void close() {
            server.disposeNow(Duration.ofSeconds(5));
        }
    }

    private static final class Recording implements AgentMcpTerminalObserver {

        private final ConcurrentLinkedQueue<AgentMcpTerminalEvent> events = new ConcurrentLinkedQueue<>();

        private final ConcurrentLinkedQueue<AgentInvocationAuditEvent> audits = new ConcurrentLinkedQueue<>();

        @Override
        public void record(final AgentMcpTerminalEvent event) {
            events.add(event);
        }

        private AgentMcpTerminalEvent only(final Outcome outcome) {
            await().atMost(Duration.ofSeconds(3)).until(() -> !events.isEmpty());
            assertEquals(1, events.size());
            AgentMcpTerminalEvent event = events.element();
            assertEquals(outcome, event.getOutcome());
            if (!audits.isEmpty()) {
                List<AgentInvocationAuditEvent> logical = audits.stream().filter(audit -> audit.getRecordType() == AgentInvocationAuditEvent.RecordType.LOGICAL).toList();
                assertEquals(1, logical.size());
                assertEquals(event.getOutcome().name(), logical.get(0).getOutcome());
                assertEquals(event.getRequestId(), logical.get(0).getRequestId());
            }
            return event;
        }
    }
}
