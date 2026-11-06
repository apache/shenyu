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

package org.apache.shenyu.plugin.agent.gateway.remote;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.sun.net.httpserver.HttpServer;
import io.modelcontextprotocol.common.McpTransportContext;
import io.modelcontextprotocol.spec.McpSchema;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.AgentGatewayMcpConfig;
import org.apache.shenyu.plugin.agent.gateway.AgentTrafficContext;
import org.apache.shenyu.common.metrics.AgentMcpCallObserver.Outcome;
import org.apache.shenyu.common.metrics.AgentMcpCallObserver;
import org.apache.shenyu.common.tracing.AgentTraceContext;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpRemoteCatalog;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpDispatcher;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpExecutionContext;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpHttpHandler;
import org.apache.shenyu.plugin.agent.gateway.security.AgentMcpIdentity;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolRegistry;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import reactor.core.Disposable;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Hooks;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;
import org.springframework.http.MediaType;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;

import java.net.InetSocketAddress;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpConnectTimeoutException;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.awaitility.Awaitility.await;

class RequestScopedMcpClientTest {

    private static final Duration WAIT = Duration.ofSeconds(5);

    @ParameterizedTest
    @ValueSource(ints = {200, 201, 204, 206})
    void rejectsInvalidInitializedStatus(final int status) throws Exception {
        try (Fixture fixture = new Fixture(false)) {
            fixture.initializedStatus = status;
            assertRejectedAcknowledgement(fixture);
        }
    }

    @Test
    void rejectsNonEmptyInitializedAcknowledgement() throws Exception {
        try (Fixture fixture = new Fixture(false)) {
            fixture.initializedBody = "{}".getBytes(StandardCharsets.UTF_8);
            assertRejectedAcknowledgement(fixture);
        }
    }

    @Test
    void acceptsEmptyInitializedAcknowledgement() throws Exception {
        try (Fixture fixture = new Fixture(false)) {
            RequestScopedMcpClient client = fixture.client();
            try {
                assertEquals("2025-06-18", client.initialize().block(WAIT).protocolVersion());
                assertEquals(1, client.listTools(null).block(WAIT).tools().size());
                awaitIdle(client);
                assertEquals(1, fixture.requests.stream().filter(request -> "initialize".equals(request.get("method"))).count());
            } finally {
                client.closeGracefully().block(WAIT);
            }
            assertEquals(1, fixture.deletes);
        }
    }

    private void assertRejectedAcknowledgement(final Fixture fixture) throws InterruptedException {
        RequestScopedMcpClient client = fixture.client();
        try {
            for (int attempt = 0; attempt < 2; attempt++) {
                StepVerifier.create(client.initialize()).expectErrorMatches(error -> error instanceof IllegalArgumentException
                        && "Invalid notification acknowledgement".equals(error.getMessage())).verify(WAIT);
            }
            StepVerifier.create(client.listTools(null)).expectError(IllegalArgumentException.class).verify(WAIT);
            StepVerifier.create(client.callRaw(new McpSchema.CallToolRequest("lookup", Map.of())))
                    .expectError(IllegalArgumentException.class).verify(WAIT);
            StepVerifier.create(client.initialize()).expectError(IllegalArgumentException.class).verify(WAIT);
            awaitIdle(client);
            assertEquals(2, fixture.requests.size());
            assertEquals(1, fixture.requests.stream().filter(request -> "initialize".equals(request.get("method"))).count());
            assertEquals(1, fixture.requests.stream().filter(request -> "notifications/initialized".equals(request.get("method"))).count());
        } finally {
            client.closeGracefully().block(WAIT);
        }
        assertEquals(1, fixture.deletes);
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void traceHeadersBelongOnlyToCallsNeverSharedHandshakeOrDiscovery(final boolean sse) throws Exception {
        try (Fixture fixture = new Fixture(sse)) {
            RequestScopedMcpClient client = fixture.client();
            try {
                Map<String, String> first = traceHeaders(1);
                reactor.util.context.Context firstContext = tracedContext(first);
                client.initialize().contextWrite(firstContext).block(WAIT);
                client.initialize().contextWrite(tracedContext(traceHeaders(2))).block(WAIT);
                client.listTools(null).contextWrite(firstContext).block(WAIT);
                client.callRaw(new McpSchema.CallToolRequest("lookup", Map.of("token", "first"))).contextWrite(firstContext).block(WAIT);
                client.callRaw(new McpSchema.CallToolRequest("lookup", Map.of("token", "second")))
                        .contextWrite(tracedContext(traceHeaders(2))).block(WAIT);
                client.callRaw(new McpSchema.CallToolRequest("lookup", Map.of("token", "untraced")))
                        .contextWrite(context -> context.put(AgentTraceContext.HEADERS_KEY, first)).block(WAIT);
                assertEquals(1, fixture.requests.stream().filter(request -> "initialize".equals(request.get("method"))).count());
                assertEquals(1, fixture.requests.stream().filter(request -> "notifications/initialized".equals(request.get("method"))).count());
                for (Map<String, String> request : fixture.requests) {
                    String token = request.get("token");
                    boolean traced = "first".equals(token) || "second".equals(token);
                    assertEquals(traced ? "1" : "0", request.get("traceCount"));
                    assertEquals(traced ? "vendor=opaque" : "", request.get("tracestate"));
                    assertEquals("first".equals(token) ? first.get("traceparent")
                            : "second".equals(token) ? traceHeaders(2).get("traceparent") : "", request.get("traceparent"));
                    assertEquals("", request.get("baggage"));
                    assertEquals("", request.get("subjectHeader"));
                    assertEquals("", request.get("cookie"));
                    assertEquals("Bearer fixed-test-token", request.get("authorization"));
                }
            } finally {
                client.closeGracefully().contextWrite(tracedContext(traceHeaders(1))).block(WAIT);
            }
            assertFalse(fixture.cleanupHadTrace);
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void cancellationDoesNotLeakTraceIntoConcurrentOrLaterPeer(final boolean sse) throws Exception {
        try (Fixture fixture = new Fixture(sse)) {
            RequestScopedMcpClient client = fixture.client();
            Disposable cancelled = null;
            try {
                client.initialize().block(WAIT);
                cancelled = client.callRaw(new McpSchema.CallToolRequest("lookup", Map.of("token", "cancel-traced", "delay", 600)))
                        .contextWrite(tracedContext(traceHeaders(1))).subscribe();
                assertTrue(fixture.delayed.await(2, TimeUnit.SECONDS));
                Flux.range(2, 16).flatMap(index -> client.callRaw(new McpSchema.CallToolRequest("lookup", Map.of("token", "trace-" + index)))
                        .contextWrite(tracedContext(traceHeaders(index))), 16).then().block(Duration.ofSeconds(10));
                cancelled.dispose();
                awaitIdle(client);
                client.callRaw(new McpSchema.CallToolRequest("lookup", Map.of("token", "later-untraced"))).block(WAIT);
                List<Map<String, String>> calls = fixture.requests.stream().filter(request -> "tools/call".equals(request.get("method"))).toList();
                assertEquals(18, calls.size());
                for (Map<String, String> call : calls) {
                    String token = call.get("token");
                    String expected = token.startsWith("trace-") ? traceHeaders(Integer.parseInt(token.substring(6))).get("traceparent")
                            : "cancel-traced".equals(token) ? traceHeaders(1).get("traceparent") : "";
                    assertEquals(expected, call.get("traceparent"));
                }
                assertEquals(0, fixture.deletes);
            } finally {
                if (Objects.nonNull(cancelled)) {
                    cancelled.dispose();
                }
                client.closeGracefully().block(WAIT);
            }
        }
    }

    @Test
    void traceCarrierSnapshotsRejectInvalidDataAndDropUnrelatedKeys() {
        Map<String, String> mutable = new HashMap<>(traceHeaders(1));
        Map<String, String> snapshot = AgentTraceContext.headers(mutable);
        mutable.put("traceparent", traceHeaders(2).get("traceparent"));
        assertEquals(traceHeaders(1).get("traceparent"), snapshot.get("traceparent"));
        assertEquals(Set.of("traceparent", "tracestate"), snapshot.keySet());
        assertThrows(UnsupportedOperationException.class, () -> snapshot.put("baggage", "secret"));
        assertEquals(Map.of(), AgentTraceContext.headers(null));
        assertEquals(Map.of(), AgentTraceContext.headers("not-a-map"));
        assertEquals(Map.of(), AgentTraceContext.headers(Map.of("traceparent", 42)));
        for (String invalid : List.of("not-w3c", "00-00000000000000000000000000000000-1111111111111111-01",
                "00-11111111111111111111111111111111-0000000000000000-01", traceHeaders(1).get("traceparent") + "\r\nX-Private: secret")) {
            assertEquals(Map.of(), AgentTraceContext.headers(Map.of("traceparent", invalid, "tracestate", "vendor=opaque")));
        }
        for (String invalid : List.of("vendor=opaque\r\nX-Private: secret", "vendor=one,vendor=two", "UPPER=bad", "vendor=", "vendor=a=b", "a".repeat(513))) {
            assertEquals(Map.of("traceparent", traceHeaders(1).get("traceparent")),
                    AgentTraceContext.headers(Map.of("traceparent", traceHeaders(1).get("traceparent"), "tracestate", invalid)));
        }
    }

    @Test
    void malformedTraceHeadersCannotFailAnOtherwiseValidRemoteCall() throws Exception {
        try (Fixture fixture = new Fixture(false)) {
            RequestScopedMcpClient client = fixture.client();
            try {
                client.initialize().block(WAIT);
                client.callRaw(new McpSchema.CallToolRequest("lookup", Map.of("token", "valid")))
                        .contextWrite(tracedContext(Map.of("traceparent", "invalid\r\nHeader: secret"))).block(WAIT);
                assertTrue(fixture.requests.stream().allMatch(request -> request.get("traceparent").isEmpty()));
            } finally {
                client.closeGracefully().block(WAIT);
            }
        }
    }

    private static Map<String, String> traceHeaders(final int index) {
        return Map.of("traceparent", String.format("00-%032x-%016x-01", index, index), "tracestate", "vendor=opaque",
                "baggage", "subject=private", "X-Spike-Subject", "private", "Authorization", "Bearer agent-secret", "Cookie", "private");
    }

    private static reactor.util.context.Context tracedContext(final Map<String, String> headers) {
        return reactor.util.context.Context.of(AgentTraceContext.HEADERS_KEY, headers, McpTransportContext.KEY,
                McpTransportContext.create(Map.of(AgentTraceContext.HEADERS_KEY, headers, "subject", "private", "deadline", Instant.now().plusSeconds(30))));
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void expiredDeadlineIsATimeoutWithoutAnUpstreamRequest(final boolean sse) throws Exception {
        List<Map<String, Object>> events = new CopyOnWriteArrayList<>();
        try (Fixture fixture = new Fixture(sse)) {
            RequestScopedMcpClient client = fixture.client(events::add);
            try {
                client.initialize().block(WAIT);
                awaitIdle(client);
                int before = events.size();
                int requestsBefore = fixture.requests.size();
                StepVerifier.create(client.callRaw(new McpSchema.CallToolRequest("lookup", Map.of("token", "expired")))
                                .contextWrite(context -> context.put(McpTransportContext.KEY, McpTransportContext.create(Map.of("deadline", Instant.EPOCH)))))
                        .expectError(TimeoutException.class).verify(WAIT);
                assertEquals(before, events.size());
                assertEquals(requestsBefore, fixture.requests.size());
                assertEquals(0, client.pending());
            } finally {
                client.closeGracefully().block(WAIT);
            }
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void realRequestDeadlineReleasesItsFutureAndKeepsPeerHealthy(final boolean sse) throws Exception {
        try (Fixture fixture = new Fixture(sse)) {
            RequestScopedMcpClient client = fixture.client();
            try {
                client.initialize().block(WAIT);
                call(client, "warm-deadline", 0, "a").block(WAIT);
                StepVerifier.create(client.callRaw(new McpSchema.CallToolRequest("lookup", Map.of("token", "deadline", "delay", 3000)))
                                .contextWrite(context -> context.put(McpTransportContext.KEY,
                                        McpTransportContext.create(Map.of("deadline", Instant.now().plusSeconds(1))))))
                        .expectErrorMatches(error -> error instanceof TimeoutException || error instanceof java.net.http.HttpTimeoutException).verify(WAIT);
                awaitIdle(client);
                assertEquals("peer", call(client, "peer", 0, "b").block(WAIT).path("structuredContent").path("token").textValue());
                assertEquals(3, fixture.requests.stream().filter(request -> "tools/call".equals(request.get("method"))).count());
            } finally {
                client.closeGracefully().block(WAIT);
            }
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void realRemoteDeadlineHasStableResponseMetricAndHealthyPeer(final boolean sse) throws Exception {
        try (Fixture fixture = new Fixture(sse)) {
            RequestScopedMcpClient client = fixture.client();
            try {
                client.initialize().block(WAIT);
                call(client, "warm-handler", 0, "a").block(WAIT);
                AgentMcpRemoteCatalog.Snapshot snapshot = new AgentMcpRemoteCatalog.Snapshot() {
                    @Override
                    public Map<String, ObjectNode> definitions() {
                        ObjectNode tool = new ObjectMapper().createObjectNode().put("name", "orders.lookup").put("description", "Read fixture");
                        tool.putObject("inputSchema").put("type", "object");
                        return Map.of("orders.lookup", tool);
                    }

                    @Override
                    public Mono<ObjectNode> invoke(final String name, final JsonObject arguments, final AgentMcpExecutionContext context) {
                        return client.callRaw(new McpSchema.CallToolRequest("lookup", Map.of("token", arguments.get("token").getAsString(),
                                        "delay", arguments.get("delay").getAsInt())))
                                .contextWrite(value -> value.put(McpTransportContext.KEY, McpTransportContext.create(Map.of("deadline", context.getDeadline()))));
                    }
                };
                AgentMcpRemoteCatalog catalog = operation -> operation.apply(snapshot);
                final AgentMcpHttpHandler handler = new AgentMcpHttpHandler(new AgentMcpDispatcher(new AgentToolRegistry(List.of()), "fixture", "1", catalog),
                        ignored -> Mono.just(new AgentMcpIdentity("trusted-fixture", Set.of("orders.lookup"))));
                final AgentGatewayMcpConfig config = AgentGatewayMcpConfig.parse(JsonParser.parseString("{\"allowedTools\":[\"orders.lookup\"],\"timeoutMs\":3000,"
                        + "\"responseMode\":\"" + (sse ? "sse" : "json") + "\"}").getAsJsonObject(), true);
                final AgentTrafficContext traffic = new AgentTrafficContext("private-request", "mcp", "selector", "rule");
                List<Outcome> outcomes = new CopyOnWriteArrayList<>();
                AgentMcpCallObserver observer = (outcome, millis) -> outcomes.add(outcome);
                MockServerWebExchange timedOut = gatewayRequest("slow-handler", 6000, observer);
                StepVerifier.create(handler.handle(timedOut, config, traffic, 1)).verifyComplete();
                assertEquals(504, timedOut.getResponse().getStatusCode().value());
                assertEquals("{\"error\":\"Request deadline exceeded\"}", timedOut.getResponse().getBodyAsString().block(WAIT));
                awaitIdle(client);
                MockServerWebExchange peer = gatewayRequest("peer", 0, observer);
                StepVerifier.create(handler.handle(peer, config, traffic, 2)).verifyComplete();
                assertEquals(200, peer.getResponse().getStatusCode().value());
                assertTrue(peer.getResponse().getBodyAsString().block(WAIT).contains("peer"));
                assertEquals(List.of(Outcome.TIMEOUT, Outcome.TOOL_ERROR), outcomes);
                assertEquals(3, fixture.requests.stream().filter(request -> "tools/call".equals(request.get("method"))).count());
                awaitIdle(client);
            } finally {
                client.closeGracefully().block(WAIT);
            }
        }
    }

    private MockServerWebExchange gatewayRequest(final String token, final int delay, final AgentMcpCallObserver observer) {
        String body = "{\"jsonrpc\":\"2.0\",\"id\":\"same-id\",\"method\":\"tools/call\",\"params\":{\"name\":\"orders.lookup\","
                + "\"arguments\":{\"token\":\"" + token + "\",\"delay\":" + delay + "},\"_meta\":{\"io.modelcontextprotocol/protocolVersion\":\"2026-07-28\","
                + "\"io.modelcontextprotocol/clientCapabilities\":{}}}}";
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/mcp").contentType(MediaType.APPLICATION_JSON)
                .header("Accept", "application/json, text/event-stream").header("MCP-Protocol-Version", "2026-07-28")
                .header("Mcp-Method", "tools/call").header("Mcp-Name", "orders.lookup").body(body));
        exchange.getAttributes().put(Constants.METRICS_AGENT_MCP_CALL, observer);
        return exchange;
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void responseDiagnosticsHaveFixedShapeAndNoRequestData(final boolean sse) throws Exception {
        List<Map<String, Object>> events = new CopyOnWriteArrayList<>();
        try (Fixture fixture = new Fixture(sse)) {
            RequestScopedMcpClient client = fixture.client(events::add);
            try {
                client.initialize().block(WAIT);
                client.listTools(null).block(WAIT);
                call(client, "PRIVATE-ARGUMENT", 0, "PRIVATE-SUBJECT").block(WAIT);
                assertThrows(RuntimeException.class, () -> call(client, "upstream-failure", 0, "PRIVATE-SUBJECT").block(WAIT));
                assertEquals(5, events.size());
                for (Map<String, Object> event : events) {
                    assertEquals(Set.of("event", "server", "method", "id", "status", "contentType", "bytes"), event.keySet());
                    assertEquals("http-response", event.get("event"));
                    assertEquals("orders", event.get("server"));
                    assertTrue(event.get("id") instanceof String);
                    assertTrue(event.get("status") instanceof Integer);
                    assertTrue(event.get("bytes") instanceof Integer);
                    assertEquals("notifications/initialized".equals(event.get("method")) ? 0 : fixture.responseBytes.get(event.get("id")), event.get("bytes"));
                    assertThrows(UnsupportedOperationException.class, () -> event.put("private", "value"));
                    assertTrue(!event.toString().contains("PRIVATE-") && !event.toString().contains("fixed-test-token"));
                }
                assertEquals(List.of(200, 202, 200, 200, 503), events.stream().map(event -> event.get("status")).toList());
                assertEquals(events.size(), events.stream().map(event -> event.get("id")).distinct().count());
            } finally {
                client.closeGracefully().block(WAIT);
            }
        }
    }

    @Test
    void lateConnectFailureAfterCancellationDoesNotDropAnError() {
        List<Throwable> dropped = new CopyOnWriteArrayList<>();
        AtomicBoolean cancelled = new AtomicBoolean();
        CompletableFuture<String> future = new CompletableFuture<>() {
            @Override
            public boolean cancel(final boolean mayInterruptIfRunning) {
                cancelled.set(true);
                return false;
            }
        };
        Hooks.onErrorDropped(dropped::add);
        try {
            Disposable subscription = RequestScopedMcpClient.requestSignal(future).subscribe();
            subscription.dispose();
            assertTrue(cancelled.get());
            future.completeExceptionally(new CompletionException(new HttpConnectTimeoutException("Controlled late failure")));
            assertTrue(dropped.isEmpty());
        } finally {
            Hooks.resetOnErrorDropped();
        }
    }

    @Test
    void activeConnectFailureStillReachesSubscriber() {
        CompletableFuture<String> future = new CompletableFuture<>();
        Mono<String> signal = RequestScopedMcpClient.requestSignal(future);
        HttpConnectTimeoutException failure = new HttpConnectTimeoutException("Controlled active failure");
        future.completeExceptionally(new CompletionException(failure));
        StepVerifier.create(signal).expectErrorSatisfies(error -> assertSame(failure, error)).verify(WAIT);
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void preservesNativeResultsAndSeparatesConcurrentSubjects(final boolean sse) throws Exception {
        try (Fixture fixture = new Fixture(sse)) {
            RequestScopedMcpClient client = fixture.client();
            client.initialize().block(WAIT);
            assertEquals(1, client.listTools(null).block(WAIT).tools().size());
            for (int connections : new int[] {4, 8, 16, 24, 32}) {
                Flux.range(0, connections).flatMap(index -> call(client, "warmup-" + index, 200, "warmup"))
                        .then().block(WAIT);
            }
            List<ObjectNode> results = Flux.range(0, 32).flatMap(index -> call(client, "token-" + index, 0, "subject-" + index)).collectList().block(WAIT);
            assertEquals(32, results.size());
            assertEquals(32, results.stream().map(result -> result.path("structuredContent").path("token").asText()).distinct().count());
            for (ObjectNode result : results) {
                assertTrue(result.path("structuredContent").path("token").asText().startsWith("token-"));
                assertEquals("opaque", result.path("_meta").path("Authorization").textValue());
                assertTrue(result.path("isError").booleanValue());
                assertTrue(result.has("extension"));
            }
            assertTrue(fixture.requests.stream().allMatch(value -> "Bearer fixed-test-token".equals(value.get("authorization"))));
            assertTrue(fixture.requests.stream().allMatch(value -> value.get("subjectHeader").isEmpty() && value.get("cookie").isEmpty()));
            client.closeGracefully().block(WAIT);
            assertEquals(1, fixture.deletes);
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void cancellationOwnsOnlyCurrentRequest(final boolean sse) throws Exception {
        try (Fixture fixture = new Fixture(sse)) {
            RequestScopedMcpClient client = fixture.client();
            client.initialize().block(WAIT);
            Disposable cancelled = call(client, "cancel", 600, "a").subscribe();
            assertTrue(fixture.delayed.await(2, TimeUnit.SECONDS));
            ObjectNode peer = call(client, "peer", 0, "b").block(WAIT);
            cancelled.dispose();
            assertEquals("peer", peer.path("structuredContent").path("token").textValue());
            awaitIdle(client);
            assertEquals(0, fixture.deletes);
            client.closeGracefully().block(WAIT);
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void deadlineCancelsFutureWithoutClosingPeerSession(final boolean sse) throws Exception {
        try (Fixture fixture = new Fixture(sse)) {
            RequestScopedMcpClient client = fixture.client();
            client.initialize().block(WAIT);
            assertThrows(RuntimeException.class, () -> client.callRaw(new McpSchema.CallToolRequest("lookup", Map.of("token", "timeout", "delay", 600)))
                    .contextWrite(value -> value.put(McpTransportContext.KEY, McpTransportContext.create(Map.of("subject", "a", "deadline", Instant.now().plusMillis(80)))))
                    .block(WAIT));
            awaitIdle(client);
            assertEquals("peer", call(client, "peer", 0, "b").block(WAIT).path("structuredContent").path("token").textValue());
            assertEquals(0, fixture.deletes);
            client.closeGracefully().block(WAIT);
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void failedCleanupCanBeRetriedWithoutReplayingBusinessRequests(final boolean sse) throws Exception {
        try (Fixture fixture = new Fixture(sse)) {
            RequestScopedMcpClient client = fixture.client();
            client.initialize().block(WAIT);
            fixture.deleteStatus = 503;
            assertThrows(RuntimeException.class, () -> client.closeGracefully().block(WAIT));
            assertThrows(RuntimeException.class, () -> client.closeGracefully().block(WAIT));
            fixture.deleteStatus = 204;
            client.closeGracefully().block(WAIT);
            client.closeGracefully().block(WAIT);
            assertEquals(3, fixture.deletes);
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void sessionExpiryFailsOnceWithoutReplayAndReinitializesOnlyTheNextCall(final boolean sse) throws Exception {
        try (Fixture fixture = new Fixture(sse)) {
            fixture.uniqueSessions = true;
            RequestScopedMcpClient client = fixture.client();
            client.initialize().block(WAIT);
            assertEquals("expired", call(client, "expired", 0, "a").block(WAIT).path("structuredContent").path("token").textValue());
            fixture.expireSessions();
            final String oldSession = client.currentSessionForTest();
            StepVerifier.create(call(client, "expired", 0, "b"))
                    .expectErrorMatches(error -> error instanceof RemoteSessionExpiredException
                            && "Remote MCP session expired".equals(error.getMessage()))
                    .verify(WAIT);
            assertEquals(2, fixture.requests.stream()
                    .filter(request -> "tools/call".equals(request.get("method")) && request.get("token").equals("expired"))
                    .count());
            assertTrue(Objects.isNull(client.currentSessionForTest()) || !client.currentSessionForTest().equals(oldSession));
            assertEquals("peer", call(client, "peer", 0, "c").block(WAIT).path("structuredContent").path("token").textValue());
            assertEquals(2, fixture.requests.stream().filter(request -> "initialize".equals(request.get("method"))).count(),
                    fixture.requests.toString());
            client.closeGracefully().block(WAIT);
            assertEquals(1, fixture.deletes);
            assertEquals(2, fixture.requests.stream()
                    .filter(request -> "tools/call".equals(request.get("method")) && request.get("token").equals("expired"))
                    .count());
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void gatewayReturnsAnUnknownOutcomeAfterRemoteSessionExpiry(final boolean sse) throws Exception {
        try (Fixture fixture = new Fixture(sse)) {
            fixture.uniqueSessions = true;
            RequestScopedMcpClient client = fixture.client();
            client.initialize().block(WAIT);
            fixture.expireSessions();
            AgentMcpRemoteCatalog.Snapshot snapshot = new AgentMcpRemoteCatalog.Snapshot() {
                @Override
                public Map<String, ObjectNode> definitions() {
                    ObjectNode tool = new ObjectMapper().createObjectNode().put("name", "orders.lookup").put("description", "Read fixture");
                    tool.putObject("inputSchema").put("type", "object");
                    return Map.of("orders.lookup", tool);
                }

                @Override
                public Mono<ObjectNode> invoke(final String name, final JsonObject arguments, final AgentMcpExecutionContext context) {
                    return call(client, "expired", 0, context.getSubject());
                }
            };
            AgentMcpRemoteCatalog catalog = operation -> operation.apply(snapshot);
            AgentMcpHttpHandler handler = new AgentMcpHttpHandler(
                    new AgentMcpDispatcher(new AgentToolRegistry(List.of()), "fixture", "1", catalog),
                    ignored -> Mono.just(new AgentMcpIdentity("fixture", Set.of("orders.lookup"))));
            AgentGatewayMcpConfig config = AgentGatewayMcpConfig.parse(JsonParser.parseString(
                    "{\"allowedTools\":[\"orders.lookup\"],\"timeoutMs\":5000,\"responseMode\":\"json\"}").getAsJsonObject(), true);
            MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/mcp")
                    .contentType(MediaType.APPLICATION_JSON)
                    .header("Accept", "application/json, text/event-stream")
                    .header("Mcp-Protocol-Version", "2026-07-28")
                    .header("Mcp-Method", "tools/call")
                    .header("Mcp-Name", "orders.lookup")
                    .body("{\"jsonrpc\":\"2.0\",\"id\":\"expired\",\"method\":\"tools/call\",\"params\":{\"name\":\"orders.lookup\",\"arguments\":{},"
                            + "\"_meta\":{\"io.modelcontextprotocol/protocolVersion\":\"2026-07-28\","
                            + "\"io.modelcontextprotocol/clientCapabilities\":{}}}}"));
            StepVerifier.create(handler.handle(exchange, config, new AgentTrafficContext("request", "mcp", "selector", "rule"), 1)).verifyComplete();
            JsonNode response = new ObjectMapper().readTree(exchange.getResponse().getBodyAsString().block(WAIT));
            assertEquals(502, exchange.getResponse().getStatusCode().value(), response.toString());
            assertEquals(-32026, response.path("error").path("code").asInt());
            assertEquals("Remote tool outcome is unknown after session expiry", response.path("error").path("message").textValue());
            assertEquals(1, fixture.requests.stream().filter(request -> "tools/call".equals(request.get("method"))).count());
            assertEquals("peer", call(client, "peer", 0, "next").block(WAIT).path("structuredContent").path("token").textValue());
            assertEquals(2, fixture.requests.stream().filter(request -> "initialize".equals(request.get("method"))).count());
            client.closeGracefully().block(WAIT);
        }
    }

    @Test
    void revokedGenerationCannotRecoverWithCapturedCredentials() throws Exception {
        try (Fixture fixture = new Fixture(false)) {
            fixture.uniqueSessions = true;
            RequestScopedMcpClient client = fixture.client();
            AtomicBoolean authorized = new AtomicBoolean(true);
            client.recoveryGuard(authorized::get);
            client.initialize().block(WAIT);
            fixture.expireSessions();
            assertThrows(RemoteSessionExpiredException.class, () -> call(client, "expired", 0, "a").block(WAIT));
            authorized.set(false);
            assertThrows(AgentMcpRemoteCatalog.CatalogUnavailableException.class, () -> call(client, "peer", 0, "b").block(WAIT));
            assertEquals(1, fixture.requests.stream().filter(request -> "initialize".equals(request.get("method"))).count());
            client.closeGracefully().block(WAIT);
        }
    }

    @Test
    void abandonedCleanupStopsRetryingCallsButIsRetriedAtShutdown() throws Exception {
        try (Fixture fixture = new Fixture(false)) {
            fixture.uniqueSessions = true;
            RequestScopedMcpClient client = fixture.client(Duration.ofMillis(25));
            client.initialize().block(WAIT);
            await().atMost(WAIT).until(client::sessionIdleForTest);
            fixture.deleteStatus = 503;
            call(client, "first", 0, "a").block(WAIT);
            call(client, "second", 0, "b").block(WAIT);
            call(client, "third", 0, "c").block(WAIT);
            assertEquals(2, fixture.deletes);
            assertEquals(1, client.exhaustedCleanupCount());
            fixture.deleteStatus = 204;
            client.closeGracefully().block(WAIT);
            assertEquals(4, fixture.deletes);
        }
    }

    @Test
    void concurrentExpiryFailsEachCallOnceAndCoalescesReinitialization() throws Exception {
        try (Fixture fixture = new Fixture(false)) {
            fixture.uniqueSessions = true;
            fixture.blockConcurrentCalls = true;
            RequestScopedMcpClient client = fixture.client();
            client.initialize().block(WAIT);
            CountDownLatch finished = new CountDownLatch(4);
            for (int index = 0; index < 4; index++) {
                call(client, "expired", 0, "concurrent-" + index).subscribe(ignored -> finished.countDown(),
                        error -> finished.countDown(), finished::countDown);
            }
            assertTrue(fixture.concurrentCalls.await(WAIT.toMillis(), TimeUnit.MILLISECONDS), fixture.requests.toString());
            fixture.expireSessions();
            fixture.allowConcurrentCalls();
            assertTrue(finished.await(WAIT.toMillis(), TimeUnit.MILLISECONDS));
            assertEquals(4, fixture.requests.stream()
                    .filter(request -> "tools/call".equals(request.get("method")) && request.get("token").equals("expired"))
                    .count());
            assertEquals("peer", call(client, "peer", 0, "peer").block(WAIT).path("structuredContent").path("token").textValue());
            assertEquals(2, fixture.requests.stream().filter(request -> "initialize".equals(request.get("method"))).count());
            client.closeGracefully().block(WAIT);
        }
    }

    @Test
    void idleSessionIsDeletedAndReinitializedOnceBeforeTheNextCall() throws Exception {
        try (Fixture fixture = new Fixture(false)) {
            fixture.uniqueSessions = true;
            RequestScopedMcpClient client = fixture.client(Duration.ofMillis(25));
            client.initialize().block(WAIT);
            await().atMost(WAIT).until(client::sessionIdleForTest);
            assertEquals("peer", call(client, "peer", 0, "peer").block(WAIT).path("structuredContent").path("token").textValue());
            assertEquals(2, fixture.requests.stream().filter(request -> "initialize".equals(request.get("method"))).count());
            assertEquals(1, fixture.deletes);
            client.closeGracefully().block(WAIT);
            assertEquals(2, fixture.deletes);
        }
    }

    @Test
    void failedIdleDeleteRetriesOnlyThePendingCleanupBeforeRecovery() throws Exception {
        try (Fixture fixture = new Fixture(false)) {
            fixture.uniqueSessions = true;
            RequestScopedMcpClient client = fixture.client(Duration.ofMillis(25));
            client.initialize().block(WAIT);
            await().atMost(WAIT).until(client::sessionIdleForTest);
            fixture.deleteStatus = 503;
            assertEquals("first-recovery", call(client, "first-recovery", 0, "a").block(WAIT)
                    .path("structuredContent").path("token").textValue());
            assertTrue(client.sessionInitializedForTest());
            fixture.deleteStatus = 204;
            assertEquals("peer", call(client, "peer", 0, "b").block(WAIT).path("structuredContent").path("token").textValue());
            assertEquals(2, fixture.requests.stream().filter(request -> "initialize".equals(request.get("method"))).count());
            assertEquals(2, fixture.deletes);
            client.closeGracefully().block(WAIT);
            assertEquals(3, fixture.deletes);
        }
    }

    @Test
    void hangingIdleDeleteIsBoundedAndDoesNotBlockRecovery() throws Exception {
        try (Fixture fixture = new Fixture(false)) {
            fixture.uniqueSessions = true;
            RequestScopedMcpClient client = fixture.client(Duration.ofMillis(25));
            client.initialize().block(WAIT);
            await().atMost(WAIT).until(client::sessionIdleForTest);
            fixture.deleteGate = new CountDownLatch(1);
            try {
                assertEquals("peer", call(client, "peer", 0, "a").block(WAIT).path("structuredContent").path("token").textValue());
                assertEquals(1, fixture.deletes);
            } finally {
                fixture.deleteGate.countDown();
                client.closeGracefully().block(WAIT);
            }
        }
    }

    @Test
    void expiredCallerBudgetDoesNotBeginRecoveryOrBusinessTraffic() throws Exception {
        try (Fixture fixture = new Fixture(false)) {
            RequestScopedMcpClient client = fixture.client();
            StepVerifier.create(client.callRaw(new McpSchema.CallToolRequest("lookup", Map.of()))
                    .contextWrite(context -> context.put(McpTransportContext.KEY,
                            McpTransportContext.create(Map.of("deadline", Instant.now().minusSeconds(1))))))
                    .expectError(TimeoutException.class).verify(WAIT);
            assertTrue(fixture.requests.isEmpty());
            client.closeGracefully().block(WAIT);
        }
    }

    @Test
    void safeDirectoryFailureDoesNotClaimUnknownMutatingOutcome() {
        AgentMcpRemoteCatalog catalog = operation -> Mono.error(new RemoteSessionExpiredException());
        AgentMcpDispatcher dispatcher = new AgentMcpDispatcher(new AgentToolRegistry(List.of()), "fixture", "1", catalog);
        String body = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/list\",\"params\":{\"_meta\":{"
                + "\"io.modelcontextprotocol/protocolVersion\":\"2026-07-28\",\"io.modelcontextprotocol/clientCapabilities\":{}}}}";
        org.springframework.http.HttpHeaders headers = new org.springframework.http.HttpHeaders();
        headers.set("MCP-Protocol-Version", "2026-07-28");
        headers.set("Mcp-Method", "tools/list");
        var request = new org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpRequestParser()
                .parse(body.getBytes(StandardCharsets.UTF_8), headers, 8192);
        StepVerifier.create(dispatcher.dispatch(request, () -> new AgentMcpExecutionContext("read", "reader", "rule", 1, Set.of(), Set.of())))
                .expectErrorSatisfies(error -> assertEquals(AgentMcpRemoteCatalog.CatalogUnavailableException.HTTP_STATUS,
                        ((org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpProtocolException) error).getHttpStatus())).verify();
    }

    @Test
    void statelessHttp404RemainsAnOrdinaryUpstreamErrorWithoutInventedSession() throws Exception {
        try (Fixture fixture = new Fixture(false)) {
            fixture.stateless = true;
            fixture.uniqueSessions = true;
            RequestScopedMcpClient client = fixture.client();
            client.initialize().block(WAIT);
            assertTrue(Objects.isNull(client.currentSessionForTest()));
            assertThrows(IllegalStateException.class, () -> call(client, "stateless-404", 0, "a").block(WAIT));
            assertTrue(Objects.isNull(client.currentSessionForTest()));
            assertEquals("peer", call(client, "peer", 0, "b").block(WAIT).path("structuredContent").path("token").textValue());
            assertEquals(1, fixture.requests.stream().filter(request -> "initialize".equals(request.get("method"))).count());
            client.closeGracefully().block(WAIT);
            assertEquals(0, fixture.deletes);
        }
    }

    @Test
    void staleSession404DoesNotInvalidateANewerSession() throws Exception {
        try (Fixture fixture = new Fixture(false)) {
            fixture.uniqueSessions = true;
            RequestScopedMcpClient client = fixture.client();
            client.initialize().block(WAIT);
            final String first = client.currentSessionForTest();
            fixture.expireSessions();
            assertThrows(RuntimeException.class, () -> call(client, "expired", 0, "a").block(WAIT));
            fixture.acceptSessions();
            assertEquals("peer", call(client, "peer", 0, "b").block(WAIT).path("structuredContent").path("token").textValue());
            String second = client.currentSessionForTest();
            assertTrue(!first.equals(second));
            client.invalidateSessionForTest(first, 0);
            assertTrue(Objects.nonNull(client.currentSessionForTest()));
            assertEquals(second, client.currentSessionForTest());
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void repeatedCancellationPreservesPeerErrorsWithoutDroppedSignals(final boolean sse) throws Exception {
        List<Throwable> dropped = new CopyOnWriteArrayList<>();
        Hooks.onErrorDropped(dropped::add);
        try (Fixture fixture = new Fixture(sse)) {
            RequestScopedMcpClient client = fixture.client();
            client.initialize().block(WAIT);
            for (int index = 0; index < 8; index++) {
                int before = fixture.requests.size();
                Disposable cancelled = call(client, "cancel-" + index, 150, "a").subscribe();
                long until = System.nanoTime() + TimeUnit.SECONDS.toNanos(2);
                while (fixture.requests.size() == before && System.nanoTime() < until) {
                    Thread.sleep(5);
                }
                assertTrue(fixture.requests.size() > before);
                cancelled.dispose();
                awaitIdle(client);
            }
            assertThrows(RuntimeException.class, () -> call(client, "upstream-failure", 0, "b").block(WAIT));
            assertEquals("peer", call(client, "peer", 0, "b").block(WAIT).path("structuredContent").path("token").textValue());
            client.closeGracefully().block(WAIT);
            Thread.sleep(200);
            assertTrue(dropped.isEmpty(), dropped.toString());
        } finally {
            Hooks.resetOnErrorDropped();
        }
    }

    private static Mono<ObjectNode> call(final RequestScopedMcpClient client, final String token, final int delay, final String subject) {
        return client.callRaw(new McpSchema.CallToolRequest("lookup", Map.of("token", token, "delay", delay)))
                .transformDeferredContextual((response, context) -> response.doOnNext(ignored ->
                        assertEquals(subject, ((McpTransportContext) context.get(McpTransportContext.KEY)).get("subject"))))
                .contextWrite(value -> value.put(McpTransportContext.KEY, McpTransportContext.create(Map.of("subject", subject))));
    }

    private static void awaitIdle(final RequestScopedMcpClient client) throws InterruptedException {
        await().atMost(WAIT).untilAsserted(() -> assertEquals(0, client.pending()));
    }

    private static final class Fixture implements AutoCloseable {

        private final ObjectMapper json = new ObjectMapper();

        private final HttpServer server;

        private final ExecutorService executor = Executors.newFixedThreadPool(16);

        private final List<Map<String, String>> requests = new CopyOnWriteArrayList<>();

        private final Map<String, Integer> responseBytes = new ConcurrentHashMap<>();

        private final CountDownLatch delayed = new CountDownLatch(1);

        private final CountDownLatch concurrentCalls = new CountDownLatch(4);

        private final CountDownLatch concurrentCallsRelease = new CountDownLatch(1);

        private volatile boolean blockConcurrentCalls;

        private volatile int deleteStatus = 204;

        private volatile int deleteDelayMillis;

        private volatile int initializedStatus = 202;

        private volatile byte[] initializedBody = new byte[0];

        private volatile int deletes;

        private volatile boolean cleanupHadTrace;

        private volatile boolean sessionsExpired;

        private volatile boolean stateless;

        private volatile boolean uniqueSessions;

        private volatile boolean rejectExpiredSession;

        private volatile CountDownLatch deleteGate;

        private final AtomicInteger sessionCounter = new AtomicInteger();

        private Fixture(final boolean sse) throws Exception {
            server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
            server.setExecutor(executor);
            ((ThreadPoolExecutor) executor).prestartAllCoreThreads();
            server.createContext("/ready", exchange -> {
                exchange.sendResponseHeaders(204, -1);
                exchange.close();
            });
            server.createContext("/mcp", exchange -> {
                try {
                    if ("DELETE".equals(exchange.getRequestMethod())) {
                        cleanupHadTrace = exchange.getRequestHeaders().containsKey("traceparent") || exchange.getRequestHeaders().containsKey("tracestate");
                        deletes++;
                        if (deleteDelayMillis > 0) {
                            Thread.sleep(deleteDelayMillis);
                        }
                        if (Objects.nonNull(deleteGate)) {
                            assertTrue(deleteGate.await(WAIT.toMillis(), TimeUnit.MILLISECONDS));
                        }
                        String deletedSession = exchange.getRequestHeaders().getFirst("MCP-Session-Id");
                        if (rejectExpiredSession && Objects.nonNull(deletedSession) && deletedSession.endsWith("-1")) {
                            exchange.sendResponseHeaders(404, -1);
                            return;
                        }
                        if (sessionsExpired) {
                            exchange.sendResponseHeaders(404, -1);
                            return;
                        }
                        exchange.sendResponseHeaders(deleteStatus, -1);
                        return;
                    }
                    ObjectNode request = (ObjectNode) json.readTree(exchange.getRequestBody());
                    String method = request.path("method").textValue();
                    requests.add(Map.of("method", method, "authorization", exchange.getRequestHeaders().getFirst("Authorization"),
                            "subjectHeader", Objects.toString(exchange.getRequestHeaders().getFirst("X-Spike-Subject"), ""),
                            "cookie", Objects.toString(exchange.getRequestHeaders().getFirst("Cookie"), ""),
                            "traceparent", Objects.toString(exchange.getRequestHeaders().getFirst("traceparent"), ""),
                            "traceCount", Integer.toString(exchange.getRequestHeaders().getOrDefault("traceparent", List.of()).size()),
                            "tracestate", Objects.toString(exchange.getRequestHeaders().getFirst("tracestate"), ""),
                            "baggage", Objects.toString(exchange.getRequestHeaders().getFirst("baggage"), ""),
                            "token", request.path("params").path("arguments").path("token").asText()));
                    if (blockConcurrentCalls && "tools/call".equals(method)
                            && "expired".equals(request.path("params").path("arguments").path("token").textValue())
                            && Objects.nonNull(exchange.getRequestHeaders().getFirst("MCP-Session-Id"))) {
                        concurrentCalls.countDown();
                        if (!concurrentCallsRelease.await(WAIT.toMillis(), TimeUnit.MILLISECONDS)) {
                            throw new IllegalStateException("Concurrent calls were not released");
                        }
                    }
                    if ("notifications/initialized".equals(method)) {
                        responseBytes.put(request.path("id").asText(), 0);
                        exchange.sendResponseHeaders(initializedStatus, initializedBody.length == 0 ? -1 : initializedBody.length);
                        if (initializedBody.length > 0) {
                            exchange.getResponseBody().write(initializedBody);
                        }
                        return;
                    }
                    ObjectNode result = json.createObjectNode();
                    if ("initialize".equals(method)) {
                        if (!stateless) {
                            exchange.getResponseHeaders().set("MCP-Session-Id",
                                    uniqueSessions ? "unit-session-" + sessionCounter.incrementAndGet() : "unit-session");
                        }
                        result.put("protocolVersion", "2025-06-18");
                        result.putObject("capabilities").putObject("tools");
                        result.putObject("serverInfo").put("name", "unit").put("version", "1");
                    } else if ("tools/list".equals(method)) {
                        var tool = result.putArray("tools").addObject().put("name", "lookup").put("description", "lookup");
                        tool.putObject("inputSchema").put("type", "object");
                    } else {
                        if ("upstream-failure".equals(request.path("params").path("arguments").path("token").textValue())) {
                            responseBytes.put(request.path("id").asText(), 0);
                            exchange.sendResponseHeaders(503, -1);
                            return;
                        }
                        int delay = request.path("params").path("arguments").path("delay").asInt();
                        if (delay > 0) {
                            delayed.countDown();
                            Thread.sleep(delay);
                        }
                        result.putArray("content").addObject().put("type", "text").put("text", "native");
                        String token = request.path("params").path("arguments").path("token").textValue();
                        String session = exchange.getRequestHeaders().getFirst("MCP-Session-Id");
                        if ("expired".equals(token) && rejectExpiredSession && Objects.nonNull(session) && session.endsWith("-1")) {
                            responseBytes.put(request.path("id").asText(), 0);
                            exchange.sendResponseHeaders(404, -1);
                            return;
                        }
                        if ("stateless-404".equals(token)) {
                            responseBytes.put(request.path("id").asText(), 0);
                            exchange.sendResponseHeaders(404, -1);
                            return;
                        }
                        if ("expired".equals(token) && sessionsExpired) {
                            responseBytes.put(request.path("id").asText(), 0);
                            exchange.sendResponseHeaders(404, -1);
                            return;
                        }
                        result.putObject("structuredContent").put("token", "expired".equals(token) ? "expired" : token);
                        result.put("isError", true).put("extension", "native-extension");
                        result.putObject("_meta").put("Authorization", "opaque");
                    }
                    ObjectNode envelope = json.createObjectNode().put("jsonrpc", "2.0");
                    envelope.set("id", request.get("id"));
                    envelope.set("result", result);
                    boolean streaming = sse && !"initialize".equals(method);
                    byte[] response = (streaming ? "event: message\ndata: " + envelope + "\n\n" : envelope.toString()).getBytes(StandardCharsets.UTF_8);
                    responseBytes.put(request.path("id").asText(), response.length);
                    exchange.getResponseHeaders().set("Content-Type", streaming ? "text/event-stream" : "application/json");
                    exchange.sendResponseHeaders(200, response.length);
                    exchange.getResponseBody().write(response);
                } catch (Exception expected) {
                    // Cancelled connections and fixture interruption are deliberately exercised.
                } finally {
                    exchange.close();
                }
            });
            server.start();
            HttpClient warmup = HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(30)).build();
            HttpRequest ready = HttpRequest.newBuilder(URI.create("http://127.0.0.1:" + server.getAddress().getPort() + "/ready"))
                    .timeout(Duration.ofSeconds(30)).GET().build();
            assertEquals(204, warmup.send(ready, HttpResponse.BodyHandlers.discarding()).statusCode());
        }

        private RequestScopedMcpClient client() {
            return client(ignored -> { });
        }

        private RequestScopedMcpClient client(final Consumer<Map<String, Object>> observer) {
            URI uri = URI.create("http://127.0.0.1:" + server.getAddress().getPort() + "/mcp");
            RemoteServerBinding.Config target = new RemoteServerBinding.Config("orders", uri, "service/orders", "v1");
            return new RequestScopedMcpClient(RemoteServerBinding.resolve(target, Set.of(uri),
                    ignored -> new RemoteServerBinding.Credential(target, "fixed-test-token")), observer);
        }

        private RequestScopedMcpClient client(final Duration sessionIdleTtl) {
            URI uri = URI.create("http://127.0.0.1:" + server.getAddress().getPort() + "/mcp");
            RemoteServerBinding.Config target = new RemoteServerBinding.Config("orders", uri, "service/orders", "v1");
            return new RequestScopedMcpClient(RemoteServerBinding.resolve(target, Set.of(uri),
                    ignored -> new RemoteServerBinding.Credential(target, "fixed-test-token")), ignored -> { }, null, sessionIdleTtl);
        }

        private void expireSessions() {
            sessionsExpired = true;
        }

        private void releaseConcurrentCalls() {
            concurrentCallsRelease.countDown();
        }

        private void allowConcurrentCalls() {
            blockConcurrentCalls = false;
            releaseConcurrentCalls();
        }

        private void acceptSessions() {
            sessionsExpired = false;
            rejectExpiredSession = true;
        }

        @Override
        public void close() {
            server.stop(0);
            executor.shutdownNow();
        }
    }
}
