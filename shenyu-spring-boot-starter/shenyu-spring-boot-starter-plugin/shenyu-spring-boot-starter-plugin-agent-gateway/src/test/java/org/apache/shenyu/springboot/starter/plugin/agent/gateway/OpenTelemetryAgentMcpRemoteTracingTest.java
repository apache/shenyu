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

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.sun.net.httpserver.HttpServer;
import io.opentelemetry.api.common.AttributeKey;
import io.opentelemetry.api.trace.Span;
import io.opentelemetry.api.trace.SpanKind;
import io.opentelemetry.context.Context;
import io.opentelemetry.sdk.OpenTelemetrySdk;
import io.opentelemetry.sdk.testing.exporter.InMemorySpanExporter;
import io.opentelemetry.sdk.trace.SdkTracerProvider;
import io.opentelemetry.sdk.trace.export.SimpleSpanProcessor;
import org.apache.shenyu.common.dto.AgentGatewayMcpConfig;
import org.apache.shenyu.common.tracing.AgentTraceContext;
import org.apache.shenyu.plugin.agent.gateway.AgentTrafficContext;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpDispatcher;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpHttpHandler;
import org.apache.shenyu.plugin.agent.gateway.remote.ManagedRemoteMcpCatalog;
import org.apache.shenyu.plugin.agent.gateway.remote.RemoteServerBinding;
import org.apache.shenyu.plugin.agent.gateway.security.AgentMcpIdentity;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolRegistry;
import org.apache.shenyu.plugin.tracing.TracingConstants;
import org.apache.shenyu.plugin.tracing.TracingPlugin;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.http.server.reactive.ReactorHttpHandlerAdapter;
import org.springframework.web.server.adapter.WebHttpHandlerBuilder;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.netty.DisposableServer;
import reactor.test.StepVerifier;

import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/** Real catalog and outbound HTTP regression for request-local logical spans. */
class OpenTelemetryAgentMcpRemoteTracingTest {

    private static final Duration WAIT = Duration.ofSeconds(30);

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void gatewayParentLogicalCallAndOutboundHeadersStayCorrelated(final boolean sse) throws Exception {
        try (Fixture fixture = new Fixture(sse)) {
            StepVerifier.create(Flux.range(0, 32).flatMap(index -> fixture.call("call-" + index), 32)).verifyComplete();
            fixture.assertCalls(32);
            fixture.assertServiceIsolation();
            assertEquals(1, fixture.requests.stream().filter(request -> "initialize".equals(request.method())).count());
            assertEquals(1, fixture.requests.stream().filter(request -> "notifications/initialized".equals(request.method())).count());
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void cancellingOneLogicalCallDoesNotEndPeerOrReuseItsHeaders(final boolean sse) throws Exception {
        try (Fixture fixture = new Fixture(sse)) {
            fixture.holdPeer = true;
            final CompletableFuture<Void> peer = fixture.cancelWhilePeerPending();
            await().atMost(WAIT).until(() -> fixture.exporter.getFinishedSpanItems().stream()
                    .filter(span -> "cancelled".equals(span.getAttributes().get(AttributeKey.stringKey("shenyu.mcp.outcome")))).count() == 1);
            assertFalse(peer.isDone());
            assertTrue(fixture.exporter.getFinishedSpanItems().stream()
                    .noneMatch(span -> fixture.inboundTraces.get("peer").equals(span.getTraceId())));
            fixture.peerRelease.countDown();
            peer.get(30, TimeUnit.SECONDS);
            fixture.call("later").block(WAIT);
            fixture.assertCalls(3);
            fixture.assertServiceIsolation();
            assertEquals(1, fixture.exporter.getFinishedSpanItems().stream()
                    .filter(span -> "cancelled".equals(span.getAttributes().get(AttributeKey.stringKey("shenyu.mcp.outcome")))).count());
            assertEquals(2, fixture.exporter.getFinishedSpanItems().stream()
                    .filter(span -> "success".equals(span.getAttributes().get(AttributeKey.stringKey("shenyu.mcp.outcome")))).count());
            assertFalse(fixture.requests.stream().anyMatch(request -> "DELETE".equals(request.method())));
        }
    }

    private record Request(String method, String token, String traceparent, Map<String, List<String>> headers) { }

    private static final class Fixture implements AutoCloseable {

        private final InMemorySpanExporter exporter = InMemorySpanExporter.create();

        private final OpenTelemetrySdk sdk = OpenTelemetrySdk.builder().setTracerProvider(SdkTracerProvider.builder()
                .addSpanProcessor(SimpleSpanProcessor.create(exporter)).build()).build();

        private final ExecutorService executor = Executors.newFixedThreadPool(8);

        private final CountDownLatch entered = new CountDownLatch(1);

        private final CountDownLatch release = new CountDownLatch(1);

        private final CountDownLatch peerEntered = new CountDownLatch(1);

        private final CountDownLatch peerRelease = new CountDownLatch(1);

        private final Map<String, String> parents = new ConcurrentHashMap<>();

        private final Map<String, String> inboundTraces = new ConcurrentHashMap<>();

        private final List<Request> requests = new CopyOnWriteArrayList<>();

        private final HttpServer server;

        private final DisposableServer gateway;

        private final HttpClient client = HttpClient.newBuilder().connectTimeout(WAIT).build();

        private volatile boolean holdPeer;

        private final ManagedRemoteMcpCatalog catalog;

        private final AgentMcpHttpHandler handler;

        private final TracingPlugin tracing = new TracingPlugin(sdk.getTracer("gateway-fixture"));

        private final AgentGatewayMcpConfig config;

        private Fixture(final boolean sse) throws Exception {
            server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
            server.setExecutor(executor);
            server.createContext("/ready", exchange -> {
                exchange.sendResponseHeaders(204, -1);
                exchange.close();
            });
            server.createContext("/mcp", exchange -> {
                try {
                    if ("DELETE".equals(exchange.getRequestMethod())) {
                        requests.add(new Request("DELETE", "", Objects.toString(exchange.getRequestHeaders().getFirst("traceparent"), ""),
                                Map.copyOf(exchange.getRequestHeaders())));
                        exchange.sendResponseHeaders(204, -1);
                        return;
                    }
                    ObjectMapper json = new ObjectMapper();
                    var request = json.readTree(exchange.getRequestBody());
                    String method = request.path("method").asText();
                    String token = request.path("params").path("arguments").path("token").asText();
                    requests.add(new Request(method, token, Objects.toString(exchange.getRequestHeaders().getFirst("traceparent"), ""),
                            Map.copyOf(exchange.getRequestHeaders())));
                    if ("notifications/initialized".equals(method)) {
                        exchange.sendResponseHeaders(202, -1);
                        return;
                    }
                    var result = json.createObjectNode();
                    if ("initialize".equals(method)) {
                        exchange.getResponseHeaders().set("MCP-Session-Id", "shared-service-session");
                        result.put("protocolVersion", "2025-06-18");
                        result.putObject("capabilities").putObject("tools");
                        result.putObject("serverInfo").put("name", "fixture").put("version", "1");
                    } else if ("tools/list".equals(method)) {
                        result.putArray("tools").addObject().put("name", "lookup").put("description", "Read")
                                .putObject("inputSchema").put("type", "object");
                    } else {
                        if ("cancel".equals(token)) {
                            entered.countDown();
                            release.await(60, TimeUnit.SECONDS);
                        } else if (holdPeer && "peer".equals(token)) {
                            peerEntered.countDown();
                            peerRelease.await(60, TimeUnit.SECONDS);
                        }
                        result.putArray("content").addObject().put("type", "text").put("text", token);
                        result.putObject("structuredContent").put("token", token);
                        result.put("isError", false);
                    }
                    var envelope = json.createObjectNode().put("jsonrpc", "2.0");
                    envelope.set("id", request.get("id"));
                    envelope.set("result", result);
                    boolean streaming = sse && "tools/call".equals(method);
                    byte[] bytes = (streaming ? "data: " + envelope + "\n\n" : envelope.toString()).getBytes(StandardCharsets.UTF_8);
                    exchange.getResponseHeaders().set("Content-Type", streaming ? "text/event-stream" : "application/json");
                    exchange.sendResponseHeaders(200, bytes.length);
                    exchange.getResponseBody().write(bytes);
                } catch (Exception expected) {
                    // Cancellation deliberately closes a real remote HTTP connection.
                } finally {
                    exchange.close();
                }
            });
            server.start();
            URI endpoint = URI.create("http://127.0.0.1:" + server.getAddress().getPort() + "/mcp");
            assertEquals(204, HttpClient.newHttpClient().send(HttpRequest.newBuilder(endpoint.resolve("/ready"))
                    .timeout(WAIT).build(), HttpResponse.BodyHandlers.discarding()).statusCode());
            catalog = new ManagedRemoteMcpCatalog(Set.of(endpoint), Set.of(), target -> new RemoteServerBinding.Credential(target, "fixed-service-token"));
            String source = "{\"aggregation\":{\"revision\":\"trace-v1\",\"servers\":[{\"name\":\"orders\",\"endpoint\":\"" + endpoint
                    + "\",\"credentialRef\":\"service/orders\",\"credentialVersion\":\"v1\"}]}}";
            catalog.reload(source).contextWrite(value -> value.put(AgentTraceContext.HEADERS_KEY,
                    Map.of("traceparent", "00-11111111111111111111111111111111-2222222222222222-01"))).block(WAIT);
            handler = new AgentMcpHttpHandler(new AgentMcpDispatcher(new AgentToolRegistry(List.of()), "fixture", "1", catalog),
                    ignored -> Mono.just(new AgentMcpIdentity("private-subject", Set.of("orders.lookup"))), new OpenTelemetryAgentMcpCallTracer(sdk));
            config = AgentGatewayMcpConfig.parse(JsonParser.parseString("{\"allowedTools\":[\"orders.lookup\"],\"timeoutMs\":30000,\"responseMode\":\""
                    + (sse ? "sse" : "json") + "\"}").getAsJsonObject(), true);
            assertEquals(TracingConstants.REACTOR_CONTEXT_KEY, AgentTraceContext.GATEWAY_REACTOR_CONTEXT_KEY);
            ReactorHttpHandlerAdapter adapter = new ReactorHttpHandlerAdapter(WebHttpHandlerBuilder.webHandler(exchange -> tracing.execute(exchange,
                    next -> Mono.deferContextual(view -> {
                        String token = next.getRequest().getPath().value().substring("/agent/".length());
                        Context parent = view.get(TracingConstants.REACTOR_CONTEXT_KEY);
                        parents.put(token, Span.fromContext(parent).getSpanContext().getSpanId());
                        return handler.handle(next, config, new AgentTrafficContext("private-id", "mcp", "selector", "rule"), 1);
                    }))).build());
            gateway = reactor.netty.http.server.HttpServer.create().host("127.0.0.1").port(0).handle(adapter).bindNow(WAIT);
        }

        private Mono<Void> call(final String token) {
            return Mono.defer(() -> {
                HttpRequest request = HttpRequest.newBuilder(URI.create("http://127.0.0.1:" + gateway.port() + "/agent/" + token)).timeout(WAIT)
                        .header("Content-Type", "application/json").header("Accept", "application/json, text/event-stream")
                        .header("MCP-Protocol-Version", "2026-07-28").header("Mcp-Method", "tools/call").header("Mcp-Name", "orders.lookup")
                        .header("Authorization", "Bearer private-agent-token").header("Cookie", "private-cookie").header("baggage", "subject=private")
                        .header("X-Agent-Private", "must-not-escape").header("X-Spike-Subject", "private-agent")
                        .header("traceparent", traceparent(token)).header("tracestate", "vendor=opaque")
                        .POST(HttpRequest.BodyPublishers.ofString(body(token), StandardCharsets.UTF_8)).build();
                return Mono.fromFuture(client.sendAsync(request, HttpResponse.BodyHandlers.ofString())).doOnNext(response -> {
                    assertEquals(200, response.statusCode(), response.body());
                    assertTrue(response.body().contains("same-id"));
                    assertTrue(response.body().contains(token));
                    assertFalse(response.body().contains("\"error\""));
                }).then();
            });
        }

        private String traceparent(final String token) {
            String traceId = UUID.randomUUID().toString().replace("-", "");
            inboundTraces.put(token, traceId);
            return "00-" + traceId + "-bbbbbbbbbbbbbbbb-01";
        }

        private String body(final String token) {
            JsonObject body = JsonParser.parseString("{\"jsonrpc\":\"2.0\",\"id\":\"same-id\",\"method\":\"tools/call\",\"params\":{\"name\":\"orders.lookup\","
                    + "\"arguments\":{},\"_meta\":{\"io.modelcontextprotocol/protocolVersion\":\"2026-07-28\",\"io.modelcontextprotocol/clientCapabilities\":{}}}}").getAsJsonObject();
            body.getAsJsonObject("params").getAsJsonObject("arguments").addProperty("token", token);
            return body.toString();
        }

        private CompletableFuture<Void> cancelWhilePeerPending() throws Exception {
            try (Socket socket = openCancelledCall()) {
                assertTrue(entered.await(10, TimeUnit.SECONDS));
                final CompletableFuture<Void> peer = call("peer").toFuture();
                assertTrue(peerEntered.await(10, TimeUnit.SECONDS));
                return peer;
            }
        }

        private Socket openCancelledCall() throws Exception {
            Socket socket = new Socket();
            try {
                socket.connect(new InetSocketAddress("127.0.0.1", gateway.port()), 3000);
                socket.setSoLinger(true, 0);
                byte[] body = body("cancel").getBytes(StandardCharsets.UTF_8);
                String headers = "POST /agent/cancel HTTP/1.1\r\nHost: localhost\r\nContent-Type: application/json\r\n"
                        + "Accept: application/json, text/event-stream\r\nMCP-Protocol-Version: 2026-07-28\r\n"
                        + "Mcp-Method: tools/call\r\nMcp-Name: orders.lookup\r\ntraceparent: " + traceparent("cancel")
                        + "\r\ntracestate: vendor=opaque\r\nbaggage: subject=private\r\nAuthorization: Bearer private-agent-token\r\n"
                        + "Cookie: private-cookie\r\nX-Agent-Private: must-not-escape\r\nContent-Length: " + body.length + "\r\n\r\n";
                socket.getOutputStream().write(headers.getBytes(StandardCharsets.UTF_8));
                socket.getOutputStream().write(body);
                socket.getOutputStream().flush();
                return socket;
            } catch (Exception error) {
                socket.close();
                throw error;
            }
        }

        private void assertCalls(final int count) {
            await().atMost(WAIT).until(() -> exporter.getFinishedSpanItems().size() == count * 2);
            var spans = exporter.getFinishedSpanItems();
            List<Request> calls = requests.stream().filter(request -> "tools/call".equals(request.method())).toList();
            assertEquals(count, calls.size());
            assertEquals(count, calls.stream().map(Request::traceparent).distinct().count());
            for (Request request : calls) {
                String[] trace = request.traceparent().split("-");
                assertEquals(4, trace.length);
                var logical = spans.stream().filter(span -> trace[2].equals(span.getSpanId())).findFirst().orElseThrow();
                assertEquals("shenyu.agent_gateway.tools.call", logical.getName());
                assertEquals(SpanKind.INTERNAL, logical.getKind());
                assertEquals(trace[1], logical.getTraceId());
                assertEquals(inboundTraces.get(request.token()), logical.getTraceId());
                assertEquals(parents.get(request.token()), logical.getParentSpanId());
                var gateway = spans.stream().filter(span -> logical.getParentSpanId().equals(span.getSpanId())).findFirst().orElseThrow();
                assertEquals(SpanKind.SERVER, gateway.getKind());
                assertEquals(gateway.getTraceId(), logical.getTraceId());
                assertEquals("bbbbbbbbbbbbbbbb", gateway.getParentSpanId());
                assertTrue(request.headers().entrySet().stream().filter(entry -> "tracestate".equalsIgnoreCase(entry.getKey()))
                        .anyMatch(entry -> List.of("vendor=opaque").equals(entry.getValue())));
                assertEquals(1, request.headers().entrySet().stream().filter(entry -> "traceparent".equalsIgnoreCase(entry.getKey()))
                        .mapToInt(entry -> entry.getValue().size()).sum());
            }
        }

        private void assertServiceIsolation() {
            for (Request request : requests) {
                if (!"tools/call".equals(request.method())) {
                    assertTrue(request.headers().keySet().stream().noneMatch(name -> "traceparent".equalsIgnoreCase(name) || "tracestate".equalsIgnoreCase(name)));
                }
                assertTrue(request.headers().keySet().stream().noneMatch(name -> "baggage".equalsIgnoreCase(name)
                        || "X-Spike-Subject".equalsIgnoreCase(name) || "Cookie".equalsIgnoreCase(name) || "X-Agent-Private".equalsIgnoreCase(name)));
                assertTrue(request.headers().entrySet().stream().filter(entry -> "Authorization".equalsIgnoreCase(entry.getKey()))
                        .anyMatch(entry -> List.of("Bearer fixed-service-token").equals(entry.getValue())));
            }
        }

        @Override
        public void close() {
            release.countDown();
            peerRelease.countDown();
            try {
                gateway.disposeNow(WAIT);
                catalog.closeAsync().block(WAIT);
                assertServiceIsolation();
            } finally {
                server.stop(0);
                executor.shutdownNow();
                sdk.close();
            }
        }
    }
}
