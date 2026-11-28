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
import com.sun.net.httpserver.HttpsConfigurator;
import com.sun.net.httpserver.HttpsServer;
import io.modelcontextprotocol.common.McpTransportContext;
import io.modelcontextprotocol.spec.McpSchema;
import org.apache.shenyu.common.dto.AgentGatewayAggregationConfig.ProtocolProfile;
import org.apache.shenyu.common.tracing.AgentTraceContext;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.ArgumentMatchers;
import reactor.adapter.JdkFlowAdapter;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLHandshakeException;
import javax.net.ssl.TrustManagerFactory;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpHeaders;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.KeyStore;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.UnaryOperator;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class StatelessMcpClientTest {

    private static final ObjectMapper JSON = new ObjectMapper();

    private static final Duration WAIT = Duration.ofSeconds(5);

    private static final String VERSION = "2026-07-28";

    private static final String TRACEPARENT = "00-11111111111111111111111111111111-2222222222222222-01";

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void sendsSelfDescribingPostsAndPreservesCompleteResults(final boolean sse) {
        Fixture fixture = new Fixture();
        fixture.sse = sse;
        McpTransportContext context = McpTransportContext.create(Map.of("subject", "private-agent", AgentTraceContext.HEADERS_KEY,
                Map.of("traceparent", TRACEPARENT, "baggage", "private-baggage", "Authorization", "agent-token", "Cookie", "private-cookie")));
        var discovered = fixture.client.initialize().contextWrite(c -> c.put(McpTransportContext.KEY, context)).block(WAIT);
        assertEquals(VERSION, discovered.protocolVersion());
        assertNull(discovered.capabilities().resources());
        fixture.client.initialize().block(WAIT);
        assertEquals("lookup", fixture.client.listTools("page-2").block(WAIT).tools().get(0).name());
        ObjectNode result = fixture.call("查询").contextWrite(c -> c.put(McpTransportContext.KEY, context)).block(WAIT);
        assertEquals("private-output", result.path("content").get(0).path("text").textValue());
        assertEquals("preserved", result.path("extension").textValue());
        assertEquals("opaque", result.path("_meta").path("example.org/value").textValue());
        assertEquals("private-output", fixture.client.callTool(new McpSchema.CallToolRequest("lookup", Map.of())).block(WAIT)
                .content().stream().map(McpSchema.TextContent.class::cast).findFirst().orElseThrow().text());
        fixture.client.closeGracefully().block(WAIT);
        assertEquals(List.of("server/discover", "tools/list", "tools/call", "tools/call"), fixture.methods());
        assertEquals(4, new HashSet<>(fixture.bodies.stream().map(node -> node.path("id").textValue()).toList()).size());
        assertEquals("page-2", fixture.bodies.get(1).at("/params/cursor").textValue());
        assertEquals("查询", fixture.bodies.get(2).at("/params/name").textValue());
        assertEquals("=?base64?5p+l6K+i?=", fixture.requests.get(2).headers().firstValue("Mcp-Name").orElseThrow());
        assertEquals("lookup", fixture.requests.get(3).headers().firstValue("Mcp-Name").orElseThrow());
        for (int i = 0; i < fixture.requests.size(); i++) {
            HttpRequest request = fixture.requests.get(i);
            HttpHeaders headers = request.headers();
            assertEquals("POST", request.method());
            assertEquals("application/json", headers.firstValue("Content-Type").orElseThrow());
            assertEquals("application/json, text/event-stream", headers.firstValue("Accept").orElseThrow());
            assertEquals("Bearer service-token", headers.firstValue("Authorization").orElseThrow());
            assertEquals(fixture.methods().get(i), headers.firstValue("Mcp-Method").orElseThrow());
            assertEquals(VERSION, headers.firstValue("MCP-Protocol-Version").orElseThrow());
            JsonNode meta = fixture.bodies.get(i).at("/params/_meta");
            assertEquals(VERSION, meta.path("io.modelcontextprotocol/protocolVersion").textValue());
            assertEquals(JSON.createObjectNode(), meta.path("io.modelcontextprotocol/clientCapabilities"));
            assertEquals(i == 2 ? List.of(TRACEPARENT) : List.of(), headers.allValues("traceparent"));
            assertTrue(headers.allValues("MCP-Session-Id").isEmpty());
            assertTrue(headers.allValues("Cookie").isEmpty());
            assertTrue(headers.allValues("baggage").isEmpty());
            assertFalse(headers.map().toString().contains("private-agent"));
        }
        assertFalse(fixture.events.isEmpty());
        for (var event : fixture.events) {
            assertFalse(event.toString().contains("private-"));
            assertFalse(event.toString().contains("service-token"));
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"wrong-version", "missing-tools", "wrong-type", "legacy-field"})
    void failedDiscoveryPreventsListAndCall(final String failure) {
        Fixture fixture = new Fixture();
        fixture.mutate = envelope -> {
            ObjectNode result = (ObjectNode) envelope.get("result");
            switch (failure) {
                case "wrong-version" -> result.putArray("supportedVersions").add("2025-06-18");
                case "missing-tools" -> result.putObject("capabilities");
                case "wrong-type" -> result.putObject("capabilities").put("tools", true);
                default -> result.set("protocolVersions", result.remove("supportedVersions"));
            }
            return envelope;
        };
        StepVerifier.create(fixture.client.initialize()).expectError(IllegalArgumentException.class).verify(WAIT);
        StepVerifier.create(fixture.client.listTools(null)).expectError(IllegalStateException.class).verify(WAIT);
        StepVerifier.create(fixture.call("lookup")).expectError(IllegalStateException.class).verify(WAIT);
        assertEquals(List.of("server/discover"), fixture.methods());
    }

    @ParameterizedTest
    @ValueSource(strings = {"input_required", "extension", "absent", "null", "wrong-id", "malformed-meta", "rpc-error", "http-error", "disconnect"})
    void failedCallsHaveExactlyOneUpstreamSideEffect(final String failure) {
        Fixture fixture = new Fixture();
        fixture.client.initialize().block(WAIT);
        fixture.status = "http-error".equals(failure) ? 500 : 200;
        fixture.disconnect = "disconnect".equals(failure);
        fixture.mutate = envelope -> {
            ObjectNode result = (ObjectNode) envelope.get("result");
            switch (failure) {
                case "absent" -> result.remove("resultType");
                case "null" -> result.putNull("resultType");
                case "wrong-id" -> envelope.put("id", "other");
                case "malformed-meta" -> result.put("_meta", "invalid");
                case "rpc-error" -> {
                    envelope.remove("result");
                    envelope.putObject("error").put("code", -32601).put("message", "Method not found");
                }
                case "http-error", "disconnect" -> { }
                default -> result.put("resultType", failure);
            }
            return envelope;
        };
        StepVerifier.create(fixture.call("lookup")).expectError().verify(WAIT);
        assertEquals(1, fixture.sideEffects.get());
        assertEquals(List.of("server/discover", "tools/call"), fixture.methods());
        assertEquals(0, fixture.client.pending());
    }

    @Test
    void cancellationOnlyCancelsTheDispatchedFutureOwnedByThisCall() {
        Fixture fixture = new Fixture();
        fixture.client.initialize().block(WAIT);
        fixture.hold = true;
        final var first = fixture.call("first").toFuture();
        final var firstExchange = fixture.held;
        fixture.hold = false;
        assertEquals("complete", fixture.call("peer").block(WAIT).path("resultType").textValue());
        assertEquals(1, fixture.client.pending());
        assertTrue(first.cancel(true));
        assertTrue(firstExchange.isCancelled());
        assertEquals(0, fixture.client.pending());
        assertEquals(2, fixture.sideEffects.get());
        assertEquals(List.of("server/discover", "tools/call", "tools/call"), fixture.methods());
    }

    @Test
    void remainingDeadlineCancelsOnlyTheDispatchedFutureAndNeverReplays() {
        Fixture fixture = new Fixture();
        fixture.client.initialize().block(WAIT);
        fixture.hold = true;
        StepVerifier.withVirtualTime(() -> fixture.call("lookup").contextWrite(c -> c.put(McpTransportContext.KEY,
                        McpTransportContext.create(Map.of("deadline", Instant.now().plusSeconds(2))))))
                .expectSubscription()
                .then(() -> assertEquals(1, fixture.sideEffects.get()))
                .thenAwait(Duration.ofSeconds(3))
                .expectError(TimeoutException.class)
                .verify(WAIT);
        assertTrue(fixture.held.isCancelled());
        assertEquals(0, fixture.client.pending());
        assertEquals(1, fixture.sideEffects.get());
    }

    @Test
    void httpTransportCannotSubscribeTwiceToTheSameToolBody() {
        Fixture fixture = new Fixture();
        fixture.client.initialize().block(WAIT);
        fixture.call("increment").block(WAIT);
        var body = fixture.requests.get(1).bodyPublisher().orElseThrow();
        StepVerifier.create(JdkFlowAdapter.flowPublisherToFlux(body)).expectError(IOException.class).verify(WAIT);
        assertEquals(1, fixture.sideEffects.get());
    }

    @Test
    void rejectsResourcesDelegatedMetadataAndOversizedRequestsBeforeSending() {
        Fixture fixture = new Fixture();
        fixture.client.initialize().block(WAIT);
        StepVerifier.create(fixture.client.listResources(null)).expectError(UnsupportedOperationException.class).verify(WAIT);
        StepVerifier.create(fixture.client.listResourceTemplates(null)).expectError(UnsupportedOperationException.class).verify(WAIT);
        StepVerifier.create(fixture.client.readResource("file:///resource")).expectError(UnsupportedOperationException.class).verify(WAIT);
        var delegated = new McpSchema.CallToolRequest("lookup", Map.of(), Map.of("extra", "private-meta"));
        StepVerifier.create(fixture.client.callRaw(delegated)).expectError(IllegalArgumentException.class).verify(WAIT);
        var oversized = new McpSchema.CallToolRequest("lookup", Map.of("data", "x".repeat(8192)));
        StepVerifier.create(fixture.client.callRaw(oversized)).expectError(IllegalArgumentException.class).verify(WAIT);
        assertEquals(List.of("server/discover"), fixture.methods());
        fixture.mutate = envelope -> envelope.put("padding", "x".repeat(8192));
        StepVerifier.create(fixture.call("lookup")).expectError(IllegalArgumentException.class).verify(WAIT);
        assertEquals(1, fixture.sideEffects.get());
    }

    @Test
    void encodesReservedSentinelsAndWhitespaceWithoutChangingTheMirroredValue() {
        assertEquals("=?base64?PT9iYXNlNjQ/bGl0ZXJhbD89?=", RemoteMcpHttpSupport.encodeMirroredName("=?base64?literal?="));
        assertEquals("=?base64?IHBhZGRlZCA=?=", RemoteMcpHttpSupport.encodeMirroredName(" padded "));
        assertEquals("two words", RemoteMcpHttpSupport.encodeMirroredName("two words"));
    }

    @ParameterizedTest
    @ValueSource(strings = {"schema-constraint", "parameter-header", "wrong-name", "display-metadata"})
    void listsOnlyToolsWhoseInvocationContractCanBeRepresented(final String scenario) {
        Fixture fixture = new Fixture();
        fixture.client.initialize().block(WAIT);
        fixture.mutate = envelope -> {
            ObjectNode tool = (ObjectNode) envelope.at("/result/tools/0");
            ObjectNode schema = (ObjectNode) tool.get("inputSchema");
            switch (scenario) {
                case "schema-constraint" -> schema.putArray("allOf").addObject().putArray("required").add("key");
                case "parameter-header" -> schema.putObject("properties").putObject("group").putObject("properties")
                        .putObject("id").put("type", "string").put("x-mcp-header", "Id");
                case "wrong-name" -> tool.put("name", 1);
                default -> {
                    tool.putArray("icons").addObject().put("src", "https://example.org/icon");
                    schema.put("$schema", "https://json-schema.org/draft/2020-12/schema");
                }
            }
            return envelope;
        };
        if ("display-metadata".equals(scenario)) {
            assertEquals("lookup", fixture.client.listTools(null).block(WAIT).tools().get(0).name());
        } else {
            StepVerifier.create(fixture.client.listTools(null)).expectError(IllegalArgumentException.class).verify(WAIT);
        }
        assertEquals(List.of("server/discover", "tools/list"), fixture.methods());
    }

    @ParameterizedTest
    @ValueSource(strings = {"event: unrelated\ndata: %s\n\n", "data: %s\n", "data: %s\n\ndata: torn", "data\n\ndata: %s\n\n"})
    void rejectsUnsupportedOrIncompleteSseWithoutReplay(final String stream) {
        Fixture fixture = new Fixture();
        fixture.client.initialize().block(WAIT);
        fixture.sse = true;
        fixture.event = stream::formatted;
        StepVerifier.create(fixture.call("lookup")).expectError(IllegalArgumentException.class).verify(WAIT);
        assertEquals(1, fixture.sideEffects.get());
    }

    @Test
    void acceptsDefaultSseMessageWithCommentsAndNoEventType() {
        Fixture fixture = new Fixture();
        fixture.sse = true;
        fixture.event = message -> ": keepalive\r\rdata: " + message + "\r\r: done\r";
        assertEquals(VERSION, fixture.client.initialize().block(WAIT).protocolVersion());
        assertEquals("complete", fixture.call("lookup").block(WAIT).path("resultType").textValue());
    }

    @Nested
    class Network {

        @Test
        void verifiesTlsAndDoesNotReplayWhenUpstreamClosesAfterPerformingTool(final @TempDir Path directory) throws Exception {
            SSLContext tls = loopbackCertificate(directory);
            HttpsServer server = HttpsServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
            final AtomicInteger discoveries = new AtomicInteger();
            final AtomicInteger sideEffects = new AtomicInteger();
            server.setHttpsConfigurator(new HttpsConfigurator(tls));
            server.createContext("/mcp", exchange -> {
                try {
                    JsonNode message = JSON.readTree(exchange.getRequestBody());
                    if ("tools/call".equals(message.path("method").textValue())) {
                        sideEffects.incrementAndGet();
                        // The tool has run. Close the connection before even writing response headers.
                        return;
                    }
                    if (!"server/discover".equals(message.path("method").textValue())) {
                        exchange.sendResponseHeaders(400, -1);
                        return;
                    }
                    discoveries.incrementAndGet();
                    ObjectNode response = JSON.createObjectNode().put("jsonrpc", "2.0");
                    response.set("id", message.get("id"));
                    ObjectNode result = response.putObject("result").put("resultType", "complete");
                    result.putArray("supportedVersions").add(VERSION);
                    result.putObject("capabilities").putObject("tools");
                    byte[] bytes = JSON.writeValueAsBytes(response);
                    exchange.getResponseHeaders().set("Content-Type", "application/json");
                    exchange.sendResponseHeaders(200, bytes.length);
                    exchange.getResponseBody().write(bytes);
                } finally {
                    exchange.close();
                }
            });
            server.start();
            URI endpoint = URI.create("https://127.0.0.1:" + server.getAddress().getPort() + "/mcp");
            RemoteServerBinding.Config config = new RemoteServerBinding.Config("tls", endpoint, "fixture", "v1");
            RemoteServerBinding binding = RemoteServerBinding.resolve(config, ProtocolProfile.NATIVE_2026_07_28, Set.of(endpoint),
                    target -> new RemoteServerBinding.Credential(target, "fixture-token"));
            StatelessMcpClient trusted = new StatelessMcpClient(binding, ignored -> { }, RemoteMcpHttpSupport.newHttpClient(tls));
            RemoteMcpEndpoint untrusted = binding.newClient();
            try {
                StepVerifier.create(untrusted.initialize()).expectError(SSLHandshakeException.class).verify(WAIT);
                assertEquals(0, discoveries.get());
                assertEquals(VERSION, trusted.initialize().block(WAIT).protocolVersion());
                StepVerifier.create(trusted.callRaw(new McpSchema.CallToolRequest("increment", Map.of())))
                        .expectErrorMatches(IOException.class::isInstance).verify(WAIT);
            } finally {
                untrusted.closeGracefully().block(WAIT);
                trusted.closeGracefully().block(WAIT);
                server.stop(0);
            }
            assertEquals(1, discoveries.get());
            assertEquals(1, sideEffects.get());
        }

        private SSLContext loopbackCertificate(final Path directory) throws Exception {
            Path file = directory.resolve("loopback.p12");
            String keytool = Path.of(System.getProperty("java.home"), "bin", System.getProperty("os.name").startsWith("Windows") ? "keytool.exe" : "keytool").toString();
            Process process = new ProcessBuilder(keytool, "-genkeypair", "-alias", "fixture", "-keyalg", "RSA", "-keysize", "2048", "-validity", "2",
                    "-dname", "CN=fixture", "-ext", "SAN=IP:127.0.0.1", "-storetype", "PKCS12", "-keystore", file.toString(), "-storepass", "changeit", "-noprompt")
                    .redirectErrorStream(true).redirectOutput(directory.resolve("keytool.log").toFile()).start();
            try {
                assertTrue(process.waitFor(45, TimeUnit.SECONDS));
                assertEquals(0, process.exitValue());
            } finally {
                process.destroyForcibly();
            }
            KeyStore store = KeyStore.getInstance("PKCS12");
            char[] password = "changeit".toCharArray();
            try (var input = Files.newInputStream(file)) {
                store.load(input, password);
            }
            KeyManagerFactory keys = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm());
            keys.init(store, password);
            TrustManagerFactory trust = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm());
            trust.init(store);
            SSLContext tls = SSLContext.getInstance("TLS");
            tls.init(keys.getKeyManagers(), trust.getTrustManagers(), null);
            return tls;
        }
    }

    private static final class Fixture {

        private final List<HttpRequest> requests = new ArrayList<>();

        private final List<ObjectNode> bodies = new ArrayList<>();

        private final List<Map<String, Object>> events = new ArrayList<>();

        private final AtomicInteger sideEffects = new AtomicInteger();

        private final StatelessMcpClient client;

        private UnaryOperator<ObjectNode> mutate = UnaryOperator.identity();

        private CompletableFuture<HttpResponse<byte[]>> held;

        private boolean hold;

        private boolean sse;

        private UnaryOperator<String> event = message -> "event: message\r\ndata: " + message + "\r\n\r\n";

        private boolean disconnect;

        private int status = 200;

        private Fixture() {
            HttpClient http = mock();
            when(http.sendAsync(any(), ArgumentMatchers.<HttpResponse.BodyHandler<byte[]>>any()))
                    .thenAnswer(invocation -> receive(invocation.getArgument(0), invocation.getArgument(1)));
            URI endpoint = URI.create("https://192.0.2.10:443/mcp");
            RemoteServerBinding.Config config = new RemoteServerBinding.Config("orders", endpoint, "service/orders", "v1");
            RemoteServerBinding binding = RemoteServerBinding.resolve(config, ProtocolProfile.NATIVE_2026_07_28, Set.of(endpoint),
                    target -> new RemoteServerBinding.Credential(target, "service-token"));
            client = new StatelessMcpClient(binding, events::add, http);
        }

        private Mono<ObjectNode> call(final String name) {
            return client.callRaw(new McpSchema.CallToolRequest(name, Map.of("key", "private-argument")));
        }

        private List<String> methods() {
            return bodies.stream().map(body -> body.path("method").textValue()).toList();
        }

        private CompletableFuture<HttpResponse<byte[]>> receive(final HttpRequest request, final HttpResponse.BodyHandler<byte[]> handler) throws IOException {
            var reader = HttpResponse.BodySubscribers.ofString(StandardCharsets.UTF_8);
            JdkFlowAdapter.flowPublisherToFlux(request.bodyPublisher().orElseThrow()).map(List::of)
                    .as(JdkFlowAdapter::publisherToFlowPublisher).subscribe(reader);
            ObjectNode body = (ObjectNode) JSON.readTree(reader.getBody().toCompletableFuture().join());
            requests.add(request);
            bodies.add(body);
            ObjectNode result = JSON.createObjectNode().put("resultType", "complete");
            switch (body.path("method").textValue()) {
                case "server/discover" -> {
                    result.putArray("supportedVersions").add(VERSION);
                    ObjectNode capabilities = result.putObject("capabilities");
                    capabilities.putObject("tools");
                    capabilities.putObject("resources");
                }
                case "tools/list" -> result.putArray("tools").addObject().put("name", "lookup").put("description", "Lookup")
                        .putObject("inputSchema").put("type", "object");
                case "tools/call" -> {
                    sideEffects.incrementAndGet();
                    if (disconnect) {
                        return CompletableFuture.failedFuture(new IOException("Upstream disconnected after dispatch"));
                    }
                    if (hold) {
                        held = new CompletableFuture<>();
                        return held;
                    }
                    result.putArray("content").addObject().put("type", "text").put("text", "private-output");
                    result.putObject("_meta").put("example.org/value", "opaque");
                    result.put("extension", "preserved");
                }
                default -> throw new AssertionError("Unexpected MCP method");
            }
            ObjectNode envelope = JSON.createObjectNode().put("jsonrpc", "2.0");
            envelope.set("id", body.get("id"));
            envelope.set("result", result);
            String message = mutate.apply(envelope).toString();
            byte[] bytes = (sse ? event.apply(message) : message).getBytes(StandardCharsets.UTF_8);
            HttpResponse.BodySubscriber<byte[]> subscriber = handler.apply(mock());
            Flux.just(List.of(ByteBuffer.wrap(bytes))).as(JdkFlowAdapter::publisherToFlowPublisher).subscribe(subscriber);
            return subscriber.getBody().thenApply(payload -> {
                HttpResponse<byte[]> response = mock();
                when(response.statusCode()).thenReturn(status);
                when(response.headers()).thenReturn(HttpHeaders.of(Map.of("Content-Type", List.of(sse ? "text/event-stream" : "application/json")), (k, v) -> true));
                when(response.body()).thenReturn(payload);
                return response;
            }).toCompletableFuture();
        }
    }
}
