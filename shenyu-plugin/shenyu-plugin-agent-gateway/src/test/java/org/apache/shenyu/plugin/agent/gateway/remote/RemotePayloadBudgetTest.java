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

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;
import io.modelcontextprotocol.spec.McpSchema;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.zip.GZIPOutputStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Real loopback HTTP fixtures for trusted remote MCP payload budgets.
 * Wire bytes are counted on the serialized UTF-8 body of a single JSON-RPC message.
 */
class RemotePayloadBudgetTest {

    private static final Duration WAIT = Duration.ofSeconds(10);

    private static final ObjectMapper JSON = new ObjectMapper();

    @Test
    void legacyBindingKeepsTheFixedEightKiBResponseLimit() throws Exception {
        try (Fixture fixture = new Fixture()) {
            RequestScopedMcpClient client = client(fixture, 8192, 8192);
            try {
                client.initialize().block(WAIT);
                IllegalArgumentException error = assertThrows(IllegalArgumentException.class,
                        () -> client.callRaw(call(9000)).block(WAIT));
                assertEquals("Response byte limit", message(error));
            } finally {
                client.closeGracefully().block(WAIT);
            }
        }
    }

    @Test
    void explicitBudgetAllowsTwentyKiBToolAndResourceResults() throws Exception {
        try (Fixture fixture = new Fixture()) {
            RequestScopedMcpClient client = client(fixture, 16384, 32768);
            try {
                client.initialize().block(WAIT);
                ObjectNode tool = client.callRaw(call(20480)).block(WAIT);
                assertEquals(20480, fixture.lastResponseBytes);
                assertTrue(tool.path("content").get(0).path("text").textValue().length() > 20000);
                ObjectNode resource = client.readResource("file:///20480").block(WAIT);
                assertEquals(20480, fixture.lastResponseBytes);
                assertEquals(1, resource.path("contents").size());
            } finally {
                client.closeGracefully().block(WAIT);
            }
        }
    }

    @ParameterizedTest
    @ValueSource(ints = {16383, 16384})
    void acceptsResponsesAtAndJustBelowTheConfiguredBound(final int bytes) throws Exception {
        try (Fixture fixture = new Fixture()) {
            RequestScopedMcpClient client = client(fixture, 8192, 16384);
            try {
                client.initialize().block(WAIT);
                assertTrue(Objects.nonNull(client.callRaw(call(bytes)).block(WAIT)));
                assertEquals(bytes, fixture.lastResponseBytes);
            } finally {
                client.closeGracefully().block(WAIT);
            }
        }
    }

    @Test
    void rejectsResponseOneByteAboveTheConfiguredBound() throws Exception {
        try (Fixture fixture = new Fixture()) {
            RequestScopedMcpClient client = client(fixture, 8192, 16384);
            try {
                client.initialize().block(WAIT);
                IllegalArgumentException error = assertThrows(IllegalArgumentException.class,
                        () -> client.callRaw(call(16385)).block(WAIT));
                assertEquals("Response byte limit", message(error));
            } finally {
                client.closeGracefully().block(WAIT);
            }
        }
    }

    @Test
    void requestBudgetAcceptsAtTheBoundAndRejectsOneByteAbove() throws Exception {
        try (Fixture fixture = new Fixture()) {
            int observed;
            RequestScopedMcpClient probe = client(fixture, 16384, 16384);
            try {
                probe.initialize().block(WAIT);
                probe.callRaw(call(1024, 1200)).block(WAIT);
                observed = fixture.lastRequestBytes;
            } finally {
                probe.closeGracefully().block(WAIT);
            }
            assertTrue(observed >= 1024);
            RequestScopedMcpClient atLimit = client(fixture, observed, 16384);
            try {
                atLimit.initialize().block(WAIT);
                atLimit.callRaw(call(1024, 1200)).block(WAIT);
                assertEquals(observed, fixture.lastRequestBytes);
            } finally {
                atLimit.closeGracefully().block(WAIT);
            }
            RequestScopedMcpClient above = client(fixture, observed, 16384);
            try {
                above.initialize().block(WAIT);
                IllegalArgumentException error = assertThrows(IllegalArgumentException.class,
                        () -> above.callRaw(call(1024, 1201)).block(WAIT));
                assertEquals("Request byte limit", message(error));
            } finally {
                above.closeGracefully().block(WAIT);
            }
        }
    }

    @Test
    void oversizedChunkedSseStopsReceivingBeforeTheWholeBody() throws Exception {
        try (Fixture fixture = new Fixture()) {
            fixture.streaming = true;
            RequestScopedMcpClient client = client(fixture, 8192, 4096);
            try {
                client.initialize().block(WAIT);
                IllegalArgumentException error = assertThrows(IllegalArgumentException.class,
                        () -> client.callRaw(call(0)).block(WAIT));
                assertEquals("Response byte limit", message(error));
                // The upstream body was cancelled: the fixture never finished its planned stream.
                assertTrue(fixture.streamingStop.await(5, java.util.concurrent.TimeUnit.SECONDS));
                assertTrue(fixture.streamedBytes.get() < 65536);
            } finally {
                client.closeGracefully().block(WAIT);
            }
        }
    }

    @Test
    void compressedResponseIsRejectedWithoutInflatingIt() throws Exception {
        try (Fixture fixture = new Fixture()) {
            fixture.gzip = true;
            RequestScopedMcpClient client = client(fixture, 8192, 8192);
            try {
                client.initialize().block(WAIT);
                IllegalArgumentException error = assertThrows(IllegalArgumentException.class,
                        () -> client.callRaw(call(1024)).block(WAIT));
                assertEquals("Unsupported upstream content encoding", message(error));
            } finally {
                client.closeGracefully().block(WAIT);
            }
        }
    }

    @Test
    void declaredLengthAboveTheBudgetIsRejectedBeforeTheBody() throws Exception {
        try (Fixture fixture = new Fixture()) {
            fixture.declaredLength = 200000;
            RequestScopedMcpClient client = client(fixture, 8192, 8192);
            try {
                client.initialize().block(WAIT);
                IllegalArgumentException error = assertThrows(IllegalArgumentException.class,
                        () -> client.callRaw(call(1024)).block(WAIT));
                assertEquals("Response byte limit", message(error));
            } finally {
                client.closeGracefully().block(WAIT);
            }
        }
    }

    @Test
    void concurrentLegacyAndLargeBudgetBindingsStayIsolated() throws Exception {
        try (Fixture fixture = new Fixture()) {
            RequestScopedMcpClient legacy = client(fixture, 8192, 8192);
            RequestScopedMcpClient large = client(fixture, 16384, 32768);
            try {
                legacy.initialize().block(WAIT);
                large.initialize().block(WAIT);
                AtomicInteger accepted = new AtomicInteger();
                AtomicInteger rejected = new AtomicInteger();
                CopyOnWriteArrayList<String> failures = new CopyOnWriteArrayList<>();
                Flux.range(0, 4).flatMap(index -> Mono.when(
                        large.callRaw(call(20480)).doOnNext(ignored -> accepted.incrementAndGet()).onErrorResume(error -> Mono.empty()),
                        legacy.callRaw(call(20480))
                            .doOnError(error -> {
                                failures.add(String.valueOf(message(error)));
                                rejected.incrementAndGet();
                            })
                            .onErrorResume(error -> Mono.empty())
                )).blockLast(WAIT);
                assertEquals(4, accepted.get());
                assertEquals(4, rejected.get());
                failures.forEach(value -> assertEquals("Response byte limit", value));
            } finally {
                legacy.closeGracefully().block(WAIT);
                large.closeGracefully().block(WAIT);
            }
        }
    }

    @Test
    void rejectsBudgetsOutsideTheSupportedRange() {
        URI uri = URI.create("http://127.0.0.1:8080/mcp");
        assertThrows(IllegalArgumentException.class, () -> new RemoteServerBinding.Config("orders", uri, "service/orders", "v1", 512, 8192));
        assertThrows(IllegalArgumentException.class, () -> new RemoteServerBinding.Config("orders", uri, "service/orders", "v1", 8192, 1048577));
        assertThrows(IllegalArgumentException.class, () -> new RemoteServerBinding.Config("orders", uri, "service/orders", "v1", 65537, 8192));
        assertEquals(8192, new RemoteServerBinding.Config("orders", uri, "service/orders", "v1").responseBytes());
    }

    @Test
    void legacyResolverWithoutBudgetMirroringKeepsNonDefaultBudgetsResolvable() {
        URI uri = URI.create("http://127.0.0.1:8080/mcp");
        RemoteServerBinding.Config trusted = new RemoteServerBinding.Config("orders", uri, "service/orders", "v1", 16384, 32768);
        // A third-party resolver written before payload budgets existed rebuilds the credential target
        // from the reference fields only, so its Config keeps the legacy 8 KiB defaults. Budgets must
        // never take part in credential binding identity, or every non-default budget would be rejected
        // as a mismatched binding and silently drop the data-sync update.
        RemoteServiceCredentialResolver installed = target -> new RemoteServerBinding.Credential(
                new RemoteServerBinding.Config(target.name(), target.endpoint(), target.credentialRef(), target.credentialVersion()),
                "legacy-service-token");
        RemoteServerBinding binding = RemoteServerBinding.resolve(trusted, Set.of(uri), installed::resolve);
        assertEquals(16384, binding.config().requestBytes());
        assertEquals(32768, binding.config().responseBytes());
        assertEquals(trusted, binding.config());
    }

    @Test
    void credentialBindingIdentityIgnoresBudgetsButRejectsOtherTargets() {
        URI uri = URI.create("http://127.0.0.1:8080/mcp");
        RemoteServerBinding.Config trusted = new RemoteServerBinding.Config("orders", uri, "service/orders", "v1", 16384, 32768);
        RemoteServerBinding.Config legacyBudgets = new RemoteServerBinding.Config("orders", uri, "service/orders", "v1", 8192, 8192);
        assertTrue(trusted.bindsTo(legacyBudgets));
        assertNotEquals(trusted, legacyBudgets);
        assertEquals(trusted, new RemoteServerBinding.Config("orders", uri, "service/orders", "v1", 16384, 32768));
        assertEquals(trusted.hashCode(), new RemoteServerBinding.Config("orders", uri, "service/orders", "v1", 16384, 32768).hashCode());
        // The budget-free comparison must not weaken the credential check it replaces.
        assertThrows(SecurityException.class, () -> RemoteServerBinding.resolve(trusted, Set.of(uri),
                target -> new RemoteServerBinding.Credential(new RemoteServerBinding.Config("orders", uri, "service/other", "v1"), "service-token")));
        assertThrows(SecurityException.class, () -> RemoteServerBinding.resolve(trusted, Set.of(uri),
                target -> new RemoteServerBinding.Credential(new RemoteServerBinding.Config("orders", uri, "service/orders", "v2"), "service-token")));
        assertThrows(SecurityException.class, () -> RemoteServerBinding.resolve(trusted, Set.of(uri),
                target -> new RemoteServerBinding.Credential(new RemoteServerBinding.Config("orders", URI.create("http://127.0.0.1:8080/other"),
                        "service/orders", "v1"), "service-token")));
        assertThrows(SecurityException.class, () -> RemoteServerBinding.resolve(trusted, Set.of(uri), target -> null));
    }

    private static McpSchema.CallToolRequest call(final int bytes) {
        return call(bytes, 0);
    }

    private static McpSchema.CallToolRequest call(final int bytes, final int requestPad) {
        return new McpSchema.CallToolRequest("lookup", Map.of("bytes", bytes, "pad", "p".repeat(requestPad)));
    }

    private static RequestScopedMcpClient client(final Fixture fixture, final int requestBytes, final int responseBytes) {
        URI uri = fixture.endpoint();
        RemoteServerBinding.Config target = new RemoteServerBinding.Config("orders", uri, "service/orders", "v1", requestBytes, responseBytes);
        return new RequestScopedMcpClient(RemoteServerBinding.resolve(target, Set.of(uri),
                ignored -> new RemoteServerBinding.Credential(target, "fixed-test-token")), ignored -> { });
    }

    private static String message(final Throwable error) {
        Throwable current = error;
        while (Objects.nonNull(current.getCause()) && !Objects.equals(current.getCause(), current)) {
            current = current.getCause();
        }
        return current.getMessage();
    }

    private static final class Fixture implements AutoCloseable {

        private final HttpServer server;

        private final ExecutorService executor = Executors.newFixedThreadPool(8);

        private final CountDownLatch streamingStop = new CountDownLatch(1);

        private final AtomicInteger streamedBytes = new AtomicInteger();

        private volatile boolean streaming;

        private volatile boolean gzip;

        private volatile int declaredLength = -1;

        private volatile int lastRequestBytes;

        private volatile int lastResponseBytes;

        private Fixture() throws Exception {
            server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
            server.setExecutor(executor);
            ((ThreadPoolExecutor) executor).prestartAllCoreThreads();
            server.createContext("/mcp", this::handle);
            server.start();
        }

        private URI endpoint() {
            return URI.create("http://127.0.0.1:" + server.getAddress().getPort() + "/mcp");
        }

        private void handle(final HttpExchange exchange) {
            try (exchange) {
                if ("DELETE".equals(exchange.getRequestMethod())) {
                    exchange.sendResponseHeaders(204, -1);
                    return;
                }
                byte[] body = exchange.getRequestBody().readAllBytes();
                lastRequestBytes = body.length;
                ObjectNode request = (ObjectNode) JSON.readTree(body);
                String method = request.path("method").textValue();
                if ("notifications/initialized".equals(method)) {
                    exchange.sendResponseHeaders(202, -1);
                    return;
                }
                if ("initialize".equals(method)) {
                    exchange.getResponseHeaders().set("MCP-Session-Id", "budget-session");
                    ObjectNode result = JSON.createObjectNode().put("protocolVersion", "2025-06-18");
                    result.putObject("capabilities").putObject("tools");
                    result.putObject("serverInfo").put("name", "budget").put("version", "1");
                    write(exchange, envelopeOf(request, result), false);
                    return;
                }
                if (streaming) {
                    stream(exchange, request);
                    return;
                }
                boolean resource = "resources/read".equals(method);
                int target = resource
                    ? Integer.parseInt(request.path("params").path("uri").asText("file:///0").substring("file:///".length()))
                    : request.path("params").path("arguments").path("bytes").asInt();
                write(exchange, envelope(request, resource, target), true);
            } catch (Exception expected) {
                // Cancelled bodies and fixture interruption are deliberately exercised.
            }
        }

        private byte[] envelopeOf(final ObjectNode request, final ObjectNode result) {
            ObjectNode envelope = JSON.createObjectNode().put("jsonrpc", "2.0");
            envelope.set("id", request.get("id"));
            envelope.set("result", result);
            return envelope.toString().getBytes(StandardCharsets.UTF_8);
        }

        private byte[] envelope(final ObjectNode request, final boolean resource, final int targetBytes) {
            int overhead = envelopeValue(request, resource, 0).length;
            int filler = targetBytes - overhead;
            if (filler < 0) {
                throw new IllegalArgumentException("Fixture target below envelope overhead");
            }
            byte[] bytes = envelopeValue(request, resource, filler);
            if (bytes.length != targetBytes) {
                throw new IllegalArgumentException("Fixture produced " + bytes.length + " bytes, expected " + targetBytes);
            }
            return bytes;
        }

        private byte[] envelopeValue(final ObjectNode request, final boolean resource, final int filler) {
            ObjectNode result = JSON.createObjectNode();
            String text = "x".repeat(Math.max(0, filler));
            if (resource) {
                result.putArray("contents").addObject().put("uri", "file:///report").put("mimeType", "text/plain").put("text", text);
            } else {
                result.putArray("content").addObject().put("type", "text").put("text", text);
            }
            ObjectNode envelope = JSON.createObjectNode().put("jsonrpc", "2.0");
            envelope.set("id", request.get("id"));
            envelope.set("result", result);
            return envelope.toString().getBytes(StandardCharsets.UTF_8);
        }

        private void write(final HttpExchange exchange, final byte[] bytes, final boolean applyFault) throws Exception {
            lastResponseBytes = bytes.length;
            if (gzip && applyFault) {
                ByteArrayOutputStream buffer = new ByteArrayOutputStream();
                try (GZIPOutputStream stream = new GZIPOutputStream(buffer)) {
                    stream.write(bytes);
                }
                exchange.getResponseHeaders().set("Content-Encoding", "gzip");
                exchange.getResponseHeaders().set("Content-Type", "application/json");
                exchange.sendResponseHeaders(200, buffer.size());
                exchange.getResponseBody().write(buffer.toByteArray());
                return;
            }
            if (declaredLength > 0 && applyFault) {
                exchange.getResponseHeaders().set("Content-Type", "application/json");
                exchange.sendResponseHeaders(200, declaredLength);
                exchange.getResponseBody().write(bytes);
                return;
            }
            exchange.getResponseHeaders().set("Content-Type", "application/json");
            exchange.sendResponseHeaders(200, bytes.length);
            exchange.getResponseBody().write(bytes);
        }

        private void stream(final HttpExchange exchange, final ObjectNode request) throws Exception {
            exchange.getResponseHeaders().set("Content-Type", "text/event-stream");
            exchange.sendResponseHeaders(200, 0);
            OutputStream output = exchange.getResponseBody();
            String id = request.get("id").asText();
            try {
                for (int index = 0; index < 64; index++) {
                    String payload = "event: message\ndata: {\"jsonrpc\":\"2.0\",\"id\":\"" + id + "\",\"result\":{\"chunk\":\""
                            + "y".repeat(1000 - index) + "\"}}\n\n";
                    byte[] part = payload.getBytes(StandardCharsets.UTF_8);
                    output.write(part);
                    output.flush();
                    streamedBytes.addAndGet(part.length);
                    Thread.sleep(2);
                }
            } catch (Exception cancelled) {
                // Expected: the client cancelled the body at its configured bound.
            } finally {
                streamingStop.countDown();
            }
        }

        @Override
        public void close() {
            server.stop(0);
            executor.shutdownNow();
        }
    }
}
