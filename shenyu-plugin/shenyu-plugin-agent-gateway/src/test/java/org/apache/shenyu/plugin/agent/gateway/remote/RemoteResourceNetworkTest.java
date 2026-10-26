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
import org.apache.shenyu.common.dto.AgentGatewayResultFilterConfig;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpDispatcher;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpExecutionContext;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpRequestParser;
import org.apache.shenyu.plugin.agent.gateway.resource.AgentResourceNamespace;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolRegistry;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.http.HttpHeaders;
import reactor.core.publisher.Mono;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class RemoteResourceNetworkTest {

    private static final Duration WAIT = Duration.ofSeconds(5);

    private final ObjectMapper mapper = new ObjectMapper();

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void resourcesOnlyServersSupportPagedCatalogAndCorrectRouting(final boolean sse) throws Exception {
        try (Fixture orders = new Fixture("orders", sse); Fixture inventory = new Fixture("inventory", sse);
                ManagedRemoteMcpCatalog catalog = catalog(orders, inventory, true)) {
            catalog.reload(config(orders, inventory)).block(WAIT);
            AgentMcpDispatcher dispatcher = new AgentMcpDispatcher(new AgentToolRegistry(List.of()), "gateway", "1", catalog);
            Set<String> grants = Set.of("orders.status", "orders.audit", "orders.byId", "inventory.status");
            ObjectNode listed = dispatch(dispatcher, "resources/list", "", grants);
            assertEquals(3, listed.path("result").path("resources").size());
            assertEquals(1, dispatch(dispatcher, "resources/templates/list", "", grants).path("result").path("resourceTemplates").size());
            for (String name : List.of("orders", "inventory")) {
                String uri = AgentResourceNamespace.expose(name, "test://status", false);
                ObjectNode read = dispatch(dispatcher, "resources/read", uri, grants);
                assertEquals(uri, read.path("result").path("contents").get(0).path("uri").asText());
                assertEquals(name + ":test://status", read.path("result").path("contents").get(0).path("text").asText());
            }
            String templateUri = AgentResourceNamespace.expose("orders", "test://status/42", false);
            assertEquals("orders:test://status/42", dispatch(dispatcher, "resources/read", templateUri, grants)
                    .path("result").path("contents").get(0).path("text").asText());
            for (Fixture fixture : List.of(orders, inventory)) {
                assertFalse(fixture.requests.stream().anyMatch(wire -> "tools/list".equals(wire.method())));
                assertTrue(fixture.requests.stream().allMatch(wire -> "Bearer resources-test-service".equals(wire.authorization())
                        && wire.subject().isEmpty() && wire.cookie().isEmpty()));
            }
        }
    }

    @Test
    void remoteResourcesStayOffForExistingToolsOnlyDeployments() throws Exception {
        try (Fixture first = new Fixture("orders", false); Fixture second = new Fixture("inventory", false);
                ManagedRemoteMcpCatalog catalog = catalog(first, second, false)) {
            catalog.reload(config(first, second)).block(WAIT);
            catalog.withSnapshot(snapshot -> {
                assertTrue(snapshot.resourceDefinitions().isEmpty());
                return Mono.just(mapper.createObjectNode());
            }).block(WAIT);
            assertFalse(first.requests.stream().anyMatch(wire -> wire.method().startsWith("resources/")));
            assertFalse(second.requests.stream().anyMatch(wire -> wire.method().startsWith("resources/")));
        }
    }

    @Test
    void cancellingAnActualHttpReadDoesNotCancelItsPeer() throws Exception {
        try (Fixture fixture = new Fixture("orders", false)) {
            RemoteServerBinding.Config target = new RemoteServerBinding.Config("orders", fixture.endpoint(), "service", "v1");
            RequestScopedMcpClient client = RemoteServerBinding.resolve(target, Set.of(fixture.endpoint()),
                    reference -> new RemoteServerBinding.Credential(reference, "resources-test-service")).newClient();
            try {
                client.initialize().block(WAIT);
                fixture.holdNext.set(true);
                var first = client.readResource("test://status").toFuture();
                assertTrue(fixture.started.await(5, TimeUnit.SECONDS));
                assertEquals("orders:test://status", client.readResource("test://status").block(WAIT).path("contents").get(0).path("text").asText());
                assertTrue(first.cancel(true));
                assertEquals(0, client.pending());
                fixture.release.countDown();
                assertEquals("orders:test://status/next", client.readResource("test://status/next").block(WAIT).path("contents").get(0).path("text").asText());
            } finally {
                fixture.release.countDown();
                client.closeGracefully().block(WAIT);
            }
        }
    }

    private ManagedRemoteMcpCatalog catalog(final Fixture first, final Fixture second, final boolean resources) {
        return new ManagedRemoteMcpCatalog(Set.of(first.endpoint(), second.endpoint()), Set.of(),
                target -> new RemoteServerBinding.Credential(target, "resources-test-service"), resources);
    }

    private String config(final Fixture first, final Fixture second) {
        return mapper.valueToTree(Map.of("aggregation", Map.of("revision", "resources-v1", "servers", List.of(
                Map.of("name", first.name, "endpoint", first.endpoint().toString(), "credentialRef", "service", "credentialVersion", "v1"),
                Map.of("name", second.name, "endpoint", second.endpoint().toString(), "credentialRef", "service", "credentialVersion", "v1"))))).toString();
    }

    private ObjectNode dispatch(final AgentMcpDispatcher dispatcher, final String method, final String uri, final Set<String> grants) {
        ObjectNode request = mapper.createObjectNode().put("jsonrpc", "2.0").put("id", "same-id").put("method", method);
        ObjectNode params = request.putObject("params").put("uri", uri);
        params.putObject("_meta").put("io.modelcontextprotocol/protocolVersion", AgentMcpRequestParser.VERSION)
                .putObject("io.modelcontextprotocol/clientCapabilities");
        HttpHeaders headers = new HttpHeaders();
        headers.set("MCP-Protocol-Version", AgentMcpRequestParser.VERSION);
        headers.set("Mcp-Method", method);
        if ("resources/read".equals(method)) {
            headers.set("Mcp-Name", uri);
        }
        return dispatcher.dispatch(new AgentMcpRequestParser().parse(request.toString().getBytes(StandardCharsets.UTF_8), headers, 262144),
                () -> new AgentMcpExecutionContext("trusted-request", "private-principal", "rule", 1, Set.of(), Set.of(),
                        Instant.now().plusSeconds(4), AgentGatewayResultFilterConfig.empty(), grants, grants)).block(WAIT);
    }

    private record Wire(String method, String authorization, String subject, String cookie) {
    }

    private static final class Fixture implements AutoCloseable {

        private final ObjectMapper mapper = new ObjectMapper();

        private final String name;

        private final boolean sse;

        private final HttpServer server;

        private final ExecutorService executor = Executors.newCachedThreadPool();

        private final List<Wire> requests = new CopyOnWriteArrayList<>();

        private final AtomicBoolean holdNext = new AtomicBoolean();

        private final CountDownLatch started = new CountDownLatch(1);

        private final CountDownLatch release = new CountDownLatch(1);

        private Fixture(final String name, final boolean sse) throws IOException {
            this.name = name;
            this.sse = sse;
            server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
            server.createContext("/mcp", this::handle);
            server.setExecutor(executor);
            server.start();
        }

        private URI endpoint() {
            return URI.create("http://127.0.0.1:" + server.getAddress().getPort() + "/mcp");
        }

        private void handle(final HttpExchange exchange) throws IOException {
            try (exchange) {
                if ("DELETE".equals(exchange.getRequestMethod())) {
                    exchange.sendResponseHeaders(200, -1);
                    return;
                }
                ObjectNode request = (ObjectNode) mapper.readTree(exchange.getRequestBody());
                String method = request.path("method").asText();
                requests.add(new Wire(method, exchange.getRequestHeaders().getFirst("Authorization"),
                        Objects.isNull(exchange.getRequestHeaders().getFirst("X-Spike-Subject")) ? "" : exchange.getRequestHeaders().getFirst("X-Spike-Subject"),
                        Objects.isNull(exchange.getRequestHeaders().getFirst("Cookie")) ? "" : exchange.getRequestHeaders().getFirst("Cookie")));
                if ("notifications/initialized".equals(method)) {
                    exchange.sendResponseHeaders(202, -1);
                    return;
                }
                ObjectNode result = result(request, method);
                ObjectNode response = mapper.createObjectNode().put("jsonrpc", "2.0");
                response.set("id", request.get("id"));
                response.set("result", result);
                byte[] bytes = (sse ? "event: message\ndata: " + response + "\n\n" : response.toString()).getBytes(StandardCharsets.UTF_8);
                exchange.getResponseHeaders().set("Content-Type", sse ? "text/event-stream" : "application/json");
                exchange.getResponseHeaders().set("MCP-Session-Id", name + "-session");
                exchange.sendResponseHeaders(200, bytes.length);
                exchange.getResponseBody().write(bytes);
            }
        }

        private ObjectNode result(final ObjectNode request, final String method) {
            ObjectNode result = mapper.createObjectNode();
            if ("initialize".equals(method)) {
                result.put("protocolVersion", "2025-06-18");
                result.putObject("capabilities").putObject("resources");
                result.putObject("serverInfo").put("name", name).put("version", "1");
            } else if ("resources/list".equals(method)) {
                boolean next = request.path("params").has("cursor");
                result.putArray("resources").addObject().put("name", next ? "audit" : "status").put("uri", next ? "test://audit" : "test://status");
                if (!next) {
                    result.put("nextCursor", "second");
                }
            } else if ("resources/templates/list".equals(method)) {
                result.putArray("resourceTemplates").addObject().put("name", "byId").put("uriTemplate", "test://status/{id}");
            } else if ("resources/read".equals(method)) {
                if (holdNext.compareAndSet(true, false)) {
                    started.countDown();
                    try {
                        if (!release.await(5, TimeUnit.SECONDS)) {
                            throw new IllegalStateException("Controlled read was not released");
                        }
                    } catch (InterruptedException error) {
                        Thread.currentThread().interrupt();
                        throw new IllegalStateException(error);
                    }
                }
                String uri = request.path("params").path("uri").asText();
                result.putArray("contents").addObject().put("uri", uri).put("text", name + ":" + uri);
            } else {
                throw new IllegalStateException("Unexpected protocol method");
            }
            return result;
        }

        @Override
        public void close() {
            release.countDown();
            server.stop(0);
            executor.shutdownNow();
        }
    }
}
