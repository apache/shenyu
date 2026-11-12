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
import com.sun.net.httpserver.HttpServer;
import io.modelcontextprotocol.spec.McpSchema;
import reactor.core.publisher.Mono;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpResourceAdmission;
import org.junit.jupiter.api.Test;

import java.net.InetSocketAddress;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.time.Instant;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.List;

import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertEquals;

class RequestScopedMcpClientAdmissionTest {

    @Test
    void slowFailingAndLargeUpstreamsRemainBoundedAndIndependent() throws Exception {
        try (Fixture slowFixture = new Fixture(); Fixture fastFixture = new Fixture()) {
            RequestScopedMcpClient slow = slowFixture.client("slow");
            RequestScopedMcpClient fast = fastFixture.client("fast");
            slow.initialize().block(Duration.ofSeconds(5));
            fast.initialize().block(Duration.ofSeconds(5));
            AgentMcpResourceAdmission.ResourceAdmissionController admission =
                    AgentMcpResourceAdmission.create(new org.apache.shenyu.common.dto.AgentGatewayAdmissionConfig(
                            4, 4, 4, 4, 1, 2));
            try {
                final var slowCall = admission.withRemote("slow", Instant.now().plusSeconds(5),
                                call(slow, "slow", 700)).toFuture();
                await().atMost(Duration.ofSeconds(2)).until(() -> slowFixture.active.get() == 1);
                assertEquals("fast", call(fast, "fast", 0)
                        .block(Duration.ofSeconds(5)).path("structuredContent").path("token").textValue());
                assertEquals(1, slowFixture.active.get());

                List<Throwable> failures = new CopyOnWriteArrayList<>();
                admission.withRemote("fast", Instant.now().plusSeconds(5), call(fast, "upstream-failure", 0))
                        .subscribe(value -> { }, failures::add);
                await().atMost(Duration.ofSeconds(2)).until(() -> failures.size() == 1);
                await().atMost(Duration.ofSeconds(2)).until(() -> fastFixture.active.get() == 0);

                List<Throwable> oversize = new CopyOnWriteArrayList<>();
                admission.withRemote("fast", Instant.now().plusSeconds(5), call(fast, "large", 0))
                        .subscribe(value -> { }, oversize::add);
                await().atMost(Duration.ofSeconds(2)).until(() -> oversize.size() == 1);
                assertEquals(1, fastFixture.oversizeResponses.get());
                await().atMost(Duration.ofSeconds(2)).until(() -> fastFixture.active.get() == 0);

                assertEquals("slow", slowCall.get(5, TimeUnit.SECONDS)
                        .path("structuredContent").path("token").textValue());
                assertEquals(0, slowFixture.active.get());
            } finally {
                admission.close();
                slow.closeGracefully().onErrorResume(ignored -> Mono.empty()).block(Duration.ofSeconds(5));
                fast.closeGracefully().onErrorResume(ignored -> Mono.empty()).block(Duration.ofSeconds(5));
            }
        }
    }

    private static Mono<ObjectNode> call(final RequestScopedMcpClient client, final String token, final int delay) {
        return client.callRaw(new McpSchema.CallToolRequest("lookup", Map.of("token", token, "delay", delay)));
    }

    private static final class Fixture implements AutoCloseable {

        private final ObjectMapper json = new ObjectMapper();

        private final HttpServer server;

        private final ExecutorService executor = Executors.newFixedThreadPool(8);

        private final AtomicInteger active = new AtomicInteger();

        private final AtomicInteger oversizeResponses = new AtomicInteger();

        private Fixture() throws Exception {
            server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
            server.setExecutor(executor);
            server.createContext("/ready", exchange -> {
                exchange.sendResponseHeaders(204, -1);
                exchange.close();
            });
            server.createContext("/mcp", exchange -> {
                try {
                    ObjectNode request = (ObjectNode) json.readTree(exchange.getRequestBody());
                    String method = request.path("method").textValue();
                    if ("notifications/initialized".equals(method)) {
                        exchange.sendResponseHeaders(202, -1);
                        return;
                    }
                    ObjectNode result = json.createObjectNode();
                    if ("initialize".equals(method)) {
                        exchange.getResponseHeaders().set("MCP-Session-Id", "unit-session");
                        result.put("protocolVersion", "2025-06-18");
                        result.putObject("capabilities").putObject("tools");
                        result.putObject("serverInfo").put("name", "unit").put("version", "1");
                    } else if ("tools/list".equals(method)) {
                        var tool = result.putArray("tools").addObject().put("name", "lookup").put("description", "lookup");
                        tool.putObject("inputSchema").put("type", "object");
                    } else {
                        String token = request.path("params").path("arguments").path("token").textValue();
                        int delay = request.path("params").path("arguments").path("delay").asInt();
                        int slot = active.incrementAndGet();
                        try {
                            if ("upstream-failure".equals(token)) {
                                exchange.sendResponseHeaders(503, -1);
                                return;
                            }
                            if ("large".equals(token)) {
                                result.putArray("content").addObject().put("type", "text").put("text", "x".repeat(16384));
                            } else {
                                if (delay > 0) {
                                    Thread.sleep(delay);
                                }
                                result.putArray("content").addObject().put("type", "text").put("text", "native");
                                result.putObject("structuredContent").put("token", token);
                            }
                        } finally {
                            active.decrementAndGet();
                            if (slot == 0) {
                                throw new IllegalStateException("Invalid active slot");
                            }
                        }
                    }
                    ObjectNode envelope = json.createObjectNode().put("jsonrpc", "2.0");
                    envelope.set("id", request.get("id"));
                    envelope.set("result", result);
                    byte[] response = envelope.toString().getBytes(StandardCharsets.UTF_8);
                    if (response.length > 8192) {
                        oversizeResponses.incrementAndGet();
                    }
                    exchange.getResponseHeaders().set("Content-Type", "application/json");
                    exchange.sendResponseHeaders(200, response.length);
                    exchange.getResponseBody().write(response);
                } catch (Exception expected) {
                    // Cancelled connections may surface here and are intentionally tolerated.
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

        private RequestScopedMcpClient client(final String name) {
            URI uri = URI.create("http://127.0.0.1:" + server.getAddress().getPort() + "/mcp");
            RemoteServerBinding.Config target = new RemoteServerBinding.Config(name, uri, "service/" + name, "v1");
            return new RequestScopedMcpClient(RemoteServerBinding.resolve(target, Set.of(uri),
                    ignored -> new RemoteServerBinding.Credential(target, "fixed-test-token")), ignored -> { });
        }

        @Override
        public void close() {
            server.stop(0);
            executor.shutdownNow();
        }
    }
}
