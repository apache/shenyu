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
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolInvocation;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolProvider;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolRegistry;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.http.HttpHeaders;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AgentMcpAggregationTest {

    private final ObjectMapper mapper = new ObjectMapper();

    private final AtomicInteger active = new AtomicInteger();

    private final AtomicInteger calls = new AtomicInteger();

    private final AtomicInteger localCalls = new AtomicInteger();

    @ParameterizedTest
    @ValueSource(strings = {"server/discover", "tools/list", "tools/call"})
    void shouldDistinguishUnavailableCatalogWithoutFallingBackToLocalTools(final String method) {
        AgentMcpRemoteCatalog unavailable = operation -> Mono.error(new AgentMcpRemoteCatalog.CatalogUnavailableException());
        AgentMcpDispatcher dispatcher = dispatcher("local", unavailable);
        StepVerifier.create(dispatcher.dispatch(request(method, "local"), () -> context("A", Set.of("local"))))
                .expectErrorSatisfies(error -> {
                    AgentMcpProtocolException protocol = (AgentMcpProtocolException) error;
                    assertEquals(503, protocol.getHttpStatus());
                    assertEquals(-32023, protocol.getCode());
                    assertEquals("Remote catalog unavailable", protocol.toResponse().path("error").path("message").asText());
                    assertEquals("same-rpc-id", protocol.toResponse().path("id").asText());
                }).verify();
        assertEquals(0, localCalls.get());
    }

    @Test
    void shouldNotLabelUnexpectedCatalogFailuresAsUnavailable() {
        AgentMcpRemoteCatalog broken = operation -> Mono.error(new IllegalStateException("private diagnostic"));
        AgentMcpDispatcher dispatcher = new AgentMcpDispatcher(new AgentToolRegistry(List.of()), "gateway", "1", broken);
        StepVerifier.create(dispatcher.dispatch(request("tools/list", ""), () -> context("A", Set.of())))
                .expectErrorSatisfies(error -> {
                    AgentMcpProtocolException protocol = (AgentMcpProtocolException) error;
                    assertEquals(500, protocol.getHttpStatus());
                    assertEquals(-32603, protocol.getCode());
                    assertEquals("Internal error", protocol.getMessage());
                }).verify();
    }

    @Test
    void shouldCombineLocalAndRemoteListsWithIdenticalPermissionPredicate() {
        AgentMcpDispatcher dispatcher = dispatcher("local");
        StepVerifier.create(dispatcher.dispatch(request("tools/list", ""), () -> context("A", Set.of("orders.lookup"))))
                .assertNext(value -> {
                    assertEquals(1, value.path("result").path("tools").size());
                    assertEquals("orders.lookup", value.path("result").path("tools").get(0).path("name").asText());
                }).verifyComplete();
        StepVerifier.create(dispatcher.dispatch(request("tools/list", ""), () -> context("B", Set.of("local", "orders.lookup"))))
                .assertNext(value -> assertEquals(2, value.path("result").path("tools").size())).verifyComplete();
        assertEquals(0, active.get());
    }

    @Test
    void shouldPreserveNativeRemoteBusinessFailureAndMetadata() {
        StepVerifier.create(dispatcher("local").dispatch(request("tools/call", "orders.lookup"), () -> context("A", Set.of("orders.lookup"))))
                .assertNext(value -> {
                    assertTrue(value.path("result").path("isError").asBoolean());
                    assertEquals("complete", value.path("result").path("resultType").asText());
                    assertEquals("A", value.path("result").path("structuredContent").path("subject").asText());
                    assertEquals("original", value.path("result").path("_meta").path("marker").asText());
                    assertTrue(value.path("result").path("extension").asBoolean());
                }).verifyComplete();
        assertEquals(1, calls.get());
        assertEquals(0, active.get());
    }

    @Test
    void shouldDenyBeforeRemoteInvocationAndReleaseLease() {
        StepVerifier.create(dispatcher("local").dispatch(request("tools/call", "orders.lookup"), () -> context("A", Set.of())))
                .expectErrorMatches(error -> error instanceof AgentMcpProtocolException && ((AgentMcpProtocolException) error).getHttpStatus() == 403).verify();
        assertEquals(0, calls.get());
        assertEquals(0, active.get());
    }

    @Test
    void shouldRejectLocalRemoteCollisionWithoutExecutingEitherTarget() {
        StepVerifier.create(dispatcher("orders.lookup").dispatch(request("tools/list", ""), () -> context("A", Set.of("orders.lookup"))))
                .expectErrorMatches(error -> error instanceof AgentMcpProtocolException && ((AgentMcpProtocolException) error).getHttpStatus() == 500).verify();
        assertEquals(0, calls.get());
        assertEquals(0, active.get());
    }

    @Test
    void shouldIsolateSameRpcIdAcrossSchedulersAndLocalRemoteTargets() {
        AgentMcpDispatcher dispatcher = dispatcher("local");
        StepVerifier.create(Flux.range(0, 32).flatMap(index -> dispatcher.dispatch(request("tools/call", index % 2 == 0 ? "orders.lookup" : "local"),
                        () -> context("subject-" + index, Set.of("local", "orders.lookup")))
                .publishOn(Schedulers.parallel()).map(value -> Map.entry(index, value)), 8).collectList())
                .assertNext(values -> {
                    assertEquals(32, values.size());
                    values.forEach(item -> {
                        assertEquals("same-rpc-id", item.getValue().path("id").asText());
                        assertEquals("subject-" + item.getKey(), item.getValue().path("result").path("structuredContent").path("subject").asText());
                    });
                }).verifyComplete();
        assertEquals(0, active.get());
    }

    private AgentMcpDispatcher dispatcher(final String localName) {
        return dispatcher(localName, null);
    }

    private AgentMcpDispatcher dispatcher(final String localName, final AgentMcpRemoteCatalog override) {
        AgentToolProvider provider = new AgentToolProvider() {
            @Override
            public String getName() {
                return localName;
            }

            @Override
            public String getDescription() {
                return "Read local test value";
            }

            @Override
            public JsonObject getInputSchema() {
                return JsonParser.parseString("{\"type\":\"object\"}").getAsJsonObject();
            }

            @Override
            public void validate(final JsonObject arguments) {
                if (arguments.size() != 0) {
                    throw new IllegalArgumentException("Test local tool expects empty arguments");
                }
            }

            @Override
            public Mono<JsonObject> invoke(final AgentToolInvocation invocation) {
                return localValue(invocation);
            }
        };
        AgentMcpRemoteCatalog catalog = new AgentMcpRemoteCatalog() {
            @Override
            public Mono<ObjectNode> withSnapshot(final Function<Snapshot, Mono<ObjectNode>> operation) {
                return Mono.usingWhen(Mono.fromSupplier(() -> {
                    active.incrementAndGet();
                    return snapshot();
                }), operation, ignored -> Mono.fromRunnable(active::decrementAndGet),
                        (ignored, error) -> Mono.fromRunnable(active::decrementAndGet), ignored -> Mono.fromRunnable(active::decrementAndGet));
            }
        };
        return new AgentMcpDispatcher(new AgentToolRegistry(List.of(provider)), "candidate", "1", Objects.isNull(override) ? catalog : override);
    }

    private Mono<JsonObject> localValue(final AgentToolInvocation invocation) {
        localCalls.incrementAndGet();
        JsonObject value = new JsonObject();
        value.addProperty("subject", invocation.getSubject());
        return Mono.just(value);
    }

    private AgentMcpRemoteCatalog.Snapshot snapshot() {
        return new AgentMcpRemoteCatalog.Snapshot() {
            @Override
            public Map<String, ObjectNode> definitions() {
                ObjectNode definition = mapper.createObjectNode().put("name", "orders.lookup").put("description", "Remote lookup");
                definition.putObject("inputSchema").put("type", "object");
                return Map.of("orders.lookup", definition);
            }

            @Override
            public Mono<ObjectNode> invoke(final String name, final JsonObject arguments, final AgentMcpExecutionContext context) {
                calls.incrementAndGet();
                ObjectNode result = mapper.createObjectNode().put("isError", true).put("extension", true);
                result.putObject("structuredContent").put("subject", context.getSubject());
                result.putArray("content").addObject().put("type", "text").put("text", "Business failure");
                result.putObject("_meta").put("marker", "original");
                return Mono.just(result);
            }
        };
    }

    private AgentMcpRequest request(final String method, final String name) {
        ObjectNode body = mapper.createObjectNode().put("jsonrpc", "2.0").put("id", "same-rpc-id").put("method", method);
        ObjectNode params = body.putObject("params");
        params.putObject("_meta").put("io.modelcontextprotocol/protocolVersion", "2026-07-28")
                .putObject("io.modelcontextprotocol/clientCapabilities");
        HttpHeaders headers = new HttpHeaders();
        headers.set("MCP-Protocol-Version", "2026-07-28");
        headers.set("Mcp-Method", method);
        if ("tools/call".equals(method)) {
            params.put("name", name).putObject("arguments");
            headers.set("Mcp-Name", name);
        }
        return new AgentMcpRequestParser().parse(body.toString().getBytes(StandardCharsets.UTF_8), headers, 8192);
    }

    private AgentMcpExecutionContext context(final String subject, final Set<String> grants) {
        return new AgentMcpExecutionContext("private-" + subject, subject, "rule", 1, Set.of("local", "orders.lookup"), grants);
    }
}
