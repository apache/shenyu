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
import com.google.gson.JsonParser;
import org.apache.shenyu.common.dto.AgentGatewayAdmissionConfig;
import org.apache.shenyu.common.dto.AgentGatewayMcpConfig;
import org.apache.shenyu.plugin.agent.gateway.AgentTrafficContext;
import org.apache.shenyu.plugin.agent.gateway.prompt.AgentPromptDefinition;
import org.apache.shenyu.plugin.agent.gateway.prompt.AgentPromptInvocation;
import org.apache.shenyu.plugin.agent.gateway.prompt.AgentPromptProvider;
import org.apache.shenyu.plugin.agent.gateway.prompt.AgentPromptRegistry;
import org.apache.shenyu.plugin.agent.gateway.resource.AgentResourceRegistry;
import org.apache.shenyu.plugin.agent.gateway.security.AgentMcpIdentity;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolRegistry;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;
import reactor.core.scheduler.Schedulers;
import reactor.test.StepVerifier;

import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class AgentMcpPromptIntegrationTest {

    private final ObjectMapper json = new ObjectMapper();

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void authorizedListGetAndDiscoverCarryIndependentPermissionAndHttpTools(final String mode) {
        AgentMcpHttpHandler handler = handler(input -> Mono.deferContextual(reactorContext -> {
            AgentMcpExecutionContext trusted = reactorContext.get(AgentMcpExecutionContext.class);
            assertEquals("tenant-a", input.tenant());
            assertEquals("owner", input.subject());
            assertEquals(input.requestId(), trusted.getRequestId());
            assertEquals(Set.of("summary"), trusted.getAllowedPrompts());
            return Mono.just(result("private-template:" + input.arguments().get("topic")));
        }), identity(Set.of("summary"), Set.of()));
        for (String method : List.of("prompts/list", "prompts/get", "server/discover", "tools/list")) {
            MockServerWebExchange exchange = exchange(method, "summary", "\"topic\":\"gateway\"");
            handler.handle(exchange, config(mode, true), traffic(method), 1).block(Duration.ofSeconds(5));
            assertEquals(200, exchange.getResponse().getStatusCode().value(), exchange.getResponse().getBodyAsString().block());
            String wire = exchange.getResponse().getBodyAsString().block();
            if ("prompts/get".equals(method)) {
                assertTrue(wire.contains("private-template:gateway"), wire);
            } else if ("server/discover".equals(method)) {
                assertTrue(wire.contains("\"prompts\":{}"), wire);
            } else if ("tools/list".equals(method)) {
                assertTrue(wire.contains("\"lookup\""), wire);
            } else {
                assertTrue(wire.contains("\"name\":\"summary\""), wire);
            }
        }
    }

    @Test
    void toolGrantsAndExplicitPromptDenialsDoNotExposeTemplates() {
        AtomicInteger calls = new AtomicInteger();
        for (AgentMcpIdentity identity : List.of(identity(Set.of(), Set.of()), identity(Set.of("summary"), Set.of("summary")))) {
            AgentMcpHttpHandler handler = handler(input -> {
                calls.incrementAndGet();
                return Mono.just(result("secret"));
            }, identity);
            MockServerWebExchange list = exchange("prompts/list", "summary", "");
            handler.handle(list, config("json", false), traffic("list"), 1).block();
            assertTrue(list.getResponse().getBodyAsString().block().contains("\"prompts\":[]"));
            MockServerWebExchange get = exchange("prompts/get", "summary", "\"topic\":\"x\"");
            handler.handle(get, config("json", false), traffic("get"), 1).block();
            assertEquals(403, get.getResponse().getStatusCode().value());
            assertFalse(get.getResponse().getBodyAsString().block().contains("secret"));
        }
        assertEquals(0, calls.get());
    }

    @ParameterizedTest
    @ValueSource(strings = {"", "\"topic\":12", "\"unknown\":\"x\"", "\"topic\":null", "\"topic\":{}", "\"topic\":\"x\",\"extra\":\"y\""})
    void malformedAndMissingArgumentsNeverEnterProvider(final String arguments) {
        AtomicInteger calls = new AtomicInteger();
        AgentMcpHttpHandler handler = handler(input -> {
            calls.incrementAndGet();
            return Mono.just(result("secret"));
        }, identity(Set.of("summary"), Set.of()));
        MockServerWebExchange exchange = exchange("prompts/get", "summary", arguments);
        handler.handle(exchange, config("json", false), traffic("invalid"), 1).block();
        assertEquals(400, exchange.getResponse().getStatusCode().value());
        assertEquals(0, calls.get());
    }

    @Test
    void mirroredHeaderMismatchAndUnknownCursorFailExplicitly() {
        AgentMcpHttpHandler handler = handler(input -> Mono.just(result("secret")), identity(Set.of("summary"), Set.of()));
        MockServerWebExchange mismatch = MockServerWebExchange.from(builder("prompts/get").header("Mcp-Name", "different")
                .body(body("prompts/get", "summary", "\"topic\":\"x\"")));
        handler.handle(mismatch, config("json", false), traffic("invalid"), 1).block();
        assertEquals(400, mismatch.getResponse().getStatusCode().value());
        ObjectNode body = (ObjectNode) read(body("prompts/list", "summary", ""));
        ((ObjectNode) body.path("params")).put("cursor", "arbitrary");
        MockServerWebExchange cursor = MockServerWebExchange.from(builder("prompts/list").body(body.toString()));
        handler.handle(cursor, config("json", false), traffic("cursor"), 1).block();
        assertEquals(400, cursor.getResponse().getStatusCode().value());
    }

    @Test
    void cancellingSameRpcIdDoesNotCancelAnotherTenantRequest() {
        AtomicBoolean cancelled = new AtomicBoolean();
        Sinks.One<ObjectNode> survivor = Sinks.one();
        AgentMcpHttpHandler first = handler(input -> Mono.<ObjectNode>never().doOnCancel(() -> cancelled.set(true)),
                new AgentMcpIdentity("tenant-a", "first", Set.of(), Set.of(), Set.of()).withPromptPermissions(Set.of("summary"), Set.of()));
        AgentMcpHttpHandler second = handler(input -> {
            assertEquals("tenant-b", input.tenant());
            return survivor.asMono();
        }, new AgentMcpIdentity("tenant-b", "second", Set.of(), Set.of(), Set.of()).withPromptPermissions(Set.of("summary"), Set.of()));
        var cancelledCall = first.handle(exchange("prompts/get", "summary", "\"topic\":\"a\""), config("json", false), traffic("first"), 1).subscribe();
        MockServerWebExchange exchange = exchange("prompts/get", "summary", "\"topic\":\"b\"");
        StepVerifier.create(second.handle(exchange, config("json", false), traffic("second"), 1))
                .then(cancelledCall::dispose).then(() -> assertTrue(cancelled.get()))
                .then(() -> survivor.tryEmitValue(result("tenant-b-only"))).verifyComplete();
        assertTrue(exchange.getResponse().getBodyAsString().block().contains("tenant-b-only"));
    }

    @Test
    void schedulerHopsKeepEveryRequestContextPrivate() {
        AgentMcpHttpHandler handler = handler(input -> Mono.deferContextual(context -> {
            assertEquals(input.requestId(), context.get(AgentMcpExecutionContext.class).getRequestId());
            return Mono.just(result(input.requestId()));
        }).subscribeOn(Schedulers.parallel()), identity(Set.of("summary"), Set.of()));
        List<String> outputs = Flux.range(0, 32).flatMap(index -> {
            MockServerWebExchange exchange = exchange("prompts/get", "summary", "\"topic\":\"x\"");
            return handler.handle(exchange, config("json", false), traffic("request-" + index), 1)
                    .then(Mono.defer(exchange.getResponse()::getBodyAsString));
        }).collectList().block(Duration.ofSeconds(10));
        assertEquals(32, outputs.stream().distinct().count());
    }

    @Test
    void emptyInvalidAndOversizedProviderResultsDoNotEscapeAsSuccess() {
        for (Function<AgentPromptInvocation, Mono<ObjectNode>> provider : List.<Function<AgentPromptInvocation, Mono<ObjectNode>>>of(
                input -> Mono.empty(), input -> Mono.just(result("x".repeat(1048576))), input -> Mono.just(json.createObjectNode()))) {
            MockServerWebExchange exchange = exchange("prompts/get", "summary", "\"topic\":\"x\"");
            handler(provider, identity(Set.of("summary"), Set.of())).handle(exchange, config("json", false), traffic("bad-result"), 1).block();
            assertTrue(exchange.getResponse().getStatusCode().is5xxServerError());
            assertFalse(exchange.getResponse().getBodyAsString().block().contains("\"messages\""));
        }
    }

    private AgentMcpIdentity identity(final Set<String> grants, final Set<String> denials) {
        return new AgentMcpIdentity("tenant-a", "owner", Set.of("summary", "lookup"), Set.of(), Set.of()).withPromptPermissions(grants, denials);
    }

    @Test
    void exhaustedAdmissionNeverCallsProviderAndReleaseRestoresCapacity() {
        try (var admission = AgentMcpResourceAdmission.create(new AgentGatewayAdmissionConfig(1, 0, 1, 0, 1, 0))) {
            AtomicInteger calls = new AtomicInteger();
            AgentMcpHttpHandler handler = handler(input -> {
                calls.incrementAndGet();
                return Mono.just(result("ok"));
            }, identity(Set.of("summary"), Set.of()), admission);
            var active = admission.withLocal(Instant.now().plusSeconds(20), Mono.never()).subscribe();
            try {
                MockServerWebExchange rejected = exchange("prompts/get", "summary", "\"topic\":\"x\"");
                handler.handle(rejected, config("json", false), traffic("saturated"), 1).block();
                assertEquals(429, rejected.getResponse().getStatusCode().value());
                assertEquals(0, calls.get());
            } finally {
                active.dispose();
            }
            MockServerWebExchange accepted = exchange("prompts/get", "summary", "\"topic\":\"x\"");
            handler.handle(accepted, config("json", false), traffic("recovered"), 1).block();
            assertEquals(200, accepted.getResponse().getStatusCode().value());
            assertEquals(1, calls.get());
            assertTrue(admission.snapshot().stream().allMatch(state -> state.inFlight() == 0 && state.queued() == 0));
        }
    }

    @Test
    void deadlineCancelsPromptAndReturnsTimeout() {
        AtomicBoolean cancelled = new AtomicBoolean();
        AgentMcpHttpHandler handler = handler(input -> Mono.<ObjectNode>never().doOnCancel(() -> cancelled.set(true)), identity(Set.of("summary"), Set.of()));
        MockServerWebExchange exchange = exchange("prompts/get", "summary", "\"topic\":\"x\"");
        AgentGatewayMcpConfig config = AgentGatewayMcpConfig.parse(JsonParser.parseString("{\"allowedPrompts\":[\"summary\"],\"timeoutMs\":100}"), true);
        StepVerifier.withVirtualTime(() -> handler.handle(exchange, config, traffic("timeout"), 1))
                .thenAwait(Duration.ofMillis(101)).verifyComplete();
        assertEquals(504, exchange.getResponse().getStatusCode().value());
        assertTrue(cancelled.get());
    }

    @Test
    void remoteNamespaceCollisionAndInvalidServerNamesFailBeforeRouting() {
        for (String name : List.of("summary", "bad.prefix.summary")) {
            AgentMcpRemoteCatalog.Snapshot snapshot = mock(AgentMcpRemoteCatalog.Snapshot.class);
            when(snapshot.definitions()).thenReturn(Map.of());
            when(snapshot.resourceDefinitions()).thenReturn(Map.of());
            when(snapshot.promptDefinitions()).thenReturn(Map.of(name, json.createObjectNode().put("name", name)));
            AgentMcpRemoteCatalog catalog = operation -> operation.apply(snapshot);
            AgentPromptProvider local = new AgentPromptProvider() {
                @Override
                public AgentPromptDefinition definition() {
                    return new AgentPromptDefinition(json.createObjectNode().put("name", name));
                }

                @Override
                public Mono<ObjectNode> get(final AgentPromptInvocation input) {
                    return Mono.error(new AssertionError("Namespace conflicts must never enter a provider"));
                }
            };
            AgentMcpHttpHandler handler = new AgentMcpHttpHandler(new AgentMcpDispatcher(new AgentToolRegistry(List.of()), "gateway", "1", catalog,
                    new AgentResourceRegistry(List.of()), AgentMcpResourceAdmission.NOOP, new AgentPromptRegistry(List.of(local))),
                    exchange -> Mono.just(identity(Set.of("summary"), Set.of())));
            MockServerWebExchange exchange = exchange("prompts/list", "summary", "");
            handler.handle(exchange, config("json", false), traffic("collision"), 1).block();
            assertEquals(500, exchange.getResponse().getStatusCode().value());
        }
    }

    private AgentMcpHttpHandler handler(final Function<AgentPromptInvocation, Mono<ObjectNode>> get, final AgentMcpIdentity identity) {
        return handler(get, identity, AgentMcpResourceAdmission.NOOP);
    }

    private AgentMcpHttpHandler handler(final Function<AgentPromptInvocation, Mono<ObjectNode>> get, final AgentMcpIdentity identity,
                                       final AgentMcpResourceAdmission admission) {
        AgentPromptProvider provider = new AgentPromptProvider() {
            @Override
            public AgentPromptDefinition definition() {
                ObjectNode metadata = json.createObjectNode().put("name", "summary");
                metadata.putArray("arguments").addObject().put("name", "topic").put("required", true);
                return new AgentPromptDefinition(metadata);
            }

            @Override
            public Mono<ObjectNode> get(final AgentPromptInvocation invocation) {
                return get.apply(invocation);
            }
        };
        return new AgentMcpHttpHandler(new AgentMcpDispatcher(new AgentToolRegistry(List.of()), "gateway", "1", null,
                new AgentResourceRegistry(List.of()), admission, new AgentPromptRegistry(List.of(provider))), exchange -> Mono.just(identity));
    }

    private AgentGatewayMcpConfig config(final String mode, final boolean httpTools) {
        String raw = "{\"allowedPrompts\":[\"summary\"],\"responseMode\":\"" + mode + "\""
                + (httpTools ? ",\"allowedTools\":[\"lookup\"],\"httpTools\":{\"lookup\":{\"description\":\"Lookup\","
                + "\"inputSchema\":{\"type\":\"object\",\"properties\":{}},"
                + "\"requestConfig\":{\"requestTemplate\":{\"method\":\"GET\",\"url\":\"/status\"},\"argsPosition\":{}}}}" : "") + "}";
        return AgentGatewayMcpConfig.parse(JsonParser.parseString(raw), true);
    }

    private AgentTrafficContext traffic(final String id) {
        return new AgentTrafficContext(id, "mcp", "selector", "rule");
    }

    private MockServerWebExchange exchange(final String method, final String name, final String arguments) {
        var builder = builder(method);
        if ("prompts/get".equals(method)) {
            builder.header("Mcp-Name", name);
        }
        return MockServerWebExchange.from(builder.body(body(method, name, arguments)));
    }

    private MockServerHttpRequest.BodyBuilder builder(final String method) {
        return MockServerHttpRequest.post("/agent").contentType(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.ACCEPT, "application/json, text/event-stream")
                .header("MCP-Protocol-Version", AgentMcpRequestParser.VERSION).header("Mcp-Method", method);
    }

    private String body(final String method, final String name, final String arguments) {
        ObjectNode body = json.createObjectNode().put("jsonrpc", "2.0").put("id", "same-id").put("method", method);
        ObjectNode params = body.putObject("params");
        params.putObject("_meta").put("io.modelcontextprotocol/protocolVersion", AgentMcpRequestParser.VERSION)
                .putObject("io.modelcontextprotocol/clientCapabilities");
        if ("prompts/get".equals(method)) {
            params.put("name", name);
            params.set("arguments", read("{" + arguments + "}"));
        }
        return body.toString();
    }

    private com.fasterxml.jackson.databind.JsonNode read(final String raw) {
        try {
            return json.readTree(raw);
        } catch (java.io.IOException error) {
            throw new IllegalArgumentException(error);
        }
    }

    private ObjectNode result(final String text) {
        ObjectNode result = json.createObjectNode();
        result.putArray("messages").addObject().put("role", "user").putObject("content").put("type", "text").put("text", text);
        return result;
    }
}
