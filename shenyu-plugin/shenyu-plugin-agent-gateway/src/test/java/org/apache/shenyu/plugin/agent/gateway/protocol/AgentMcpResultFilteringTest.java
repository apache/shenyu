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
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.AgentGatewayMcpConfig;
import org.apache.shenyu.common.metrics.AgentMcpCallObserver;
import org.apache.shenyu.plugin.agent.gateway.AgentTrafficContext;
import org.apache.shenyu.plugin.agent.gateway.security.AgentMcpIdentity;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolInvocation;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolProvider;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolRegistry;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.http.MediaType;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;
import reactor.core.scheduler.Schedulers;
import reactor.test.StepVerifier;

import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AgentMcpResultFilteringTest {

    private final ObjectMapper mapper = new ObjectMapper();

    private final AtomicInteger calls = new AtomicInteger();

    private final AgentTrafficContext traffic = new AgentTrafficContext("private-request", "mcp", "selector", "rule");

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void localProviderRebuildsTheFilteredTextCopy(final String mode) throws Exception {
        AgentToolProvider provider = new AgentToolProvider() {
            @Override
            public String getName() {
                return "crm.profile";
            }

            @Override
            public String getDescription() {
                return "Local field-deletion fixture";
            }

            @Override
            public JsonObject getInputSchema() {
                return JsonParser.parseString("{\"type\":\"object\"}").getAsJsonObject();
            }

            @Override
            public void validate(final JsonObject arguments) {
                assertEquals(0, arguments.size());
            }

            @Override
            public Mono<JsonObject> invoke(final AgentToolInvocation invocation) {
                calls.incrementAndGet();
                return Mono.just(JsonParser.parseString("{\"owner\":\"A\",\"secret\":\"private-marker\"}").getAsJsonObject());
            }
        };
        AgentMcpHttpHandler handler = new AgentMcpHttpHandler(new AgentMcpDispatcher(new AgentToolRegistry(List.of(provider)), "gateway", "1"),
                exchange -> Mono.just(new AgentMcpIdentity("A", Set.of("crm.profile"))));
        MockServerWebExchange exchange = exchange("tools/call", "A");
        StepVerifier.create(handler.handle(exchange, config(mode, "/secret", 30000), traffic, 1)).verifyComplete();
        ObjectNode response = reply(exchange, mode);
        assertFalse(response.path("result").path("structuredContent").has("secret"));
        assertFalse(response.toString().contains("private-marker"));
        assertEquals(response.path("result").path("structuredContent"),
                mapper.readTree(response.path("result").path("content").get(0).path("text").asText()));
        assertEquals(1, calls.get());
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void filtersBeforeSerializationAndRebuildsVerifiedCopies(final String mode) throws Exception {
        AgentMcpHttpHandler handler = handler(optionalSchema(), context -> Mono.just(result(context.getSubject())), true);
        MockServerWebExchange exchange = exchange("tools/call", "A");
        StepVerifier.create(handler.handle(exchange, config(mode, "/secret", 30000), traffic, 1)).verifyComplete();
        ObjectNode reply = reply(exchange, mode);
        assertEquals(200, exchange.getResponse().getStatusCode().value());
        assertFalse(reply.path("result").path("structuredContent").has("secret"));
        assertEquals("A", reply.path("result").path("structuredContent").path("owner").asText());
        assertEquals(reply.path("result").path("structuredContent"),
                mapper.readTree(reply.path("result").path("content").get(0).path("text").asText()));
        assertFalse(reply.toString().contains("private-marker"));
        assertEquals("same-id", reply.path("id").asText());
        assertEquals(1, calls.get());
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void requiredFieldDeletionReturnsSafeJsonErrorWithoutFallback(final String mode) throws Exception {
        ObjectNode schema = mapper.createObjectNode().put("type", "object");
        schema.putArray("required").add("secret");
        AgentMcpHttpHandler handler = handler(schema, context -> Mono.just(result("A")), true);
        MockServerWebExchange exchange = exchange("tools/call", "A");
        AtomicInteger observations = new AtomicInteger();
        AtomicReference<AgentMcpCallObserver.Outcome> outcome = new AtomicReference<>();
        exchange.getAttributes().put(Constants.METRICS_AGENT_MCP_CALL, (AgentMcpCallObserver) (value, millis) -> {
            observations.incrementAndGet();
            outcome.set(value);
        });
        StepVerifier.create(handler.handle(exchange, config(mode, "/secret", 30000), traffic, 1)).verifyComplete();
        assertEquals(502, exchange.getResponse().getStatusCode().value());
        assertEquals(MediaType.APPLICATION_JSON, exchange.getResponse().getHeaders().getContentType());
        ObjectNode response = reply(exchange, "json");
        assertEquals(-32024, response.path("error").path("code").asInt());
        assertEquals("Tool result filtering failed", response.path("error").path("message").asText());
        assertFalse(response.has("result"));
        assertFalse(response.toString().contains("private-marker"));
        assertEquals(1, observations.get());
        assertEquals(AgentMcpCallObserver.Outcome.SERVER_ERROR, outcome.get());
    }

    @Test
    void unsupportedSchemaFailsBeforeInvokingTheTool() {
        ObjectNode schema = mapper.createObjectNode().put("$ref", "http://127.0.0.1:1/private");
        AgentMcpHttpHandler handler = handler(schema, context -> Mono.just(result("A")), true);
        MockServerWebExchange exchange = exchange("tools/call", "A");
        StepVerifier.create(handler.handle(exchange, config("json", "/secret", 30000), traffic, 1)).verifyComplete();
        assertEquals(502, exchange.getResponse().getStatusCode().value());
        assertEquals(0, calls.get());
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void opaqueResultChannelsReturnOneSafeFailureWithoutRetry(final String mode) throws Exception {
        for (String variant : List.of("text", "mismatch", "image", "metadata", "extension", "block-meta", "missing-structured")) {
            ObjectNode value = result("A");
            ObjectNode block = (ObjectNode) value.path("content").get(0);
            switch (variant) {
                case "text" -> block.put("text", "private-marker");
                case "mismatch" -> block.put("text", "{\"owner\":\"A\",\"secret\":\"other-private-marker\"}");
                case "image" -> block.put("type", "image").put("data", "private-marker");
                case "metadata" -> value.putObject("_meta").put("secret", "private-marker");
                case "extension" -> value.put("extension", "private-marker");
                case "block-meta" -> block.putObject("_meta").put("secret", "private-marker");
                default -> value.remove("structuredContent");
            }
            MockServerWebExchange exchange = exchange("tools/call", "A");
            AtomicInteger observations = new AtomicInteger();
            exchange.getAttributes().put(Constants.METRICS_AGENT_MCP_CALL, (AgentMcpCallObserver) (outcome, millis) -> {
                assertEquals(AgentMcpCallObserver.Outcome.SERVER_ERROR, outcome);
                observations.incrementAndGet();
            });
            final int before = calls.get();
            StepVerifier.create(handler(null, context -> Mono.just(value), true)
                    .handle(exchange, config(mode, "/secret", 30000), traffic, 1)).verifyComplete();
            ObjectNode response = reply(exchange, "json");
            assertEquals(502, exchange.getResponse().getStatusCode().value());
            assertEquals(MediaType.APPLICATION_JSON, exchange.getResponse().getHeaders().getContentType());
            assertEquals(-32024, response.path("error").path("code").asInt());
            assertEquals("Tool result filtering failed", response.path("error").path("message").asText());
            assertFalse(response.toString().contains("private-marker"));
            assertFalse(response.has("result"));
            assertEquals(before + 1, calls.get());
            assertEquals(1, observations.get());
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void localValidationFailureUsesSafeStructuredCopies(final String mode) throws Exception {
        AgentToolProvider provider = new AgentToolProvider() {
            @Override
            public String getName() {
                return "crm.profile";
            }

            @Override
            public String getDescription() {
                return "Validation failure fixture";
            }

            @Override
            public JsonObject getInputSchema() {
                return JsonParser.parseString("{\"type\":\"object\"}").getAsJsonObject();
            }

            @Override
            public void validate(final JsonObject arguments) {
                throw new IllegalArgumentException("private-marker");
            }

            @Override
            public Mono<JsonObject> invoke(final AgentToolInvocation invocation) {
                calls.incrementAndGet();
                return Mono.error(new IllegalStateException("Must not invoke"));
            }
        };
        AgentMcpHttpHandler handler = new AgentMcpHttpHandler(new AgentMcpDispatcher(new AgentToolRegistry(List.of(provider)), "gateway", "1"),
                exchange -> Mono.just(new AgentMcpIdentity("A", Set.of("crm.profile"))));
        MockServerWebExchange exchange = exchange("tools/call", "A");
        StepVerifier.create(handler.handle(exchange, config(mode, "/secret", 30000), traffic, 1)).verifyComplete();
        ObjectNode value = reply(exchange, mode).withObject("result");
        assertEquals(200, exchange.getResponse().getStatusCode().value());
        assertTrue(value.path("isError").asBoolean());
        assertEquals("Invalid tool arguments", value.path("structuredContent").path("error").asText());
        assertEquals(value.get("structuredContent"), mapper.readTree(value.path("content").get(0).get("text").asText()));
        assertFalse(value.has("_meta"));
        assertFalse(value.toString().contains("private-marker"));
        assertEquals(0, calls.get());
    }

    @Test
    void unauthorizedCallDoesNotCompileSchemaOrInvokeTool() {
        AgentMcpHttpHandler handler = handler(mapper.createObjectNode().put("$ref", "http://127.0.0.1:1/private"),
                context -> Mono.just(result("A")), false);
        MockServerWebExchange exchange = exchange("tools/call", "A");
        StepVerifier.create(handler.handle(exchange, config("json", "/secret", 30000), traffic, 1)).verifyComplete();
        assertEquals(403, exchange.getResponse().getStatusCode().value());
        assertEquals(0, calls.get());
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void toolListRetainsOriginalSchemaWithoutExecutingTheTool(final String mode) throws Exception {
        ObjectNode schema = optionalSchema();
        MockServerWebExchange exchange = exchange("tools/list", "A");
        StepVerifier.create(handler(schema, context -> Mono.just(result("A")), true)
                .handle(exchange, config(mode, "/secret", 30000), traffic, 1)).verifyComplete();
        assertEquals(schema, reply(exchange, mode).path("result").path("tools").get(0).path("outputSchema"));
        assertEquals(0, calls.get());
    }

    @Test
    void inFlightCallsKeepTheirCapturedPlanDuringAnUpdate() throws Exception {
        Sinks.One<ObjectNode> oldResult = Sinks.one();
        AgentMcpHttpHandler handler = handler(null, context -> "A".equals(context.getSubject()) ? oldResult.asMono() : Mono.just(result("B")), true);
        MockServerWebExchange first = exchange("tools/call", "A");
        MockServerWebExchange second = exchange("tools/call", "B");
        var oldCall = handler.handle(first, config("json", "/secret", 30000), traffic, 1).subscribe();
        try {
            StepVerifier.create(handler.handle(second, config("json", "/owner", 30000), traffic, 2)).verifyComplete();
            oldResult.tryEmitValue(result("A"));
            assertFalse(reply(first, "json").path("result").path("structuredContent").has("secret"));
            assertTrue(reply(first, "json").path("result").path("structuredContent").has("owner"));
            assertFalse(reply(second, "json").path("result").path("structuredContent").has("owner"));
            assertTrue(reply(second, "json").path("result").path("structuredContent").has("secret"));
        } finally {
            oldCall.dispose();
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void timeoutCancelsTheProviderWithoutReturningItsRawResult(final String mode) {
        AtomicBoolean cancelled = new AtomicBoolean();
        AgentMcpHttpHandler handler = handler(null, context -> Mono.<ObjectNode>never().doOnCancel(() -> cancelled.set(true)), true);
        MockServerWebExchange exchange = exchange("tools/call", "A");
        StepVerifier.withVirtualTime(() -> handler.handle(exchange, config(mode, "/secret", 100), traffic, 1))
                .thenAwait(Duration.ofMillis(101)).verifyComplete();
        assertEquals(504, exchange.getResponse().getStatusCode().value());
        assertTrue(cancelled.get());
    }

    @Test
    void cancellingOneCallDoesNotCancelItsSameIdPeer() throws Exception {
        AtomicBoolean cancelled = new AtomicBoolean();
        AgentMcpHttpHandler handler = handler(null, context -> "A".equals(context.getSubject())
                ? Mono.<ObjectNode>never().doOnCancel(() -> cancelled.set(true)) : Mono.just(result("B")), true);
        MockServerWebExchange first = exchange("tools/call", "A");
        StepVerifier.create(handler.handle(first, config("json", "/secret", 30000), traffic, 1))
                .then(() -> assertEquals(1, calls.get())).thenCancel().verify();
        MockServerWebExchange second = exchange("tools/call", "B");
        StepVerifier.create(handler.handle(second, config("json", "/secret", 30000), traffic, 1)).verifyComplete();
        assertTrue(cancelled.get());
        assertEquals("B", reply(second, "json").path("result").path("structuredContent").path("owner").asText());
        assertEquals("same-id", reply(second, "json").path("id").asText());
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void responseLimitAppliesToTheFilteredSerializedResponse(final String mode) throws Exception {
        AgentMcpHttpHandler handler = handler(null, context -> {
            ObjectNode response = result("A");
            ((ObjectNode) response.path("structuredContent")).put("secret", "x".repeat(2048));
            ((ObjectNode) response.path("content").get(0)).put("text", response.path("structuredContent").toString());
            return Mono.just(response);
        }, true);
        JsonObject settings = JsonParser.parseString(settings(mode, "/secret", 30000)).getAsJsonObject();
        settings.addProperty("maxResponseBytes", 1024);
        MockServerWebExchange exchange = exchange("tools/call", "A");
        StepVerifier.create(handler.handle(exchange, AgentGatewayMcpConfig.parse(settings, true), traffic, 1)).verifyComplete();
        assertEquals(200, exchange.getResponse().getStatusCode().value());
        assertFalse(reply(exchange, mode).path("result").path("structuredContent").has("secret"));
    }

    @Test
    void sameRpcIdAcrossThirtyTwoCallsHasPrivatePlanAndResult() {
        AgentMcpHttpHandler handler = handler(null, context -> Mono.just(result(context.getSubject())).delayElement(Duration.ofMillis(1)), true);
        StepVerifier.create(Flux.range(0, 32).flatMap(index -> {
            String mode = index % 2 == 0 ? "json" : "sse";
            String path = index % 2 == 0 ? "/secret" : "/owner";
            MockServerWebExchange exchange = exchange("tools/call", "owner-" + index);
            return handler.handle(exchange, config(mode, path, 30000), traffic, index)
                    .then(Mono.defer(() -> exchange.getResponse().getBodyAsString())).flatMap(body -> Mono.fromCallable(() -> {
                        ObjectNode response = decode(body, mode);
                        assertEquals("same-id", response.path("id").asText());
                        assertEquals(index % 2 == 0, response.path("result").path("structuredContent").has("owner"));
                        assertEquals(index % 2 != 0, response.path("result").path("structuredContent").has("secret"));
                        if (index % 2 == 0) {
                            assertEquals("owner-" + index, response.path("result").path("structuredContent").path("owner").asText());
                        }
                        return response;
                    })).subscribeOn(Schedulers.parallel());
        }, 8).collectList()).assertNext(results -> assertEquals(32, results.size())).verifyComplete();
        assertEquals(32, calls.get());
    }

    private AgentMcpHttpHandler handler(final ObjectNode outputSchema, final Function<AgentMcpExecutionContext, Mono<ObjectNode>> result,
                                       final boolean authorized) {
        AgentMcpRemoteCatalog catalog = operation -> operation.apply(new AgentMcpRemoteCatalog.Snapshot() {
            @Override
            public Map<String, ObjectNode> definitions() {
                ObjectNode definition = mapper.createObjectNode().put("name", "crm.profile").put("description", "Remote profile");
                definition.putObject("inputSchema").put("type", "object");
                if (Objects.nonNull(outputSchema)) {
                    definition.set("outputSchema", outputSchema);
                }
                return Map.of("crm.profile", definition);
            }

            @Override
            public Mono<ObjectNode> invoke(final String name, final JsonObject arguments, final AgentMcpExecutionContext context) {
                calls.incrementAndGet();
                return result.apply(context);
            }
        });
        AgentMcpDispatcher dispatcher = new AgentMcpDispatcher(new AgentToolRegistry(List.of()), "gateway", "1", catalog);
        return new AgentMcpHttpHandler(dispatcher, exchange -> Mono.just(new AgentMcpIdentity(exchange.getAttribute("trusted-subject"),
                authorized ? Set.of("crm.profile") : Set.of())));
    }

    private MockServerWebExchange exchange(final String method, final String subject) {
        ObjectNode body = mapper.createObjectNode().put("jsonrpc", "2.0").put("id", "same-id").put("method", method);
        ObjectNode params = body.putObject("params");
        params.putObject("_meta").put("io.modelcontextprotocol/protocolVersion", "2026-07-28")
                .putObject("io.modelcontextprotocol/clientCapabilities");
        var builder = MockServerHttpRequest.post("/agent/mcp").contentType(MediaType.APPLICATION_JSON)
                .header("Accept", "application/json, text/event-stream").header("MCP-Protocol-Version", "2026-07-28").header("Mcp-Method", method);
        if ("tools/call".equals(method)) {
            params.put("name", "crm.profile").putObject("arguments");
            builder.header("Mcp-Name", "crm.profile");
        }
        MockServerWebExchange exchange = MockServerWebExchange.from(builder.body(body.toString()));
        exchange.getAttributes().put("trusted-subject", subject);
        return exchange;
    }

    private AgentGatewayMcpConfig config(final String mode, final String path, final int timeout) {
        return AgentGatewayMcpConfig.parse(JsonParser.parseString(settings(mode, path, timeout)), true);
    }

    private String settings(final String mode, final String path, final int timeout) {
        return "{\"allowedTools\":[\"crm.profile\"],\"responseMode\":\"" + mode + "\",\"timeoutMs\":" + timeout
                + ",\"structuredContentRemovals\":{\"crm.profile\":[\"" + path + "\"]}}";
    }

    private ObjectNode optionalSchema() {
        ObjectNode schema = mapper.createObjectNode().put("type", "object");
        schema.putObject("properties").putObject("owner").put("type", "string");
        schema.putArray("required").add("owner");
        return schema;
    }

    private ObjectNode result(final String owner) {
        ObjectNode result = mapper.createObjectNode().put("isError", false);
        result.putObject("structuredContent").put("owner", owner).put("secret", "private-marker");
        result.putArray("content").addObject().put("type", "text").put("text", result.get("structuredContent").toString());
        return result;
    }

    private ObjectNode reply(final MockServerWebExchange exchange, final String mode) throws Exception {
        String body = exchange.getResponse().getBodyAsString().block();
        return decode(body, mode);
    }

    private ObjectNode decode(final String body, final String mode) throws Exception {
        String json = "sse".equals(mode) ? body.substring(body.indexOf("data: ") + 6).trim() : body;
        return (ObjectNode) mapper.readTree(json);
    }
}
