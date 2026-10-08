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

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.apache.shenyu.plugin.agent.gateway.AgentGatewayConstants;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolExecutionException;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolInvocation;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolProvider;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolRegistry;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.http.HttpHeaders;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;
import reactor.core.scheduler.Schedulers;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import java.util.function.Function;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AgentMcpDispatcherTest {

    private final AgentMcpRequestParser parser = new AgentMcpRequestParser();

    @Test
    void shouldAdvertiseOnlyImplementedCapabilitiesAndPrivateCacheMetadata() {
        AgentMcpDispatcher dispatcher = dispatcher(provider("order_status", input -> Mono.just(input.getArguments())));
        StepVerifier.create(dispatcher.dispatch(request("server/discover", "1", "order_status"), () -> context("A", Set.of())))
                .assertNext(response -> {
                    assertEquals("2.0", response.get("jsonrpc").textValue());
                    ObjectNode result = (ObjectNode) response.get("result");
                    assertEquals("complete", result.get("resultType").textValue());
                    assertEquals("2026-07-28", result.get("supportedVersions").get(0).textValue());
                    assertEquals(1, result.get("capabilities").size());
                    assertTrue(result.get("capabilities").get("tools").isEmpty());
                    assertEquals("private", result.get("cacheScope").textValue());
                    assertEquals(0, result.get("ttlMs").intValue());
                    assertEquals("shenyu-agent-gateway", result.path("_meta").path("io.modelcontextprotocol/serverInfo").path("name").textValue());
                    assertEquals(AgentGatewayConstants.MCP_SERVER_VERSION, result.path("_meta").path("io.modelcontextprotocol/serverInfo").path("version").textValue());
                }).verifyComplete();
    }

    @Test
    void shouldFilterListInRegistrationOrderAndCopyDefinitions() {
        AgentMcpDispatcher dispatcher = dispatcher(provider("second", input -> Mono.empty()), provider("first", input -> Mono.empty()));
        StepVerifier.create(dispatcher.dispatch(request("tools/list", "1", "first"),
                () -> new AgentMcpExecutionContext("id", "A", "rule", 1, Set.of("first", "second"), Set.of("first", "second"))))
                .assertNext(response -> {
                    assertEquals("second", response.path("result").path("tools").get(0).path("name").textValue());
                    assertEquals("first", response.path("result").path("tools").get(1).path("name").textValue());
                    assertEquals("Read first", response.path("result").path("tools").get(1).path("description").textValue());
                    ((ObjectNode) response.path("result").path("tools").get(0).path("inputSchema")).put("type", "changed");
                }).verifyComplete();
        StepVerifier.create(dispatcher.dispatch(request("tools/list", "1", "first"),
                () -> new AgentMcpExecutionContext("id", "B", "rule", 1, Set.of("first"), Set.of("first", "second"))))
                .assertNext(response -> {
                    assertEquals(1, response.path("result").path("tools").size());
                    assertEquals("first", response.path("result").path("tools").get(0).path("name").textValue());
                    assertEquals("object", response.path("result").path("tools").get(0).path("inputSchema").path("type").textValue());
                    assertEquals("private", response.path("result").path("cacheScope").textValue());
                    assertEquals(0, response.path("result").path("ttlMs").intValue());
                }).verifyComplete();
    }

    @Test
    void shouldHaveNoDefaultTools() {
        AgentMcpDispatcher dispatcher = dispatcher();
        StepVerifier.create(dispatcher.dispatch(request("tools/list", "1", "order_status"),
                () -> new AgentMcpExecutionContext("id", "A", "rule", 0, Set.of(), Set.of("order_status"))))
                .assertNext(response -> assertTrue(response.path("result").path("tools").isEmpty())).verifyComplete();
    }

    @ParameterizedTest
    @ValueSource(strings = {"\"same-id\"", "1", "92233720368547758081234"})
    void shouldPreserveRpcIdAndEncodeOneStructuredResult(final String id) {
        AtomicInteger calls = new AtomicInteger();
        JsonObject business = new JsonObject();
        business.addProperty("state", "ready");
        AgentMcpDispatcher dispatcher = dispatcher(provider("order_status", input -> {
            calls.incrementAndGet();
            assertEquals("internal-A", input.getRequestId());
            assertEquals("A", input.getSubject());
            return Mono.just(business);
        }));
        StepVerifier.create(dispatcher.dispatch(request("tools/call", id, "order_status"), () -> context("A", Set.of("order_status"))))
                .assertNext(response -> {
                    assertEquals(id, response.get("id").toString());
                    assertEquals("complete", response.path("result").path("resultType").textValue());
                    assertFalse(response.path("result").path("isError").booleanValue());
                    assertEquals("ready", response.path("result").path("structuredContent").path("state").textValue());
                    assertEquals(1, response.path("result").path("content").size());
                    assertEquals("{\"state\":\"ready\"}", response.path("result").path("content").get(0).path("text").textValue());
                    ((ObjectNode) response.path("result").path("structuredContent")).put("state", "mutated");
                }).verifyComplete();
        assertEquals("ready", business.get("state").getAsString());
        assertEquals(1, calls.get());
    }

    @Test
    void shouldBeColdAndBuildFreshContextForEachSubscription() {
        AtomicInteger contexts = new AtomicInteger();
        AtomicInteger calls = new AtomicInteger();
        Set<String> internalIds = ConcurrentHashMap.newKeySet();
        AgentMcpDispatcher dispatcher = dispatcher(provider("order_status", input -> {
            internalIds.add(input.getRequestId());
            calls.incrementAndGet();
            return Mono.just(input.getArguments());
        }));
        Mono<ObjectNode> response = dispatcher.dispatch(request("tools/call", "1", "order_status"), () ->
                context("subject-" + contexts.incrementAndGet(), Set.of("order_status")));
        assertEquals(0, contexts.get());
        assertEquals(0, calls.get());
        StepVerifier.create(response).expectNextCount(1).verifyComplete();
        StepVerifier.create(response).expectNextCount(1).verifyComplete();
        assertEquals(2, contexts.get());
        assertEquals(2, calls.get());
        assertEquals(2, internalIds.size());
    }

    @ParameterizedTest
    @ValueSource(strings = {"order_status", "unknown"})
    void shouldDenyHiddenAndUnknownToolsBeforeValidationOrInvocation(final String name) {
        AtomicInteger validation = new AtomicInteger();
        AtomicInteger calls = new AtomicInteger();
        AgentMcpDispatcher dispatcher = dispatcher(provider("order_status", input -> validation.incrementAndGet(), input -> {
            calls.incrementAndGet();
            return Mono.just(input.getArguments());
        }));
        StepVerifier.create(dispatcher.dispatch(request("tools/call", "1", name), () -> context("A", Set.of())))
                .expectErrorSatisfies(error -> assertProtocol(error, 403, -32602, "Tool is not available")).verify();
        assertEquals(0, validation.get());
        assertEquals(0, calls.get());
    }

    @Test
    void shouldRejectMissingConfiguredToolsInsteadOfSilentlyHidingThem() {
        StepVerifier.create(dispatcher().dispatch(request("tools/list", "1", "order_status"), () -> context("A", Set.of())))
                .expectErrorSatisfies(error -> assertProtocol(error, 500, -32603, "Configured tool is not registered")).verify();
    }

    @Test
    void shouldTreatInputValidationAsToolFailureWithoutExecutingProvider() {
        AtomicInteger calls = new AtomicInteger();
        AgentMcpDispatcher dispatcher = dispatcher(provider("order_status", input -> {
            throw new IllegalArgumentException("private argument value");
        }, input -> {
            calls.incrementAndGet();
            return Mono.just(input.getArguments());
        }));
        StepVerifier.create(dispatcher.dispatch(request("tools/call", "1", "order_status"), () -> context("A", Set.of("order_status"))))
                .assertNext(response -> {
                    assertTrue(response.path("result").path("isError").booleanValue());
                    assertEquals("Invalid tool arguments", response.path("result").path("content").get(0).path("text").textValue());
                    assertFalse(response.toString().contains("private argument value"));
                }).verifyComplete();
        assertEquals(0, calls.get());
    }

    @Test
    void shouldTreatExplicitBusinessFailureAsOneSanitizedResultWithoutRetry() {
        AtomicInteger calls = new AtomicInteger();
        AgentMcpDispatcher dispatcher = dispatcher(provider("order_status", input -> {
            calls.incrementAndGet();
            return Mono.error(new AgentToolExecutionException("api-key=secret"));
        }));
        StepVerifier.create(dispatcher.dispatch(request("tools/call", "1", "order_status"), () -> context("A", Set.of("order_status"))))
                .assertNext(response -> {
                    assertTrue(response.path("result").path("isError").booleanValue());
                    assertEquals("complete", response.path("result").path("resultType").textValue());
                    assertEquals("Tool execution failed", response.path("result").path("content").get(0).path("text").textValue());
                    assertFalse(response.has("error"));
                    assertFalse(response.toString().contains("secret"));
                }).verifyComplete();
        assertEquals(1, calls.get());
    }

    @Test
    void shouldNotTreatUnexpectedInvocationArgumentExceptionAsInputValidation() {
        AgentMcpDispatcher dispatcher = dispatcher(provider("order_status", input -> {
            throw new IllegalArgumentException("internal secret");
        }));
        StepVerifier.create(dispatcher.dispatch(request("tools/call", "1", "order_status"), () -> context("A", Set.of("order_status"))))
                .expectErrorSatisfies(error -> {
                    assertProtocol(error, 500, -32603, "Internal error");
                    assertFalse(((AgentMcpProtocolException) error).toResponse().toString().contains("secret"));
                }).verify();
    }

    @Test
    void shouldRejectEmptyCompletionAsInternalFailure() {
        StepVerifier.create(dispatcher(provider("order_status", input -> Mono.empty()))
                .dispatch(request("tools/call", "1", "order_status"), () -> context("A", Set.of("order_status"))))
                .expectErrorSatisfies(error -> assertProtocol(error, 500, -32603, "Internal error")).verify();
    }

    @Test
    void shouldMapRecordAuthorizationDenialWithoutLeakingDetails() {
        StepVerifier.create(dispatcher(provider("order_status", input -> Mono.error(new SecurityException("private order exists"))))
                .dispatch(request("tools/call", "1", "order_status"), () -> context("A", Set.of("order_status"))))
                .expectErrorSatisfies(error -> assertProtocol(error, 403, -32602, "Tool is not available")).verify();
    }

    @Test
    void shouldRejectCursorRatherThanRepeatTheFirstPage() {
        AgentMcpRequest request = request("tools/list", "1", "order_status");
        ObjectNode params = request.getParams().put("cursor", "unknown-page");
        AgentMcpRequest paginated = new AgentMcpRequest(request.getId(), request.getMethod(), params);
        StepVerifier.create(dispatcher(provider("order_status", input -> Mono.empty())).dispatch(paginated, () -> context("A", Set.of())))
                .expectErrorSatisfies(error -> assertProtocol(error, 400, -32602, "Cursor is not supported by this unpaginated tool list")).verify();
    }

    @Test
    void shouldFreezePermissionsButUseNewSnapshotOnTheNextCall() {
        Set<String> rule = new HashSet<>(Set.of("order_status"));
        Set<String> grants = new HashSet<>(Set.of("order_status"));
        AgentMcpExecutionContext old = new AgentMcpExecutionContext("old", "A", "rule", 1, rule, grants);
        rule.clear();
        grants.clear();
        assertThrows(UnsupportedOperationException.class, () -> old.getAllowedTools().clear());
        AgentMcpDispatcher dispatcher = dispatcher(provider("order_status", input -> Mono.just(input.getArguments())));
        StepVerifier.create(dispatcher.dispatch(request("tools/call", "1", "order_status"), () -> old)).expectNextCount(1).verifyComplete();
        AgentMcpExecutionContext current = new AgentMcpExecutionContext("new", "A", "rule", 2, rule, grants);
        StepVerifier.create(dispatcher.dispatch(request("tools/call", "1", "order_status"), () -> current))
                .expectErrorSatisfies(error -> assertProtocol(error, 403, -32602, "Tool is not available")).verify();
    }

    @Test
    void shouldIgnoreForgedMetadataWhenCreatingInvocation() {
        AgentMcpRequest request = request("tools/call", "1", "order_status");
        ObjectNode params = request.getParams();
        ((ObjectNode) params.get("_meta")).put("subject", "admin").putArray("allowedTools").add("order_status");
        AgentMcpRequest forged = new AgentMcpRequest(request.getId(), request.getMethod(), params);
        AgentMcpDispatcher dispatcher = dispatcher(provider("order_status", input -> {
            assertEquals("A", input.getSubject());
            return Mono.just(input.getArguments());
        }));
        StepVerifier.create(dispatcher.dispatch(forged, () -> context("A", Set.of())))
                .expectErrorSatisfies(error -> assertProtocol(error, 403, -32602, "Tool is not available")).verify();
        StepVerifier.create(dispatcher.dispatch(forged, () -> context("A", Set.of("order_status")))).expectNextCount(1).verifyComplete();
    }

    @Test
    void shouldRestoreOuterContextAndPreserveTrustedContextAcrossSchedulers() {
        AgentMcpExecutionContext outer = context("outer", Set.of());
        AgentMcpDispatcher dispatcher = dispatcher(provider("order_status", input -> Mono.deferContextual(reactorContext -> {
            AgentMcpExecutionContext trusted = reactorContext.get(AgentMcpExecutionContext.class);
            assertEquals("A", trusted.getSubject());
            assertEquals("rule", trusted.getRuleId());
            assertEquals(1, trusted.getConfigurationVersion());
            assertEquals(input.getRequestId(), trusted.getRequestId());
            assertNotSame(outer, trusted);
            return Mono.just(input.getArguments());
        }).subscribeOn(Schedulers.parallel()).publishOn(Schedulers.boundedElastic())));
        StepVerifier.create(dispatcher.dispatch(request("tools/call", "1", "order_status"), () -> context("A", Set.of("order_status")))
                .flatMap(response -> Mono.deferContextual(context -> {
                    assertEquals(outer, context.get(AgentMcpExecutionContext.class));
                    return Mono.just(response);
                })).contextWrite(context -> context.put(AgentMcpExecutionContext.class, outer))).expectNextCount(1).verifyComplete();
    }

    @Test
    void shouldKeep32ConcurrentCallsWithSameRpcIdIndependent() {
        AtomicInteger started = new AtomicInteger();
        Set<String> ids = ConcurrentHashMap.newKeySet();
        Sinks.One<Boolean> release = Sinks.one();
        AgentMcpDispatcher dispatcher = dispatcher(provider("order_status", input -> Mono.deferContextual(context -> {
            AgentMcpExecutionContext trusted = context.get(AgentMcpExecutionContext.class);
            assertEquals(input.getSubject(), trusted.getSubject());
            assertEquals(input.getSubject(), input.getArguments().get("owner").getAsString());
            ids.add(input.getRequestId());
            if (started.incrementAndGet() == 32) {
                assertEquals(Sinks.EmitResult.OK, release.tryEmitValue(true));
            }
            return release.asMono().map(ignored -> {
                JsonObject result = input.getArguments();
                result.addProperty("subject", input.getSubject());
                return result;
            });
        }).subscribeOn(Schedulers.parallel())));
        StepVerifier.create(Flux.range(0, 32).flatMap(index -> {
            AgentMcpRequest request = request("tools/call", "\"same-id\"", "order_status");
            ObjectNode params = request.getParams();
            params.putObject("arguments").put("owner", "owner-" + index);
            AgentMcpRequest call = new AgentMcpRequest(request.getId(), request.getMethod(), params);
            return dispatcher.dispatch(call, () -> context("owner-" + index, Set.of("order_status"))).map(response -> {
                assertEquals("\"same-id\"", response.get("id").toString());
                assertEquals("owner-" + index, response.path("result").path("structuredContent").path("subject").textValue());
                assertEquals("owner-" + index, response.path("result").path("structuredContent").path("owner").textValue());
                return response;
            });
        }, 32)).expectNextCount(32).verifyComplete();
        assertEquals(32, started.get());
        assertEquals(32, ids.size());
    }

    @Test
    void shouldCancelOneCallWithoutAffectingSurvivorWithTheSameRpcId() {
        AtomicBoolean cancelled = new AtomicBoolean();
        Sinks.One<JsonObject> survivor = Sinks.one();
        AgentMcpDispatcher dispatcher = dispatcher(provider("order_status", input -> {
            if ("A".equals(input.getSubject())) {
                return Mono.<JsonObject>never().doOnCancel(() -> cancelled.set(true));
            }
            return survivor.asMono();
        }));
        AgentMcpRequest request = request("tools/call", "\"same-id\"", "order_status");
        StepVerifier.create(dispatcher.dispatch(request, () -> context("B", Set.of("order_status"))))
                .then(() -> {
                    StepVerifier.create(dispatcher.dispatch(request, () -> context("A", Set.of("order_status")))).thenCancel().verify();
                    assertTrue(cancelled.get());
                    JsonObject result = new JsonObject();
                    result.addProperty("owner", "B");
                    assertEquals(Sinks.EmitResult.OK, survivor.tryEmitValue(result));
                }).assertNext(response -> assertEquals("B", response.path("result").path("structuredContent").path("owner").textValue())).verifyComplete();
    }

    @Test
    void shouldPropagateCallerTimeoutAndNotCancelNormalCompletion() {
        AtomicBoolean cancelled = new AtomicBoolean();
        AgentMcpDispatcher waiting = dispatcher(provider("order_status", input -> Mono.<JsonObject>never().doOnCancel(() -> cancelled.set(true))));
        StepVerifier.withVirtualTime(() -> waiting.dispatch(request("tools/call", "1", "order_status"), () -> context("A", Set.of("order_status")))
                .timeout(Duration.ofSeconds(1))).expectSubscription().thenAwait(Duration.ofSeconds(1)).expectError(java.util.concurrent.TimeoutException.class).verify();
        assertTrue(cancelled.get());
        cancelled.set(false);
        AgentMcpDispatcher completed = dispatcher(provider("order_status", input -> Mono.just(input.getArguments()).doOnCancel(() -> cancelled.set(true))));
        StepVerifier.create(completed.dispatch(request("tools/call", "1", "order_status"), () -> context("A", Set.of("order_status"))))
                .expectNextCount(1).verifyComplete();
        assertFalse(cancelled.get());
    }

    @Test
    void shouldMapContextFactoryFailureToSanitizedInternalError() {
        StepVerifier.create(dispatcher().dispatch(request("server/discover", "1", "order_status"), () -> {
            throw new IllegalStateException("private token");
        })).expectErrorSatisfies(error -> assertProtocol(error, 500, -32603, "Internal error")).verify();
        assertThrows(IllegalArgumentException.class, () -> context("", Set.of()));
        assertThrows(IllegalArgumentException.class, () -> new AgentMcpExecutionContext("id", "A", "rule", -1, Set.of(), Set.of()));
    }

    private void assertProtocol(final Throwable error, final int status, final int code, final String message) {
        assertTrue(error instanceof AgentMcpProtocolException);
        AgentMcpProtocolException failure = (AgentMcpProtocolException) error;
        assertEquals(status, failure.getHttpStatus());
        assertEquals(code, failure.getCode());
        assertEquals(message, failure.toResponse().path("error").path("message").textValue());
        assertFalse(failure.toResponse().has("result"));
    }

    @Test
    void shouldMapMissingCapabilityToProtocolErrorAndPreserveTheRpcId() {
        AtomicInteger validation = new AtomicInteger();
        AtomicInteger calls = new AtomicInteger();
        AgentToolProvider tool = provider("order_status", JsonParser.parseString("{\"sampling\":{}}").getAsJsonObject(),
                input -> validation.incrementAndGet(), input -> {
                    calls.incrementAndGet();
                    return Mono.just(input.getArguments());
                });
        AgentMcpDispatcher dispatcher = dispatcher(tool);
        StepVerifier.create(dispatcher.dispatch(request("tools/call", "\"shared-id\"", "order_status"), () -> context("A", Set.of("order_status"))))
                .expectErrorSatisfies(error -> {
                    AgentMcpProtocolException failure = (AgentMcpProtocolException) error;
                    assertEquals(400, failure.getHttpStatus());
                    assertEquals(-32021, failure.getCode());
                    assertEquals("shared-id", failure.toResponse().get("id").textValue());
                    assertTrue(failure.toResponse().path("error").path("data").path("requiredCapabilities").path("sampling").isObject());
                }).verify();
        assertEquals(0, validation.get());
        assertEquals(0, calls.get());
    }

    @Test
    void shouldKeepDeclaredCapabilitiesRequestLocalAndSeparateFromAuthorization() {
        AtomicInteger calls = new AtomicInteger();
        AgentToolProvider tool = provider("order_status", JsonParser.parseString("{\"sampling\":{}}").getAsJsonObject(), input -> { }, input -> {
            calls.incrementAndGet();
            assertTrue(input.getClientCapabilities().get("sampling").isJsonObject());
            input.getClientCapabilities().remove("sampling");
            assertTrue(input.getClientCapabilities().has("sampling"));
            return Mono.just(input.getArguments());
        });
        AgentMcpDispatcher dispatcher = dispatcher(tool);
        AgentMcpRequest original = request("tools/call", "1", "order_status");
        ObjectNode params = original.getParams();
        ((ObjectNode) params.path("_meta")).putObject("io.modelcontextprotocol/clientCapabilities").putObject("sampling");
        AgentMcpRequest capable = new AgentMcpRequest(original.getId(), original.getMethod(), params);
        ((ObjectNode) params.path("_meta")).putObject("io.modelcontextprotocol/clientCapabilities");
        StepVerifier.create(dispatcher.dispatch(capable, () -> context("A", Set.of("order_status")))).expectNextCount(1).verifyComplete();
        StepVerifier.create(dispatcher.dispatch(original, () -> context("A", Set.of("order_status"))))
                .expectErrorSatisfies(error -> assertEquals(-32021, ((AgentMcpProtocolException) error).getCode())).verify();
        StepVerifier.create(dispatcher.dispatch(capable, () -> context("B", Set.of())))
                .expectErrorSatisfies(error -> {
                    assertEquals(403, ((AgentMcpProtocolException) error).getHttpStatus());
                    assertFalse(((AgentMcpProtocolException) error).toResponse().path("error").has("data"));
                }).verify();
        assertEquals(1, calls.get());
    }

    private AgentMcpExecutionContext context(final String subject, final Set<String> grants) {
        return new AgentMcpExecutionContext("internal-" + subject, subject, "rule", 1, Set.of("order_status"), grants);
    }

    private AgentMcpDispatcher dispatcher(final AgentToolProvider... providers) {
        return new AgentMcpDispatcher(new AgentToolRegistry(List.of(providers)), "shenyu-agent-gateway", AgentGatewayConstants.MCP_SERVER_VERSION);
    }

    private AgentToolProvider provider(final String name, final Function<AgentToolInvocation, Mono<JsonObject>> action) {
        return provider(name, input -> { }, action);
    }

    private AgentToolProvider provider(final String name, final Consumer<JsonObject> validation, final Function<AgentToolInvocation, Mono<JsonObject>> action) {
        return provider(name, new JsonObject(), validation, action);
    }

    private AgentToolProvider provider(final String name, final JsonObject capabilities, final Consumer<JsonObject> validation,
                                       final Function<AgentToolInvocation, Mono<JsonObject>> action) {
        AgentToolProvider tool = org.mockito.Mockito.mock(AgentToolProvider.class);
        org.mockito.Mockito.when(tool.getName()).thenReturn(name);
        org.mockito.Mockito.when(tool.getDescription()).thenReturn("Read " + name);
        org.mockito.Mockito.when(tool.getInputSchema()).thenReturn(JsonParser.parseString("{\"type\":\"object\"}").getAsJsonObject());
        org.mockito.Mockito.when(tool.getRequiredClientCapabilities()).thenReturn(capabilities);
        org.mockito.Mockito.doAnswer(invocation -> {
            validation.accept(invocation.getArgument(0));
            return null;
        }).when(tool).validate(org.mockito.ArgumentMatchers.any());
        org.mockito.Mockito.when(tool.invoke(org.mockito.ArgumentMatchers.any())).thenAnswer(invocation -> action.apply(invocation.getArgument(0)));
        return tool;
    }

    private AgentMcpRequest request(final String method, final String id, final String name) {
        final String body = "{\"jsonrpc\":\"2.0\",\"id\":" + id + ",\"method\":\"" + method + "\",\"params\":{\"name\":\"" + name + "\",\"_meta\":{"
                + "\"io.modelcontextprotocol/protocolVersion\":\"2026-07-28\",\"io.modelcontextprotocol/clientCapabilities\":{}}}}";
        HttpHeaders headers = new HttpHeaders();
        headers.set("MCP-Protocol-Version", "2026-07-28");
        headers.set("Mcp-Method", method);
        headers.set("Mcp-Name", name);
        return parser.parse(body.getBytes(StandardCharsets.UTF_8), headers, 262144);
    }
}
