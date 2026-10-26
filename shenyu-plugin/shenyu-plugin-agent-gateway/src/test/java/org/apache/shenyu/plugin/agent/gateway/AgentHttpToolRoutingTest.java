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

package org.apache.shenyu.plugin.agent.gateway;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.metrics.AgentMcpCallObserver.Outcome;
import org.apache.shenyu.plugin.agent.gateway.handler.AgentGatewayPluginDataHandler;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpCallTracer;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpDispatcher;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpExecutionContext;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpHttpHandler;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpRequestParser;
import org.apache.shenyu.plugin.agent.gateway.security.AgentMcpIdentity;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolRegistry;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.web.handler.ShenyuWebHandler;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.http.MediaType;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Sinks;
import reactor.test.StepVerifier;
import reactor.util.context.Context;

import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Production Agent Gateway execution over the actual web-handler cursor.
 * The fixture selects the rule directly; it does not claim an Admin/WebSocket network test.
 */
class AgentHttpToolRoutingTest {

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void preservesTrustedTenantThroughConfiguredHttpDispatch(final String mode) {
        AtomicInteger calls = new AtomicInteger();
        Fixture fixture = new Fixture(mode, child -> Mono.deferContextual(context -> {
            AgentMcpExecutionContext trusted = context.get(AgentMcpExecutionContext.class);
            assertEquals("tenant-a", trusted.getTenant().orElseThrow());
            assertEquals("trusted", trusted.getSubject());
            assertEquals(Set.of("lookup"), trusted.getAllowedTools());
            calls.incrementAndGet();
            return write(child, "{\"orderId\":\"A\"}");
        }), null, "tenant-a");
        MockServerWebExchange call = request("tools/call", "A", false);
        StepVerifier.create(fixture.handler.handle(call)).verifyComplete();
        assertEquals(200, call.getResponse().getStatusCode().value());
        assertEquals(1, calls.get());
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void preservesTracingContextThroughConfiguredHttpDispatch(final String mode) {
        final Object span = new Object();
        final AtomicInteger starts = new AtomicInteger();
        final AtomicInteger finishes = new AtomicInteger();
        final AtomicInteger calls = new AtomicInteger();
        AgentMcpCallTracer tracer = parent -> {
            assertEquals("caller-parent", parent.get("test-parent"));
            starts.incrementAndGet();
            return new AgentMcpCallTracer.Trace() {
                @Override
                public Context attach(final Context context) {
                    return context.put("test-span", span);
                }

                @Override
                public void finish(final Outcome outcome) {
                    assertEquals(Outcome.SUCCESS, outcome);
                    finishes.incrementAndGet();
                }
            };
        };
        Fixture fixture = new Fixture(mode, child -> Mono.deferContextual(context -> {
            assertEquals("caller-parent", context.get("test-parent"));
            assertSame(span, context.get("test-span"));
            calls.incrementAndGet();
            return write(child, "{\"orderId\":\"A\"}");
        }), tracer);
        MockServerWebExchange call = request("tools/call", "A", false);
        StepVerifier.create(fixture.handler.handle(call).contextWrite(Context.of("test-parent", "caller-parent"))).verifyComplete();
        assertEquals(200, call.getResponse().getStatusCode().value());
        assertEquals("A", result(call).getAsJsonObject("structuredContent").getAsJsonObject("body").get("orderId").getAsString());
        assertEquals(1, calls.get());
        assertEquals(1, starts.get());
        assertEquals(1, finishes.get());
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void listsAndCallsConfiguredHttpToolsWithoutPerToolJavaBeans(final String mode) {
        AtomicInteger calls = new AtomicInteger();
        Fixture fixture = new Fixture(mode, child -> {
            calls.incrementAndGet();
            assertEquals("/orders/status", child.getRequest().getURI().getPath());
            assertFalse(child.getRequest().getHeaders().containsKey("Mcp-Name"));
            return write(child, "{\"orderId\":\"" + child.getRequest().getQueryParams().getFirst("id") + "\",\"privateNote\":\"secret\"}");
        });
        MockServerWebExchange list = request("tools/list", "A", false);
        StepVerifier.create(fixture.handler.handle(list)).verifyComplete();
        assertTrue(list.getResponse().getBodyAsString().block().contains("lookup"));
        assertEquals(0, calls.get());
        MockServerWebExchange call = request("tools/call", "A", false);
        StepVerifier.create(fixture.handler.handle(call)).verifyComplete();
        assertEquals(200, call.getResponse().getStatusCode().value());
        assertEquals("A", result(call).getAsJsonObject("structuredContent").getAsJsonObject("body").get("orderId").getAsString());
        assertEquals(1, calls.get());
        assertFalse(call.getAttributes().containsKey(AgentGatewayConstants.HTTP_TOOL_CALL_ATTRIBUTE));
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void preservesAuthorizationBeforeTheHttpSideEffect(final String mode) {
        AtomicInteger calls = new AtomicInteger();
        Fixture fixture = new Fixture(mode, child -> {
            calls.incrementAndGet();
            return write(child, "{}");
        });
        MockServerWebExchange denied = request("tools/call", "A", true);
        StepVerifier.create(fixture.handler.handle(denied)).verifyComplete();
        assertEquals(403, denied.getResponse().getStatusCode().value());
        assertEquals(0, calls.get());
    }

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void filtersHttpResultAndItsTextCopyUsingTheMergedPolicy(final String mode) {
        Fixture fixture = new Fixture(mode, child -> write(child, "{\"id\":\"A\",\"privateNote\":\"secret\"}"));
        JsonObject settings = config(mode, "/orders/status");
        settings.getAsJsonObject("mcp").add("structuredContentRemovals", JsonParser.parseString("{\"lookup\":[\"/body/privateNote\"]}"));
        fixture.update(settings);
        MockServerWebExchange call = request("tools/call", "A", false);
        StepVerifier.create(fixture.handler.handle(call)).verifyComplete();
        assertEquals(200, call.getResponse().getStatusCode().value());
        String body = call.getResponse().getBodyAsString().block();
        assertFalse(body.contains("privateNote"));
        assertFalse(body.contains("secret"));
        assertEquals("A", result(call).getAsJsonObject("structuredContent").getAsJsonObject("body").get("id").getAsString());
    }

    @Test
    void configurationChangesKeepTheInFlightTargetAndUseNewDefinitionsWithoutRestart() {
        final Sinks.One<String> release = Sinks.one();
        final AtomicReference<String> firstTarget = new AtomicReference<>();
        Fixture fixture = new Fixture("json", child -> {
            String path = child.getRequest().getURI().getPath();
            if ("/orders/status".equals(path)) {
                firstTarget.set(path);
                return release.asMono().flatMap(value -> write(child, "{\"target\":\"" + value + "\"}"));
            }
            return write(child, "{\"target\":\"new\"}");
        });
        MockServerWebExchange old = request("tools/call", "A", false);
        StepVerifier.create(fixture.handler.handle(old)).then(() -> {
            assertEquals("/orders/status", firstTarget.get());
            fixture.update(config("json", "/orders/updated"));
            MockServerWebExchange next = request("tools/call", "B", false);
            StepVerifier.create(fixture.handler.handle(next)).verifyComplete();
            assertEquals("new", result(next).getAsJsonObject("structuredContent").getAsJsonObject("body").get("target").getAsString());
            assertEquals(Sinks.EmitResult.OK, release.tryEmitValue("old"));
        }).verifyComplete();
        assertEquals("old", result(old).getAsJsonObject("structuredContent").getAsJsonObject("body").get("target").getAsString());
    }

    @Test
    void parallelSameRpcIdsHaveIndependentCursorsAndResponses() {
        Fixture fixture = new Fixture("json", child -> write(child, "{\"id\":\"" + child.getRequest().getQueryParams().getFirst("id") + "\"}"));
        StepVerifier.create(Flux.range(0, 32).flatMap(index -> {
            MockServerWebExchange exchange = request("tools/call", "order" + index, false);
            return fixture.handler.handle(exchange).then(Mono.fromSupplier(() -> result(exchange).getAsJsonObject("structuredContent")
                    .getAsJsonObject("body").get("id").getAsString()));
        }).collectList()).assertNext(values -> assertEquals(32, Set.copyOf(values).size())).verifyComplete();
    }

    @Test
    void ordinaryHttpTrafficStillPassesThroughTheOriginalChain() {
        Fixture fixture = new Fixture("json", child -> write(child, "{\"ordinary\":true}"));
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/orders/status").build());
        StepVerifier.create(fixture.handler.handle(exchange)).verifyComplete();
        assertEquals("{\"ordinary\":true}", exchange.getResponse().getBodyAsString().block());
    }

    private MockServerWebExchange request(final String method, final String id, final boolean denied) {
        String body = "{\"jsonrpc\":\"2.0\",\"id\":\"same-client-id\",\"method\":\"" + method
                + "\",\"params\":{\"name\":\"lookup\",\"arguments\":{\"id\":\"" + id + "\"},\"_meta\":{"
                + "\"io.modelcontextprotocol/protocolVersion\":\"" + AgentMcpRequestParser.VERSION + "\",\"io.modelcontextprotocol/clientCapabilities\":{}}}}";
        MockServerHttpRequest.BodyBuilder builder = MockServerHttpRequest.post("/agent").contentType(MediaType.APPLICATION_JSON)
                .header("Accept", "application/json, text/event-stream").header("MCP-Protocol-Version", AgentMcpRequestParser.VERSION)
                .header("Mcp-Method", method).header("Test-Denied", Boolean.toString(denied));
        if ("tools/call".equals(method)) {
            builder.header("Mcp-Name", "lookup");
        }
        return MockServerWebExchange.from(builder.body(body));
    }

    private JsonObject result(final MockServerWebExchange exchange) {
        String wire = exchange.getResponse().getBodyAsString().block();
        String json = wire.startsWith("event:") ? wire.substring(wire.indexOf("data: ") + 6).trim() : wire;
        return JsonParser.parseString(json).getAsJsonObject().getAsJsonObject("result");
    }

    private static JsonObject config(final String mode, final String path) {
        JsonObject config = JsonParser.parseString("""
                {"trafficType":"mcp","mcp":{"allowedTools":["lookup"],"httpTools":{"lookup":{"description":"Lookup an order",
                "inputSchema":{"type":"object","properties":{"id":{"type":"string"}},"required":["id"]},
                "requestConfig":{"requestTemplate":{"method":"GET","url":"/orders/status"},"argsPosition":{"id":"query"}}}}}}
                """).getAsJsonObject();
        JsonObject mcp = config.getAsJsonObject("mcp");
        mcp.addProperty("responseMode", mode);
        mcp.getAsJsonObject("httpTools").getAsJsonObject("lookup").getAsJsonObject("requestConfig")
                .getAsJsonObject("requestTemplate").addProperty("url", path);
        return config;
    }

    private static Mono<Void> write(final ServerWebExchange exchange, final String body) {
        exchange.getResponse().getHeaders().setContentType(MediaType.APPLICATION_JSON);
        return exchange.getResponse().writeWith(Mono.just(exchange.getResponse().bufferFactory().wrap(body.getBytes(StandardCharsets.UTF_8))));
    }

    private static final class Fixture {

        private final SelectedGateway gateway;

        private final ShenyuWebHandler handler;

        Fixture(final String mode, final Function<ServerWebExchange, Mono<Void>> action) {
            this(mode, action, null);
        }

        Fixture(final String mode, final Function<ServerWebExchange, Mono<Void>> action, final AgentMcpCallTracer tracer) {
            this(mode, action, tracer, null);
        }

        Fixture(final String mode, final Function<ServerWebExchange, Mono<Void>> action, final AgentMcpCallTracer tracer, final String tenant) {
            AgentMcpHttpHandler mcp = new AgentMcpHttpHandler(new AgentMcpDispatcher(new AgentToolRegistry(List.of()), "server", "1"),
                    exchange -> {
                        Set<String> grants = "true".equals(exchange.getRequest().getHeaders().getFirst("Test-Denied")) ? Set.of() : Set.of("lookup");
                        return Mono.just(Objects.isNull(tenant) ? new AgentMcpIdentity("trusted", grants) : new AgentMcpIdentity(tenant, "trusted", grants, Set.of()));
                    }, tracer);
            gateway = new SelectedGateway(mcp);
            update(config(mode, "/orders/status"));
            handler = new ShenyuWebHandler(List.of(gateway, new Route(action)), null, new ShenyuConfig());
        }

        void update(final JsonObject config) {
            RuleData rule = new RuleData();
            rule.setId(UUID.randomUUID().toString());
            rule.setSelectorId("http-selector");
            rule.setHandle(config.toString());
            new AgentGatewayPluginDataHandler().handlerRule(rule);
            gateway.rule.set(rule);
        }
    }

    private static final class SelectedGateway extends AgentGatewayPlugin {

        private final AtomicReference<RuleData> rule = new AtomicReference<>();

        SelectedGateway(final AgentMcpHttpHandler handler) {
            super(handler);
        }

        @Override
        public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
            if (!"/agent".equals(exchange.getRequest().getURI().getPath())) {
                return chain.execute(exchange);
            }
            SelectorData selector = new SelectorData();
            selector.setId("http-selector");
            selector.setContinued(true);
            return doExecute(exchange, chain, selector, rule.get());
        }
    }

    private static final class Route implements ShenyuPlugin {

        private final Function<ServerWebExchange, Mono<Void>> action;

        Route(final Function<ServerWebExchange, Mono<Void>> action) {
            this.action = action;
        }

        @Override
        public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
            return action.apply(exchange);
        }

        @Override
        public int getOrder() {
            return 200;
        }

        @Override
        public String named() {
            return "test-http-route";
        }
    }
}
