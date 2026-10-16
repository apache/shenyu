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
import org.apache.shenyu.common.dto.AgentGatewayMcpConfig;
import org.apache.shenyu.plugin.agent.gateway.AgentTrafficContext;
import org.apache.shenyu.plugin.agent.gateway.resource.AgentResourceContent;
import org.apache.shenyu.plugin.agent.gateway.resource.AgentResourceDefinition;
import org.apache.shenyu.plugin.agent.gateway.resource.AgentResourceInvocation;
import org.apache.shenyu.plugin.agent.gateway.resource.AgentResourceProvider;
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
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AgentMcpResourceIntegrationTest {

    private final ObjectMapper mapper = new ObjectMapper();

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void listsReadsAndAdvertisesOnlyConfiguredResources(final String mode) {
        AgentMcpHttpHandler handler = handler(input -> Mono.just(List.of(AgentResourceContent.text(input.getUri(), "text/plain", input.getContext().getSubject()))),
                Set.of("status"));
        for (String method : List.of("server/discover", "resources/list", "resources/templates/list", "resources/read")) {
            MockServerWebExchange exchange = exchange(method, "test://status");
            handler.handle(exchange, config(mode), traffic("request"), 1).block(Duration.ofSeconds(3));
            assertEquals(200, exchange.getResponse().getStatusCode().value());
            String wire = exchange.getResponse().getBodyAsString().block();
            assertTrue(wire.contains("\"resultType\":\"complete\""));
            if ("resources/read".equals(method)) {
                assertTrue(wire.contains("\"text\":\"trusted-owner\""));
            } else if ("server/discover".equals(method)) {
                assertTrue(wire.contains("\"resources\":{}"));
            }
        }
    }

    @Test
    void toolGrantsDoNotAuthorizeResourceReadsOrLists() {
        AtomicInteger reads = new AtomicInteger();
        AgentMcpHttpHandler handler = handler(input -> {
            reads.incrementAndGet();
            return Mono.just(List.of(AgentResourceContent.text(input.getUri(), null, "private")));
        }, Set.of());
        MockServerWebExchange listed = exchange("resources/list", "test://status");
        handler.handle(listed, config("json"), traffic("list"), 1).block();
        assertTrue(listed.getResponse().getBodyAsString().block().contains("\"resources\":[]"));
        MockServerWebExchange denied = exchange("resources/read", "test://status");
        handler.handle(denied, config("json"), traffic("read"), 1).block();
        assertEquals(403, denied.getResponse().getStatusCode().value());
        assertFalse(denied.getResponse().getBodyAsString().block().contains("private"));
        assertEquals(0, reads.get());
    }

    @Test
    void mirroredUriMismatchRejectsBeforeProvider() {
        AtomicInteger reads = new AtomicInteger();
        AgentMcpHttpHandler handler = handler(input -> {
            reads.incrementAndGet();
            return Mono.just(List.of(AgentResourceContent.text(input.getUri(), null, "")));
        }, Set.of("status"));
        MockServerWebExchange exchange = MockServerWebExchange.from(builder("resources/read").header("Mcp-Name", "test://wrong").body(body("resources/read", "test://status")));
        handler.handle(exchange, config("json"), traffic("read"), 1).block();
        assertEquals(400, exchange.getResponse().getStatusCode().value());
        assertEquals(0, reads.get());
    }

    @Test
    void cancellationAndSameRpcIdRemainRequestOwned() {
        AtomicBoolean cancelled = new AtomicBoolean();
        Sinks.One<List<AgentResourceContent>> survivor = Sinks.one();
        AtomicInteger calls = new AtomicInteger();
        AgentMcpHttpHandler handler = handler(input -> calls.incrementAndGet() == 1
                ? Mono.<List<AgentResourceContent>>never().doOnCancel(() -> cancelled.set(true)) : survivor.asMono(), Set.of("status"));
        var first = handler.handle(exchange("resources/read", "test://status"), config("json"), traffic("first"), 1).subscribe();
        MockServerWebExchange second = exchange("resources/read", "test://status");
        StepVerifier.create(handler.handle(second, config("json"), traffic("second"), 1))
                .then(first::dispose).then(() -> assertTrue(cancelled.get()))
                .then(() -> survivor.tryEmitValue(List.of(AgentResourceContent.text("test://status", null, "survivor")))).verifyComplete();
        assertEquals(200, second.getResponse().getStatusCode().value());
        assertTrue(second.getResponse().getBodyAsString().block().contains("survivor"));
    }

    @Test
    void parallelSchedulerReadsCarryPrivateTrafficContext() {
        AgentMcpHttpHandler handler = handler(input -> Mono.deferContextual(context -> {
            AgentMcpExecutionContext trusted = context.get(AgentMcpExecutionContext.class);
            assertEquals(trusted.getRequestId(), input.getContext().getRequestId());
            return Mono.just(List.of(AgentResourceContent.text(input.getUri(), null, trusted.getRequestId())));
        }).subscribeOn(Schedulers.parallel()), Set.of("status"));
        List<String> outputs = Flux.range(0, 32).flatMap(index -> {
            MockServerWebExchange exchange = exchange("resources/read", "test://status");
            return handler.handle(exchange, config("json"), traffic("request-" + index), 1)
                    .then(Mono.defer(exchange.getResponse()::getBodyAsString));
        }).collectList().block(Duration.ofSeconds(5));
        assertEquals(32, outputs.stream().distinct().count());
    }

    private AgentMcpHttpHandler handler(final Function<AgentResourceInvocation, Mono<List<AgentResourceContent>>> reads, final Set<String> grants) {
        AgentResourceProvider provider = new AgentResourceProvider() {
            @Override
            public AgentResourceDefinition getDefinition() {
                return new AgentResourceDefinition("status", "test://status", "Status", "text/plain", false);
            }

            @Override
            public Mono<List<AgentResourceContent>> read(final AgentResourceInvocation invocation) {
                return reads.apply(invocation);
            }
        };
        AgentMcpDispatcher dispatcher = new AgentMcpDispatcher(new AgentToolRegistry(List.of()), "server", "1", null, new AgentResourceRegistry(List.of(provider)));
        return new AgentMcpHttpHandler(dispatcher, exchange -> Mono.just(new AgentMcpIdentity("trusted-owner", Set.of("status"), grants)));
    }

    private AgentGatewayMcpConfig config(final String mode) {
        return AgentGatewayMcpConfig.parse(JsonParser.parseString("{\"allowedResources\":[\"status\"],\"responseMode\":\"" + mode + "\"}"), true);
    }

    private AgentTrafficContext traffic(final String id) {
        return new AgentTrafficContext(id, "mcp", "selector", "rule");
    }

    private MockServerWebExchange exchange(final String method, final String uri) {
        var builder = builder(method);
        if ("resources/read".equals(method)) {
            builder.header("Mcp-Name", uri);
        }
        return MockServerWebExchange.from(builder.body(body(method, uri)));
    }

    private MockServerHttpRequest.BodyBuilder builder(final String method) {
        return MockServerHttpRequest.post("/agent").contentType(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.ACCEPT, "application/json, text/event-stream")
                .header("MCP-Protocol-Version", AgentMcpRequestParser.VERSION).header("Mcp-Method", method);
    }

    private String body(final String method, final String uri) {
        ObjectNode request = mapper.createObjectNode().put("jsonrpc", "2.0").put("id", "same-id").put("method", method);
        ObjectNode params = request.putObject("params").put("uri", uri);
        params.putObject("_meta").put("io.modelcontextprotocol/protocolVersion", AgentMcpRequestParser.VERSION)
                .putObject("io.modelcontextprotocol/clientCapabilities");
        return request.toString();
    }
}
