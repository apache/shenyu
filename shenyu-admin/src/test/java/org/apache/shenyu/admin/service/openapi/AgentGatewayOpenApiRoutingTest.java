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

package org.apache.shenyu.admin.service.openapi;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.apache.shenyu.admin.model.result.AgentGatewayOpenApiPreview.Item;
import org.apache.shenyu.admin.validation.validator.AgentGatewayRuleHandleValidator;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.dto.AgentGatewayHttpToolConfig.Tool;
import org.apache.shenyu.common.dto.AgentGatewayMcpConfig;
import org.apache.shenyu.plugin.agent.gateway.handle.AgentGatewayRuleHandleParser;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentHttpToolExecutor;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolInvocation;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.utils.RequestUrlUtils;
import org.apache.shenyu.web.handler.ShenyuWebHandler;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.MediaType;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.BiFunction;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Generated plans through production parsers, HTTP executor, web-handler fork and final URI builder.
 * Uses an in-process ordinary route/transport fixture, not Docker or a networked Admin sync test.
 */
class AgentGatewayOpenApiRoutingTest {

    @ParameterizedTest
    @CsvSource({"GET,false", "GET,true", "POST,false", "POST,true"})
    void generatedArgumentsSurviveTheOrdinaryGatewayChain(final String method, final boolean optional) {
        Item generated = new AgentGatewayOpenApiGenerator().generate(OpenApiImportFixture.request()).items().get(method + " /backend/orders");
        JsonObject handle = JsonParser.parseString("{\"trafficType\":\"mcp\",\"mcp\":{\"allowedTools\":[],\"httpTools\":{}}}").getAsJsonObject();
        handle.getAsJsonObject("mcp").getAsJsonArray("allowedTools").add(generated.toolName());
        handle.getAsJsonObject("mcp").getAsJsonObject("httpTools").add(generated.toolName(), JsonParser.parseString(generated.after()));
        AgentGatewayRuleHandleValidator.validate(handle.toString());
        assertTrue(new AgentGatewayRuleHandleParser().parse(handle.toString()).isValid());
        final Tool tool = AgentGatewayMcpConfig.parse(handle.get("mcp"), false).getHttpTools().getTools().get(generated.toolName());
        final String value = "A&B 中文 /+?=%";
        final JsonObject arguments = new JsonObject();
        arguments.addProperty("id", value);
        if (optional) {
            arguments.addProperty("verbose", true);
        }
        if ("POST".equals(method)) {
            arguments.addProperty("name", value);
            if (optional) {
                arguments.addProperty("count", 3);
            }
        }
        final AtomicReference<JsonObject> result = new AtomicReference<>();
        ShenyuPlugin entry = new Step("tool-entry", 0, (exchange, chain) -> {
            if (!"/agent".equals(exchange.getRequest().getPath().value())) {
                return chain.execute(exchange);
            }
            return new AgentHttpToolExecutor(exchange, chain, 4096, 4096)
                    .invoke(tool, new AgentToolInvocation("request", "trusted", arguments, "agent", 1, Instant.now().plusSeconds(30))).doOnNext(result::set).then();
        });
        final AtomicInteger calls = new AtomicInteger();
        ShenyuPlugin routing = new Step("ordinary-route", 100, (child, chain) -> {
            calls.incrementAndGet();
            String uri = RequestUrlUtils.buildRequestUri(child, "http://127.0.0.1:18080").toString();
            assertTrue(uri.startsWith("http://127.0.0.1:18080/orders/status?id="));
            assertEquals(1, uri.chars().filter(character -> character == '?').count());
            String query = uri.substring(uri.indexOf('?') + 1);
            String id = query.split("&")[0].substring(3);
            assertEquals(value, URLDecoder.decode(id, StandardCharsets.UTF_8));
            assertEquals(optional, query.contains("&verbose=true"));
            assertFalse(child.getRequest().getHeaders().containsKey("Authorization"));
            assertFalse(child.getRequest().getHeaders().containsKey("Mcp-Name"));
            if ("POST".equals(method)) {
                return DataBufferUtils.join(child.getRequest().getBody()).flatMap(buffer -> {
                    String json = buffer.toString(StandardCharsets.UTF_8);
                    DataBufferUtils.release(buffer);
                    JsonObject body = JsonParser.parseString(json).getAsJsonObject();
                    assertEquals(value, body.get("name").getAsString());
                    assertEquals(optional, body.has("count"));
                    assertFalse(body.has("id"));
                    return write(child);
                });
            }
            return write(child);
        });
        ShenyuWebHandler gateway = new ShenyuWebHandler(List.of(entry, routing), null, new ShenyuConfig());
        MockServerWebExchange caller = MockServerWebExchange.from(MockServerHttpRequest.post("/agent")
                .header("Authorization", "secret").header("Mcp-Name", generated.toolName()).body("private caller body"));
        StepVerifier.create(gateway.handle(caller)).verifyComplete();
        assertEquals(1, calls.get());
        assertEquals(200, result.get().get("httpStatus").getAsInt());
        assertEquals("成功", result.get().getAsJsonObject("body").get("status").getAsString());
        assertEquals("secret", caller.getRequest().getHeaders().getFirst("Authorization"));
    }

    private Mono<Void> write(final ServerWebExchange exchange) {
        exchange.getResponse().getHeaders().setContentType(MediaType.APPLICATION_JSON);
        byte[] bytes = "{\"status\":\"成功\"}".getBytes(StandardCharsets.UTF_8);
        return exchange.getResponse().writeWith(Mono.just(exchange.getResponse().bufferFactory().wrap(bytes)));
    }

    private record Step(String name, int order, BiFunction<ServerWebExchange, ShenyuPluginChain, Mono<Void>> action) implements ShenyuPlugin {

        @Override
        public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
            return action.apply(exchange, chain);
        }

        @Override
        public String named() {
            return name;
        }

        @Override
        public int getOrder() {
            return order;
        }
    }
}
