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

package org.apache.shenyu.plugin.ai.proxy.enhanced;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.AiProxyHandle;
import org.apache.shenyu.plugin.ai.proxy.enhanced.handler.AiProxyPluginHandler;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiChat;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiResponses;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProviderFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAi;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyConfigService;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyEngine;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.WebClientAiProxyTransport;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Flux;
import reactor.netty.DisposableServer;
import reactor.netty.http.server.HttpServer;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

/**
 * Real HTTP transport exercised through the terminal plugin boundary.
 */
class AiProxyHttpCompatibilityTest {

    private static final Duration TIMEOUT = Duration.ofSeconds(5);

    private static final ObjectMapper MAPPER = new ObjectMapper();

    private static final String CHAT_REPLY = "{\"id\":\"chatcmpl-http\",\"model\":\"model\",\"choices\":[{\"index\":0,"
            + "\"message\":{\"role\":\"assistant\",\"content\":\"hello\"},\"finish_reason\":\"stop\"}],"
            + "\"usage\":{\"prompt_tokens\":3,\"completion_tokens\":2,\"total_tokens\":5}}";

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void convertsResponsesThroughThePluginAndRealHttpTransport(final boolean stream) throws Exception {
        AtomicReference<String> forwarded = new AtomicReference<>();
        String reply = stream ? "data: {\"id\":\"chatcmpl-http\",\"model\":\"model\",\"choices\":[{\"index\":0,"
                + "\"delta\":{\"role\":\"assistant\",\"content\":\"hello\"},\"finish_reason\":\"stop\"}]}\n\n"
                + "data: {\"choices\":[],\"usage\":{\"prompt_tokens\":3,\"completion_tokens\":2}}\n\ndata: [DONE]\n\n" : CHAT_REPLY;
        DisposableServer server = HttpServer.create().host("127.0.0.1").port(0).handle((request, response) -> {
            assertEquals("/v1/chat/completions", request.uri());
            assertEquals("Bearer real-key", request.requestHeaders().get("Authorization"));
            return request.receive().aggregate().asString().flatMap(body -> {
                forwarded.set(body);
                return response.header("Content-Type", stream ? "text/event-stream" : "application/json")
                        .header("ETag", "old-chat-representation").header("X-Upstream", "retained").sendString(Flux.just(reply)).then();
            });
        }).bindNow(TIMEOUT);
        try {
            MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/v1/responses")
                    .body("{\"model\":\"client-alias\",\"input\":\"hi\",\"max_output_tokens\":12,\"stream\":" + stream + "}"));
            StepVerifier.create(plugin(server, stream, OpenAiResponses.NAME).doExecute(exchange, mock(ShenyuPluginChain.class), selector(), new RuleData())).verifyComplete();
            JsonNode upstream = MAPPER.readTree(forwarded.get());
            assertEquals("model", upstream.get("model").asText());
            assertEquals("hi", upstream.at("/messages/0/content").asText());
            assertEquals(12, upstream.get("max_completion_tokens").asInt());
            assertTrue(!upstream.has("input"));
            String actual = exchange.getResponse().getBodyAsString().block(TIMEOUT);
            assertEquals(200, exchange.getResponse().getStatusCode().value());
            assertEquals("retained", exchange.getResponse().getHeaders().getFirst("X-Upstream"));
            assertTrue(!exchange.getResponse().getHeaders().containsKey("ETag"));
            if (stream) {
                assertTrue(actual.contains("event: response.output_text.delta\n"));
                assertEquals(1, actual.split("event: response.completed\n", -1).length - 1);
                assertTrue(actual.contains("\"input_tokens\":3"));
                assertTrue(!actual.contains("[DONE]"));
                assertTrue(upstream.at("/stream_options/include_usage").asBoolean());
            } else {
                JsonNode result = MAPPER.readTree(actual);
                assertEquals("response", result.get("object").asText());
                assertEquals("hello", result.at("/output/0/content/0/text").asText());
                assertEquals(5, result.at("/usage/total_tokens").asInt());
            }
        } finally {
            server.disposeNow(TIMEOUT);
        }
    }

    @Test
    void rejectsResponsesStateBeforeOpeningAnUpstreamConnection() {
        AtomicBoolean called = new AtomicBoolean();
        DisposableServer server = HttpServer.create().host("127.0.0.1").port(0).handle((request, response) -> {
            called.set(true);
            return response.send();
        }).bindNow(TIMEOUT);
        try {
            MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/v1/responses")
                    .body("{\"input\":\"hi\",\"previous_response_id\":\"resp_previous\"}"));
            StepVerifier.create(plugin(server, false, OpenAiResponses.NAME).doExecute(exchange, mock(ShenyuPluginChain.class), selector(), new RuleData())).verifyComplete();
            assertEquals(400, exchange.getResponse().getStatusCode().value());
            assertTrue(!called.get());
        } finally {
            server.disposeNow(TIMEOUT);
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void forwardsTheFirstChunkWithoutWaitingForCompletionAndCancelsTheSocket(final boolean stream) {
        AtomicBoolean cancelled = new AtomicBoolean();
        String first = stream ? "data: {\"unknown\":true}\n\n" : "{\"partial\":";
        DisposableServer server = HttpServer.create().host("127.0.0.1").port(0).handle((request, response) -> {
            assertEquals("/v1/chat/completions", request.uri());
            assertEquals("Bearer real-key", request.requestHeaders().get("Authorization"));
            return response.header("Content-Type", stream ? "text/event-stream" : "application/json")
                    .header("X-Upstream", "retained").sendString(Flux.just(first).concatWith(Flux.never()).doOnCancel(() -> cancelled.set(true)));
        }).bindNow(TIMEOUT);
        try {
            MockServerWebExchange exchange = exchange(stream);
            List<String> received = new java.util.concurrent.CopyOnWriteArrayList<>();
            exchange.getResponse().setWriteHandler(body -> body.doOnNext(buffer -> {
                received.add(buffer.toString(StandardCharsets.UTF_8));
                DataBufferUtils.release(buffer);
            }).then());
            StepVerifier.create(plugin(server, stream).doExecute(exchange, mock(ShenyuPluginChain.class), selector(), new RuleData()))
                    .then(() -> org.awaitility.Awaitility.await().atMost(TIMEOUT).until(() -> !received.isEmpty()))
                    .then(() -> assertEquals(List.of(first), received)).thenCancel().verify(TIMEOUT);
            org.awaitility.Awaitility.await().atMost(TIMEOUT).untilTrue(cancelled);
            assertEquals("retained", exchange.getResponse().getHeaders().getFirst("X-Upstream"));
        } finally {
            server.disposeNow(TIMEOUT);
        }
    }

    @Test
    void cancelsTheResponsesUpstreamSocketWithoutCompletingTheStream() {
        AtomicBoolean cancelled = new AtomicBoolean();
        String first = "data: {\"choices\":[{\"index\":0,\"delta\":{\"content\":\"hello\"}}]}\n\n";
        DisposableServer server = HttpServer.create().host("127.0.0.1").port(0).handle((request, response) ->
                response.header("Content-Type", "text/event-stream")
                        .sendString(Flux.just(first).concatWith(Flux.never()).doOnCancel(() -> cancelled.set(true)))).bindNow(TIMEOUT);
        try {
            MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/v1/responses")
                    .body("{\"input\":\"hi\",\"stream\":true}"));
            List<String> received = new java.util.concurrent.CopyOnWriteArrayList<>();
            exchange.getResponse().setWriteHandler(body -> body.doOnNext(buffer -> {
                received.add(buffer.toString(StandardCharsets.UTF_8));
                DataBufferUtils.release(buffer);
            }).then());
            StepVerifier.create(plugin(server, true, OpenAiResponses.NAME).doExecute(exchange, mock(ShenyuPluginChain.class), selector(), new RuleData()))
                    .then(() -> org.awaitility.Awaitility.await().atMost(TIMEOUT)
                            .until(() -> received.stream().anyMatch(event -> event.startsWith("event: response.output_text.delta\n"))))
                    .thenCancel().verify(TIMEOUT);
            org.awaitility.Awaitility.await().atMost(TIMEOUT).untilTrue(cancelled);
            assertTrue(received.stream().noneMatch(event -> event.startsWith("event: response.completed\n")));
        } finally {
            server.disposeNow(TIMEOUT);
        }
    }

    @Test
    void forwardsNormalBodiesBeyondTheDefaultWebClientCodecLimit() {
        String responseBody = "{\"unknown\":\"" + "x".repeat(512 * 1024) + "\"}";
        DisposableServer server = HttpServer.create().host("127.0.0.1").port(0)
                .handle((request, response) -> response.status(202).header("Content-Type", "application/json")
                        .sendString(Flux.just(responseBody))).bindNow(TIMEOUT);
        try {
            MockServerWebExchange exchange = exchange(false);
            StepVerifier.create(plugin(server, false).doExecute(exchange, mock(ShenyuPluginChain.class), selector(), new RuleData())).verifyComplete();
            assertEquals(202, exchange.getResponse().getStatusCode().value());
            assertEquals(responseBody, exchange.getResponse().getBodyAsString().block(TIMEOUT));
            assertTrue(exchange.getResponse().isCommitted());
        } finally {
            server.disposeNow(TIMEOUT);
        }
    }

    private AiProxyPlugin plugin(final DisposableServer server, final boolean stream) {
        return plugin(server, stream, OpenAiChat.NAME);
    }

    private AiProxyPlugin plugin(final DisposableServer server, final boolean stream, final String protocol) {
        final AiProxyPluginHandler handler = new AiProxyPluginHandler();
        AiProxyHandle handle = new AiProxyHandle();
        handle.setProvider("OpenAI");
        handle.setBaseUrl("http://127.0.0.1:" + server.port());
        handle.setApiKey("real-key");
        handle.setStream(stream);
        handle.setProtocol(protocol);
        handle.setModel("model");
        handler.getSelectorCachedHandle().cachedHandle(CacheKeyUtils.INST.getKey("http-selector", Constants.DEFAULT_RULE), handle);
        AiProxyEngine engine = new AiProxyEngine(new AiProxyProtocolFactory(List.of(new OpenAiChat(), new OpenAiResponses())), new AiProxyProviderFactory(List.of(new OpenAi())),
                new WebClientAiProxyTransport(WebClient.builder().build()), Duration.ZERO);
        return new AiProxyPlugin(new AiProxyConfigService(), engine, handler);
    }

    private SelectorData selector() {
        SelectorData selector = new SelectorData();
        selector.setId("http-selector");
        return selector;
    }

    private MockServerWebExchange exchange(final boolean stream) {
        return MockServerWebExchange.from(MockServerHttpRequest.post("/chat").body(
                "{\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}],\"stream\":" + stream + "}"));
    }
}
