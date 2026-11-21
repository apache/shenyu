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
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.ProxyApiKeyData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.AiProxyHandle;
import org.apache.shenyu.plugin.ai.proxy.enhanced.handler.AiProxyPluginHandler;
import org.apache.shenyu.plugin.ai.proxy.enhanced.cache.AiProxyApiKeyCache;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiChat;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiResponses;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProviderFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAi;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAiCompatible;
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
import reactor.core.publisher.Mono;
import reactor.netty.DisposableServer;
import reactor.netty.http.server.HttpServer;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.apache.shenyu.plugin.ai.proxy.enhanced.AiProxyTestUtils.protocolRouteResolver;
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
            StepVerifier.create(plugin(server, stream, OpenAiResponses.NAME, "ChatOnly")
                    .doExecute(exchange, mock(ShenyuPluginChain.class), selector(), new RuleData())).verifyComplete();
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
    void proxyKeysRemainIsolatedAcrossSequentialAndConcurrentRequests() {
        // The engine replaced cached ChatClient instances; keep isolation covered at the HTTP boundary.
        DisposableServer server = HttpServer.create().host("127.0.0.1").port(0).handle((request, response) ->
                response.header("Content-Type", "application/json")
                        .header("X-Upstream-Account", request.requestHeaders().get("Authorization"))
                        .sendString(Flux.just(CHAT_REPLY))).bindNow(TIMEOUT);
        final AiProxyPluginHandler handler = new AiProxyPluginHandler();
        AiProxyHandle handle = new AiProxyHandle();
        handle.setProvider("OpenAI");
        handle.setBaseUrl("http://127.0.0.1:" + server.port());
        handle.setApiKey("selector-default");
        handle.setStream(false);
        handle.setProxyEnabled("true");
        handle.setProtocol(OpenAiChat.NAME);
        handle.setModel("model");
        handler.getSelectorCachedHandle().cachedHandle(CacheKeyUtils.INST.getKey("http-selector", Constants.DEFAULT_RULE), handle);
        AiProxyPlugin plugin = plugin(handler);
        for (String account : List.of("account-a", "account-b")) {
            AiProxyApiKeyCache.getInstance().cache(ProxyApiKeyData.builder().selectorId("http-selector")
                    .proxyApiKey("proxy-" + account).realApiKey(account).enabled(true).build());
        }
        try {
            for (String account : List.of("account-a", "account-b", "account-a")) {
                StepVerifier.create(executeForAccount(plugin, account)).verifyComplete();
            }
            StepVerifier.create(Flux.range(0, 32)
                    .flatMap(index -> executeForAccount(plugin, index % 2 == 0 ? "account-a" : "account-b"), 8)
                    .then()).expectComplete().verify(Duration.ofSeconds(15));
            assertEquals("selector-default", handle.getApiKey());
        } finally {
            server.disposeNow(TIMEOUT);
            for (String account : List.of("account-a", "account-b")) {
                AiProxyApiKeyCache.getInstance().remove(ProxyApiKeyData.builder().selectorId("http-selector")
                        .proxyApiKey("proxy-" + account).build());
            }
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void unknownOrDisabledProxyKeysCannotUseTheSelectorCredential(final boolean disabled) {
        AtomicBoolean called = new AtomicBoolean();
        DisposableServer server = HttpServer.create().host("127.0.0.1").port(0).handle((request, response) -> {
            called.set(true);
            return response.sendString(Mono.just(CHAT_REPLY));
        }).bindNow(TIMEOUT);
        final AiProxyPluginHandler handler = new AiProxyPluginHandler();
        AiProxyHandle handle = new AiProxyHandle();
        handle.setProvider("OpenAI");
        handle.setBaseUrl("http://127.0.0.1:" + server.port());
        handle.setApiKey("selector-default");
        handle.setProxyEnabled("true");
        handle.setStream(false);
        handle.setProtocol(OpenAiChat.NAME);
        handle.setModel("model");
        handler.getSelectorCachedHandle().cachedHandle(CacheKeyUtils.INST.getKey("http-selector", Constants.DEFAULT_RULE), handle);
        ProxyApiKeyData mapping = ProxyApiKeyData.builder().selectorId("http-selector")
                .proxyApiKey("rejected-proxy-key").realApiKey("disabled-real-key").enabled(false).build();
        if (disabled) {
            AiProxyApiKeyCache.getInstance().cache(mapping);
        }
        try {
            MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/chat")
                    .header(Constants.X_API_KEY, mapping.getProxyApiKey())
                    .body("{\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}],\"stream\":false}"));
            StepVerifier.create(plugin(handler).doExecute(exchange, mock(ShenyuPluginChain.class), selector(), new RuleData()))
                    .expectComplete().verify(TIMEOUT);
            assertEquals(401, exchange.getResponse().getStatusCode().value());
            assertTrue(!called.get(), "rejected proxy keys must not open an upstream connection");
            assertEquals("selector-default", handle.getApiKey());
        } finally {
            AiProxyApiKeyCache.getInstance().remove(mapping);
            server.disposeNow(TIMEOUT);
        }
    }

    private Mono<Void> executeForAccount(final AiProxyPlugin plugin, final String account) {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/chat")
                .header(Constants.X_API_KEY, "proxy-" + account)
                .body("{\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}],\"stream\":false}"));
        return plugin.doExecute(exchange, mock(ShenyuPluginChain.class), selector(), new RuleData())
                .then(Mono.fromRunnable(() -> {
                    assertEquals(200, exchange.getResponse().getStatusCode().value());
                    assertEquals("Bearer " + account, exchange.getResponse().getHeaders().getFirst("X-Upstream-Account"));
                }));
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
            StepVerifier.create(plugin(server, false, OpenAiResponses.NAME, "ChatOnly")
                    .doExecute(exchange, mock(ShenyuPluginChain.class), selector(), new RuleData())).verifyComplete();
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
            StepVerifier.create(plugin(server, true, OpenAiResponses.NAME, "ChatOnly")
                    .doExecute(exchange, mock(ShenyuPluginChain.class), selector(), new RuleData()))
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

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void appliesPluginDefaultsAndSelectorOverridesThroughRealHttp(final boolean override) throws Exception {
        AtomicReference<String> forwarded = new AtomicReference<>();
        AtomicReference<String> authorization = new AtomicReference<>();
        DisposableServer server = HttpServer.create().host("127.0.0.1").port(0).handle((request, response) -> {
            authorization.set(request.requestHeaders().get("Authorization"));
            return request.receive().aggregate().asString().flatMap(body -> {
                forwarded.set(body);
                return response.header("Content-Type", "application/json").sendString(Flux.just(CHAT_REPLY)).then();
            });
        }).bindNow(TIMEOUT);
        try {
            AiProxyPluginHandler handler = new AiProxyPluginHandler();
            PluginData global = new PluginData();
            global.setConfig(MAPPER.createObjectNode().put("provider", "OpenAI").put("baseUrl", "http://127.0.0.1:" + server.port())
                    .put("model", "global-model").put("apiKey", "global-key").put("maxTokens", 64).toString());
            handler.handlerPlugin(global);
            SelectorData selector = selector();
            selector.setHandle(override ? "{\"model\":\"selector-model\",\"apiKey\":\"selector-key\"}" : "{}");
            handler.handlerSelector(selector);
            MockServerWebExchange exchange = exchange(false);
            StepVerifier.create(plugin(handler).doExecute(exchange, mock(ShenyuPluginChain.class), selector, new RuleData())).verifyComplete();
            assertEquals(200, exchange.getResponse().getStatusCode().value());
            assertEquals("Bearer " + (override ? "selector-key" : "global-key"), authorization.get());
            JsonNode upstream = MAPPER.readTree(forwarded.get());
            assertEquals(override ? "selector-model" : "global-model", upstream.get("model").asText());
            assertEquals(64, upstream.get("max_completion_tokens").asInt());
            assertEquals(CHAT_REPLY, exchange.getResponse().getBodyAsString().block(TIMEOUT));
        } finally {
            server.disposeNow(TIMEOUT);
        }
    }

    private AiProxyPlugin plugin(final DisposableServer server, final boolean stream) {
        return plugin(server, stream, OpenAiChat.NAME);
    }

    private AiProxyPlugin plugin(final DisposableServer server, final boolean stream, final String protocol) {
        return plugin(server, stream, protocol, "OpenAI");
    }

    private AiProxyPlugin plugin(final DisposableServer server, final boolean stream, final String protocol, final String provider) {
        final AiProxyPluginHandler handler = new AiProxyPluginHandler();
        AiProxyHandle handle = new AiProxyHandle();
        handle.setProvider(provider);
        handle.setBaseUrl("http://127.0.0.1:" + server.port());
        handle.setApiKey("real-key");
        handle.setStream(stream);
        handle.setProtocol(protocol);
        handle.setModel("model");
        handler.getSelectorCachedHandle().cachedHandle(CacheKeyUtils.INST.getKey("http-selector", Constants.DEFAULT_RULE), handle);
        return plugin(handler);
    }

    private AiProxyPlugin plugin(final AiProxyPluginHandler handler) {
        AiProxyEngine engine = new AiProxyEngine(new AiProxyProtocolFactory(List.of(new OpenAiChat(), new OpenAiResponses())),
                new AiProxyProviderFactory(List.of(new OpenAi(), new OpenAiCompatible("ChatOnly"))),
                protocolRouteResolver(), new WebClientAiProxyTransport(WebClient.builder().build()), Duration.ZERO);
        return new AiProxyPlugin(new AiProxyConfigService(handler), engine, handler);
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
