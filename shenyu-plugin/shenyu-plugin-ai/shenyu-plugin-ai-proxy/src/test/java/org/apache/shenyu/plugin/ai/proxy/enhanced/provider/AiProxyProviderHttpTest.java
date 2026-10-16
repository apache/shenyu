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

package org.apache.shenyu.plugin.ai.proxy.enhanced.provider;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiChat;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiResponses;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyEngine;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.WebClientAiProxyTransport;
import org.junit.jupiter.api.Test;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Flux;
import reactor.netty.DisposableServer;
import reactor.netty.http.server.HttpServer;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Providers construct requests; the existing engine and transport execute real HTTP.
 */
class AiProxyProviderHttpTest {

    private static final Duration TIMEOUT = Duration.ofSeconds(5);

    private static final ObjectMapper MAPPER = new ObjectMapper();

    @Test
    void customProfileAppliesHeadersQueryModelsAndTokensThroughRealTransport() throws Exception {
        AtomicReference<String> forwarded = new AtomicReference<>();
        String reply = "{\"choices\":[],\"provider_extra\":{\"kept\":true}}";
        DisposableServer server = HttpServer.create().host("127.0.0.1").port(0).handle((request, response) -> {
            assertEquals("/tenant/generate?api-version=2026-01", request.uri());
            assertEquals("gateway-key", request.requestHeaders().get("api-key"));
            assertEquals("tenant", request.requestHeaders().get("X-Tenant"));
            return request.receive().aggregate().asString().flatMap(body -> {
                forwarded.set(body);
                return response.header("Content-Type", "application/json").header("X-Provider", "custom").sendString(Flux.just(reply)).then();
            });
        }).bindNow(TIMEOUT);
        try {
            OpenAiProviderOptions options = new OpenAiProviderOptions("/generate", OpenAiProviderOptions.Authentication.HEADER, "api-key", "",
                    Map.of("X-Tenant", "tenant"), Map.of("api-version", "2026-01"), Map.of("alias", "native-model"),
                    MAPPER.createObjectNode(), AiProviderCapabilities.chat(), null, "max_tokens");
            OpenAiCompatible provider = new OpenAiCompatible("custom", options);
            AiProxyEngine engine = engine(provider);
            AiCommonConfig config = config(server, "custom", OpenAiChat.NAME);
            config.setBaseUrl(config.getBaseUrl() + "/tenant");
            config.setMaxTokens(21);
            String body = "{\"model\":\"alias\",\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}],\"vendor_field\":true}";
            StepVerifier.create(engine.execute(body, config, Optional.empty(), response -> {
                assertEquals(List.of("custom"), response.getHeaders().get("X-Provider"));
                return response.getBody().map(bytes -> new String(bytes, StandardCharsets.UTF_8)).reduce("", String::concat)
                        .doOnNext(received -> assertEquals(reply, received)).then();
            })).verifyComplete();
            JsonNode payload = MAPPER.readTree(forwarded.get());
            assertEquals("native-model", payload.get("model").asText());
            assertEquals(21, payload.get("max_tokens").asInt());
            assertFalse(payload.has("max_completion_tokens"));
            assertTrue(payload.get("vendor_field").asBoolean());
        } finally {
            server.disposeNow(TIMEOUT);
        }
    }

    @Test
    void deepSeekChatStreamConvertsReasoningAndUsageForResponsesClients() throws Exception {
        AtomicReference<String> forwarded = new AtomicReference<>();
        AtomicReference<String> received = new AtomicReference<>();
        String reply = "data: {\"id\":\"d\",\"model\":\"deepseek-flash\",\"choices\":[{\"index\":0,\"delta\":{\"reasoning_content\":\"trace\"}}]}\n\n"
                + "data: {\"id\":\"d\",\"model\":\"deepseek-flash\",\"choices\":[{\"index\":0,\"delta\":{\"content\":\"answer\"}}]}\n\n"
                + "data: {\"id\":\"d\",\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}],"
                + "\"usage\":{\"prompt_tokens\":3,\"completion_tokens\":5,\"total_tokens\":8}}\n\ndata: [DONE]\n\n";
        DisposableServer server = HttpServer.create().host("127.0.0.1").port(0).handle((request, response) -> {
            assertEquals("/chat/completions", request.uri());
            assertEquals("Bearer gateway-key", request.requestHeaders().get("Authorization"));
            return request.receive().aggregate().asString().flatMap(body -> {
                forwarded.set(body);
                return response.header("Content-Type", "text/event-stream").sendString(Flux.just(reply)).then();
            });
        }).bindNow(TIMEOUT);
        try {
            AiProxyEngine engine = engine(new OpenAiCompatible("DeepSeek"));
            AiCommonConfig config = config(server, "DeepSeek", OpenAiResponses.NAME);
            config.setModel("deepseek-flash");
            String body = "{\"input\":\"hi\",\"stream\":true,\"max_output_tokens\":20,\"reasoning\":{\"effort\":\"medium\"}}";
            StepVerifier.create(engine.execute(body, config, Optional.empty(), response -> response.getBody()
                    .map(bytes -> new String(bytes, StandardCharsets.UTF_8)).reduce("", String::concat).doOnNext(received::set).then())).verifyComplete();
            JsonNode payload = MAPPER.readTree(forwarded.get());
            assertEquals(20, payload.get("max_tokens").asInt());
            assertFalse(payload.has("max_completion_tokens"));
            assertEquals("high", payload.get("reasoning_effort").asText());
            assertTrue(payload.get("stream").asBoolean());
            assertTrue(received.get().contains("trace"));
            assertTrue(received.get().contains("answer"));
            assertTrue(received.get().contains("event: response.completed"));
            assertTrue(received.get().contains("\"total_tokens\":8"));
        } finally {
            server.disposeNow(TIMEOUT);
        }
    }

    private AiProxyEngine engine(final AiProxyProvider provider) {
        return new AiProxyEngine(new AiProxyProtocolFactory(List.of(new OpenAiChat(), new OpenAiResponses())),
                new AiProxyProviderFactory(List.of(provider)), new WebClientAiProxyTransport(WebClient.builder().build()));
    }

    private AiCommonConfig config(final DisposableServer server, final String provider, final String protocol) {
        AiCommonConfig config = new AiCommonConfig();
        config.setProvider(provider);
        config.setProtocol(protocol);
        config.setBaseUrl("http://127.0.0.1:" + server.port());
        config.setApiKey("gateway-key");
        config.setStream(false);
        return config;
    }
}
