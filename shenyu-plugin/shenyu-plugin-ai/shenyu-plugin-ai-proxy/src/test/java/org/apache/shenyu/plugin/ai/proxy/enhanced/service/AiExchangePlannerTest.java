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

package org.apache.shenyu.plugin.ai.proxy.enhanced.service;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.proxy.enhanced.conversion.AiProtocolConverterFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.conversion.ConnectResponsesToChatConverter;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiProxyException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.ShenyuAiRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiChat;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiResponses;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProviderFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAi;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAiCompatible;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAiProviderOptions;
import org.apache.shenyu.plugin.ai.proxy.enhanced.routing.AiProtocolRouteResolver;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import reactor.core.publisher.Flux;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AiExchangePlannerTest {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    private AiExchangePlanner planner;

    private AiCommonConfig config;

    @BeforeEach
    void setUp() {
        planner = new AiExchangePlanner(
                new AiProxyProtocolFactory(List.of(new OpenAiChat(), new OpenAiResponses())),
                new AiProxyProviderFactory(List.of(
                        new OpenAi(),
                        new OpenAiCompatible("DeepSeek", OpenAiProviderOptions.deepSeek()))),
                new AiProtocolRouteResolver(new AiProtocolConverterFactory(
                        List.of(new ConnectResponsesToChatConverter()))));
        config = new AiCommonConfig();
        config.setProvider(OpenAi.NAME);
        config.setProtocol(OpenAiChat.NAME);
        config.setBaseUrl("https://api.openai.com");
        config.setApiKey("test-key");
        config.setModel(null);
        config.setTemperature(null);
        config.setMaxTokens(null);
        config.setStream(null);
    }

    @Test
    void testPreservesOriginalBodyWhenPlanningDoesNotChangePayload() {
        final String body = "{ \"messages\": [{\"role\":\"user\",\"content\":\"hello\"}],"
                + " \"stream\": false, \"unknown\": true }";

        final AiPreparedExchange prepared = planner.prepare(body, config);

        assertEquals(body, new String(prepared.request().getBody(), StandardCharsets.UTF_8));
        assertEquals(OpenAiChat.NAME, prepared.clientProtocol());
        assertEquals(OpenAiChat.NAME, prepared.targetProtocol());
        assertFalse(prepared.stream());
        assertTrue(prepared.conservativeReplaySafe());
        assertEquals(ShenyuAiRequest.Operation.CHAT_COMPLETION, prepared.operation());
        assertEquals(OpenAi.NAME, prepared.providerName());
        assertTrue(prepared.cacheCompatible());
    }

    @Test
    void testRemovesFallbackConfigAndAppliesConfiguredOverrides() throws Exception {
        config.setModel("gpt-4o-mini");
        config.setStream(true);
        config.setMaxTokens(64);
        final String body = "{\"messages\":[{\"role\":\"user\",\"content\":\"hello\"}],"
                + "\"fallbackConfig\":{\"provider\":\"DeepSeek\"}}";

        final AiPreparedExchange prepared = planner.prepare(body, config);
        final JsonNode payload = MAPPER.readTree(prepared.request().getBody());

        assertFalse(payload.has("fallbackConfig"));
        assertEquals("gpt-4o-mini", payload.path("model").asText());
        assertTrue(payload.path("stream").asBoolean());
        assertEquals(64, payload.path("max_completion_tokens").asInt());
        assertTrue(payload.path("stream_options").path("include_usage").asBoolean());
        assertTrue(prepared.stream());
    }

    @Test
    void testDetectsNativeResponsesProtocolAndBuildsPreparedMetadata() throws Exception {
        config.setProtocol(null);
        final AiPreparedExchange prepared = planner.prepare(
                "{\"model\":\"gpt-4o-mini\",\"input\":\"hello\"}", config);
        final JsonNode payload = MAPPER.readTree(prepared.request().getBody());

        assertEquals(OpenAiResponses.NAME, prepared.clientProtocol());
        assertEquals(OpenAiResponses.NAME, prepared.targetProtocol());
        assertEquals(ShenyuAiRequest.Operation.RESPONSE, prepared.operation());
        assertEquals("https://api.openai.com/v1/responses", prepared.request().getUri().toString());
        assertEquals("hello", payload.path("input").asText());
        assertFalse(prepared.cacheCompatible());
    }

    @Test
    void testPlansConvertedRouteAndComposesResponseAdapters() throws Exception {
        config.setProvider("DeepSeek");
        config.setProtocol(OpenAiResponses.NAME);
        config.setBaseUrl("https://api.deepseek.com");
        final AiPreparedExchange prepared = planner.prepare(
                "{\"model\":\"deepseek-chat\",\"input\":\"hello\"}", config);
        final JsonNode request = MAPPER.readTree(prepared.request().getBody());

        assertEquals(OpenAiResponses.NAME, prepared.clientProtocol());
        assertEquals(OpenAiChat.NAME, prepared.targetProtocol());
        assertEquals("https://api.deepseek.com/chat/completions", prepared.request().getUri().toString());
        assertEquals("hello", request.at("/messages/0/content").asText());

        final String responseBody = "{\"id\":\"chatcmpl-test\",\"model\":\"deepseek-chat\","
                + "\"choices\":[{\"index\":0,\"message\":{\"role\":\"assistant\","
                + "\"content\":\"world\"},\"finish_reason\":\"stop\"}]}";
        final AiUpstreamResponse response = new AiUpstreamResponse(200, Map.of(),
                Flux.just(responseBody.getBytes(StandardCharsets.UTF_8)));
        final byte[] adaptedBody = prepared.clientResponseAdapter().adapt(response).getBody()
                .single().block(Duration.ofSeconds(3));
        final JsonNode adapted = MAPPER.readTree(adaptedBody);

        assertEquals("response", adapted.path("object").asText());
        assertEquals("world", adapted.at("/output/0/content/0/text").asText());
    }

    @Test
    void testRejectsInvalidRequestBodiesBeforePlanning() {
        assertThrows(AiProxyException.class, () -> planner.prepare(null, config));
        assertThrows(AiProxyException.class, () -> planner.prepare("not-json", config));
        assertThrows(AiProxyException.class, () -> planner.prepare("[]", config));
    }

    @Test
    void testRejectsExplicitProtocolMismatch() {
        final AiProxyException error = assertThrows(AiProxyException.class,
                () -> planner.prepare("{\"input\":\"hello\"}", config));

        assertEquals(AiProxyException.Kind.REQUEST, error.getKind());
        assertTrue(error.getMessage().contains(OpenAiChat.NAME));
    }
}
