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
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiProxyException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.ShenyuAiRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiChat;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiResponses;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProviderCapabilities;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProvider;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProviderFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAi;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiProxyTransport;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AiProxyEngineTest {

    private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

    private AiProxyEngine engine;

    private AiCommonConfig config;

    private AtomicReference<AiUpstreamRequest> transportedRequest;

    private AiUpstreamResponse upstreamResponse;

    @BeforeEach
    void setUp() {
        transportedRequest = new AtomicReference<>();
        upstreamResponse = new AiUpstreamResponse(200, Map.of(), Flux.just("ok".getBytes(StandardCharsets.UTF_8)));
        final AiProxyTransport transport = request -> {
            transportedRequest.set(request);
            return Mono.just(upstreamResponse);
        };
        engine = new AiProxyEngine(
                new AiProxyProtocolFactory(List.of(new OpenAiChat())),
                new AiProxyProviderFactory(List.of(new OpenAi())),
                transport);
        config = new AiCommonConfig();
        config.setProvider("OpenAI");
        config.setProtocol(OpenAiChat.NAME);
        config.setBaseUrl("https://api.openai.com");
        config.setApiKey("test-key");
        config.setModel(null);
        config.setTemperature(null);
        config.setMaxTokens(null);
    }

    @Test
    void testBuildRequestPassesOriginalBodyWhenUnchanged() {
        final String body = "{ \"messages\": [{\"role\":\"user\",\"content\":\"hello\"}],"
                + " \"stream\": false, \"unknown\": true }";

        final AiUpstreamRequest request = engine.buildRequest(body, config);

        assertEquals("https://api.openai.com/v1/chat/completions", request.getUri().toString());
        assertEquals("Bearer test-key", request.getHeaders().get("Authorization").get(0));
        assertEquals(body, new String(request.getBody(), StandardCharsets.UTF_8));
    }

    @Test
    void testBuildRequestAppliesOverridesAndPreservesUnknownFields() throws Exception {
        config.setModel("gpt-4o-mini");
        config.setMaxTokens(256);
        final String body = "{\"messages\":[{\"role\":\"user\",\"content\":\"hello\"}],"
                + "\"stream\":true,\"max_tokens\":32,\"unknown\":true}";

        final AiUpstreamRequest request = engine.buildRequest(body, config);
        final JsonNode payload = OBJECT_MAPPER.readTree(request.getBody());

        assertEquals("gpt-4o-mini", payload.get("model").asText());
        assertEquals(256, payload.get("max_completion_tokens").intValue());
        assertFalse(payload.has("max_tokens"));
        assertTrue(payload.get("unknown").booleanValue());
        assertTrue(payload.get("stream_options").get("include_usage").booleanValue());
        assertEquals("text/event-stream", request.getHeaders().get("Accept").get(0));
    }

    @Test
    void testBuildRequestRejectsMalformedJson() {
        assertThrows(ShenyuException.class, () -> engine.buildRequest("not-json", config));
    }

    @Test
    void testExecuteDelegatesBuiltRequestToTransport() throws Exception {
        final String body = "{\"messages\":[{\"role\":\"user\",\"content\":\"hello\"}]}";

        StepVerifier.create(engine.execute(body, config, Optional.empty(), response -> {
            assertEquals(200, response.getStatusCode());
            return response.getBody().then();
        })).verifyComplete();

        assertEquals("https://api.openai.com/v1/chat/completions",
                transportedRequest.get().getUri().toString());
        final JsonNode transportedPayload = OBJECT_MAPPER.readTree(transportedRequest.get().getBody());
        assertEquals("hello", transportedPayload.get("messages").get(0).get("content").asText());
        assertFalse(transportedPayload.get("stream").booleanValue());
    }

    @Test
    void testCustomProviderSupportsOnlyDeclaredProtocols() {
        final AiProviderCapabilities all = AiProviderCapabilities.chat();
        final AiProxyProvider provider = customProvider(new AiProviderCapabilities(Set.of(OpenAiChat.NAME), all.operations(), all.features()), new AtomicInteger());

        assertTrue(provider.supports(OpenAiChat.NAME));
        assertFalse(provider.supports(OpenAiResponses.NAME));
        assertFalse(provider.supports("unknown"));
        assertFalse(provider.supports(null));
    }

    @Test
    void testCustomProviderProtocolRejectedBeforeBuildAndTransport() {
        final AiProviderCapabilities all = AiProviderCapabilities.chat();
        final AtomicInteger builds = new AtomicInteger();
        useCustomProvider(new AiProviderCapabilities(Set.of(OpenAiChat.NAME), all.operations(), all.features()), builds);
        config.setProtocol(OpenAiResponses.NAME);

        StepVerifier.create(engine.execute("{\"input\":\"hello\"}", config, Optional.empty(), response -> response.getBody().then()))
                .expectError(ShenyuException.class).verify();

        assertEquals(0, builds.get());
        assertNull(transportedRequest.get());
    }

    @Test
    void testCustomProviderOperationRejectedBeforeBuildAndTransport() {
        final AiProviderCapabilities all = AiProviderCapabilities.chat();
        config.setProtocol(OpenAiResponses.NAME);
        assertCapabilityRejected("{\"input\":\"hello\"}",
                new AiProviderCapabilities(all.protocols(), Set.of(ShenyuAiRequest.Operation.CHAT_COMPLETION), all.features()), "operation");
    }

    @Test
    void testCustomProviderRequestStreamRejectedBeforeBuildAndTransport() {
        final AiProviderCapabilities all = AiProviderCapabilities.chat();
        config.setStream(false);
        assertCapabilityRejected("{\"messages\":[{\"role\":\"user\",\"content\":\"hello\"}],\"stream\":true}",
                new AiProviderCapabilities(all.protocols(), all.operations(), Set.of()), "STREAMING");
    }

    @Test
    void testCustomProviderConfiguredStreamRejectedBeforeBuildAndTransport() {
        final AiProviderCapabilities all = AiProviderCapabilities.chat();
        config.setStream(true);
        assertCapabilityRejected("{\"messages\":[{\"role\":\"user\",\"content\":\"hello\"}]}",
                new AiProviderCapabilities(all.protocols(), all.operations(), Set.of()), "STREAMING");
    }

    @Test
    void testCustomProviderExplicitNonStreamingOverridesConfiguration() {
        final AiProviderCapabilities all = AiProviderCapabilities.chat();
        final AtomicInteger builds = new AtomicInteger();
        useCustomProvider(new AiProviderCapabilities(all.protocols(), all.operations(), Set.of()), builds);
        config.setStream(true);

        StepVerifier.create(engine.execute("{\"messages\":[{\"role\":\"user\",\"content\":\"hello\"}],\"stream\":false}",
                config, Optional.empty(), response -> response.getBody().then())).verifyComplete();

        assertEquals(1, builds.get());
        assertEquals("https://api.openai.com/v1/chat/completions", transportedRequest.get().getUri().toString());
    }

    @Test
    void testFallbackProviderCapabilitiesCheckedBeforeBuildAndTransport() {
        final AiProviderCapabilities all = AiProviderCapabilities.chat();
        final AtomicInteger builds = new AtomicInteger();
        final AtomicInteger transports = new AtomicInteger();
        final AiProxyProvider fallback = customProvider(new AiProviderCapabilities(all.protocols(), all.operations(), Set.of()), builds);
        engine = new AiProxyEngine(new AiProxyProtocolFactory(List.of(new OpenAiChat())),
                new AiProxyProviderFactory(List.of(new OpenAi(), fallback)), request -> {
                    transports.incrementAndGet();
                    return Mono.error(new AiProxyException(AiProxyException.Kind.TRANSPORT, "unavailable", null));
                }, Duration.ZERO);
        config.setStream(true);
        final AiCommonConfig fallbackConfig = new AiCommonConfig(config);
        fallbackConfig.setProvider("custom");

        StepVerifier.create(engine.execute("{\"messages\":[{\"role\":\"user\",\"content\":\"hello\"}]}",
                config, Optional.of(fallbackConfig), response -> response.getBody().then()))
                .expectErrorMatches(error -> error instanceof AiProxyException && ((AiProxyException) error).getKind() == AiProxyException.Kind.REQUEST).verify();

        assertEquals(2, transports.get());
        assertEquals(0, builds.get());
    }

    private void assertCapabilityRejected(final String body, final AiProviderCapabilities capabilities, final String capability) {
        final AtomicInteger builds = new AtomicInteger();
        useCustomProvider(capabilities, builds);

        StepVerifier.create(engine.execute(body, config, Optional.empty(), response -> response.getBody().then()))
                .expectErrorMatches(error -> error instanceof AiProxyException && ((AiProxyException) error).getKind() == AiProxyException.Kind.REQUEST
                        && error.getMessage().contains(capability)).verify();

        assertEquals(0, builds.get());
        assertNull(transportedRequest.get());
    }

    private void useCustomProvider(final AiProviderCapabilities capabilities, final AtomicInteger builds) {
        engine = new AiProxyEngine(new AiProxyProtocolFactory(List.of(new OpenAiChat(), new OpenAiResponses())),
                new AiProxyProviderFactory(List.of(customProvider(capabilities, builds))), request -> {
                    transportedRequest.set(request);
                    return Mono.just(upstreamResponse);
                });
        config.setProvider("custom");
    }

    private AiProxyProvider customProvider(final AiProviderCapabilities capabilities, final AtomicInteger builds) {
        return new AiProxyProvider() {
            @Override
            public String name() {
                return "custom";
            }

            @Override
            public AiProviderCapabilities capabilities() {
                return capabilities;
            }

            @Override
            public AiUpstreamRequest buildRequest(final ShenyuAiRequest request, final AiCommonConfig providerConfig) {
                builds.incrementAndGet();
                return new OpenAi().buildRequest(request, providerConfig);
            }
        };
    }
}
