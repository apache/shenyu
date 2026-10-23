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
import org.apache.shenyu.plugin.ai.proxy.enhanced.conversion.AiProtocolConversion;
import org.apache.shenyu.plugin.ai.proxy.enhanced.conversion.AiProtocolConverter;
import org.apache.shenyu.plugin.ai.proxy.enhanced.conversion.AiProtocolConverterFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.conversion.ConnectResponsesToChatConverter;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiProxyException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.ShenyuAiRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.routing.AiProtocolRouteResolver;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiChat;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiResponses;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProviderCapabilities;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProviderProtocolBinding;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProviderRequestRewriter;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProvider;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProviderFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAi;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiProxyTransport;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiTransportException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.ArrayList;
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
import static org.apache.shenyu.plugin.ai.proxy.enhanced.AiProxyTestUtils.protocolRouteResolver;
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
                new AiProxyProtocolFactory(List.of(new OpenAiChat(), new OpenAiResponses())),
                new AiProxyProviderFactory(List.of(new OpenAi())),
                protocolRouteResolver(),
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
    void testOpenAiUsesNativeResponsesRoute() throws Exception {
        config.setProtocol(OpenAiResponses.NAME);
        config.setMaxTokens(32);

        AiUpstreamRequest request = engine.buildRequest("{\"model\":\"test\",\"input\":\"hello\",\"stream\":true}", config);
        JsonNode payload = OBJECT_MAPPER.readTree(request.getBody());

        assertEquals("https://api.openai.com/v1/responses", request.getUri().toString());
        assertEquals("hello", payload.path("input").asText());
        assertEquals(32, payload.path("max_output_tokens").asInt());
        assertFalse(payload.has("messages"));
        assertFalse(payload.has("max_completion_tokens"));
        assertFalse(payload.has("stream_options"));
    }

    @Test
    void testCustomProviderSupportsOnlyDeclaredProtocols() {
        final AiProviderCapabilities all = AiProviderCapabilities.chat();
        final AiProxyProvider provider = customProvider(
                new AiProviderCapabilities(all.protocolBindings(), all.operations(), all.features()), new AtomicInteger());

        assertTrue(provider.supports(OpenAiChat.NAME));
        assertFalse(provider.supports(OpenAiResponses.NAME));
        assertFalse(provider.supports("unknown"));
        assertFalse(provider.supports(null));
    }

    @Test
    void testCustomChatProviderAcceptsResponsesThroughConversion() throws Exception {
        final AiProviderCapabilities all = AiProviderCapabilities.chat();
        final AtomicInteger builds = new AtomicInteger();
        useCustomProvider(new AiProviderCapabilities(all.protocolBindings(), all.operations(), all.features()), builds);
        config.setProtocol(OpenAiResponses.NAME);

        AiUpstreamRequest request = engine.buildRequest("{\"model\":\"test\",\"input\":\"hello\"}", config);
        JsonNode payload = OBJECT_MAPPER.readTree(request.getBody());

        assertEquals(1, builds.get());
        assertEquals("hello", payload.at("/messages/0/content").asText());
        assertNull(transportedRequest.get());
    }

    @Test
    void testCustomChatProviderValidatesConvertedOperation() {
        final AiProviderCapabilities all = AiProviderCapabilities.chat();
        final AtomicInteger builds = new AtomicInteger();
        useCustomProvider(new AiProviderCapabilities(all.protocolBindings(),
                Set.of(ShenyuAiRequest.Operation.CHAT_COMPLETION), all.features()), builds);
        config.setProtocol(OpenAiResponses.NAME);

        engine.buildRequest("{\"model\":\"test\",\"input\":\"hello\"}", config);

        assertEquals(1, builds.get());
    }

    @Test
    void testCustomProviderRequestStreamRejectedBeforeBuildAndTransport() {
        final AiProviderCapabilities all = AiProviderCapabilities.chat();
        config.setStream(false);
        assertCapabilityRejected("{\"messages\":[{\"role\":\"user\",\"content\":\"hello\"}],\"stream\":true}",
                new AiProviderCapabilities(all.protocolBindings(), all.operations(), Set.of()), "STREAMING");
    }

    @Test
    void testCustomProviderConfiguredStreamRejectedBeforeBuildAndTransport() {
        final AiProviderCapabilities all = AiProviderCapabilities.chat();
        config.setStream(true);
        assertCapabilityRejected("{\"messages\":[{\"role\":\"user\",\"content\":\"hello\"}]}",
                new AiProviderCapabilities(all.protocolBindings(), all.operations(), Set.of()), "STREAMING");
    }

    @Test
    void testCustomProviderExplicitNonStreamingOverridesConfiguration() {
        final AiProviderCapabilities all = AiProviderCapabilities.chat();
        final AtomicInteger builds = new AtomicInteger();
        useCustomProvider(new AiProviderCapabilities(all.protocolBindings(), all.operations(), Set.of()), builds);
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
        final AiProxyProvider fallback = customProvider(
                new AiProviderCapabilities(all.protocolBindings(), all.operations(), Set.of()), builds);
        engine = new AiProxyEngine(new AiProxyProtocolFactory(List.of(new OpenAiChat())),
                new AiProxyProviderFactory(List.of(new OpenAi(), fallback)), protocolRouteResolver(), request -> {
                    transports.incrementAndGet();
                    return Mono.error(new AiTransportException(AiTransportException.Kind.IO_FAILURE, "unavailable", null));
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

    @Test
    void testFallbackResolvesANewNativeProtocolRoute() {
        final AtomicInteger conversions = new AtomicInteger();
        final ConnectResponsesToChatConverter delegate = new ConnectResponsesToChatConverter();
        final AiProtocolConverter converter = new AiProtocolConverter() {
            @Override
            public String sourceProtocol() {
                return delegate.sourceProtocol();
            }

            @Override
            public String targetProtocol() {
                return delegate.targetProtocol();
            }

            @Override
            public AiProtocolConversion convert(final ShenyuAiRequest request, final boolean stream) {
                conversions.incrementAndGet();
                return delegate.convert(request, stream);
            }
        };
        final AiProtocolRouteResolver routeResolver = new AiProtocolRouteResolver(
                new AiProtocolConverterFactory(List.of(converter)));
        final List<String> preparedProtocols = new ArrayList<>();
        final AiProxyProvider primary = recordingProvider(
                "primary", AiProviderCapabilities.chat(), preparedProtocols);
        final AiProviderCapabilities responses = new AiProviderCapabilities(Map.of(
                OpenAiResponses.NAME, new AiProviderProtocolBinding("/v1/responses", AiProviderRequestRewriter.identity())),
                Set.of(ShenyuAiRequest.Operation.RESPONSE), Set.of());
        final AiProxyProvider fallback = recordingProvider("fallback", responses, preparedProtocols);
        final AtomicInteger transports = new AtomicInteger();
        engine = new AiProxyEngine(
                new AiProxyProtocolFactory(List.of(new OpenAiChat(), new OpenAiResponses())),
                new AiProxyProviderFactory(List.of(primary, fallback)),
                routeResolver,
                request -> transports.incrementAndGet() == 1
                        ? Mono.error(new AiTransportException(AiTransportException.Kind.IO_FAILURE, "unavailable", null))
                        : Mono.just(upstreamResponse),
                new AiProxyRetryProperties(0, 0, Duration.ZERO));
        config.setProvider("primary");
        config.setProtocol(OpenAiResponses.NAME);
        final AiCommonConfig fallbackConfig = new AiCommonConfig(config);
        fallbackConfig.setProvider("fallback");

        StepVerifier.create(engine.execute("{\"model\":\"test\",\"input\":\"hello\"}",
                config, Optional.of(fallbackConfig), response -> response.getBody().then())).verifyComplete();

        assertEquals(List.of(OpenAiChat.NAME, OpenAiResponses.NAME), preparedProtocols);
        assertEquals(1, conversions.get());
        assertEquals(2, transports.get());
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
                new AiProxyProviderFactory(List.of(customProvider(capabilities, builds))), protocolRouteResolver(), request -> {
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

    private AiProxyProvider recordingProvider(final String name, final AiProviderCapabilities capabilities,
                                              final List<String> preparedProtocols) {
        return new AiProxyProvider() {
            @Override
            public String name() {
                return name;
            }

            @Override
            public AiProviderCapabilities capabilities() {
                return capabilities;
            }

            @Override
            public AiUpstreamRequest buildRequest(final ShenyuAiRequest request, final AiCommonConfig providerConfig) {
                preparedProtocols.add(request.getProtocol());
                return new AiUpstreamRequest();
            }
        };
    }
}
