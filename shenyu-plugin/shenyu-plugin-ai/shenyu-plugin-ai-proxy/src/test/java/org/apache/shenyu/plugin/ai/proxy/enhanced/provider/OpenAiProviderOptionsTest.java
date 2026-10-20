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
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiProxyException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.ShenyuAiRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiChat;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiResponses;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyEngine;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.EnumSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class OpenAiProviderOptionsTest {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    private static final String CHAT = "{ \"model\": \"client-alias\", \"messages\": [{\"role\":\"user\",\"content\":\"hello\"}], \"stream\": false, \"vendor_extra\": {\"nested\":[1,2]} }";

    private AiCommonConfig config;

    @BeforeEach
    void setUp() {
        config = new AiCommonConfig();
        config.setBaseUrl("https://example.org/prefix");
        config.setApiKey("test-key");
        config.setStream(null);
    }

    @Test
    void genericCompatibleProfilePreservesUnknownFieldsAndOriginalBytes() throws Exception {
        AiUpstreamRequest result = new OpenAiCompatible("custom").buildRequest(chat(CHAT), config);
        assertEquals(CHAT, text(result.getBody()));
        assertEquals("Bearer test-key", result.getHeaders().get("Authorization").get(0));
        assertEquals("https://example.org/prefix/v1/chat/completions", result.getUri().toString());
        assertNotNull(new OpenAi(null));
    }

    @Test
    void resolvesRootVersionedPrefixedAndCompleteEndpoints() throws Exception {
        OpenAi provider = new OpenAi();
        Map<String, String> endpoints = Map.of(
                "https://example.org", "https://example.org/v1/chat/completions",
                "https://example.org/v1/", "https://example.org/v1/chat/completions",
                "https://example.org/prefix/v1", "https://example.org/prefix/v1/chat/completions",
                "https://example.org/prefix/v1/chat/completions/", "https://example.org/prefix/v1/chat/completions",
                "https://example.org/prefix%20one", "https://example.org/prefix%20one/v1/chat/completions");
        for (Map.Entry<String, String> entry : endpoints.entrySet()) {
            config.setBaseUrl(entry.getKey());
            assertEquals(entry.getValue(), provider.buildRequest(chat(CHAT), config).getUri().toString());
        }
    }

    @Test
    void resolvesCustomPathHeaderAuthenticationAndEncodedQueryParameters() throws Exception {
        OpenAiProviderOptions options = options("/chat/generate", OpenAiProviderOptions.Authentication.HEADER, "api-key", "",
                Map.of("X-Tenant", "tenant", "api-key", "stale", "accept", "stale"), Map.of("version", "2026-01", "tag", "a &b/#"), Map.of(), object("{}"),
                AiProviderCapabilities.chat(), "");
        AiUpstreamRequest result = new OpenAiCompatible("custom", options).buildRequest(chat(CHAT), config);
        assertEquals("https://example.org/prefix/chat/generate?tag=a%20%26b%2F%23&version=2026-01", result.getUri().toString());
        assertEquals(List.of("test-key"), result.getHeaders().get("api-key"));
        assertEquals(List.of("tenant"), result.getHeaders().get("X-Tenant"));
        assertEquals(List.of("application/json"), result.getHeaders().get("Accept"));
        assertFalse(result.getHeaders().containsKey("Authorization"));
        assertFalse(result.getHeaders().containsKey("accept"));
    }

    @Test
    void queryCredentialsReplaceConfiguredValueAndAreEncodedOnce() throws Exception {
        config.setApiKey("a+b &/#%");
        OpenAiProviderOptions options = options("/generate", OpenAiProviderOptions.Authentication.QUERY, "key", "",
                Map.of(), Map.of("key", "stale"), Map.of(), object("{}"), AiProviderCapabilities.chat(), "");
        AiUpstreamRequest result = new OpenAiCompatible("custom", options).buildRequest(chat(CHAT), config);
        assertEquals("https://example.org/prefix/generate?key=a%2Bb%20%26%2F%23%25", result.getUri().toString());
        assertFalse(result.getHeaders().containsKey("Authorization"));
    }

    @Test
    void unauthenticatedLocalProvidersDoNotRequireApiKeys() throws Exception {
        config.setApiKey(null);
        OpenAiProviderOptions options = options("/generate", OpenAiProviderOptions.Authentication.NONE, "", "",
                Map.of(), Map.of(), Map.of(), object("{}"), AiProviderCapabilities.chat(), "");
        assertFalse(new OpenAiCompatible("local", options).buildRequest(chat(CHAT), config).getHeaders().containsKey("Authorization"));
        assertThrows(IllegalArgumentException.class, () -> new OpenAi().buildRequest(chat(CHAT), config));
    }

    @Test
    void modelAliasesAndDefaultsRespectClientValuesAndGatewayOverrides() throws Exception {
        OpenAiProviderOptions options = options("/generate", OpenAiProviderOptions.Authentication.HEADER, "Authorization", "Bearer ",
                Map.of(), Map.of(), Map.of("client-alias", "native-client", "route-alias", "native-route"),
                object("{\"model\":\"client-alias\",\"temperature\":0.7,\"vendor_default\":{\"x\":1}}"), AiProviderCapabilities.chat(), "");
        OpenAiCompatible provider = new OpenAiCompatible("custom", options);
        JsonNode first = MAPPER.readTree(provider.buildRequest(chat(CHAT), config).getBody());
        assertEquals("native-client", first.get("model").asText());
        assertEquals(0.7, first.get("temperature").asDouble());
        assertEquals(1, first.at("/vendor_default/x").asInt());
        assertEquals(object(CHAT).get("vendor_extra"), first.get("vendor_extra"));
        config.setModel("route-alias");
        config.setTemperature(0.2);
        JsonNode routed = MAPPER.readTree(provider.buildRequest(chat(CHAT), config).getBody());
        assertEquals("native-route", routed.get("model").asText());
        assertEquals(0.2, routed.get("temperature").asDouble());
        config.setModel(null);
        config.setTemperature(null);
        JsonNode absent = MAPPER.readTree(provider.buildRequest(chat("{\"messages\":[{\"role\":\"user\"}],\"temperature\":0.4}"), config).getBody());
        assertEquals("native-client", absent.get("model").asText());
        assertEquals(0.4, absent.get("temperature").asDouble());
    }

    @Test
    void optionsAreImmutableAndDoNotRenderHeaderQueryOrBodySecrets() throws Exception {
        Map<String, String> headers = new LinkedHashMap<>(Map.of("X-Secret", "header-secret"));
        ObjectNode defaults = object("{\"secret\":\"body-secret\"}");
        OpenAiProviderOptions options = options("/generate", OpenAiProviderOptions.Authentication.HEADER, "Authorization", "Bearer ",
                headers, Map.of("secret", "query-secret"), Map.of(), defaults, AiProviderCapabilities.chat(), "");
        headers.put("X-Secret", "changed");
        defaults.put("secret", "changed");
        options.defaults().put("secret", "changed");
        assertEquals("header-secret", options.headers().get("X-Secret"));
        assertEquals("body-secret", options.defaults().get("secret").asText());
        assertThrows(UnsupportedOperationException.class, () -> options.headers().put("new", "value"));
        assertFalse(options.toString().contains("header-secret"));
        assertFalse(options.toString().contains("query-secret"));
        assertFalse(options.toString().contains("body-secret"));
    }

    @Test
    void openAiMapsLegacyTokensAndOverrideToOneField() throws Exception {
        String body = CHAT.substring(0, CHAT.length() - 1) + ",\"max_tokens\":10,\"max_completion_tokens\":20}";
        JsonNode first = MAPPER.readTree(new OpenAi().buildRequest(chat(body), config).getBody());
        assertEquals(20, first.get("max_completion_tokens").asInt());
        assertFalse(first.has("max_tokens"));
        config.setMaxTokens(32);
        JsonNode overridden = MAPPER.readTree(new OpenAi().buildRequest(chat(body), config).getBody());
        assertEquals(32, overridden.get("max_completion_tokens").asInt());
        assertFalse(overridden.has("max_tokens"));
        assertEquals(object(CHAT).get("vendor_extra"), overridden.get("vendor_extra"));
        config.setMaxTokens(0);
        assertThrows(IllegalArgumentException.class, () -> new OpenAi().buildRequest(chat(CHAT), config));
    }

    @Test
    void nullModernTokenLimitFallsBackToTheLegacyValue() throws Exception {
        String body = "{\"messages\":[{\"role\":\"user\"}],\"max_tokens\":10,\"max_completion_tokens\":null}";
        JsonNode payload = MAPPER.readTree(new OpenAi().buildRequest(chat(body), config).getBody());
        assertEquals(10, payload.get("max_completion_tokens").asInt());
        assertFalse(payload.has("max_tokens"));
    }

    @Test
    void compatibleTokenFieldChoiceRunsInConnectAfterResponsesConversion() throws Exception {
        OpenAiCompatible provider = new OpenAiCompatible("legacy", OpenAiProviderOptions.generic(null).withMaxTokensField("max_tokens"));
        config.setMaxTokens(17);
        AiUpstreamRequest upstream = provider.buildRequest(responses("{\"model\":\"m\",\"input\":\"hi\",\"max_output_tokens\":7}"), config);
        JsonNode payload = MAPPER.readTree(upstream.getBody());
        assertEquals(17, payload.get("max_tokens").asInt());
        assertFalse(payload.has("max_completion_tokens"));
        assertFalse(payload.has("max_output_tokens"));
        assertEquals("hi", payload.at("/messages/0/content").asText());
    }

    @Test
    void deepSeekUsesItsExplicitDialectAndPreservesVendorFields() throws Exception {
        config.setBaseUrl("https://api.deepseek.com");
        OpenAiCompatible provider = new OpenAiCompatible("DeepSeek");
        String body = "{\"model\":\"deepseek-flash\",\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}],"
                + "\"max_completion_tokens\":20,\"reasoning_effort\":\"medium\",\"vendor_extra\":{\"x\":1}}";
        AiUpstreamRequest upstream = provider.buildRequest(chat(body), config);
        JsonNode payload = MAPPER.readTree(upstream.getBody());
        assertEquals("https://api.deepseek.com/chat/completions", upstream.getUri().toString());
        assertEquals(20, payload.get("max_tokens").asInt());
        assertFalse(payload.has("max_completion_tokens"));
        assertEquals("high", payload.get("reasoning_effort").asText());
        assertEquals("enabled", payload.at("/thinking/type").asText());
        assertEquals(1, payload.at("/vendor_extra/x").asInt());
        config.setBaseUrl("https://api.deepseek.com/beta");
        assertEquals("https://api.deepseek.com/beta/chat/completions", provider.buildRequest(chat(body), config).getUri().toString());
        assertThrows(AiProxyException.class, () -> provider.buildRequest(chat(body.replace("\"vendor_extra\"", "\"tool_choice\":\"required\",\"vendor_extra\"")), config));
    }

    @Test
    void deepSeekRejectsUnsupportedStructuredOutputAndPreservesJsonMode() throws Exception {
        OpenAiCompatible provider = new OpenAiCompatible("DeepSeek");
        String body = "{\"model\":\"deepseek-flash\",\"messages\":[{\"role\":\"user\"}],\"response_format\":{\"type\":\"json_schema\"}}";
        AiProxyException error = assertThrows(AiProxyException.class, () -> provider.buildRequest(chat(body), config));
        assertEquals(AiProxyException.Kind.REQUEST, error.getKind());
        JsonNode jsonMode = MAPPER.readTree(provider.buildRequest(chat(body.replace("json_schema", "json_object")), config).getBody());
        assertEquals("json_object", jsonMode.at("/response_format/type").asText());
    }

    @Test
    void deepSeekResponseDifferencesStayPassthroughForChatAndConvertForResponses() throws Exception {
        OpenAiCompatible provider = new OpenAiCompatible("DeepSeek");
        String reply = "{\"id\":\"chat-d\",\"created\":1,\"object\":\"chat.completion\",\"model\":\"deepseek-flash\","
                + "\"choices\":[{\"index\":0,\"message\":{\"role\":\"assistant\",\"reasoning_content\":\"think\",\"content\":\"answer\"},\"finish_reason\":\"stop\"}],"
                + "\"usage\":{\"prompt_tokens\":3,\"completion_tokens\":5,\"total_tokens\":8,\"prompt_cache_hit_tokens\":2}}";
        AiUpstreamResponse response = new AiUpstreamResponse(200, Map.of("Content-Type", List.of("application/json")), Flux.just(bytes(reply)));
        assertEquals(reply, text(provider.buildRequest(chat(CHAT), config).adaptResponse(response).getBody().single().block(Duration.ofSeconds(3))));
        AiUpstreamRequest request = provider.buildRequest(responses("{\"model\":\"deepseek-flash\",\"input\":\"hi\"}"), config);
        JsonNode converted = MAPPER.readTree(request.adaptResponse(response).getBody().single().block(Duration.ofSeconds(3)));
        assertEquals("response", converted.get("object").asText());
        assertTrue(converted.toString().contains("think"));
        assertTrue(converted.toString().contains("answer"));
        assertEquals(8, converted.at("/usage/total_tokens").asInt());
    }

    @Test
    void unsupportedOperationsFailBeforeTransportWithoutEngineChanges() {
        AiProviderCapabilities all = AiProviderCapabilities.chat();
        AiProviderCapabilities restricted = new AiProviderCapabilities(all.protocols(), Set.of(ShenyuAiRequest.Operation.CHAT_COMPLETION), all.features());
        OpenAiCompatible custom = new OpenAiCompatible("restricted", options("/generate", OpenAiProviderOptions.Authentication.NONE, "", "",
                Map.of(), Map.of(), Map.of(), MAPPER.createObjectNode(), restricted, ""));
        AtomicInteger calls = new AtomicInteger();
        AiProxyEngine engine = new AiProxyEngine(new AiProxyProtocolFactory(List.of(new OpenAiResponses())),
                new AiProxyProviderFactory(List.of(custom)), request -> {
                    calls.incrementAndGet();
                    return Mono.empty();
                });
        config.setProvider("restricted");
        config.setProtocol(OpenAiResponses.NAME);
        StepVerifier.create(engine.execute("{\"input\":\"hi\"}", config, java.util.Optional.empty(), response -> response.getBody().then()))
                .expectErrorMatches(error -> error instanceof AiProxyException && ((AiProxyException) error).getKind() == AiProxyException.Kind.REQUEST).verify();
        assertEquals(0, calls.get());
    }

    @Test
    void validatesStreamingToolsJsonAndAudioCapabilitiesOnFinalPayload() throws Exception {
        AiProviderCapabilities all = AiProviderCapabilities.chat();
        AiProviderCapabilities restricted = new AiProviderCapabilities(all.protocols(), all.operations(), Set.of());
        OpenAiCompatible provider = new OpenAiCompatible("restricted", options("/generate", OpenAiProviderOptions.Authentication.NONE, "", "",
                Map.of(), Map.of(), Map.of(), object("{}"), restricted, ""));
        for (String feature : List.of("\"stream\":true", "\"function_call\":{\"name\":\"forced\"}", "\"tools\":[{\"type\":\"function\",\"function\":{\"name\":\"f\"}}]",
                "\"response_format\":{\"type\":\"json_object\"}", "\"response_format\":{\"type\":\"json_schema\"}", "\"modalities\":[\"audio\"]")) {
            String body = "{\"messages\":[{\"role\":\"user\"}]," + feature + "}";
            assertThrows(AiProxyException.class, () -> provider.buildRequest(chat(body), config));
        }
        EnumSet<AiProviderCapabilities.Feature> features = EnumSet.allOf(AiProviderCapabilities.Feature.class);
        features.remove(AiProviderCapabilities.Feature.STREAM_USAGE);
        OpenAiCompatible noUsage = new OpenAiCompatible("no-usage", options("/generate", OpenAiProviderOptions.Authentication.NONE, "", "",
                Map.of(), Map.of(), Map.of(), object("{}"), new AiProviderCapabilities(all.protocols(), all.operations(), features), ""));
        JsonNode payload = MAPPER.readTree(noUsage.buildRequest(chat("{\"messages\":[{\"role\":\"user\"}],\"stream\":true}"), config).getBody());
        assertFalse(payload.has("stream_options"));
        assertThrows(AiProxyException.class, () -> noUsage.buildRequest(chat("{\"messages\":[{\"role\":\"user\"}],\"stream\":true,"
                + "\"stream_options\":{\"include_usage\":true}}"), config));
    }

    @Test
    void dialectCannotChangeTheStreamingModeSelectedByTheEngine() throws Exception {
        OpenAiCompatible provider = new OpenAiCompatible("invalid-dialect", OpenAiProviderOptions.generic((payload, adaptation) -> payload.addProperty("stream", true)));
        AiProxyException error = assertThrows(AiProxyException.class, () -> provider.buildRequest(chat(CHAT), config));
        assertEquals(AiProxyException.Kind.REQUEST, error.getKind());
    }

    @Test
    void rejectsInvalidProviderOptionsEndpointsAndHeaders() throws Exception {
        for (String base : List.of("ftp://example.org", "https://user:secret@example.org", "https://example.org?secret=value", "https://example.org#fragment")) {
            config.setBaseUrl(base);
            assertThrows(IllegalArgumentException.class, () -> new OpenAi().buildRequest(chat(CHAT), config));
        }
        config.setBaseUrl("https://example.org");
        for (String path : List.of("https://other.org", "//other.org", "/generate?key=x", "/generate#fragment")) {
            assertThrows(IllegalArgumentException.class, () -> options(path, OpenAiProviderOptions.Authentication.NONE, "", "",
                    Map.of(), Map.of(), Map.of(), object("{}"), AiProviderCapabilities.chat(), ""));
        }
        assertThrows(IllegalArgumentException.class, () -> OpenAiProviderOptions.openAi().withMaxTokensField("invalid"));
        assertThrows(IllegalArgumentException.class, () -> options("/generate", OpenAiProviderOptions.Authentication.NONE, "", "",
                Map.of(), Map.of(), Map.of(), object("{\"stream\":true}"), AiProviderCapabilities.chat(), ""));
        OpenAiCompatible provider = new OpenAiCompatible("invalid-header", options("/generate", OpenAiProviderOptions.Authentication.HEADER, "Authorization", "Bearer ",
                Map.of("X-Header", "bad\r\nvalue"), Map.of(), Map.of(), object("{}"), AiProviderCapabilities.chat(), ""));
        assertThrows(IllegalArgumentException.class, () -> provider.buildRequest(chat(CHAT), config));
    }

    private OpenAiProviderOptions options(final String path, final OpenAiProviderOptions.Authentication authentication,
                                          final String credentialName, final String prefix, final Map<String, String> headers,
                                          final Map<String, String> query, final Map<String, String> aliases, final ObjectNode defaults,
                                          final AiProviderCapabilities capabilities, final String tokenField) {
        return new OpenAiProviderOptions(path, authentication, credentialName, prefix, headers, query, aliases, defaults, capabilities, null, tokenField);
    }

    private ShenyuAiRequest chat(final String body) throws Exception {
        return new OpenAiChat().decodeRequest(body, object(body));
    }

    private ShenyuAiRequest responses(final String body) throws Exception {
        return new OpenAiResponses().decodeRequest(body, object(body));
    }

    private ObjectNode object(final String json) throws Exception {
        return (ObjectNode) MAPPER.readTree(json);
    }

    private byte[] bytes(final String body) {
        return body.getBytes(StandardCharsets.UTF_8);
    }

    private String text(final byte[] body) {
        return new String(body, StandardCharsets.UTF_8);
    }
}
