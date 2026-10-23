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

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import io.github.agenticai.connect.ProviderProfile;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.ShenyuAiRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiChat;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiResponses;

import java.util.EnumSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;

/**
 * Immutable configuration for a named Chat-compatible provider in the explicit registry.
 *
 * @param authentication credential placement
 * @param credentialName credential header or query parameter name
 * @param credentialPrefix credential prefix
 * @param headers provider headers
 * @param queryParameters provider query parameters
 * @param modelMappings client or routed model aliases
 * @param defaults missing-field defaults in the client protocol's payload
 * @param capabilities provider capability declarations
 * @param dialect Connect provider dialect, or null for identity adaptation
 * @param maxTokensField Chat token field, or empty to preserve the client's field choice
 */
public record OpenAiProviderOptions(Authentication authentication, String credentialName, String credentialPrefix,
                                    Map<String, String> headers, Map<String, String> queryParameters, Map<String, String> modelMappings,
                                    ObjectNode defaults, AiProviderCapabilities capabilities, ProviderProfile dialect, String maxTokensField) {

    /**
     * Copy mutable data and reject options that would change the protocol or transport contract.
     */
    public OpenAiProviderOptions {
        Objects.requireNonNull(authentication, "AI provider authentication is required");
        Objects.requireNonNull(credentialName, "AI provider credential name is required");
        Objects.requireNonNull(credentialPrefix, "AI provider credential prefix is required");
        Objects.requireNonNull(capabilities, "AI provider capabilities are required");
        if (authentication != Authentication.NONE && credentialName.isBlank()) {
            throw new IllegalArgumentException("AI provider credential name must not be blank");
        }
        headers = Map.copyOf(headers);
        queryParameters = Map.copyOf(queryParameters);
        modelMappings = Map.copyOf(modelMappings);
        defaults = defaults.deepCopy();
        for (String field : new String[] {"messages", "input", "stream", "fallbackConfig"}) {
            if (defaults.has(field)) {
                throw new IllegalArgumentException("AI provider defaults must not replace the request envelope: " + field);
            }
        }
        if (!"".equals(maxTokensField) && !"max_tokens".equals(maxTokensField) && !"max_completion_tokens".equals(maxTokensField)) {
            throw new IllegalArgumentException("AI provider token field must be max_tokens or max_completion_tokens");
        }
    }

    /**
     * Retain the Chat-only constructor used by existing compatible provider extensions.
     *
     * @param path relative upstream operation path
     * @param authentication credential placement
     * @param credentialName credential header or query parameter name
     * @param credentialPrefix credential prefix
     * @param headers provider headers
     * @param queryParameters provider query parameters
     * @param modelMappings client or routed model aliases
     * @param defaults missing-field defaults
     * @param capabilities provider capabilities
     * @param dialect Connect provider dialect
     * @param maxTokensField Chat token field
     */
    public OpenAiProviderOptions(final String path, final Authentication authentication, final String credentialName,
                                 final String credentialPrefix, final Map<String, String> headers,
                                 final Map<String, String> queryParameters, final Map<String, String> modelMappings,
                                 final ObjectNode defaults, final AiProviderCapabilities capabilities,
                                 final ProviderProfile dialect, final String maxTokensField) {
        this(authentication, credentialName, credentialPrefix, headers, queryParameters, modelMappings, defaults,
                capabilities.withProtocolBindings(Map.of(OpenAiChat.NAME, chatBinding(path, dialect, maxTokensField))),
                dialect, maxTokensField);
    }

    /**
     * OpenAI options use the current Chat token-limit field.
     *
     * @return OpenAI options
     */
    public static OpenAiProviderOptions openAi() {
        String chatPath = "/v1/chat/completions";
        String tokenField = "max_completion_tokens";
        Map<String, AiProviderProtocolBinding> bindings = Map.of(
                OpenAiChat.NAME, chatBinding(chatPath, null, tokenField),
                OpenAiResponses.NAME, responsesBinding("/v1/responses"));
        AiProviderCapabilities chat = AiProviderCapabilities.chat();
        AiProviderCapabilities capabilities = new AiProviderCapabilities(bindings,
                EnumSet.of(ShenyuAiRequest.Operation.CHAT_COMPLETION, ShenyuAiRequest.Operation.RESPONSE), chat.features());
        return new OpenAiProviderOptions(Authentication.HEADER, "Authorization", "Bearer ", Map.of(), Map.of(), Map.of(),
                JsonNodeFactory.instance.objectNode(), capabilities, null, tokenField);
    }

    /**
     * Preserve generic compatible payloads unless a dialect is explicitly supplied.
     *
     * @param dialect explicitly selected Connect dialect
     * @return compatible options
     */
    public static OpenAiProviderOptions generic(final ProviderProfile dialect) {
        return new OpenAiProviderOptions("/v1/chat/completions", Authentication.HEADER, "Authorization", "Bearer ",
                Map.of(), Map.of(), Map.of(), JsonNodeFactory.instance.objectNode(), AiProviderCapabilities.chat(), dialect, "");
    }

    /**
     * DeepSeek Chat supports function tools and JSON mode, with its Connect dialect.
     *
     * @return DeepSeek options
     */
    public static OpenAiProviderOptions deepSeek() {
        EnumSet<AiProviderCapabilities.Feature> features = EnumSet.of(AiProviderCapabilities.Feature.STREAMING,
                AiProviderCapabilities.Feature.TOOL_CALLING, AiProviderCapabilities.Feature.JSON_MODE, AiProviderCapabilities.Feature.STREAM_USAGE);
        AiProviderCapabilities all = AiProviderCapabilities.chat();
        return new OpenAiProviderOptions("/chat/completions", Authentication.HEADER, "Authorization", "Bearer ",
                Map.of(), Map.of(), Map.of(), JsonNodeFactory.instance.objectNode(),
                new AiProviderCapabilities(all.protocolBindings(), all.operations(), features), ProviderProfile.deepSeekChat(), "");
    }

    /**
     * Get a defensive copy of provider defaults.
     *
     * @return provider defaults
     */
    @Override
    public ObjectNode defaults() {
        return defaults.deepCopy();
    }

    /**
     * Select one Chat token-limit field, implemented in Connect's provider adaptation phase.
     *
     * @param field Chat token-limit field
     * @return updated options
     */
    public OpenAiProviderOptions withMaxTokensField(final String field) {
        Map<String, AiProviderProtocolBinding> bindings = new LinkedHashMap<>(capabilities.protocolBindings());
        String chatPath = protocolBinding(OpenAiChat.NAME).path();
        bindings.put(OpenAiChat.NAME, chatBinding(chatPath, dialect, field));
        return new OpenAiProviderOptions(authentication, credentialName, credentialPrefix, headers, queryParameters,
                modelMappings, defaults, capabilities.withProtocolBindings(bindings), dialect, field);
    }

    /**
     * Get the provider behavior for a native target protocol.
     *
     * @param protocol target protocol
     * @return protocol binding
     */
    public AiProviderProtocolBinding protocolBinding(final String protocol) {
        return capabilities.protocolBinding(protocol);
    }

    /**
     * Get the request-scoped Connect dialect including the explicit token-field choice.
     *
     * @return provider dialect
     */
    public ProviderProfile connectProfile() {
        return connectProfile(dialect, maxTokensField);
    }

    private static ProviderProfile connectProfile(final ProviderProfile dialect, final String maxTokensField) {
        if (maxTokensField.isEmpty()) {
            return dialect;
        }
        return new ProviderProfile() {
            @Override
            public void adapt(final JsonObject payload, final Adaptation adaptation) {
                if (Objects.nonNull(dialect)) {
                    dialect.adapt(payload, adaptation);
                }
                JsonElement tokens = payload.get("max_completion_tokens");
                if (Objects.isNull(tokens) || tokens.isJsonNull()) {
                    tokens = payload.get("max_tokens");
                }
                if (Objects.nonNull(tokens)) {
                    payload.remove("max_tokens");
                    payload.remove("max_completion_tokens");
                    payload.add(maxTokensField, tokens);
                }
            }

            @Override
            public boolean rendersReasoning() {
                return Objects.nonNull(dialect) && dialect.rendersReasoning();
            }
        };
    }

    private static AiProviderProtocolBinding chatBinding(final String path, final ProviderProfile dialect, final String tokenField) {
        ProviderProfile profile = connectProfile(dialect, tokenField);
        return new AiProviderProtocolBinding(path,
                (payload, upstreamModel) -> new ConnectProviderDialectAdapter(upstreamModel, profile).adapt(payload));
    }

    private static AiProviderProtocolBinding responsesBinding(final String path) {
        return new AiProviderProtocolBinding(path, AiProviderRequestRewriter.identity());
    }

    @Override
    public String toString() {
        return "OpenAiProviderOptions[authentication=" + authentication + ", capabilities=" + capabilities + "]";
    }

    /**
     * Credentials can be placed in a header or query parameter, or omitted.
     */
    public enum Authentication {
        HEADER, QUERY, NONE
    }
}
