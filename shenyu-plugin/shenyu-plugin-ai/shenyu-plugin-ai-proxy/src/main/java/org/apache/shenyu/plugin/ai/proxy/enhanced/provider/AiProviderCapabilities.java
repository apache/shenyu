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
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiProxyException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.ShenyuAiRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiChat;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiResponses;

import java.util.EnumSet;
import java.util.Objects;
import java.util.Set;

/**
 * Declared provider capabilities, independent of HTTP execution and model discovery.
 *
 * @param protocols supported client protocols
 * @param operations supported AI operations
 * @param features supported request features
 */
public record AiProviderCapabilities(Set<String> protocols, Set<ShenyuAiRequest.Operation> operations, Set<Feature> features) {

    /**
     * Copy declarations so request handling never mutates provider configuration.
     */
    public AiProviderCapabilities {
        protocols = Set.copyOf(protocols);
        operations = Set.copyOf(operations);
        features = Set.copyOf(features);
    }

    /**
     * General Chat upstream capabilities, including conversion from Responses clients.
     *
     * @return general capabilities
     */
    public static AiProviderCapabilities chat() {
        return new AiProviderCapabilities(Set.of(OpenAiChat.NAME, OpenAiResponses.NAME),
                EnumSet.of(ShenyuAiRequest.Operation.CHAT_COMPLETION, ShenyuAiRequest.Operation.RESPONSE),
                EnumSet.of(Feature.STREAMING, Feature.TOOL_CALLING, Feature.CUSTOM_TOOLS, Feature.JSON_MODE,
                        Feature.JSON_SCHEMA, Feature.STREAM_USAGE, Feature.AUDIO_OUTPUT));
    }

    /**
     * Validate the decoded request and effective gateway streaming setting.
     *
     * @param request decoded request
     * @param stream effective streaming setting
     */
    public void validate(final ShenyuAiRequest request, final boolean stream) {
        if (!protocols.contains(request.getProtocol())) {
            reject("protocol " + request.getProtocol());
        }
        if (Objects.isNull(request.getOperation()) || !operations.contains(request.getOperation())) {
            reject("operation " + request.getOperation());
        }
        if (stream) {
            require(Feature.STREAMING);
        }
    }

    /**
     * Validate features in the final Chat payload, after conversion and defaults.
     *
     * @param payload final provider payload
     */
    public void validatePayload(final JsonNode payload) {
        if (nonempty(payload.get("tools")) || nonempty(payload.get("functions"))) {
            require(Feature.TOOL_CALLING);
        }
        for (String field : new String[] {"tool_choice", "function_call"}) {
            JsonNode choice = payload.get(field);
            if (Objects.nonNull(choice) && !choice.isNull() && !(choice.isTextual() && "none".equals(choice.asText()))) {
                require(Feature.TOOL_CALLING);
            }
        }
        JsonNode tools = payload.get("tools");
        if (Objects.nonNull(tools) && tools.isArray()) {
            for (JsonNode tool : tools) {
                if ("custom".equals(tool.path("type").asText())) {
                    require(Feature.CUSTOM_TOOLS);
                }
            }
        }
        String format = payload.path("response_format").path("type").asText();
        if ("json_object".equals(format)) {
            require(Feature.JSON_MODE);
        } else if ("json_schema".equals(format)) {
            require(Feature.JSON_SCHEMA);
        }
        if (payload.path("stream_options").path("include_usage").asBoolean()) {
            require(Feature.STREAM_USAGE);
        }
        JsonNode modalities = payload.get("modalities");
        if (Objects.nonNull(modalities) && modalities.isArray()) {
            for (JsonNode modality : modalities) {
                if ("audio".equals(modality.asText())) {
                    require(Feature.AUDIO_OUTPUT);
                }
            }
        }
    }

    private boolean nonempty(final JsonNode node) {
        return Objects.nonNull(node) && !node.isNull() && !node.isEmpty();
    }

    private void require(final Feature feature) {
        if (!features.contains(feature)) {
            reject("feature " + feature);
        }
    }

    private void reject(final String capability) {
        throw new AiProxyException(AiProxyException.Kind.REQUEST, "AI provider does not support " + capability, null);
    }

    /**
     * Features checked when ShenYu constructs the final upstream request.
     */
    public enum Feature {
        STREAMING, TOOL_CALLING, CUSTOM_TOOLS, JSON_MODE, JSON_SCHEMA, STREAM_USAGE, AUDIO_OUTPUT
    }
}
