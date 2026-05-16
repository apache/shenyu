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

package org.apache.shenyu.plugin.ai.common.protocol;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.springframework.ai.openai.api.OpenAiApi.ChatCompletionRequest;

import java.util.Objects;

/**
 * Adapts between OpenAI Chat Completions wire format and internal representations.
 *
 * <p>Note: deserialization into Spring AI's {@link ChatCompletionRequest} preserves fields
 * modeled by that class (messages, model, temperature, maxTokens, stream, tools, etc.).
 * Provider-specific extension fields not modeled by {@code ChatCompletionRequest} are dropped
 * during deserialization. For the OpenAI-compatible providers this plugin currently supports,
 * all required fields are covered.
 */
public final class OpenAiProtocolAdapter {

    private static final com.fasterxml.jackson.databind.ObjectMapper MAPPER =
            new com.fasterxml.jackson.databind.ObjectMapper();

    private OpenAiProtocolAdapter() {
    }

    /**
     * Resolve the stream flag: client request body takes priority,
     * falls back to the provided default value.
     *
     * @param requestBody    the raw JSON request body
     * @param fallbackStream the default stream value from admin config
     * @return true if streaming, false otherwise
     */
    public static boolean resolveStream(final String requestBody, final Boolean fallbackStream) {
        if (Objects.isNull(requestBody) || requestBody.isEmpty()) {
            return Boolean.TRUE.equals(fallbackStream);
        }
        final JsonNode root = parseStrict(requestBody);
        if (Objects.isNull(root)) {
            return Boolean.TRUE.equals(fallbackStream);
        }
        if (root.hasNonNull("stream")) {
            return root.get("stream").asBoolean();
        }
        return Boolean.TRUE.equals(fallbackStream);
    }

    /**
     * Parse raw request body directly into ChatCompletionRequest, preserving fields
     * modeled by Spring AI (including reasoning_content in assistant messages).
     *
     * <p>Spring AI's createRequest() loses reasoning_content, refusal, and annotations
     * when reconstructing ChatCompletionMessage from AssistantMessage.
     * This method avoids that loss by deserializing the raw JSON directly.
     *
     * <p>Also converts max_completion_tokens to max_tokens for broader API compatibility.
     *
     * @param requestBody the raw JSON request body in OpenAI Chat Completions format
     * @param stream whether this is a streaming request (sets the stream field)
     * @return a ChatCompletionRequest with all fields preserved from the original request
     */
    public static ChatCompletionRequest toChatCompletionRequest(final String requestBody, final boolean stream) {
        return toChatCompletionRequest(requestBody, stream, null);
    }

    /**
     * Parse raw request body directly into ChatCompletionRequest, preserving modeled fields.
     * For model, temperature, max_tokens: client request takes priority;
     * if missing, falls back to the corresponding field in fallbackConfig.
     *
     * @param requestBody     the raw JSON request body in OpenAI Chat Completions format
     * @param stream          whether this is a streaming request
     * @param fallbackConfig  the admin config used as fallback when client omits fields
     * @return a ChatCompletionRequest with all fields preserved
     */
    public static ChatCompletionRequest toChatCompletionRequest(final String requestBody,
            final boolean stream, final AiCommonConfig fallbackConfig) {
        if (Objects.isNull(requestBody) || requestBody.isEmpty()) {
            throw new IllegalArgumentException("Request body must not be empty");
        }
        final JsonNode root = parseStrict(requestBody);
        if (Objects.isNull(root) || !root.isObject()) {
            throw new IllegalArgumentException("Invalid request body: expected a JSON object");
        }
        final ObjectNode mutableRoot = (ObjectNode) root;

        if (root.hasNonNull("max_completion_tokens") && !root.hasNonNull("max_tokens")) {
            final JsonNode tokenNode = root.get("max_completion_tokens");
            if (!tokenNode.isNumber()) {
                throw new IllegalArgumentException(
                        "max_completion_tokens must be a number, got: " + tokenNode.getNodeType());
            }
            mutableRoot.put("max_tokens", tokenNode.asInt());
            mutableRoot.remove("max_completion_tokens");
        }

        if (Objects.nonNull(fallbackConfig)) {
            if (!root.hasNonNull("model") && Objects.nonNull(fallbackConfig.getModel()) && !fallbackConfig.getModel().isEmpty()) {
                mutableRoot.put("model", fallbackConfig.getModel());
            }
            if (!root.hasNonNull("temperature") && Objects.nonNull(fallbackConfig.getTemperature())) {
                mutableRoot.put("temperature", fallbackConfig.getTemperature());
            }
            if (!root.hasNonNull("max_tokens") && Objects.nonNull(fallbackConfig.getMaxTokens())) {
                mutableRoot.put("max_tokens", fallbackConfig.getMaxTokens());
            }
        }

        mutableRoot.put("stream", stream);

        final ChatCompletionRequest result = JsonUtils.jsonToObject(mutableRoot.toString(), ChatCompletionRequest.class);
        if (Objects.isNull(result)) {
            throw new IllegalArgumentException("Failed to parse request body into ChatCompletionRequest");
        }
        return result;
    }

    private static JsonNode parseStrict(final String json) {
        try {
            return MAPPER.readTree(json);
        } catch (JsonProcessingException e) {
            return null;
        }
    }
}
