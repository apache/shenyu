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

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.springframework.ai.openai.api.OpenAiApi.ChatCompletionRequest;

import java.util.Objects;

/**
 * Adapts between OpenAI Chat Completions wire format and internal representations.
 */
public final class OpenAiProtocolAdapter {

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
        final JsonNode root = JsonUtils.toJsonNode(requestBody);
        if (Objects.isNull(root)) {
            return Boolean.TRUE.equals(fallbackStream);
        }
        if (root.hasNonNull("stream")) {
            return root.get("stream").asBoolean();
        }
        return Boolean.TRUE.equals(fallbackStream);
    }

    /**
     * Parse raw request body directly into ChatCompletionRequest, preserving ALL fields
     * including reasoning_content in assistant messages.
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
     * Parse raw request body directly into ChatCompletionRequest, preserving ALL fields.
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
        final JsonNode root = JsonUtils.toJsonNode(requestBody);
        if (Objects.isNull(root) || !root.isObject()) {
            throw new IllegalArgumentException("Invalid request body: expected a JSON object");
        }
        final ObjectNode mutableRoot = (ObjectNode) root;

        if (root.hasNonNull("max_completion_tokens") && !root.hasNonNull("max_tokens")) {
            mutableRoot.put("max_tokens", root.get("max_completion_tokens").asInt());
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

        return JsonUtils.jsonToObject(mutableRoot.toString(), ChatCompletionRequest.class);
    }
}
