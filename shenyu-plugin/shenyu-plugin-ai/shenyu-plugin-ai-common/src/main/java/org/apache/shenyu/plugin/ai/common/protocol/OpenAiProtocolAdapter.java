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
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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

    private static final Logger LOG = LoggerFactory.getLogger(OpenAiProtocolAdapter.class);

    private static final com.fasterxml.jackson.databind.ObjectMapper MAPPER =
            new com.fasterxml.jackson.databind.ObjectMapper();

    /**
     * OpenAI API field name constants.
     */
    private static final String FIELD_MODEL = "model";

    private static final String FIELD_TEMPERATURE = "temperature";

    private static final String FIELD_STREAM = "stream";

    private static final String FIELD_MAX_COMPLETION_TOKENS = "max_completion_tokens";

    private static final String FIELD_MAX_TOKENS = "max_tokens";

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
        if (root.hasNonNull(FIELD_STREAM)) {
            return root.get(FIELD_STREAM).asBoolean();
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
     * <p>For max_tokens and max_completion_tokens: client values are preserved as-is;
 * only when the client omits both fields does the fallbackConfig value populate both,
 * ensuring compatibility with providers that require either field.
     *
     * @param requestBody the raw JSON request body in OpenAI Chat Completions format
     * @param stream whether this is a streaming request (sets the stream field)
     * @return a ChatCompletionRequest with modeled fields preserved from the original request
     */
    public static ChatCompletionRequest toChatCompletionRequest(final String requestBody, final boolean stream) {
        return toChatCompletionRequest(requestBody, stream, null);
    }

    /**
     * Parse raw request body directly into ChatCompletionRequest, preserving modeled fields.
     * When fallbackConfig is provided, its non-null fields (model, temperature, maxTokens)
     * override the client request values, matching the original ChatModel-based fallback behavior
     * where the fallback ChatModel's config takes precedence.
     *
     * @param requestBody     the raw JSON request body in OpenAI Chat Completions format
     * @param stream          whether this is a streaming request
     * @param fallbackConfig  the fallback config whose non-null fields override client values
     * @return a ChatCompletionRequest with modeled fields preserved
     */
    public static ChatCompletionRequest toChatCompletionRequest(final String requestBody,
            final boolean stream, final AiCommonConfig fallbackConfig) {
        if (Objects.isNull(requestBody) || requestBody.isEmpty()) {
            throw new ShenyuException("Request body must not be empty");
        }
        final JsonNode root = parseStrict(requestBody);
        if (Objects.isNull(root) || !root.isObject()) {
            throw new ShenyuException("Invalid request body: expected a JSON object");
        }
        final ObjectNode mutableRoot = (ObjectNode) root;

        if (Objects.nonNull(fallbackConfig)) {
            if (Objects.nonNull(fallbackConfig.getModel()) && !fallbackConfig.getModel().isEmpty()) {
                mutableRoot.put(FIELD_MODEL, fallbackConfig.getModel());
            }
            if (Objects.nonNull(fallbackConfig.getTemperature())) {
                mutableRoot.put(FIELD_TEMPERATURE, fallbackConfig.getTemperature());
            }
            if (Objects.nonNull(fallbackConfig.getMaxTokens())) {
                mutableRoot.put(FIELD_MAX_TOKENS, fallbackConfig.getMaxTokens());
                mutableRoot.put(FIELD_MAX_COMPLETION_TOKENS, fallbackConfig.getMaxTokens());
            }
        }

        mutableRoot.put(FIELD_STREAM, stream);

        try {
            return MAPPER.treeToValue(mutableRoot, ChatCompletionRequest.class);
        } catch (Exception e) {
            LOG.error("[AiProxy] Failed to parse request body into ChatCompletionRequest", e);
            throw new ShenyuException("Failed to parse request body into ChatCompletionRequest", e);
        }
    }

    private static JsonNode parseStrict(final String json) {
        try {
            return MAPPER.readTree(json);
        } catch (JsonProcessingException e) {
            return null;
        }
    }
}
