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

package org.apache.shenyu.plugin.ai.proxy.enhanced.protocol;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.ShenyuAiMessage;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.ShenyuAiRequest;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * OpenAI Chat Completions protocol decoder.
 */
public final class OpenAiChat implements AiProxyProtocol {

    public static final String NAME = "openai-chat";

    private static final String FIELD_MESSAGES = "messages";

    private static final String FIELD_MODEL = "model";

    private static final String FIELD_STREAM = "stream";

    private static final String FIELD_MAX_TOKENS = "max_tokens";

    private static final String FIELD_MAX_COMPLETION_TOKENS = "max_completion_tokens";

    @Override
    public String name() {
        return NAME;
    }

    @Override
    public boolean matches(final JsonNode payload) {
        return Objects.nonNull(payload)
                && payload.isObject()
                && payload.has(FIELD_MESSAGES)
                && payload.get(FIELD_MESSAGES).isArray();
    }

    @Override
    public ShenyuAiRequest decodeRequest(final String rawBody, final ObjectNode payload) {
        validateRequest(payload);

        final ShenyuAiRequest request =
                new ShenyuAiRequest();

        request.setProtocol(NAME);
        request.setRawBody(rawBody);
        request.setRawPayload(payload.deepCopy());
        request.setModel(readOptionalText(
                payload, FIELD_MODEL));
        request.setStream(readStream(payload));
        request.setMaxTokens(readMaxTokens(payload));
        request.setMessages(readMessages(payload));

        return request;
    }

    private void validateRequest(
            final ObjectNode payload) {

        final JsonNode messages =
                payload.get(FIELD_MESSAGES);

        if (Objects.isNull(messages)
                || !messages.isArray()
                || messages.isEmpty()) {
            throw new ShenyuException(
                    "OpenAI Chat request requires "
                            + "a non-empty messages array");
        }

        for (JsonNode message : messages) {
            validateMessage(message);
        }

        validateBoolean(payload, FIELD_STREAM);
        validatePositiveInteger(payload, FIELD_MAX_COMPLETION_TOKENS);
        validatePositiveInteger(payload, FIELD_MAX_TOKENS);
    }

    private void validateBoolean(final ObjectNode payload, final String fieldName) {
        final JsonNode value = payload.get(fieldName);
        if (Objects.nonNull(value) && !value.isNull() && !value.isBoolean()) {
            throw new ShenyuException(
                    "OpenAI Chat field '" + fieldName + "' "
                            + "must be boolean");
        }
    }

    private void validatePositiveInteger(final ObjectNode payload, final String fieldName) {
        final JsonNode value = payload.get(fieldName);
        if (Objects.nonNull(value) && !value.isNull()
                && (!value.isIntegralNumber() || value.intValue() <= 0)) {
            throw new ShenyuException(
                    "OpenAI Chat field '" + fieldName + "' must be a positive integer");
        }
    }

    private void validateMessage(final JsonNode message) {
        if (!message.isObject()) {
            throw new ShenyuException(
                    "OpenAI Chat message must be an object");
        }

        final JsonNode role = message.get("role");
        if (Objects.isNull(role)
                || !role.isTextual()
                || role.asText().isEmpty()) {
            throw new ShenyuException(
                    "OpenAI Chat message requires role");
        }
        final JsonNode content = message.get("content");
        if (Objects.nonNull(content)
                && !content.isNull()
                && !content.isTextual()
                && !content.isArray()) {
            throw new ShenyuException(
                    "OpenAI Chat message content must be "
                            + "a string, array, or null");
        }
    }

    private Boolean readStream(
            final ObjectNode payload) {

        final JsonNode stream =
                payload.get(FIELD_STREAM);

        return Objects.nonNull(stream) && !stream.isNull()
                ? stream.booleanValue()
                : null;
    }

    private Integer readMaxTokens(
            final ObjectNode payload) {

        /*
         * Prefer the current OpenAI field but also accept
         * the legacy/OpenAI-compatible field.
         */
        final JsonNode completionTokens =
                payload.get(FIELD_MAX_COMPLETION_TOKENS);

        if (Objects.nonNull(completionTokens)
                && completionTokens.isIntegralNumber()) {
            return completionTokens.intValue();
        }

        final JsonNode maxTokens =
                payload.get(FIELD_MAX_TOKENS);

        if (Objects.nonNull(maxTokens)
                && maxTokens.isIntegralNumber()) {
            return maxTokens.intValue();
        }

        return null;
    }

    private String readOptionalText(
            final ObjectNode payload,
            final String fieldName) {

        final JsonNode value = payload.get(fieldName);

        if (Objects.isNull(value) || value.isNull()) {
            return null;
        }

        if (!value.isTextual()) {
            throw new ShenyuException(
                    "OpenAI Chat field '" + fieldName
                            + "' must be a string");
        }

        return value.asText();
    }

    private List<ShenyuAiMessage> readMessages(
            final ObjectNode payload) {

        final List<ShenyuAiMessage> result =
                new ArrayList<>();

        for (JsonNode node : payload.get(FIELD_MESSAGES)) {
            final ShenyuAiMessage message =
                    new ShenyuAiMessage();

            message.setRole(node.get("role").asText());
            final JsonNode content = node.get("content");
            if (Objects.nonNull(content) && content.isTextual()) {
                message.setContent(content.asText());
            }

            /*
             * Preserve the complete message because content can
             * be text, multimodal parts, tool calls, reasoning
             * content, or fields introduced in the future.
             */
            message.setRawPayload(
                    ((ObjectNode) node).deepCopy());

            result.add(message);
        }
        return result;
    }
}
