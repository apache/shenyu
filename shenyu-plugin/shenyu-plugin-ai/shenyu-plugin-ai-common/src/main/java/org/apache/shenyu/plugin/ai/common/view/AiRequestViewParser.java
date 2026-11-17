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

package org.apache.shenyu.plugin.ai.common.view;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.DeserializationFeature;
import org.apache.shenyu.common.ai.AiNormalizedRequestView;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * Structural parser behind the bounded normalized request view.
 *
 * <p>This is a lightweight structural interpretation, not the native protocol
 * decoding owned by the AI proxy: it recognizes the openai-chat message shape and
 * the openai-responses input shape, treats every other body as an explicitly
 * unsupported representation, and never retains the original body. Structured
 * message content is reported by kind and size, never flattened into text.</p>
 */
final class AiRequestViewParser {

    private static final ObjectMapper MAPPER = new ObjectMapper().enable(DeserializationFeature.FAIL_ON_TRAILING_TOKENS);

    private static final String PROTOCOL_CHAT = "openai-chat";

    private static final String PROTOCOL_RESPONSES = "openai-responses";

    private static final String OPERATION_CHAT_COMPLETION = "CHAT_COMPLETION";

    private static final String OPERATION_RESPONSE = "RESPONSE";

    private AiRequestViewParser() {
    }

    /**
     * Parse one body version into a bounded view.
     *
     * @param body the body version to interpret
     * @param viewKind whether this is the wire or an effective view
     * @param mutator component that produced an effective body, {@code null} for wire views
     * @param generation publication generation, one-based
     * @return the bounded view; unsupported bodies yield a visible unsupported view
     */
    static AiNormalizedRequestView parse(final String body, final AiNormalizedRequestView.ViewKind viewKind,
                                         final String mutator, final long generation) {
        if (Objects.isNull(body) || body.isBlank()) {
            return unsupported(viewKind, mutator, generation, "request body is empty");
        }
        final JsonNode root;
        try {
            root = MAPPER.readTree(body);
        } catch (JsonProcessingException error) {
            return unsupported(viewKind, mutator, generation, "request body is not valid JSON");
        }
        if (!root.isObject()) {
            return unsupported(viewKind, mutator, generation, "request body is not a JSON object");
        }
        JsonNode messages = root.get("messages");
        if (Objects.nonNull(messages) && messages.isArray()) {
            String model = textualOrNull(root.get("model"));
            return new AiNormalizedRequestView(PROTOCOL_CHAT, OPERATION_CHAT_COMPLETION, model,
                    boolOrFalse(root.get("stream")), boundedMessages(messages), hasArrayEntries(root.get("tools")),
                    carriesToolCalls(messages), viewKind, mutator, generation, false, null);
        }
        if (Objects.nonNull(root.get("input"))) {
            // responses shape: structural metadata only, deep coverage is a follow-up
            String model = textualOrNull(root.get("model"));
            return new AiNormalizedRequestView(PROTOCOL_RESPONSES, OPERATION_RESPONSE, model,
                    boolOrFalse(root.get("stream")), List.of(), hasArrayEntries(root.get("tools")), false,
                    viewKind, mutator, generation, false, null);
        }
        return unsupported(viewKind, mutator, generation, "no known AI protocol fields");
    }

    private static AiNormalizedRequestView unsupported(final AiNormalizedRequestView.ViewKind viewKind,
                                                       final String mutator, final long generation, final String reason) {
        return new AiNormalizedRequestView(null, null, null, false, List.of(), false, false,
                viewKind, mutator, generation, true, reason);
    }

    private static List<AiNormalizedRequestView.BoundedMessage> boundedMessages(final JsonNode messages) {
        List<AiNormalizedRequestView.BoundedMessage> bounded = new ArrayList<>();
        int limit = Math.min(messages.size(), AiNormalizedRequestView.MAX_MESSAGES);
        for (int i = 0; i < limit; i++) {
            bounded.add(boundedMessage(messages.get(i)));
        }
        return bounded;
    }

    private static AiNormalizedRequestView.BoundedMessage boundedMessage(final JsonNode message) {
        String role = textualOrNull(Objects.isNull(message) ? null : message.get("role"));
        JsonNode content = Objects.isNull(message) ? null : message.get("content");
        if (Objects.isNull(content) || content.isNull() || content.isMissingNode()) {
            return new AiNormalizedRequestView.BoundedMessage(role, AiNormalizedRequestView.ContentKind.OPAQUE, -1, false, null);
        }
        if (content.isTextual()) {
            String text = content.asText();
            boolean truncated = text.length() > AiNormalizedRequestView.MAX_MESSAGE_CHARS;
            String prefix = truncated ? text.substring(0, AiNormalizedRequestView.MAX_MESSAGE_CHARS) : text;
            return new AiNormalizedRequestView.BoundedMessage(role, AiNormalizedRequestView.ContentKind.TEXT,
                    text.length(), truncated, prefix);
        }
        // structured content is reported by kind only, it is never flattened into text
        return new AiNormalizedRequestView.BoundedMessage(role, AiNormalizedRequestView.ContentKind.STRUCTURED, -1, false, null);
    }

    private static boolean carriesToolCalls(final JsonNode messages) {
        for (JsonNode message : messages) {
            JsonNode toolCalls = Objects.isNull(message) ? null : message.get("tool_calls");
            if (Objects.nonNull(toolCalls) && !toolCalls.isNull()) {
                return true;
            }
        }
        return false;
    }

    private static boolean hasArrayEntries(final JsonNode node) {
        return Objects.nonNull(node) && node.isArray() && node.size() > 0;
    }

    private static String textualOrNull(final JsonNode node) {
        return Objects.nonNull(node) && node.isTextual() ? node.asText() : null;
    }

    private static boolean boolOrFalse(final JsonNode node) {
        return Objects.nonNull(node) && node.isBoolean() && node.asBoolean();
    }
}
