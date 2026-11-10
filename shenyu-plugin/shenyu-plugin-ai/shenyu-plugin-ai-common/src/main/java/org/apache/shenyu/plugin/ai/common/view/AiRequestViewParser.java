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
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.shenyu.common.ai.AiNormalizedRequestView;
import org.apache.shenyu.plugin.ai.protocol.AiProtocols;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * Structural parser behind the bounded normalized request view.
 *
 * <p>This is a lightweight structural interpretation, not the native protocol
 * decoding owned by the AI proxy. It interprets the body through the protocol
 * identity resolved by the shared detector instead of classifying the shape on
 * its own, treats every other body as an explicitly unsupported representation,
 * and never retains the original body. Structured message content is reported by
 * kind and size, never flattened into text.</p>
 */
final class AiRequestViewParser {

    private static final ObjectMapper MAPPER = new ObjectMapper().enable(DeserializationFeature.FAIL_ON_TRAILING_TOKENS);

    private static final String OPERATION_CHAT_COMPLETION = "CHAT_COMPLETION";

    private static final String OPERATION_RESPONSE = "RESPONSE";

    private AiRequestViewParser() {
    }

    /**
     * Parse one body version into a bounded view.
     *
     * @param body the body version to interpret
     * @param parsedRoot the already parsed body, {@code null} lets the parser decode it
     * @param viewKind whether this is the wire or an effective view
     * @param mutator component that produced an effective body, {@code null} for wire views
     * @param generation publication generation, one-based
     * @param protocolId the resolved protocol identity; {@code null} or
     *                   {@link AiProtocols#UNKNOWN} keeps the identity unresolved and
     *                   derives no hint from the body
     * @return the bounded view; unsupported bodies yield a visible unsupported view
     */
    static AiNormalizedRequestView parse(final String body, final JsonNode parsedRoot,
                                         final AiNormalizedRequestView.ViewKind viewKind,
                                         final String mutator, final long generation, final String protocolId) {
        final JsonNode root;
        if (Objects.nonNull(parsedRoot)) {
            root = parsedRoot;
        } else {
            if (Objects.isNull(body) || body.isBlank()) {
                return unsupported(viewKind, mutator, generation, null, "request body is empty");
            }
            try {
                root = MAPPER.readTree(body);
            } catch (JsonProcessingException error) {
                return unsupported(viewKind, mutator, generation, null, "request body is not valid JSON");
            }
        }
        if (Objects.isNull(root) || !root.isObject()) {
            return unsupported(viewKind, mutator, generation, null, "request body is not a JSON object");
        }
        final String protocol = resolvedProtocol(protocolId);
        if (AiProtocols.OPENAI_CHAT.equals(protocol)) {
            JsonNode messages = root.get("messages");
            if (Objects.isNull(messages) || !messages.isArray()) {
                return unsupported(viewKind, mutator, generation, protocol,
                        "request body does not match the resolved AI protocol");
            }
            return new AiNormalizedRequestView(AiProtocols.OPENAI_CHAT, OPERATION_CHAT_COMPLETION,
                    textualOrNull(root.get("model")), boolOrFalse(root.get("stream")), boundedMessages(messages),
                    hasArrayEntries(root.get("tools")), carriesToolCalls(messages),
                    viewKind, mutator, generation, false, null);
        }
        if (AiProtocols.OPENAI_RESPONSES.equals(protocol)) {
            if (!root.has("input")) {
                return unsupported(viewKind, mutator, generation, protocol,
                        "request body does not match the resolved AI protocol");
            }
            // responses shape: structural metadata only, deep coverage is a follow-up
            return new AiNormalizedRequestView(AiProtocols.OPENAI_RESPONSES, OPERATION_RESPONSE,
                    textualOrNull(root.get("model")), boolOrFalse(root.get("stream")), List.of(),
                    hasArrayEntries(root.get("tools")), false,
                    viewKind, mutator, generation, false, null);
        }
        return unsupported(viewKind, mutator, generation, protocol,
                Objects.isNull(protocol) ? "no known AI protocol fields" : "unsupported AI protocol view");
    }

    private static String resolvedProtocol(final String protocolId) {
        return Objects.isNull(protocolId) || protocolId.isBlank() ? null : protocolId;
    }

    private static AiNormalizedRequestView unsupported(final AiNormalizedRequestView.ViewKind viewKind,
                                                       final String mutator, final long generation,
                                                       final String protocol, final String reason) {
        return new AiNormalizedRequestView(protocol, null, null, false, List.of(), false, false,
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
