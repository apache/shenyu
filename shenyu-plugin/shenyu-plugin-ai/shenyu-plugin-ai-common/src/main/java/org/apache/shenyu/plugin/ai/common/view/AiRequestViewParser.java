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
import org.apache.shenyu.plugin.ai.common.protocol.AiClientProtocolShape;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * Structural parser behind the bounded normalized request view.
 *
 * <p>This is a lightweight structural interpretation, not the native protocol
 * decoding owned by the AI proxy: it recognizes the openai-chat message shape,
 * the openai-responses input shape, the Anthropic Messages shape and the
 * Gemini generateContent shape (classification delegated to the shared
 * {@link AiClientProtocolShape} detector), treats every other body as an
 * explicitly unsupported representation, and never retains the original body.
 * Structured message content is reported by kind and size, never flattened
 * into text; Anthropic/Gemini text blocks are the exception — their {@code text}
 * members are plain prose, so they surface as bounded TEXT like a chat string
 * would.</p>
 */
final class AiRequestViewParser {

    private static final ObjectMapper MAPPER = new ObjectMapper().enable(DeserializationFeature.FAIL_ON_TRAILING_TOKENS);

    private static final String OPERATION_CHAT_COMPLETION = "CHAT_COMPLETION";

    private static final String OPERATION_RESPONSE = "RESPONSE";

    private static final String OPERATION_MESSAGES = "MESSAGES";

    private static final String OPERATION_GENERATE_CONTENT = "GENERATE_CONTENT";

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
        switch (AiClientProtocolShape.detect(root)) {
            case OPENAI_CHAT:
                return chatView(root, viewKind, mutator, generation);
            case OPENAI_RESPONSES:
                return responsesView(root, viewKind, mutator, generation);
            case ANTHROPIC_MESSAGES:
                return anthropicView(root, viewKind, mutator, generation);
            case GEMINI_GENERATE_CONTENT:
                return geminiView(root, viewKind, mutator, generation);
            default:
                return unsupported(viewKind, mutator, generation, "no known AI protocol fields");
        }
    }

    private static AiNormalizedRequestView chatView(final JsonNode root, final AiNormalizedRequestView.ViewKind viewKind,
                                                    final String mutator, final long generation) {
        JsonNode messages = root.get("messages");
        String model = textualOrNull(root.get("model"));
        return new AiNormalizedRequestView(AiClientProtocolShape.OPENAI_CHAT, OPERATION_CHAT_COMPLETION, model,
                boolOrFalse(root.get("stream")), boundedMessages(messages), hasArrayEntries(root.get("tools")),
                carriesToolCalls(messages), viewKind, mutator, generation, false, null);
    }

    private static AiNormalizedRequestView responsesView(final JsonNode root, final AiNormalizedRequestView.ViewKind viewKind,
                                                         final String mutator, final long generation) {
        // responses shape: structural metadata only, deep coverage is a follow-up
        String model = textualOrNull(root.get("model"));
        return new AiNormalizedRequestView(AiClientProtocolShape.OPENAI_RESPONSES, OPERATION_RESPONSE, model,
                boolOrFalse(root.get("stream")), List.of(), hasArrayEntries(root.get("tools")), false,
                viewKind, mutator, generation, false, null);
    }

    private static AiNormalizedRequestView anthropicView(final JsonNode root, final AiNormalizedRequestView.ViewKind viewKind,
                                                         final String mutator, final long generation) {
        JsonNode messages = root.get("messages");
        List<AiNormalizedRequestView.BoundedMessage> bounded = new ArrayList<>();
        AiNormalizedRequestView.BoundedMessage system = systemMessage(root.get("system"));
        if (Objects.nonNull(system)) {
            bounded.add(system);
        }
        int limit = Math.min(messages.size(), AiNormalizedRequestView.MAX_MESSAGES - bounded.size());
        for (int i = 0; i < limit; i++) {
            JsonNode message = messages.get(i);
            bounded.add(blockMessage(textualOrNull(Objects.isNull(message) ? null : message.get("role")),
                    Objects.isNull(message) ? null : message.get("content")));
        }
        return new AiNormalizedRequestView(AiClientProtocolShape.ANTHROPIC_MESSAGES, OPERATION_MESSAGES,
                textualOrNull(root.get("model")),
                boolOrFalse(root.get("stream")), List.copyOf(bounded), hasArrayEntries(root.get("tools")),
                carriesToolUseBlocks(messages), viewKind, mutator, generation, false, null);
    }

    private static AiNormalizedRequestView geminiView(final JsonNode root, final AiNormalizedRequestView.ViewKind viewKind,
                                                      final String mutator, final long generation) {
        JsonNode contents = root.get("contents");
        List<AiNormalizedRequestView.BoundedMessage> bounded = new ArrayList<>();
        AiNormalizedRequestView.BoundedMessage system = systemMessage(root.get("systemInstruction"));
        if (Objects.nonNull(system)) {
            bounded.add(system);
        }
        int limit = Math.min(contents.size(), AiNormalizedRequestView.MAX_MESSAGES - bounded.size());
        for (int i = 0; i < limit; i++) {
            JsonNode content = contents.get(i);
            // a Gemini content object carries role and parts directly
            bounded.add(blockMessage(textualOrNull(Objects.isNull(content) ? null : content.get("role")), content));
        }
        return new AiNormalizedRequestView(AiClientProtocolShape.GEMINI_GENERATE_CONTENT, OPERATION_GENERATE_CONTENT,
                null, false, List.copyOf(bounded), hasArrayEntries(root.get("tools")),
                carriesFunctionCalls(contents), viewKind, mutator, generation, false, null);
    }

    /**
     * Surface the protocol's standalone system prompt (Anthropic top-level
     * {@code system}, Gemini {@code systemInstruction}) as the leading system
     * message of the bounded view; an absent system prompt yields {@code null},
     * and one that carries no text is reported by kind only.
     */
    private static AiNormalizedRequestView.BoundedMessage systemMessage(final JsonNode system) {
        if (Objects.isNull(system) || system.isNull() || system.isMissingNode()) {
            return null;
        }
        String text = blockText(system);
        if (Objects.isNull(text) || text.isEmpty()) {
            return new AiNormalizedRequestView.BoundedMessage("system", AiNormalizedRequestView.ContentKind.STRUCTURED, -1, false, null);
        }
        boolean truncated = text.length() > AiNormalizedRequestView.MAX_MESSAGE_CHARS;
        return new AiNormalizedRequestView.BoundedMessage("system", AiNormalizedRequestView.ContentKind.TEXT,
                text.length(), truncated, truncated ? text.substring(0, AiNormalizedRequestView.MAX_MESSAGE_CHARS) : text);
    }

    /**
     * Bound one Anthropic message content (string or typed blocks) or one
     * Gemini content object ({@code role} plus {@code parts}).
     *
     * @param role the message role, {@code null} when absent
     * @param content the content node: string, block array, or Gemini content object
     * @return the bounded message
     */
    private static AiNormalizedRequestView.BoundedMessage blockMessage(final String role, final JsonNode content) {
        if (Objects.isNull(content) || content.isNull() || content.isMissingNode()) {
            return new AiNormalizedRequestView.BoundedMessage(role, AiNormalizedRequestView.ContentKind.OPAQUE, -1, false, null);
        }
        if (content.isTextual()) {
            return textMessage(role, content.asText());
        }
        // Anthropic content blocks and Gemini parts: their text members are plain
        // prose, so they surface as bounded TEXT rather than opaque structure
        String text = blockText(content);
        if (Objects.isNull(text) || text.isEmpty()) {
            return new AiNormalizedRequestView.BoundedMessage(role, AiNormalizedRequestView.ContentKind.STRUCTURED, -1, false, null);
        }
        return textMessage(role, text);
    }

    private static AiNormalizedRequestView.BoundedMessage textMessage(final String role, final String text) {
        boolean truncated = text.length() > AiNormalizedRequestView.MAX_MESSAGE_CHARS;
        return new AiNormalizedRequestView.BoundedMessage(role, AiNormalizedRequestView.ContentKind.TEXT,
                text.length(), truncated, truncated ? text.substring(0, AiNormalizedRequestView.MAX_MESSAGE_CHARS) : text);
    }

    /**
     * Concatenate the {@code text} members of {@code type:"text"} blocks (or,
     * for a bare string node, the string itself; for a Gemini content object,
     * the {@code text} members of its {@code parts}). Non-text blocks contribute
     * nothing; a node with none yields {@code null}.
     *
     * @param node a string node, an array of blocks with optional {@code text} members,
     *             or a Gemini content object carrying {@code parts}
     * @return the concatenated text, or {@code null} when there is none
     */
    private static String blockText(final JsonNode node) {
        if (Objects.isNull(node) || node.isNull() || node.isMissingNode()) {
            return null;
        }
        if (node.isTextual()) {
            return node.asText();
        }
        JsonNode blocks = node;
        if (node.isObject()) {
            blocks = node.get("parts");
        }
        if (Objects.isNull(blocks) || !blocks.isArray()) {
            return null;
        }
        StringBuilder combined = new StringBuilder();
        for (JsonNode block : blocks) {
            JsonNode text = Objects.isNull(block) ? null : block.get("text");
            if (Objects.nonNull(text) && text.isTextual()) {
                combined.append(text.asText());
            }
        }
        return combined.length() > 0 ? combined.toString() : null;
    }

    private static boolean carriesToolUseBlocks(final JsonNode messages) {
        for (JsonNode message : messages) {
            JsonNode content = Objects.isNull(message) ? null : message.get("content");
            if (Objects.nonNull(content) && content.isArray()) {
                for (JsonNode block : content) {
                    JsonNode type = Objects.isNull(block) ? null : block.get("type");
                    if (Objects.nonNull(type) && type.isTextual() && "tool_use".equals(type.asText())) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    private static boolean carriesFunctionCalls(final JsonNode contents) {
        for (JsonNode content : contents) {
            JsonNode parts = Objects.isNull(content) ? null : content.get("parts");
            if (Objects.isNull(parts) || !parts.isArray()) {
                continue;
            }
            for (JsonNode part : parts) {
                JsonNode functionCall = Objects.isNull(part) ? null : part.get("functionCall");
                if (Objects.nonNull(functionCall) && !functionCall.isNull()) {
                    return true;
                }
            }
        }
        return false;
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
