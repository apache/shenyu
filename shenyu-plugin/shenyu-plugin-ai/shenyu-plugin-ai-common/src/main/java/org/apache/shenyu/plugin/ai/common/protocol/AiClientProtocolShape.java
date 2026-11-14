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

import java.util.Objects;

/**
 * Single-source structural detection of the client AI protocol shapes the
 * governance plugins read (#8458). The detector is deliberately shallow: it
 * classifies by the discriminating top-level members only — it never validates
 * a complete provider schema and never retains payload content.
 *
 * <p>An Anthropic Messages request without a top-level {@code system} member is
 * structurally indistinguishable from an openai-chat request (block-shaped
 * content is valid in both); it classifies as {@link Shape#OPENAI_CHAT}, and
 * the openai-chat readers handle its block content gracefully.</p>
 */
public final class AiClientProtocolShape {

    /**
     * The protocol names used across the AI plugins, aligned with
     * {@code AiProxyProtocol.name()} and {@code AiCommonConfig.protocol}.
     */
    public static final String OPENAI_CHAT = "openai-chat";

    public static final String OPENAI_RESPONSES = "openai-responses";

    public static final String ANTHROPIC_MESSAGES = "anthropic-messages";

    public static final String GEMINI_GENERATE_CONTENT = "gemini-generate-content";

    /**
     * The detected client protocol shape.
     */
    public enum Shape {
        /**
         * OpenAI chat completions: {@code messages} without a top-level {@code system}.
         */
        OPENAI_CHAT,
        /**
         * OpenAI responses: top-level {@code input}.
         */
        OPENAI_RESPONSES,
        /**
         * Anthropic Messages: {@code messages} plus the top-level {@code system}
         * member that openai-chat never carries.
         */
        ANTHROPIC_MESSAGES,
        /**
         * Gemini generateContent: top-level {@code contents}.
         */
        GEMINI_GENERATE_CONTENT,
        /**
         * No known AI protocol shape.
         */
        UNKNOWN
    }

    private AiClientProtocolShape() {
    }

    /**
     * Detect the client protocol shape of a parsed request body.
     *
     * @param root the parsed request body, may be {@code null}
     * @return the shape; {@link Shape#UNKNOWN} for null or unrecognized roots
     */
    public static Shape detect(final JsonNode root) {
        if (Objects.isNull(root) || !root.isObject()) {
            return Shape.UNKNOWN;
        }
        JsonNode messages = root.get("messages");
        if (Objects.nonNull(messages) && messages.isArray()) {
            return Objects.nonNull(root.get("system")) ? Shape.ANTHROPIC_MESSAGES : Shape.OPENAI_CHAT;
        }
        if (Objects.nonNull(root.get("input"))) {
            return Shape.OPENAI_RESPONSES;
        }
        JsonNode contents = root.get("contents");
        if (Objects.nonNull(contents) && contents.isArray()) {
            return Shape.GEMINI_GENERATE_CONTENT;
        }
        return Shape.UNKNOWN;
    }

    /**
     * The protocol name string for a shape, aligned with the AI proxy's
     * protocol identifiers.
     *
     * @param shape the detected shape
     * @return the protocol name, or {@code null} for {@link Shape#UNKNOWN}
     */
    public static String protocolName(final Shape shape) {
        switch (shape) {
            case OPENAI_CHAT:
                return OPENAI_CHAT;
            case OPENAI_RESPONSES:
                return OPENAI_RESPONSES;
            case ANTHROPIC_MESSAGES:
                return ANTHROPIC_MESSAGES;
            case GEMINI_GENERATE_CONTENT:
                return GEMINI_GENERATE_CONTENT;
            default:
                return null;
        }
    }
}
