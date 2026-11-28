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

package org.apache.shenyu.plugin.ai.pii.profile;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * The Anthropic Messages profile (#8458). A request carries a top-level
 * {@code system} prompt (string or text blocks) and {@code messages} whose
 * {@code content} is a string or an array of typed blocks — text blocks carry
 * a {@code text} member, thinking blocks carry a {@code thinking} member,
 * {@code tool_use} blocks carry a JSON {@code input} object whose string
 * leaves (through nested objects and arrays) are text slots, and
 * {@code tool_result} blocks carry string-or-block content. A non streaming
 * response carries the same block shape below the top-level {@code content}.
 */
public class AnthropicMessagesProfile implements PiiProtocolProfile {

    /**
     * The profile identifier.
     */
    public static final String ID = "anthropic-messages";

    /**
     * The type of a plain text block.
     */
    private static final String TEXT_BLOCK = "text";

    /**
     * The type of a tool invocation block.
     */
    private static final String TOOL_USE_BLOCK = "tool_use";

    /**
     * The type of an extended-thinking block.
     */
    private static final String THINKING_BLOCK = "thinking";

    /**
     * The type of a tool result block.
     */
    private static final String TOOL_RESULT_BLOCK = "tool_result";

    /**
     * Bound on how deep the tool-input walker descends into nested objects
     * and arrays; deep enough for array-bearing tool inputs, deeper leaves are
     * skipped and this bound is the documented truncation point (#8458 review).
     */
    private static final int MAX_INPUT_DEPTH = 4;

    @Override
    public String id() {
        return ID;
    }

    @Override
    public List<PiiTextSlot> requestSlots(final JsonObject root) {
        List<PiiTextSlot> slots = new ArrayList<>();
        collectSystemSlots(root, slots);
        JsonElement messages = root.get("messages");
        if (Objects.nonNull(messages) && messages.isJsonArray()) {
            for (JsonElement message : messages.getAsJsonArray()) {
                if (message.isJsonObject()) {
                    collectContentSlots(message.getAsJsonObject(), slots, 0);
                }
            }
        }
        return slots;
    }

    @Override
    public List<PiiTextSlot> responseSlots(final JsonObject root) {
        List<PiiTextSlot> slots = new ArrayList<>();
        JsonElement content = root.get("content");
        if (Objects.nonNull(content) && content.isJsonArray()) {
            collectBlockSlots(content.getAsJsonArray(), slots, 0);
        }
        return slots;
    }

    private void collectSystemSlots(final JsonObject root, final List<PiiTextSlot> slots) {
        JsonElement system = root.get("system");
        if (Objects.isNull(system) || system.isJsonNull()) {
            return;
        }
        if (system.isJsonPrimitive() && system.getAsJsonPrimitive().isString()) {
            PiiTextSlot slot = PiiTextSlot.ofMember(root, "system");
            if (Objects.nonNull(slot)) {
                slots.add(slot);
            }
            return;
        }
        if (system.isJsonArray()) {
            collectBlockSlots(system.getAsJsonArray(), slots, 0);
        }
    }

    private void collectContentSlots(final JsonObject message, final List<PiiTextSlot> slots, final int depth) {
        JsonElement content = message.get("content");
        if (Objects.isNull(content) || content.isJsonNull()) {
            return;
        }
        if (content.isJsonPrimitive() && content.getAsJsonPrimitive().isString()) {
            PiiTextSlot slot = PiiTextSlot.ofMember(message, "content");
            if (Objects.nonNull(slot)) {
                slots.add(slot);
            }
            return;
        }
        if (content.isJsonArray()) {
            collectBlockSlots(content.getAsJsonArray(), slots, depth);
        }
    }

    private void collectBlockSlots(final JsonArray blocks, final List<PiiTextSlot> slots, final int depth) {
        for (JsonElement block : blocks) {
            if (!block.isJsonObject()) {
                continue;
            }
            JsonObject object = block.getAsJsonObject();
            PiiTextSlot slot = PiiTextSlot.ofMember(object, "text");
            if (Objects.nonNull(slot)) {
                slots.add(slot);
                continue;
            }
            JsonElement type = object.get("type");
            String name = Objects.nonNull(type) && isString(type) ? type.getAsString() : "";
            if (THINKING_BLOCK.equals(name)) {
                // extended-thinking prose is model- and user-authored content on both
                // request and response paths; redacted_thinking is ciphertext and needs none
                PiiTextSlot thinkingSlot = PiiTextSlot.ofMember(object, "thinking");
                if (Objects.nonNull(thinkingSlot)) {
                    slots.add(thinkingSlot);
                }
            } else if (TOOL_USE_BLOCK.equals(name)) {
                collectInputSlots(object.get("input"), slots, 0);
            } else if (TOOL_RESULT_BLOCK.equals(name) && depth < MAX_INPUT_DEPTH) {
                collectContentSlots(object, slots, depth + 1);
            }
        }
    }

    private void collectInputSlots(final JsonElement input, final List<PiiTextSlot> slots, final int depth) {
        if (Objects.isNull(input) || !input.isJsonObject() || depth > MAX_INPUT_DEPTH) {
            return;
        }
        JsonObject object = input.getAsJsonObject();
        for (String member : object.keySet()) {
            JsonElement value = object.get(member);
            if (Objects.isNull(value) || value.isJsonNull()) {
                continue;
            }
            if (value.isJsonPrimitive() && value.getAsJsonPrimitive().isString()) {
                PiiTextSlot slot = PiiTextSlot.ofMember(object, member);
                if (Objects.nonNull(slot)) {
                    slots.add(slot);
                }
            } else if (value.isJsonObject() && depth < MAX_INPUT_DEPTH) {
                collectInputSlots(value, slots, depth + 1);
            } else if (value.isJsonArray() && depth < MAX_INPUT_DEPTH) {
                collectInputArraySlots(value.getAsJsonArray(), slots, depth + 1);
            }
        }
    }

    private void collectInputArraySlots(final JsonArray array, final List<PiiTextSlot> slots, final int depth) {
        if (depth > MAX_INPUT_DEPTH) {
            return;
        }
        for (int i = 0; i < array.size(); i++) {
            JsonElement element = array.get(i);
            if (Objects.isNull(element) || element.isJsonNull()) {
                continue;
            }
            if (element.isJsonPrimitive() && element.getAsJsonPrimitive().isString()) {
                PiiTextSlot slot = PiiTextSlot.ofElement(array, i);
                if (Objects.nonNull(slot)) {
                    slots.add(slot);
                }
            } else if (element.isJsonObject() && depth < MAX_INPUT_DEPTH) {
                collectInputSlots(element, slots, depth + 1);
            } else if (element.isJsonArray() && depth < MAX_INPUT_DEPTH) {
                collectInputArraySlots(element.getAsJsonArray(), slots, depth + 1);
            }
        }
    }

    private boolean isString(final JsonElement element) {
        return Objects.nonNull(element) && element.isJsonPrimitive() && element.getAsJsonPrimitive().isString();
    }
}
