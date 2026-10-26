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

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * The OpenAI chat completions profile. A request carries {@code messages}
 * whose {@code content} is either a string or an array of typed parts, and a
 * non streaming response carries the same shape below
 * {@code choices[].message.content}. Besides the plain {@code text} of a part,
 * the nested text of {@code document} and {@code file} parts and the arguments
 * of the tool calls of a message are extracted too.
 */
public class OpenAiChatProfile implements PiiProtocolProfile {

    /**
     * The profile identifier.
     */
    public static final String ID = "openai-chat";

    @Override
    public String id() {
        return ID;
    }

    @Override
    public List<PiiTextSlot> requestSlots(final JsonObject root) {
        List<PiiTextSlot> slots = new ArrayList<>();
        JsonElement messages = root.get("messages");
        if (Objects.nonNull(messages) && messages.isJsonArray()) {
            for (JsonElement message : messages.getAsJsonArray()) {
                if (message.isJsonObject()) {
                    collectContentSlots(message.getAsJsonObject(), slots);
                }
            }
        }
        return slots;
    }

    @Override
    public List<PiiTextSlot> responseSlots(final JsonObject root) {
        List<PiiTextSlot> slots = new ArrayList<>();
        JsonElement choices = root.get("choices");
        if (Objects.isNull(choices) || !choices.isJsonArray()) {
            return slots;
        }
        for (JsonElement choice : choices.getAsJsonArray()) {
            JsonElement message = choice.isJsonObject() ? choice.getAsJsonObject().get("message") : null;
            if (Objects.nonNull(message) && message.isJsonObject()) {
                collectContentSlots(message.getAsJsonObject(), slots);
            }
        }
        return slots;
    }

    private void collectContentSlots(final JsonObject message, final List<PiiTextSlot> slots) {
        JsonElement content = message.get("content");
        if (Objects.nonNull(content)) {
            if (content.isJsonPrimitive() && content.getAsJsonPrimitive().isString()) {
                PiiTextSlot slot = PiiTextSlot.ofMember(message, "content");
                if (Objects.nonNull(slot)) {
                    slots.add(slot);
                }
            } else if (content.isJsonArray()) {
                PiiContentParts.collectPartSlots(content.getAsJsonArray(), slots);
            }
        }
        // an assistant message can embed identifiers in the tool call arguments
        PiiContentParts.collectToolCallSlots(message, slots);
    }
}
