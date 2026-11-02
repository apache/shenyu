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
 * The OpenAI responses profile. A request carries {@code input}, either a
 * string or an array of items whose message text lives in {@code content},
 * itself a string, an array of typed parts with a {@code text} member, or a
 * plain {@code text} member. A non streaming response carries the message
 * text below {@code output[].content[].text}.
 */
public class OpenAiResponsesProfile implements PiiProtocolProfile {

    /**
     * The profile identifier.
     */
    public static final String ID = "openai-responses";

    @Override
    public String id() {
        return ID;
    }

    @Override
    public List<PiiTextSlot> requestSlots(final JsonObject root) {
        List<PiiTextSlot> slots = new ArrayList<>();
        JsonElement input = root.get("input");
        if (Objects.isNull(input)) {
            return slots;
        }
        if (input.isJsonPrimitive() && input.getAsJsonPrimitive().isString()) {
            PiiTextSlot slot = PiiTextSlot.ofMember(root, "input");
            if (Objects.nonNull(slot)) {
                slots.add(slot);
            }
            return slots;
        }
        if (input.isJsonArray()) {
            for (JsonElement item : input.getAsJsonArray()) {
                if (!item.isJsonObject()) {
                    continue;
                }
                JsonObject object = item.getAsJsonObject();
                collectItemSlots(object, slots);
            }
        }
        return slots;
    }

    @Override
    public List<PiiTextSlot> responseSlots(final JsonObject root) {
        List<PiiTextSlot> slots = new ArrayList<>();
        JsonElement output = root.get("output");
        if (Objects.isNull(output) || !output.isJsonArray()) {
            return slots;
        }
        for (JsonElement item : output.getAsJsonArray()) {
            if (!item.isJsonObject()) {
                continue;
            }
            JsonObject object = item.getAsJsonObject();
            collectItemSlots(object, slots);
        }
        return slots;
    }

    private void collectItemSlots(final JsonObject object, final List<PiiTextSlot> slots) {
        JsonElement content = object.get("content");
        if (Objects.nonNull(content)) {
            if (content.isJsonPrimitive() && content.getAsJsonPrimitive().isString()) {
                PiiTextSlot slot = PiiTextSlot.ofMember(object, "content");
                if (Objects.nonNull(slot)) {
                    slots.add(slot);
                }
            } else if (content.isJsonArray()) {
                for (JsonElement part : content.getAsJsonArray()) {
                    if (!part.isJsonObject()) {
                        continue;
                    }
                    PiiTextSlot slot = PiiTextSlot.ofMember(part.getAsJsonObject(), "text");
                    if (Objects.nonNull(slot)) {
                        slots.add(slot);
                    }
                }
            }
        }
        PiiTextSlot text = PiiTextSlot.ofMember(object, "text");
        if (Objects.nonNull(text)) {
            slots.add(text);
        }
    }
}
