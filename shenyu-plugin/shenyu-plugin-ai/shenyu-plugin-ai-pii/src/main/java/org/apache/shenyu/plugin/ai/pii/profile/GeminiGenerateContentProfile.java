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
 * The Gemini generateContent profile (#8458). A request carries
 * {@code contents} whose {@code parts} carry a {@code text} member for text
 * parts, plus an optional {@code systemInstruction} with the same shape; a
 * response (stream line or complete body) carries the same content shape
 * below {@code candidates[].content}.
 */
public class GeminiGenerateContentProfile implements PiiProtocolProfile {

    /**
     * The profile identifier.
     */
    public static final String ID = "gemini-generate-content";

    @Override
    public String id() {
        return ID;
    }

    @Override
    public List<PiiTextSlot> requestSlots(final JsonObject root) {
        List<PiiTextSlot> slots = new ArrayList<>();
        JsonElement instruction = root.get("systemInstruction");
        if (Objects.nonNull(instruction) && instruction.isJsonObject()) {
            collectPartsSlots(instruction.getAsJsonObject(), slots);
        }
        JsonElement contents = root.get("contents");
        if (Objects.nonNull(contents) && contents.isJsonArray()) {
            for (JsonElement content : contents.getAsJsonArray()) {
                if (content.isJsonObject()) {
                    collectPartsSlots(content.getAsJsonObject(), slots);
                }
            }
        }
        return slots;
    }

    @Override
    public List<PiiTextSlot> responseSlots(final JsonObject root) {
        List<PiiTextSlot> slots = new ArrayList<>();
        JsonElement candidates = root.get("candidates");
        if (Objects.nonNull(candidates) && candidates.isJsonArray()) {
            for (JsonElement candidate : candidates.getAsJsonArray()) {
                JsonElement content = candidate.isJsonObject() ? candidate.getAsJsonObject().get("content") : null;
                if (Objects.nonNull(content) && content.isJsonObject()) {
                    collectPartsSlots(content.getAsJsonObject(), slots);
                }
            }
        }
        return slots;
    }

    private void collectPartsSlots(final JsonObject content, final List<PiiTextSlot> slots) {
        JsonElement parts = content.get("parts");
        if (Objects.isNull(parts) || !parts.isJsonArray()) {
            return;
        }
        for (JsonElement part : parts.getAsJsonArray()) {
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
