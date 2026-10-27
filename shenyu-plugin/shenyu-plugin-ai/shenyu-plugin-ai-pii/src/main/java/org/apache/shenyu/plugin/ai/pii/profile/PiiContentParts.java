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

import java.util.List;
import java.util.Objects;

/**
 * Shared extraction of the nested text of a content part array and of the tool
 * call arguments, used by both the chat and the responses profiles. Besides the
 * plain {@code text} member of a typed part, the text of a {@code document}
 * part lives in {@code source.data} when the source is text, and the text of a
 * {@code file} part lives in a textual base64 {@code file_data}; binary
 * document sources, images and audio are left out of scope. The arguments of a
 * tool call are extracted as a text slot too, so identifiers embedded in a
 * function call are scanned instead of passed through unredacted.
 */
final class PiiContentParts {

    private PiiContentParts() {
    }

    /**
     * Collect the text slots of a content part array.
     *
     * @param parts the typed content parts
     * @param slots the collector
     */
    static void collectPartSlots(final JsonArray parts, final List<PiiTextSlot> slots) {
        for (JsonElement part : parts) {
            if (!part.isJsonObject()) {
                continue;
            }
            JsonObject object = part.getAsJsonObject();
            // the plain text member is read for every part before the type dispatch: a
            // document part that also carries a text member yields two slots, which the
            // overlapping range merge in PiiScanner collapses into a single redaction
            addIfPresent(slots, PiiTextSlot.ofMember(object, "text"));
            String type = typeOf(object);
            if ("document".equals(type)) {
                collectDocumentSlots(object, slots);
            } else if ("file".equals(type)) {
                collectFileSlots(object, slots);
            }
        }
    }

    /**
     * Collect the argument slots of the tool calls of a chat message, covering
     * both the current {@code tool_calls} array and the legacy
     * {@code function_call} member.
     *
     * @param message the chat message
     * @param slots the collector
     */
    static void collectToolCallSlots(final JsonObject message, final List<PiiTextSlot> slots) {
        JsonElement calls = message.get("tool_calls");
        if (Objects.nonNull(calls) && calls.isJsonArray()) {
            for (JsonElement call : calls.getAsJsonArray()) {
                if (!call.isJsonObject()) {
                    continue;
                }
                JsonElement function = call.getAsJsonObject().get("function");
                if (Objects.nonNull(function) && function.isJsonObject()) {
                    addIfPresent(slots, PiiTextSlot.ofMember(function.getAsJsonObject(), "arguments"));
                }
            }
        }
        JsonElement functionCall = message.get("function_call");
        if (Objects.nonNull(functionCall) && functionCall.isJsonObject()) {
            addIfPresent(slots, PiiTextSlot.ofMember(functionCall.getAsJsonObject(), "arguments"));
        }
    }

    /**
     * Collect the arguments of a responses {@code function_call} item. The
     * arguments are gated on the item type, so a stray {@code arguments} member
     * on another item type is not mistaken for message text.
     *
     * @param item the responses item
     * @param slots the collector
     */
    static void collectFunctionCallArguments(final JsonObject item, final List<PiiTextSlot> slots) {
        if ("function_call".equals(stringMember(item, "type"))) {
            addIfPresent(slots, PiiTextSlot.ofMember(item, "arguments"));
        }
    }

    private static void collectDocumentSlots(final JsonObject part, final List<PiiTextSlot> slots) {
        JsonElement source = part.get("source");
        if (Objects.isNull(source) || !source.isJsonObject()) {
            return;
        }
        JsonObject sourceObject = source.getAsJsonObject();
        // only a text source carries scannable plain text; a base64 source is binary
        if ("text".equals(stringMember(sourceObject, "type"))) {
            addIfPresent(slots, PiiTextSlot.ofMember(sourceObject, "data"));
        }
    }

    private static void collectFileSlots(final JsonObject part, final List<PiiTextSlot> slots) {
        JsonElement file = part.get("file");
        if (Objects.isNull(file) || !file.isJsonObject()) {
            return;
        }
        // file_data is a base64 data URL; only a textual media type is scannable
        addIfPresent(slots, PiiTextSlot.ofBase64TextData(file.getAsJsonObject(), "file_data"));
    }

    private static String typeOf(final JsonObject object) {
        return stringMember(object, "type");
    }

    private static String stringMember(final JsonObject object, final String member) {
        JsonElement element = object.get(member);
        return Objects.nonNull(element) && element.isJsonPrimitive() && element.getAsJsonPrimitive().isString()
                ? element.getAsString()
                : null;
    }

    private static void addIfPresent(final List<PiiTextSlot> slots, final PiiTextSlot slot) {
        if (Objects.nonNull(slot)) {
            slots.add(slot);
        }
    }
}
