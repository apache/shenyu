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

import java.util.Objects;

/**
 * The known protocol profiles. Selection validates the containers and text
 * members used by extraction, not the complete provider schema. Malformed
 * extraction shapes are unsupported modes and follow the rule's failure policy.
 * A valid message with no text, such as a tool call, is not a malformed payload.
 */
public final class PiiProtocolProfiles {

    private static final PiiProtocolProfile CHAT = new OpenAiChatProfile();

    private static final PiiProtocolProfile RESPONSES = new OpenAiResponsesProfile();

    private PiiProtocolProfiles() {
    }

    /**
     * Select the profile of a request payload by its validated extraction shape.
     *
     * @param root the parsed payload
     * @return the matching profile, or null for unknown or malformed shapes
     */
    public static PiiProtocolProfile select(final JsonObject root) {
        if (Objects.isNull(root)) {
            return null;
        }
        if (root.has("messages")) {
            return validMessages(root.get("messages")) ? CHAT : null;
        }
        JsonElement input = root.get("input");
        return isString(input) || validItems(input) ? RESPONSES : null;
    }

    /**
     * Select the profile of a non streaming response by its extraction shape.
     *
     * @param root the parsed payload
     * @return the matching profile, or null for unknown or malformed shapes
     */
    public static PiiProtocolProfile selectResponse(final JsonObject root) {
        if (Objects.isNull(root)) {
            return null;
        }
        if (root.has("choices")) {
            return validChoices(root.get("choices")) ? CHAT : null;
        }
        return validItems(root.get("output")) ? RESPONSES : null;
    }

    private static boolean validMessages(final JsonElement messages) {
        if (Objects.isNull(messages) || !messages.isJsonArray()) {
            return false;
        }
        for (JsonElement message : messages.getAsJsonArray()) {
            if (!message.isJsonObject() || !validMessage(message.getAsJsonObject())) {
                return false;
            }
        }
        return true;
    }

    private static boolean validChoices(final JsonElement choices) {
        if (Objects.isNull(choices) || !choices.isJsonArray()) {
            return false;
        }
        for (JsonElement choice : choices.getAsJsonArray()) {
            if (!choice.isJsonObject()) {
                return false;
            }
            JsonElement message = choice.getAsJsonObject().get("message");
            if (Objects.isNull(message) || !message.isJsonObject() || !validMessage(message.getAsJsonObject())) {
                return false;
            }
        }
        return true;
    }

    private static boolean validMessage(final JsonObject message) {
        JsonElement content = message.get("content");
        if (Objects.nonNull(content) && !content.isJsonNull()) {
            return validContent(content);
        }
        JsonElement calls = message.get("tool_calls");
        JsonElement function = message.get("function_call");
        return Objects.nonNull(calls) && calls.isJsonArray()
                || Objects.nonNull(function) && function.isJsonObject()
                || isString(message.get("refusal"));
    }

    private static boolean validItems(final JsonElement items) {
        if (Objects.isNull(items) || !items.isJsonArray()) {
            return false;
        }
        for (JsonElement item : items.getAsJsonArray()) {
            if (!item.isJsonObject()) {
                return false;
            }
            JsonObject object = item.getAsJsonObject();
            JsonElement type = object.get("type");
            if (Objects.nonNull(type) && !isString(type)) {
                return false;
            }
            if (!validContent(object.get("content")) || !validTextMember(object)) {
                return false;
            }
            if ((Objects.isNull(type) || "message".equals(type.getAsString()))
                    && !validMessage(object) && !isString(object.get("text"))) {
                return false;
            }
        }
        return true;
    }

    private static boolean validContent(final JsonElement content) {
        if (Objects.isNull(content) || content.isJsonNull() || isString(content)) {
            return true;
        }
        if (!content.isJsonArray()) {
            return false;
        }
        for (JsonElement part : content.getAsJsonArray()) {
            if (!part.isJsonObject()) {
                return false;
            }
            JsonObject object = part.getAsJsonObject();
            if (!validTextMember(object)) {
                return false;
            }
            JsonElement type = object.get("type");
            if (Objects.nonNull(type)) {
                if (!isString(type)) {
                    return false;
                }
                String name = type.getAsString();
                if (("text".equals(name) || "input_text".equals(name) || "output_text".equals(name))
                        && !isString(object.get("text"))) {
                    return false;
                }
            }
        }
        return true;
    }

    private static boolean validTextMember(final JsonObject object) {
        return !object.has("text") || isString(object.get("text"));
    }

    private static boolean isString(final JsonElement element) {
        return Objects.nonNull(element) && element.isJsonPrimitive() && element.getAsJsonPrimitive().isString();
    }
}
