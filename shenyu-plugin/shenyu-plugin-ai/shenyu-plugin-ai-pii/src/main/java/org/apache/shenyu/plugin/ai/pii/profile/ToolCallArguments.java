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
import com.google.gson.JsonParseException;
import com.google.gson.JsonParser;
import com.google.gson.JsonPrimitive;

import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * The decoded string values of a tool or function call {@code arguments} member.
 * The arguments are themselves a serialized JSON document, so scanning the raw
 * member text misses any identifier the tool recovers by parsing it:
 * {@code {"email":"alice\u0040example.com"}} carries no literal {@code @} yet
 * decodes to {@code alice@example.com}. The member is therefore parsed and every
 * string value inside it is exposed as a slot over its decoded text, see
 * {@link ArgumentValueSlot}. A blank member carries no text and yields no slot;
 * a member that is neither blank nor parseable JSON is malformed and is rejected
 * at selection by {@link PiiProtocolProfiles}, so it follows the rule failure
 * policy instead of leaking an unscannable value.
 */
final class ToolCallArguments {

    private ToolCallArguments() {
    }

    /**
     * Collect a slot per decoded string value of the {@code arguments} member.
     *
     * @param holder the object that holds the member
     * @param member the member name
     * @param slots the collector
     */
    static void collectSlots(final JsonObject holder, final String member, final List<PiiTextSlot> slots) {
        JsonElement element = holder.get(member);
        if (!isString(element)) {
            return;
        }
        JsonElement parsed = parse(element.getAsString());
        if (Objects.isNull(parsed)) {
            // blank carries no text and malformed is rejected at selection, neither yields a slot
            return;
        }
        walk(holder, member, parsed, parsed, slots);
    }

    /**
     * Whether the member is a non blank string that is not parseable JSON, a
     * malformed extraction shape rather than an empty argument list.
     *
     * @param element the member value
     * @return true when the arguments are present but cannot be parsed
     */
    static boolean isMalformed(final JsonElement element) {
        if (!isString(element)) {
            return false;
        }
        String value = element.getAsString();
        // a blank argument list carries no text and is not malformed
        return !value.trim().isEmpty() && Objects.isNull(parse(value));
    }

    private static void walk(final JsonObject holder, final String member, final JsonElement root,
                             final JsonElement current, final List<PiiTextSlot> slots) {
        if (current.isJsonObject()) {
            JsonObject object = current.getAsJsonObject();
            for (Map.Entry<String, JsonElement> entry : object.entrySet()) {
                String key = entry.getKey();
                if (isString(entry.getValue())) {
                    slots.add(new ArgumentValueSlot(holder, member, root,
                            () -> stringOf(object.get(key)), value -> object.addProperty(key, value)));
                } else {
                    walk(holder, member, root, entry.getValue(), slots);
                }
            }
        } else if (current.isJsonArray()) {
            JsonArray array = current.getAsJsonArray();
            for (int i = 0; i < array.size(); i++) {
                final int index = i;
                if (isString(array.get(index))) {
                    slots.add(new ArgumentValueSlot(holder, member, root,
                            () -> stringOf(array.get(index)), value -> array.set(index, new JsonPrimitive(value))));
                } else {
                    walk(holder, member, root, array.get(index), slots);
                }
            }
        }
    }

    private static JsonElement parse(final String arguments) {
        if (arguments.trim().isEmpty()) {
            return null;
        }
        try {
            return JsonParser.parseString(arguments);
        } catch (JsonParseException e) {
            return null;
        }
    }

    private static String stringOf(final JsonElement element) {
        return isString(element) ? element.getAsString() : null;
    }

    private static boolean isString(final JsonElement element) {
        return Objects.nonNull(element) && element.isJsonPrimitive() && element.getAsJsonPrimitive().isString();
    }
}
