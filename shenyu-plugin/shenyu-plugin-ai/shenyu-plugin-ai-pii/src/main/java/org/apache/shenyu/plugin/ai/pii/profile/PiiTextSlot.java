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
import com.google.gson.JsonPrimitive;
import java.util.Objects;

/**
 * One text field of a protocol payload: its current value and the means to
 * replace it in place. A slot is a reference into the parsed JSON tree, so a
 * redaction replaces the value inside the tree and the rest of the document,
 * including escaping, tool definitions and nontext fields, is preserved by
 * the re serialization.
 */
public interface PiiTextSlot {

    /**
     * get the current value of the field.
     *
     * @return the text
     */
    String text();

    /**
     * Replace the value of the field inside the tree the slot belongs to.
     *
     * @param newText the replacement text
     */
    void replace(String newText);

    /**
     * A slot for an object member.
     *
     * @param parent the object that holds the member
     * @param member the member name
     * @return the slot, or null when the member is absent or not text
     */
    static PiiTextSlot ofMember(final JsonObject parent, final String member) {
        JsonElement element = parent.get(member);
        if (Objects.isNull(element) || !element.isJsonPrimitive() || !element.getAsJsonPrimitive().isString()) {
            return null;
        }
        return new PiiTextSlot() {

            @Override
            public String text() {
                return element.getAsString();
            }

            @Override
            public void replace(final String newText) {
                parent.addProperty(member, newText);
            }
        };
    }

    /**
     * A slot for an array element.
     *
     * @param parent the array that holds the element
     * @param index the element index
     * @return the slot, or null when the index is out of bounds or not text
     */
    static PiiTextSlot ofElement(final JsonArray parent, final int index) {
        if (index < 0 || index >= parent.size()) {
            return null;
        }
        JsonElement element = parent.get(index);
        if (!element.isJsonPrimitive() || !element.getAsJsonPrimitive().isString()) {
            return null;
        }
        return new PiiTextSlot() {

            @Override
            public String text() {
                return element.getAsString();
            }

            @Override
            public void replace(final String newText) {
                parent.set(index, new JsonPrimitive(newText));
            }
        };
    }
}
