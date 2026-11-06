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
                // read through the parent, so a slot replaced by another scan of the
                // same tree reports the replaced value instead of a stale capture
                JsonElement current = parent.get(member);
                return Objects.isNull(current) ? null : current.getAsString();
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
                // read through the parent, see ofMember
                JsonElement current = index < parent.size() ? parent.get(index) : null;
                return Objects.isNull(current) ? null : current.getAsString();
            }

            @Override
            public void replace(final String newText) {
                parent.set(index, new JsonPrimitive(newText));
            }
        };
    }

    /**
     * A slot for a base64 {@code data:} URL member that carries text, for
     * example the {@code file_data} of an OpenAI file content part. The text is
     * the decoded payload, so detection scans the readable characters; it is
     * decoded once and cached for the lifetime of the slot, so a large payload
     * is not decoded again per read. A replacement re encodes the redacted text
     * under the original data URL header, so the member stays a valid base64
     * {@code data:} URL. The re encoding uses the standard padded alphabet, so
     * a MIME wrapped or URL safe original is normalised: this one member does
     * not keep its exact encoding style outside the redacted range, which is a
     * documented tradeoff rather than a corruption. A binary media type or a
     * non data URL string yields no slot and is left untouched, since binary
     * content cannot be meaningfully regex scanned; see {@link Base64TextData}.
     *
     * @param parent the object that holds the member
     * @param member the member name
     * @return the slot, or null when the member is not a textual base64 data URL
     */
    static PiiTextSlot ofBase64TextData(final JsonObject parent, final String member) {
        JsonElement element = parent.get(member);
        if (Objects.isNull(element) || !element.isJsonPrimitive()
                || !Base64TextData.isTextualDataUrl(element.getAsString())) {
            return null;
        }
        return new Base64TextDataSlot(parent, member);
    }
}
