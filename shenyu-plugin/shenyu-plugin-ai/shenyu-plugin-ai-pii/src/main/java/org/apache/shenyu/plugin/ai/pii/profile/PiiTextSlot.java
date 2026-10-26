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

import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.Locale;
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
     * the decoded payload, so detection scans the readable characters; a
     * replacement re encodes the redacted text and keeps the data URL header,
     * so the member stays a valid base64 {@code data:} URL. Binary media types,
     * non data URL strings and payloads that are not valid base64 yield no slot
     * and are left untouched, since binary content cannot be meaningfully
     * regex scanned.
     *
     * @param parent the object that holds the member
     * @param member the member name
     * @return the slot, or null when the member is not a textual base64 data URL
     */
    static PiiTextSlot ofBase64TextData(final JsonObject parent, final String member) {
        if (Objects.isNull(dataUrlPrefix(parent.get(member)))) {
            return null;
        }
        return new PiiTextSlot() {

            @Override
            public String text() {
                // read through the parent, so a slot replaced by another scan of the
                // same tree reports the replaced value instead of a stale capture
                return decodeDataUrl(parent.get(member));
            }

            @Override
            public void replace(final String newText) {
                String prefix = dataUrlPrefix(parent.get(member));
                if (Objects.nonNull(prefix)) {
                    parent.addProperty(member, prefix
                            + Base64.getEncoder().encodeToString(newText.getBytes(StandardCharsets.UTF_8)));
                }
            }
        };
    }

    /**
     * The header of a textual base64 {@code data:} URL, up to and including the
     * comma, or null when the element is not one.
     */
    private static String dataUrlPrefix(final JsonElement element) {
        if (Objects.isNull(element) || !element.isJsonPrimitive() || !element.getAsJsonPrimitive().isString()) {
            return null;
        }
        String value = element.getAsString();
        if (!value.startsWith("data:")) {
            return null;
        }
        int comma = value.indexOf(',');
        if (comma < 0) {
            return null;
        }
        String meta = value.substring("data:".length(), comma).toLowerCase(Locale.ROOT);
        if (!meta.endsWith(";base64")) {
            return null;
        }
        int semicolon = meta.indexOf(';');
        String mediaType = semicolon < 0 ? meta : meta.substring(0, semicolon);
        return isTextualMediaType(mediaType) ? value.substring(0, comma + 1) : null;
    }

    /**
     * Whether a media type carries text that can be regex scanned.
     */
    private static boolean isTextualMediaType(final String mediaType) {
        return mediaType.startsWith("text/") || "application/json".equals(mediaType);
    }

    /**
     * Decode the base64 payload of a textual {@code data:} URL element to text,
     * or null when the element is not one or the payload is not valid base64.
     */
    private static String decodeDataUrl(final JsonElement element) {
        if (Objects.isNull(dataUrlPrefix(element))) {
            return null;
        }
        String payload = element.getAsString().substring(element.getAsString().indexOf(',') + 1);
        try {
            return new String(Base64.getDecoder().decode(payload), StandardCharsets.UTF_8);
        } catch (IllegalArgumentException e) {
            return null;
        }
    }
}
