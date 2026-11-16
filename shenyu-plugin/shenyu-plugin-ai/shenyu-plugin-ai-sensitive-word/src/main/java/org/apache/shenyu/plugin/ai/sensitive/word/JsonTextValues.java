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

package org.apache.shenyu.plugin.ai.sensitive.word;

import com.google.gson.stream.JsonReader;

import java.io.IOException;
import java.io.StringReader;
import java.util.Objects;
import java.util.function.Predicate;

/**
 * Walks the string values of a JSON request body so that the sensitive word scan sees the text
 * a model would see instead of its serialized form.
 *
 * <p>A JSON document can spell the same text with escape sequences, for example a Unicode escape for
 * one of its letters instead of the letter itself. Scanning the raw body only would let such a
 * request reach the model while the equivalent literal request is rejected. This walker resolves the escapes and hands the
 * decoded value of every string member to the caller, which covers the model visible text of the
 * supported AI protocols because it is carried by string members: {@code messages[].content}
 * (a string or the {@code text} members of multimodal content parts), tool definitions and function
 * arguments of the OpenAI chat protocol, and {@code input} items of the OpenAI responses protocol.
 * Object member names and non string values are not model visible and are not reported.
 *
 * <p>The body itself is never modified, so the original bytes are still what the downstream
 * receives. The scan is streaming and bounded by the size of the body and by a nesting limit; a
 * string value that holds JSON is decoded once, it is not parsed again. A body that is not a JSON
 * object or array is left to the plain text scan of the caller, and so is a malformed document: the
 * walk stops at the first malformed token and the raw bytes have already been scanned as text, so a
 * malformed body is never treated as decoded and clean.
 */
final class JsonTextValues {

    /**
     * The deepest nesting that is walked. A real AI payload nests a few levels; the bound keeps the
     * work of a hostile document finite and matches the default nesting limit of the JSON reader.
     */
    private static final int MAX_NESTING_DEPTH = 64;

    private JsonTextValues() {
    }

    /**
     * Test the decoded string values of a body, in document order, until the predicate matches.
     *
     * @param body the request body
     * @param predicate the test applied to every decoded string value
     * @return true when the predicate matched a value, false for a body that is not a JSON object or
     *         array, for a malformed document and when no value matched
     */
    static boolean anyMatch(final String body, final Predicate<String> predicate) {
        if (!isJsonContainer(body)) {
            return false;
        }
        try (JsonReader reader = new JsonReader(new StringReader(body))) {
            reader.setNestingLimit(MAX_NESTING_DEPTH);
            return walk(reader, predicate);
        } catch (IOException | RuntimeException e) {
            // The raw body has already been scanned as text by the caller. A malformed document
            // cannot be decoded further, and reporting it as clean is exactly what must not happen:
            // the caller keeps the plain text result instead.
            return false;
        }
    }

    private static boolean walk(final JsonReader reader, final Predicate<String> predicate) throws IOException {
        switch (reader.peek()) {
            case BEGIN_OBJECT:
                reader.beginObject();
                while (reader.hasNext()) {
                    // member names are not model visible
                    reader.nextName();
                    if (walk(reader, predicate)) {
                        return true;
                    }
                }
                reader.endObject();
                return false;
            case BEGIN_ARRAY:
                reader.beginArray();
                while (reader.hasNext()) {
                    if (walk(reader, predicate)) {
                        return true;
                    }
                }
                reader.endArray();
                return false;
            case STRING:
                return predicate.test(reader.nextString());
            default:
                reader.skipValue();
                return false;
        }
    }

    /**
     * Whether the body starts with the opening token of a JSON object or array. A scalar or plain
     * text body is not walked: it carries no model visible string members.
     *
     * @param body the request body
     * @return true when the first non whitespace character opens a JSON container
     */
    private static boolean isJsonContainer(final String body) {
        if (Objects.isNull(body)) {
            return false;
        }
        for (int i = 0; i < body.length(); i++) {
            char c = body.charAt(i);
            if (!Character.isWhitespace(c)) {
                return c == '{' || c == '[';
            }
        }
        return false;
    }
}
