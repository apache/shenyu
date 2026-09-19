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

package org.apache.shenyu.k8s.common;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;

import java.util.Objects;

/**
 * Null-safe field accessors for reading optional fields out of the gson {@link JsonObject}
 * trees of dynamic Gateway API objects, where any field may be absent, JSON null, or of an
 * unexpected type (status sections are written by other controllers and are not covered by
 * CRD schema validation). Accessors return null instead of throwing on type mismatch, so a
 * malformed object degrades to a reconcile no-op instead of an infinite retry loop.
 */
public final class JsonFields {

    private JsonFields() {
    }

    public static String getString(final JsonObject obj, final String field) {
        if (Objects.isNull(obj) || !obj.has(field) || obj.get(field).isJsonNull()
                || !obj.get(field).isJsonPrimitive()) {
            return null;
        }
        return obj.get(field).getAsString();
    }

    /**
     * Read an optional numeric field as Long.
     *
     * @param obj the object to read from
     * @param field the field name
     * @return the number, or null when absent or not numeric
     */
    public static Long getLong(final JsonObject obj, final String field) {
        if (Objects.isNull(obj) || !obj.has(field) || obj.get(field).isJsonNull()
                || !obj.get(field).isJsonPrimitive()) {
            return null;
        }
        try {
            return obj.get(field).getAsLong();
        } catch (NumberFormatException | UnsupportedOperationException ex) {
            return null;
        }
    }

    public static JsonObject getJsonObject(final JsonObject obj, final String field) {
        if (Objects.isNull(obj) || !obj.has(field) || obj.get(field).isJsonNull()
                || !obj.get(field).isJsonObject()) {
            return null;
        }
        return obj.getAsJsonObject(field);
    }

    public static JsonArray getJsonArray(final JsonObject obj, final String field) {
        if (Objects.isNull(obj) || !obj.has(field) || obj.get(field).isJsonNull()
                || !obj.get(field).isJsonArray()) {
            return null;
        }
        return obj.getAsJsonArray(field);
    }
}
