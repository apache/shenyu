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

package org.apache.shenyu.plugin.shadow.compare;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

/**
 * JSON path normalization for body comparison.
 *
 * <p>Paths use dot notation with an optional {@code $} prefix, for example
 * {@code $.timestamp} or {@code meta.traceId}. An ignored path is removed from
 * every matching object, including objects inside arrays.</p>
 */
final class JsonPaths {

    private JsonPaths() {
    }

    static JsonElement normalized(final JsonElement source, final List<String> paths) {
        JsonElement copy = source.deepCopy();
        for (String path : paths) {
            List<String> segments = segments(path);
            if (!segments.isEmpty()) {
                remove(copy, segments, 0);
            }
        }
        return copy;
    }

    static Optional<String> firstDifference(final JsonElement left, final JsonElement right, final String path) {
        if (left.equals(right)) {
            return Optional.empty();
        }
        if (left.isJsonObject() && right.isJsonObject()) {
            return objectDifference(left.getAsJsonObject(), right.getAsJsonObject(), path);
        }
        if (left.isJsonArray() && right.isJsonArray()) {
            return arrayDifference(left.getAsJsonArray(), right.getAsJsonArray(), path);
        }
        return Optional.of(path);
    }

    private static Optional<String> objectDifference(final JsonObject left, final JsonObject right, final String path) {
        Set<String> keys = new LinkedHashSet<>(left.keySet());
        keys.addAll(right.keySet());
        for (String key : keys) {
            JsonElement leftValue = left.get(key);
            JsonElement rightValue = right.get(key);
            if (Objects.isNull(leftValue) || Objects.isNull(rightValue)) {
                return Optional.of(path + '.' + key);
            }
            Optional<String> difference = firstDifference(leftValue, rightValue, path + '.' + key);
            if (difference.isPresent()) {
                return difference;
            }
        }
        return Optional.of(path);
    }

    private static Optional<String> arrayDifference(final JsonArray left, final JsonArray right, final String path) {
        if (left.size() != right.size()) {
            return Optional.of(path + ".length");
        }
        for (int index = 0; index < left.size(); index++) {
            Optional<String> difference = firstDifference(left.get(index), right.get(index), path + '[' + index + ']');
            if (difference.isPresent()) {
                return difference;
            }
        }
        return Optional.of(path);
    }

    private static List<String> segments(final String path) {
        String normalized = path.trim();
        if (normalized.startsWith("$")) {
            normalized = normalized.substring(1);
        }
        if (normalized.startsWith(".")) {
            normalized = normalized.substring(1);
        }
        return normalized.isEmpty() ? Collections.emptyList() : Arrays.asList(normalized.split("\\."));
    }

    private static void remove(final JsonElement element, final List<String> segments, final int index) {
        if (element.isJsonArray()) {
            element.getAsJsonArray().forEach(child -> remove(child, segments, index));
            return;
        }
        if (!element.isJsonObject()) {
            return;
        }
        JsonObject object = element.getAsJsonObject();
        String segment = segments.get(index);
        if (index == segments.size() - 1) {
            object.remove(segment);
            return;
        }
        JsonElement child = object.get(segment);
        if (Objects.nonNull(child)) {
            remove(child, segments, index + 1);
        }
    }
}
