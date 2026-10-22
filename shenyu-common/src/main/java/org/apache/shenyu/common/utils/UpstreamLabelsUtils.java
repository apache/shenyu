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

package org.apache.shenyu.common.utils;

import com.google.gson.JsonElement;
import org.apache.commons.lang3.StringUtils;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * Shared validation of upstream labels in discovery properties.
 */
public final class UpstreamLabelsUtils {

    private UpstreamLabelsUtils() {
    }

    /**
     * Parse labels without coercing JSON numbers or booleans to strings.
     *
     * @param labels the labels object, or {@code null} when the property is omitted
     * @return an immutable map; omitted or empty labels clear existing labels
     * @throws IllegalArgumentException if labels are not an object of nonblank strings
     */
    public static Map<String, String> parse(final JsonElement labels) {
        if (Objects.isNull(labels)) {
            return Collections.emptyMap();
        }
        if (!labels.isJsonObject()) {
            throw new IllegalArgumentException("props.labels must be an object");
        }
        Map<String, String> result = new HashMap<>();
        for (Map.Entry<String, JsonElement> entry : labels.getAsJsonObject().entrySet()) {
            if (StringUtils.isBlank(entry.getKey())) {
                throw new IllegalArgumentException("props.labels keys must not be blank");
            }
            JsonElement value = entry.getValue();
            if (!value.isJsonPrimitive() || !value.getAsJsonPrimitive().isString()) {
                throw new IllegalArgumentException("props.labels." + entry.getKey() + " must be a string");
            }
            if (StringUtils.isBlank(value.getAsString())) {
                throw new IllegalArgumentException("props.labels." + entry.getKey() + " must not be blank");
            }
            result.put(entry.getKey(), value.getAsString());
        }
        return Collections.unmodifiableMap(result);
    }
}
