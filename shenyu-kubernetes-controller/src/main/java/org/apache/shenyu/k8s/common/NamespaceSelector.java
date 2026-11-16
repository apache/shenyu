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
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.regex.Pattern;

/**
 * Strict, read-only Kubernetes label selector matching for Namespace labels.
 */
public final class NamespaceSelector {

    private static final Set<String> SELECTOR_FIELDS = Set.of("matchLabels", "matchExpressions");

    private static final Set<String> EXPRESSION_FIELDS = Set.of("key", "operator", "values");

    private static final Pattern LABEL_NAME = Pattern.compile("[A-Za-z0-9]([A-Za-z0-9_.-]*[A-Za-z0-9])?");

    private static final Pattern DNS_PREFIX = Pattern.compile("[a-z0-9]([a-z0-9-]*[a-z0-9])?(\\.[a-z0-9]([a-z0-9-]*[a-z0-9])?)*");

    private NamespaceSelector() {
    }

    /**
     * Match all requirements after validating the entire selector.
     * Omitted optional fields and empty collections impose no requirements, so an empty
     * selector matches any present Namespace, including one with no labels. Explicit JSON
     * nulls, unknown fields and malformed requirements fail closed without JSON coercion.
     * In and NotIn require nonempty values; Exists and DoesNotExist allow omitted or empty
     * values. NotIn also matches an absent key, as required by Kubernetes.
     *
     * @param selector raw label selector, null when missing
     * @param labels Namespace labels; use an empty map for a present, unlabeled Namespace and null for a missing Namespace
     * @return true only when the selector is valid, the Namespace is present and all requirements match
     */
    public static boolean matches(final JsonObject selector, final Map<String, String> labels) {
        if (Objects.isNull(selector) || Objects.isNull(labels) || !isValidSelector(selector)) {
            return false;
        }
        if (selector.has("matchLabels")) {
            for (Map.Entry<String, JsonElement> entry : selector.getAsJsonObject("matchLabels").entrySet()) {
                if (!entry.getValue().getAsString().equals(labels.get(entry.getKey()))) {
                    return false;
                }
            }
        }
        if (selector.has("matchExpressions")) {
            for (JsonElement expression : selector.getAsJsonArray("matchExpressions")) {
                if (!matchesExpression(expression.getAsJsonObject(), labels)) {
                    return false;
                }
            }
        }
        return true;
    }

    private static boolean isValidSelector(final JsonObject selector) {
        if (!SELECTOR_FIELDS.containsAll(selector.keySet())) {
            return false;
        }
        if (selector.has("matchLabels")) {
            JsonElement matchLabels = selector.get("matchLabels");
            if (!matchLabels.isJsonObject()) {
                return false;
            }
            for (Map.Entry<String, JsonElement> entry : matchLabels.getAsJsonObject().entrySet()) {
                if (!isValidKey(entry.getKey()) || !isValidValue(entry.getValue())) {
                    return false;
                }
            }
        }
        if (selector.has("matchExpressions")) {
            JsonElement expressions = selector.get("matchExpressions");
            if (!expressions.isJsonArray()) {
                return false;
            }
            for (JsonElement expression : expressions.getAsJsonArray()) {
                if (!expression.isJsonObject() || !isValidExpression(expression.getAsJsonObject())) {
                    return false;
                }
            }
        }
        return true;
    }

    private static boolean isValidExpression(final JsonObject expression) {
        if (!EXPRESSION_FIELDS.containsAll(expression.keySet())
                || !isString(expression.get("key")) || !isValidKey(expression.get("key").getAsString())
                || !isString(expression.get("operator"))) {
            return false;
        }
        JsonElement values = expression.get("values");
        if (expression.has("values") && !values.isJsonArray()) {
            return false;
        }
        if (Objects.nonNull(values)) {
            for (JsonElement value : values.getAsJsonArray()) {
                if (!isValidValue(value)) {
                    return false;
                }
            }
        }
        String operator = expression.get("operator").getAsString();
        switch (operator) {
            case "In":
            case "NotIn":
                return Objects.nonNull(values) && !values.getAsJsonArray().isEmpty();
            case "Exists":
            case "DoesNotExist":
                return Objects.isNull(values) || values.getAsJsonArray().isEmpty();
            default:
                return false;
        }
    }

    private static boolean matchesExpression(final JsonObject expression, final Map<String, String> labels) {
        String key = expression.get("key").getAsString();
        String operator = expression.get("operator").getAsString();
        switch (operator) {
            case "In":
                return labels.containsKey(key) && containsValue(expression.getAsJsonArray("values"), labels.get(key));
            case "NotIn":
                return !labels.containsKey(key) || !containsValue(expression.getAsJsonArray("values"), labels.get(key));
            case "Exists":
                return labels.containsKey(key);
            case "DoesNotExist":
                return !labels.containsKey(key);
            default:
                return false;
        }
    }

    private static boolean containsValue(final JsonArray values, final String label) {
        for (JsonElement value : values) {
            if (value.getAsString().equals(label)) {
                return true;
            }
        }
        return false;
    }

    private static boolean isString(final JsonElement value) {
        return Objects.nonNull(value) && value.isJsonPrimitive() && value.getAsJsonPrimitive().isString();
    }

    private static boolean isValidValue(final JsonElement value) {
        if (!isString(value)) {
            return false;
        }
        String text = value.getAsString();
        return text.length() <= 63 && (text.isEmpty() || LABEL_NAME.matcher(text).matches());
    }

    private static boolean isValidKey(final String key) {
        int slash = key.indexOf('/');
        String name = slash < 0 ? key : key.substring(slash + 1);
        if (name.isEmpty() || name.length() > 63 || !LABEL_NAME.matcher(name).matches()) {
            return false;
        }
        if (slash < 0) {
            return true;
        }
        String prefix = key.substring(0, slash);
        return !prefix.isEmpty() && prefix.length() <= 253 && DNS_PREFIX.matcher(prefix).matches();
    }
}
