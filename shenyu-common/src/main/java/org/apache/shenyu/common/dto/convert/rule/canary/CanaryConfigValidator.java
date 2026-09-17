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

package org.apache.shenyu.common.dto.convert.rule.canary;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.convert.rule.impl.DivideRuleHandle;
import org.apache.shenyu.common.utils.DateUtils;
import org.apache.shenyu.common.utils.GsonUtils;

import java.util.Map;
import java.util.Objects;
import java.util.function.Consumer;
import java.util.regex.Pattern;

/**
 * Shared validation of Canary configuration before persistence or gateway caching.
 */
public final class CanaryConfigValidator {

    private CanaryConfigValidator() {
    }

    /**
     * Parse a Divide handle, checking JSON types before Gson can coerce values.
     * An absent or null Canary configuration preserves legacy routing.
     *
     * @param handle the serialized rule handle
     * @return the validated handle
     * @throws IllegalArgumentException when the configuration is invalid
     */
    public static DivideRuleHandle parseHandle(final String handle) {
        try {
            JsonObject root = object(GsonUtils.getInstance().fromJson(handle, JsonElement.class), "Divide handle");
            JsonElement canary = root.get("canary");
            if (Objects.nonNull(canary) && !canary.isJsonNull()) {
                validate(object(canary, "canary"));
            }
            return GsonUtils.getInstance().fromJson(root, DivideRuleHandle.class);
        } catch (JsonParseException ex) {
            throw new IllegalArgumentException("Invalid Divide handle JSON", ex);
        }
    }

    /**
     * Check extension names against the registry available at the caller.
     * Admin uses its dictionaries; gateways use their installed SPI implementations.
     *
     * @param config the validated configuration
     * @param parameterValidator parameter source validator
     * @param operatorValidator condition operator validator
     */
    public static void validateExtensions(final CanaryConfig config, final Consumer<String> parameterValidator,
                                          final Consumer<String> operatorValidator) {
        if (Objects.nonNull(config.getStickyKey())) {
            parameterValidator.accept(config.getStickyKey().getParamType());
        }
        if (Objects.nonNull(config.getConditions())) {
            for (ConditionData condition : config.getConditions()) {
                parameterValidator.accept(condition.getParamType());
                operatorValidator.accept(condition.getOperator());
            }
        }
    }

    private static void validate(final JsonObject config) {
        if (config.has("enabled") && !(config.get("enabled").isJsonPrimitive() && config.getAsJsonPrimitive("enabled").isBoolean())) {
            throw new IllegalArgumentException("canary.enabled must be a boolean");
        }
        int percentage = integer(config, "percentage", 0);
        if (percentage < 0 || percentage > 100) {
            throw new IllegalArgumentException("canary.percentage must be between 0 and 100");
        }
        int matchMode = integer(config, "matchMode", 0);
        if (matchMode != 0 && matchMode != 1) {
            throw new IllegalArgumentException("canary.matchMode must be 0 (AND) or 1 (OR)");
        }
        if (config.has("fallbackPolicy")) {
            String policy = string(config, "fallbackPolicy", "canary.fallbackPolicy", true);
            if (!"STABLE".equals(policy) && !"REJECT".equals(policy)) {
                throw new IllegalArgumentException("canary.fallbackPolicy must be STABLE or REJECT");
            }
        }
        validateLabels(config.get("stableLabels"), "canary.stableLabels", false);
        validateLabels(config.get("canaryLabels"), "canary.canaryLabels", false);
        if (config.has("stickyKey")) {
            validateSource(object(config.get("stickyKey"), "canary.stickyKey"), "canary.stickyKey");
        } else if (config.has("enabled") && config.get("enabled").getAsBoolean() && percentage > 0 && percentage < 100) {
            throw new IllegalArgumentException("canary.stickyKey is required when enabled with a percentage between 1 and 99");
        }
        if (config.has("conditions")) {
            JsonElement conditions = config.get("conditions");
            if (!conditions.isJsonArray()) {
                throw new IllegalArgumentException("canary.conditions must be an array");
            }
            int index = 0;
            for (JsonElement element : conditions.getAsJsonArray()) {
                String path = "canary.conditions[" + index++ + "]";
                JsonObject condition = object(element, path);
                validateSource(condition, path);
                String operator = string(condition, "operator", path + ".operator", true);
                String value = string(condition, "paramValue", path + ".paramValue", !"isBlank".equals(operator));
                validateConditionValue(operator, value, path);
            }
        }
        GsonUtils.getInstance().fromJson(config, CanaryConfig.class).validatePartitionLabels();
    }

    private static void validateConditionValue(final String operator, final String value, final String path) {
        try {
            if ("regex".equals(operator)) {
                Pattern.compile(value.trim());
            } else if ("TimeBefore".equals(operator) || "TimeAfter".equals(operator)) {
                DateUtils.parseLocalDateTime(value.trim());
            }
        } catch (IllegalArgumentException | java.time.DateTimeException ex) {
            throw new IllegalArgumentException(path + ".paramValue is invalid for " + operator, ex);
        }
    }

    private static void validateSource(final JsonObject source, final String path) {
        String type = string(source, "paramType", path + ".paramType", true);
        boolean named = "header".equals(type) || "cookie".equals(type) || "query".equals(type) || "post".equals(type);
        string(source, "paramName", path + ".paramName", named);
    }

    /**
     * Validate a label map without coercing numbers or booleans to strings.
     *
     * @param labels the JSON labels
     * @param path field name for errors
     * @param allowEmpty whether an empty map is allowed (clearing instance labels)
     */
    public static void validateLabels(final JsonElement labels, final String path, final boolean allowEmpty) {
        JsonObject map = object(labels, path);
        if (!allowEmpty && map.size() == 0) {
            throw new IllegalArgumentException(path + " must not be empty");
        }
        for (Map.Entry<String, JsonElement> entry : map.entrySet()) {
            if (StringUtils.isBlank(entry.getKey())) {
                throw new IllegalArgumentException(path + " keys must not be blank");
            }
            string(map, entry.getKey(), path + "." + entry.getKey(), true);
        }
    }

    private static JsonObject object(final JsonElement value, final String path) {
        if (Objects.isNull(value) || !value.isJsonObject()) {
            throw new IllegalArgumentException(path + " must be an object");
        }
        return value.getAsJsonObject();
    }

    private static int integer(final JsonObject object, final String name, final int defaultValue) {
        if (!object.has(name)) {
            return defaultValue;
        }
        JsonElement value = object.get(name);
        if (!(value.isJsonPrimitive() && value.getAsJsonPrimitive().isNumber())) {
            throw new IllegalArgumentException("canary." + name + " must be an integer");
        }
        try {
            return value.getAsBigDecimal().intValueExact();
        } catch (ArithmeticException | NumberFormatException ex) {
            throw new IllegalArgumentException("canary." + name + " must be an integer", ex);
        }
    }

    private static String string(final JsonObject object, final String name, final String path, final boolean required) {
        if (!object.has(name) && !required) {
            return null;
        }
        JsonElement value = object.get(name);
        if (Objects.isNull(value) || !(value.isJsonPrimitive() && value.getAsJsonPrimitive().isString())) {
            throw new IllegalArgumentException(path + " must be a string");
        }
        String result = value.getAsString();
        if (required && StringUtils.isBlank(result)) {
            throw new IllegalArgumentException(path + " must not be blank");
        }
        return result;
    }
}
