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

package org.apache.shenyu.common.dto.convert.rule.sticky;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;

import java.util.List;
import java.util.Objects;
import java.util.Set;

/**
 * Management-plane validation for {@link StickySessionConfig}.
 *
 * <p>The gateway keeps a tolerant runtime posture for values: a semantically malformed
 * sticky configuration degrades to plain load balancing through the runtime activation
 * checks, so an already persisted bad rule keeps working after an upgrade. This validator
 * exists for the write path instead: the admin rejects new rules whose sticky section is
 * syntactically valid JSON but semantically unusable (an unknown failure policy, a
 * non-positive lifetime, blank secrets), so operators get an error at save time rather
 * than a silently disabled feature at runtime.</p>
 *
 * <p>JSON structure is checked before Gson can coerce values or fail with a generic parse
 * error: a sticky section that is not an object, a non-integer lifetime, a non-array
 * secret list, or a deterministic key with an unknown source produces an error naming the
 * offending field. Structural checks apply even to a disabled section, because those
 * shapes cannot be bound at all; value checks in {@link #validate(StickySessionConfig)}
 * apply only to an enabled section.</p>
 */
public final class StickySessionConfigValidator {

    /**
     * Failure policies accepted for {@code sticky.onUnavailable}. Must stay in sync with
     * the runtime comparison in {@code StickySessionAffinity} (POLICY_STRICT), which is
     * case-sensitive.
     */
    private static final Set<String> FAILURE_POLICIES = Set.of("REBIND", "STRICT");

    /**
     * Sources accepted for {@code sticky.key.type}. The runtime treats anything that is
     * not {@code COOKIE} as a header lookup, so an unknown spelling must be rejected.
     */
    private static final Set<String> KEY_TYPES = Set.of("HEADER", "COOKIE");

    private StickySessionConfigValidator() {
    }

    /**
     * Validate the sticky section of a divide rule handle.
     *
     * <p>Only an enabled configuration is validated: a disabled one is inert at runtime
     * (the gateway activation checks ignore it), so it must not stop an operator from
     * editing an unrelated field of an already stored rule.
     *
     * @param config the sticky configuration, may be null (nothing to validate)
     * @throws IllegalArgumentException when a value cannot work at runtime
     */
    public static void validate(final StickySessionConfig config) {
        if (Objects.isNull(config) || !config.isEnabled()) {
            return;
        }
        validatePolicy(config.getOnUnavailable());
        validateLifetime(config.getMaxAgeSeconds());
        validateCookieName(config.getCookieName());
        validateSecrets(config.getSecrets());
        validateKey(config.getKey());
    }

    /**
     * Check the JSON structure of the sticky section before Gson can coerce values or
     * fail with a generic parse error. A missing or null section is treated as absent.
     *
     * @param sticky the raw sticky section, may be null
     * @throws IllegalArgumentException when a field has the wrong JSON type
     */
    public static void validateJson(final JsonElement sticky) {
        if (Objects.isNull(sticky) || sticky.isJsonNull()) {
            return;
        }
        if (!sticky.isJsonObject()) {
            throw new IllegalArgumentException("sticky must be an object");
        }
        JsonObject config = sticky.getAsJsonObject();
        JsonElement enabled = config.get("enabled");
        if (Objects.nonNull(enabled)
                && !(enabled.isJsonPrimitive() && enabled.getAsJsonPrimitive().isBoolean())) {
            throw new IllegalArgumentException("sticky.enabled must be a boolean");
        }
        requireInteger(config, "maxAgeSeconds", "sticky.maxAgeSeconds");
        requireString(config, "cookieName", "sticky.cookieName");
        requireString(config, "onUnavailable", "sticky.onUnavailable");
        requireStringArray(config, "secrets", "sticky.secrets");
        JsonElement key = config.get("key");
        if (Objects.isNull(key) || key.isJsonNull()) {
            return;
        }
        if (!key.isJsonObject()) {
            throw new IllegalArgumentException("sticky.key must be an object");
        }
        JsonObject keyConfig = key.getAsJsonObject();
        requireString(keyConfig, "name", "sticky.key.name");
        JsonElement type = keyConfig.get("type");
        if (Objects.isNull(type) || type.isJsonNull()) {
            return;
        }
        requireString(keyConfig, "type", "sticky.key.type");
        String value = type.getAsString();
        if (!KEY_TYPES.contains(value)) {
            throw new IllegalArgumentException("sticky.key.type must be HEADER or COOKIE, got: " + value);
        }
    }

    private static void validatePolicy(final String onUnavailable) {
        if (StringUtils.isBlank(onUnavailable)) {
            return;
        }
        // the runtime compares exactly ("STRICT"), so any other casing would silently
        // behave as REBIND; reject it here instead
        if (!FAILURE_POLICIES.contains(onUnavailable)) {
            throw new IllegalArgumentException("sticky.onUnavailable must be REBIND or STRICT, got: " + onUnavailable);
        }
    }

    private static void validateLifetime(final int maxAgeSeconds) {
        if (maxAgeSeconds <= 0) {
            throw new IllegalArgumentException("sticky.maxAgeSeconds must be a positive number, got: " + maxAgeSeconds);
        }
    }

    private static void validateCookieName(final String cookieName) {
        // an explicit JSON null is materialized as an empty string by GsonUtils, but a
        // directly constructed configuration can still hold null; both silently disable
        // affinity, so neither may pass
        if (StringUtils.isBlank(cookieName)) {
            throw new IllegalArgumentException("sticky.cookieName must not be blank; omit it to use the default");
        }
    }

    private static void validateSecrets(final List<String> secrets) {
        if (CollectionUtils.isEmpty(secrets)) {
            throw new IllegalArgumentException("sticky.secrets must contain at least one key when sticky is enabled");
        }
        boolean blank = secrets.stream().anyMatch(StringUtils::isBlank);
        if (blank) {
            throw new IllegalArgumentException("sticky.secrets must not contain blank keys");
        }
    }

    private static void validateKey(final DeterministicKeyConfig key) {
        if (Objects.isNull(key)) {
            return;
        }
        if (StringUtils.isBlank(key.getName())) {
            throw new IllegalArgumentException("sticky.key.name must not be blank when sticky.key is configured");
        }
        if (Objects.isNull(key.getType())) {
            // Gson maps an unrecognized enum constant to null, and the extractor treats
            // anything that is not COOKIE as a header lookup
            throw new IllegalArgumentException("sticky.key.type must be HEADER or COOKIE");
        }
    }

    private static void requireInteger(final JsonObject config, final String name, final String path) {
        JsonElement value = config.get(name);
        if (Objects.isNull(value) || value.isJsonNull()) {
            return;
        }
        if (!(value.isJsonPrimitive() && value.getAsJsonPrimitive().isNumber())) {
            throw new IllegalArgumentException(path + " must be an integer");
        }
        try {
            value.getAsBigDecimal().intValueExact();
        } catch (ArithmeticException | NumberFormatException ex) {
            throw new IllegalArgumentException(path + " must be an integer", ex);
        }
    }

    private static void requireString(final JsonObject config, final String name, final String path) {
        JsonElement value = config.get(name);
        if (Objects.isNull(value) || value.isJsonNull()) {
            return;
        }
        if (!(value.isJsonPrimitive() && value.getAsJsonPrimitive().isString())) {
            throw new IllegalArgumentException(path + " must be a string");
        }
    }

    private static void requireStringArray(final JsonObject config, final String name, final String path) {
        JsonElement value = config.get(name);
        if (Objects.isNull(value) || value.isJsonNull()) {
            return;
        }
        if (!value.isJsonArray()) {
            throw new IllegalArgumentException(path + " must be an array");
        }
        JsonArray array = value.getAsJsonArray();
        for (int index = 0; index < array.size(); index++) {
            JsonElement element = array.get(index);
            if (element.isJsonNull()) {
                continue;
            }
            if (!(element.isJsonPrimitive() && element.getAsJsonPrimitive().isString())) {
                throw new IllegalArgumentException(path + "[" + index + "] must be a string");
            }
        }
    }
}
