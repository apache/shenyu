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

package org.apache.shenyu.common.dto;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

/**
 * Shared immutable MCP rule configuration for Admin and the gateway.
 */
public final class AgentGatewayMcpConfig {

    private static final Set<String> FIELDS = Set.of("allowedTools", "allowedOrigins", "responseMode", "timeoutMs", "maxRequestBytes", "maxResponseBytes");

    private final Set<String> allowedTools;

    private final Set<String> allowedOrigins;

    private final String responseMode;

    private final int timeoutMs;

    private final int maxRequestBytes;

    private final int maxResponseBytes;

    private AgentGatewayMcpConfig(final JsonObject object) {
        allowedTools = strings(object, "allowedTools", false);
        allowedOrigins = strings(object, "allowedOrigins", true);
        responseMode = text(object, "responseMode", "json");
        if (!Set.of("json", "sse").contains(responseMode)) {
            throw invalid("responseMode", "must be json or sse");
        }
        timeoutMs = integer(object, "timeoutMs", 30000, 100, 120000);
        maxRequestBytes = integer(object, "maxRequestBytes", 262144, 1024, 1048576);
        maxResponseBytes = integer(object, "maxResponseBytes", 1048576, 1024, 4194304);
    }

    /**
     * Validate known values consistently while allowing a strict Admin boundary.
     *
     * @param element MCP configuration object
     * @param rejectUnknown whether to reject unknown configuration fields
     * @return immutable configuration snapshot
     */
    public static AgentGatewayMcpConfig parse(final JsonElement element, final boolean rejectUnknown) {
        if (Objects.isNull(element) || !element.isJsonObject()) {
            throw new IllegalArgumentException("mcp must be an object");
        }
        JsonObject object = element.getAsJsonObject();
        if (rejectUnknown) {
            for (String field : object.keySet()) {
                if (!FIELDS.contains(field)) {
                    throw new IllegalArgumentException("mcp contains an unknown field");
                }
            }
        }
        return new AgentGatewayMcpConfig(object);
    }

    public Set<String> getAllowedTools() {
        return allowedTools;
    }

    public Set<String> getAllowedOrigins() {
        return allowedOrigins;
    }

    public String getResponseMode() {
        return responseMode;
    }

    public int getTimeoutMs() {
        return timeoutMs;
    }

    public int getMaxRequestBytes() {
        return maxRequestBytes;
    }

    public int getMaxResponseBytes() {
        return maxResponseBytes;
    }

    private Set<String> strings(final JsonObject object, final String field, final boolean origins) {
        if (!object.has(field)) {
            return Set.of();
        }
        JsonElement element = object.get(field);
        if (!element.isJsonArray()) {
            throw invalid(field, "must be an array");
        }
        Set<String> result = new HashSet<>();
        for (JsonElement entry : element.getAsJsonArray()) {
            if (!entry.isJsonPrimitive() || !entry.getAsJsonPrimitive().isString()) {
                throw invalid(field, "must contain strings");
            }
            String value = entry.getAsString();
            if (value.isBlank() || "*".equals(value) || !value.equals(value.trim()) || !result.add(value)) {
                throw invalid(field, "must contain unique explicit nonblank values");
            }
            if (origins && !isOrigin(value)) {
                throw invalid(field, "must contain exact HTTP origins without paths or credentials");
            }
        }
        return Set.copyOf(result);
    }

    private boolean isOrigin(final String value) {
        try {
            URI origin = new URI(value);
            return Objects.nonNull(origin.getScheme()) && Set.of("http", "https").contains(origin.getScheme()) && Objects.nonNull(origin.getHost())
                    && Objects.isNull(origin.getRawUserInfo()) && Objects.isNull(origin.getRawQuery()) && Objects.isNull(origin.getRawFragment())
                    && (Objects.isNull(origin.getRawPath()) || origin.getRawPath().isEmpty())
                    && origin.getPort() >= -1 && origin.getPort() <= 65535;
        } catch (URISyntaxException error) {
            return false;
        }
    }

    private String text(final JsonObject object, final String field, final String defaultValue) {
        if (!object.has(field)) {
            return defaultValue;
        }
        JsonElement value = object.get(field);
        if (!value.isJsonPrimitive() || !value.getAsJsonPrimitive().isString()) {
            throw invalid(field, "must be a string");
        }
        return value.getAsString();
    }

    private int integer(final JsonObject object, final String field, final int defaultValue, final int minimum, final int maximum) {
        if (!object.has(field)) {
            return defaultValue;
        }
        JsonElement value = object.get(field);
        if (!value.isJsonPrimitive() || !value.getAsJsonPrimitive().isNumber()) {
            throw invalid(field, "must be an integer");
        }
        try {
            int number = value.getAsBigDecimal().intValueExact();
            if (number < minimum || number > maximum) {
                throw invalid(field, "is outside its allowed range");
            }
            return number;
        } catch (ArithmeticException | NumberFormatException error) {
            throw invalid(field, "must be an integer within its allowed range");
        }
    }

    private IllegalArgumentException invalid(final String field, final String message) {
        return new IllegalArgumentException("mcp." + field + " " + message);
    }
}
