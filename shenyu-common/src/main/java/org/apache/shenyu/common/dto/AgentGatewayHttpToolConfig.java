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
import com.google.gson.JsonPrimitive;

import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * Immutable, bounded HTTP tool plans sharing the MCP Server requestTemplate and argsPosition vocabulary.
 * Targets are fixed gateway paths, never caller-supplied upstream URLs.
 */
public final class AgentGatewayHttpToolConfig {

    private final Map<String, Tool> tools;

    private AgentGatewayHttpToolConfig(final Map<String, Tool> tools) {
        this.tools = Collections.unmodifiableMap(new LinkedHashMap<>(tools));
    }

    public static AgentGatewayHttpToolConfig empty() {
        return new AgentGatewayHttpToolConfig(Map.of());
    }

    /**
     * Parse exact named tools at both the Admin and gateway boundary.
     * @param element optional tool map
     * @param allowedTools explicit rule permission names
     * @return frozen plans
     */
    public static AgentGatewayHttpToolConfig parse(final JsonElement element, final Set<String> allowedTools) {
        if (Objects.isNull(element)) {
            return empty();
        }
        JsonObject object = object(element, "httpTools");
        if (object.size() > 16) {
            throw invalid();
        }
        Map<String, Tool> plans = new LinkedHashMap<>();
        object.entrySet().forEach(entry -> {
            String name = entry.getKey();
            if (!name.matches("[A-Za-z0-9_][A-Za-z0-9_.-]{0,63}") || !allowedTools.contains(name)) {
                throw invalid();
            }
            plans.put(name, new Tool(name, object(entry.getValue(), "tool")));
        });
        return new AgentGatewayHttpToolConfig(plans);
    }

    public Map<String, Tool> getTools() {
        return tools;
    }

    private static JsonObject object(final JsonElement value, final String field) {
        if (Objects.isNull(value) || !value.isJsonObject()) {
            throw new IllegalArgumentException("mcp.httpTools " + field + " must be an object");
        }
        return value.getAsJsonObject();
    }

    private static void fields(final JsonObject object, final Set<String> fields) {
        if (!fields.containsAll(object.keySet())) {
            throw invalid();
        }
    }

    private static String text(final JsonObject object, final String field, final int maximum) {
        JsonElement value = object.get(field);
        if (Objects.isNull(value) || !value.isJsonPrimitive() || !value.getAsJsonPrimitive().isString()
                || value.getAsString().isBlank() || value.getAsString().length() > maximum) {
            throw invalid();
        }
        return value.getAsString();
    }

    private static IllegalArgumentException invalid() {
        return new IllegalArgumentException("mcp.httpTools contains an unsupported or invalid bounded HTTP tool plan");
    }

    /**
     * One fixed route and flat scalar argument mapping, independent of the caller's mutable JSON.
     */
    public static final class Tool {

        private final String name;

        private final String description;

        private final JsonObject schema;

        private final String method;

        private final String path;

        private final Map<String, String> positions;

        private final Map<String, String> types;

        private Tool(final String name, final JsonObject object) {
            fields(object, Set.of("description", "inputSchema", "requestConfig"));
            this.name = name;
            description = text(object, "description", 2048);
            schema = object(object.get("inputSchema"), "inputSchema").deepCopy();
            fields(schema, Set.of("type", "properties", "required", "additionalProperties"));
            if (!"object".equals(text(schema, "type", 16))) {
                throw invalid();
            }
            if (schema.has("additionalProperties") && (!schema.get("additionalProperties").isJsonPrimitive()
                    || !schema.getAsJsonPrimitive("additionalProperties").isBoolean() || schema.get("additionalProperties").getAsBoolean())) {
                throw invalid();
            }
            schema.addProperty("additionalProperties", false);
            JsonObject properties = object(schema.get("properties"), "properties");
            if (properties.size() > 16) {
                throw invalid();
            }
            Map<String, String> parsedTypes = new LinkedHashMap<>();
            properties.entrySet().forEach(entry -> {
                if (!entry.getKey().matches("[A-Za-z_][A-Za-z0-9_]{0,63}")) {
                    throw invalid();
                }
                JsonObject property = object(entry.getValue(), "property");
                fields(property, Set.of("type", "description"));
                String type = text(property, "type", 16);
                if (!Set.of("string", "number", "integer", "boolean").contains(type)) {
                    throw invalid();
                }
                if (property.has("description")) {
                    text(property, "description", 2048);
                }
                parsedTypes.put(entry.getKey(), type);
            });
            types = Collections.unmodifiableMap(parsedTypes);
            if (schema.has("required")) {
                if (!schema.get("required").isJsonArray() || schema.getAsJsonArray("required").size() > 16) {
                    throw invalid();
                }
                Set<String> unique = new HashSet<>();
                schema.getAsJsonArray("required").forEach(value -> {
                    if (!value.isJsonPrimitive() || !value.getAsJsonPrimitive().isString()
                            || !types.containsKey(value.getAsString()) || !unique.add(value.getAsString())) {
                        throw invalid();
                    }
                });
            }
            JsonObject config = object(object.get("requestConfig"), "requestConfig");
            fields(config, Set.of("requestTemplate", "argsPosition"));
            JsonObject template = object(config.get("requestTemplate"), "requestTemplate");
            fields(template, Set.of("method", "url"));
            method = text(template, "method", 8);
            path = text(template, "url", 512);
            if (!Set.of("GET", "POST").contains(method) || !path.matches("/[A-Za-z0-9_/-]+") || path.contains("//")) {
                throw invalid();
            }
            JsonObject mapping = object(config.get("argsPosition"), "argsPosition");
            if (!mapping.keySet().equals(types.keySet())) {
                throw invalid();
            }
            Map<String, String> parsedPositions = new LinkedHashMap<>();
            mapping.keySet().forEach(key -> {
                String position = text(mapping, key, 8);
                if (!Set.of("query", "body").contains(position) || ("GET".equals(method) && "body".equals(position))) {
                    throw invalid();
                }
                parsedPositions.put(key, position);
            });
            positions = Collections.unmodifiableMap(parsedPositions);
        }

        public String getName() {
            return name;
        }

        public String getDescription() {
            return description;
        }

        public JsonObject getInputSchema() {
            return schema.deepCopy();
        }

        public String getMethod() {
            return method;
        }

        /**
         * Validate the supported schema subset without coercing strings or guessing types.
         * @param arguments private invocation arguments
         */
        public void validate(final JsonObject arguments) {
            if (!types.keySet().containsAll(arguments.keySet())) {
                throw invalid();
            }
            if (schema.has("required")) {
                schema.getAsJsonArray("required").forEach(value -> {
                    if (!arguments.has(value.getAsString())) {
                        throw invalid();
                    }
                });
            }
            arguments.entrySet().forEach(entry -> {
                if (!entry.getValue().isJsonPrimitive()) {
                    throw invalid();
                }
                JsonPrimitive value = entry.getValue().getAsJsonPrimitive();
                String type = types.get(entry.getKey());
                if (value.toString().length() > 2048 || ("string".equals(type) && !value.isString())
                        || ("boolean".equals(type) && !value.isBoolean())
                        || (Set.of("number", "integer").contains(type) && !value.isNumber())) {
                    throw invalid();
                }
                if (Set.of("number", "integer").contains(type)) {
                    try {
                        if ("integer".equals(type)) {
                            value.getAsBigDecimal().toBigIntegerExact();
                        } else {
                            value.getAsBigDecimal();
                        }
                    } catch (NumberFormatException | ArithmeticException error) {
                        throw invalid();
                    }
                }
            });
        }

        /**
         * Build only a query on the fixed route; encoded argument values cannot replace the path.
         * @param arguments validated invocation arguments
         * @return relative route with encoded query
         */
        public String requestPath(final JsonObject arguments) {
            validate(arguments);
            StringBuilder target = new StringBuilder(path);
            positions.forEach((key, position) -> {
                if ("query".equals(position) && arguments.has(key)) {
                    target.append(target.indexOf("?") < 0 ? '?' : '&').append(encode(key)).append('=').append(encode(arguments.get(key).getAsString()));
                }
            });
            return target.toString();
        }

        public JsonObject requestBody(final JsonObject arguments) {
            validate(arguments);
            JsonObject body = new JsonObject();
            positions.forEach((key, position) -> {
                if ("body".equals(position) && arguments.has(key)) {
                    body.add(key, arguments.get(key).deepCopy());
                }
            });
            return body;
        }

        private String encode(final String value) {
            return URLEncoder.encode(value, StandardCharsets.UTF_8);
        }
    }
}
