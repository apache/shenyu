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

package org.apache.shenyu.plugin.agent.gateway.tool;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import java.util.Objects;
import java.util.Set;

/**
 * Frozen local tool metadata, independent of the provider's mutable schema.
 */
public final class AgentToolDefinition {

    private static final Set<String> SCHEMA_MAPS = Set.of("properties", "patternProperties", "$defs", "dependentSchemas");

    private static final Set<String> SCHEMA_CHILDREN = Set.of("additionalProperties", "unevaluatedProperties", "items", "unevaluatedItems",
            "contains", "propertyNames", "if", "then", "else", "not");

    private static final Set<String> SCHEMA_ARRAYS = Set.of("allOf", "anyOf", "oneOf", "prefixItems");

    private final String name;

    private final String description;

    private final JsonObject inputSchema;

    private final JsonObject requiredClientCapabilities;

    AgentToolDefinition(final String name, final String description, final JsonObject inputSchema) {
        this(name, description, inputSchema, new JsonObject());
    }

    AgentToolDefinition(final String name, final String description, final JsonObject inputSchema, final JsonObject requiredClientCapabilities) {
        this.name = name;
        this.description = Objects.requireNonNull(description, "description");
        if (description.isBlank()) {
            throw new IllegalArgumentException("Tool description must not be blank");
        }
        this.inputSchema = Objects.requireNonNull(inputSchema, "inputSchema").deepCopy();
        JsonElement type = this.inputSchema.get("type");
        if (Objects.isNull(type) || !type.isJsonPrimitive() || !type.getAsJsonPrimitive().isString() || !"object".equals(type.getAsString())) {
            throw new IllegalArgumentException("Tool input schema must have object type");
        }
        validateLocalSchema(this.inputSchema);
        this.requiredClientCapabilities = Objects.requireNonNull(requiredClientCapabilities, "requiredClientCapabilities").deepCopy();
        validateCapabilities(this.requiredClientCapabilities, 0);
    }

    public String getName() {
        return name;
    }

    public String getDescription() {
        return description;
    }

    public JsonObject getInputSchema() {
        return inputSchema.deepCopy();
    }

    public JsonObject getRequiredClientCapabilities() {
        return requiredClientCapabilities.deepCopy();
    }

    JsonObject missingCapabilities(final JsonObject declared) {
        return missingCapabilities(requiredClientCapabilities, declared);
    }

    private JsonObject missingCapabilities(final JsonObject required, final JsonObject declared) {
        JsonObject missing = new JsonObject();
        required.entrySet().forEach(entry -> {
            JsonElement expected = entry.getValue();
            JsonElement actual = declared.get(entry.getKey());
            if (expected.isJsonObject() && Objects.nonNull(actual) && actual.isJsonObject()) {
                JsonObject nested = missingCapabilities(expected.getAsJsonObject(), actual.getAsJsonObject());
                if (nested.size() > 0) {
                    missing.add(entry.getKey(), nested);
                }
            } else if (!expected.isJsonPrimitive() || Objects.isNull(actual) || !actual.isJsonPrimitive()
                    || !actual.getAsJsonPrimitive().isBoolean() || !actual.getAsBoolean()) {
                missing.add(entry.getKey(), expected.deepCopy());
            }
        });
        return missing;
    }

    private void validateCapabilities(final JsonObject requirements, final int depth) {
        if (depth > 32) {
            throw new IllegalArgumentException("Capability requirements are too deeply nested");
        }
        requirements.entrySet().forEach(entry -> {
            if (entry.getKey().isBlank()) {
                throw new IllegalArgumentException("Capability requirement names must not be blank");
            }
            JsonElement value = entry.getValue();
            if (value.isJsonObject()) {
                validateCapabilities(value.getAsJsonObject(), depth + 1);
            } else if (depth == 0 || !value.isJsonPrimitive() || !value.getAsJsonPrimitive().isBoolean() || !value.getAsBoolean()) {
                throw new IllegalArgumentException("Capability requirements must be objects or nested true markers");
            }
        });
    }

    private void validateLocalSchema(final JsonElement element) {
        if (element.isJsonObject()) {
            element.getAsJsonObject().entrySet().forEach(entry -> {
                String key = entry.getKey();
                JsonElement value = entry.getValue();
                if ("x-mcp-header".equals(key)) {
                    throw new IllegalArgumentException("Custom mirrored parameter headers are not supported");
                }
                if (("$ref".equals(key) || "$dynamicRef".equals(key))
                        && (!value.isJsonPrimitive() || !value.getAsJsonPrimitive().isString() || !value.getAsString().startsWith("#"))) {
                    throw new IllegalArgumentException("Only local schema references are supported");
                }
                if (SCHEMA_MAPS.contains(key) && value.isJsonObject()) {
                    value.getAsJsonObject().entrySet().forEach(property -> validateLocalSchema(property.getValue()));
                } else if (SCHEMA_CHILDREN.contains(key)) {
                    validateLocalSchema(value);
                } else if (SCHEMA_ARRAYS.contains(key) && value.isJsonArray()) {
                    value.getAsJsonArray().forEach(this::validateLocalSchema);
                }
            });
        }
    }
}
