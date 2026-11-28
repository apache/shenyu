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
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Pattern;

/**
 * Immutable, bounded HTTP tool plans sharing the MCP Server requestTemplate and argsPosition vocabulary.
 * Targets are fixed gateway paths, never caller-supplied upstream URLs.
 */
public final class AgentGatewayHttpToolConfig {

    private static final String DESCRIPTION = "description";

    private static final String INPUT_SCHEMA = "inputSchema";

    private static final String REQUEST_CONFIG = "requestConfig";

    private static final int MAX_DESCRIPTION_LENGTH = 2048;

    private static final String IDENTITY_DELEGATION = "identityDelegation";

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
     * Deployment key binding and assertion policy for one fixed HTTP tool.
     */
    public static final class IdentityDelegation {

        private static final int MAX_SCOPES = 16;

        private static final int DEFAULT_TTL_SECONDS = 30;

        private static final int MAX_TTL_SECONDS = 300;

        private static final Pattern AUDIENCE = Pattern.compile("[A-Za-z0-9._:/-]{1,256}");

        private static final Pattern SCOPE = Pattern.compile("[A-Za-z0-9_.:/-]{1,64}");

        private static final Pattern CREDENTIAL_REF = Pattern.compile("[A-Za-z0-9_-]{1,64}");

        private static final Pattern CREDENTIAL_VERSION = Pattern.compile("[A-Za-z0-9_.-]{1,64}");

        private final String audience;

        private final List<String> scopes;

        private final int ttlSeconds;

        private final String credentialRef;

        private final String credentialVersion;

        private final boolean requireTenant;

        private IdentityDelegation(final JsonObject object) {
            fields(object, Set.of("audience", "scopes", "ttlSeconds", "credentialRef", "credentialVersion", "requireTenant"));
            audience = matching(object.get("audience"), AUDIENCE);
            credentialRef = matching(object.get("credentialRef"), CREDENTIAL_REF);
            credentialVersion = matching(object.get("credentialVersion"), CREDENTIAL_VERSION);
            scopes = parseScopes(object.get("scopes"));
            ttlSeconds = AgentGatewayMcpConfig.integer(object, "ttlSeconds", DEFAULT_TTL_SECONDS, 1, MAX_TTL_SECONDS);
            JsonElement tenant = object.get("requireTenant");
            if (Objects.nonNull(tenant) && (!tenant.isJsonPrimitive() || !tenant.getAsJsonPrimitive().isBoolean())) {
                throw invalid();
            }
            requireTenant = Objects.isNull(tenant) || tenant.getAsBoolean();
        }

        private static String matching(final JsonElement value, final Pattern pattern) {
            if (Objects.isNull(value) || !value.isJsonPrimitive() || !value.getAsJsonPrimitive().isString()
                    || !pattern.matcher(value.getAsString()).matches()) {
                throw invalid();
            }
            return value.getAsString();
        }

        private static List<String> parseScopes(final JsonElement value) {
            if (Objects.isNull(value) || !value.isJsonArray() || value.getAsJsonArray().isEmpty() || value.getAsJsonArray().size() > MAX_SCOPES) {
                throw invalid();
            }
            Set<String> ordered = new LinkedHashSet<>();
            for (JsonElement scope : value.getAsJsonArray()) {
                if (!ordered.add(matching(scope, SCOPE))) {
                    throw invalid();
                }
            }
            return List.copyOf(ordered);
        }

        public String getAudience() {
            return audience;
        }

        public List<String> getScopes() {
            return scopes;
        }

        public int getTtlSeconds() {
            return ttlSeconds;
        }

        public String getCredentialRef() {
            return credentialRef;
        }

        public String getCredentialVersion() {
            return credentialVersion;
        }

        public boolean isRequireTenant() {
            return requireTenant;
        }
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

        private final IdentityDelegation identityDelegation;

        private final Map<String, String> positions;

        private final Map<String, String> types;

        private Tool(final String name, final JsonObject object) {
            fields(object, Set.of(DESCRIPTION, INPUT_SCHEMA, REQUEST_CONFIG, IDENTITY_DELEGATION));
            identityDelegation = object.has(IDENTITY_DELEGATION) ? new IdentityDelegation(object(object.get(IDENTITY_DELEGATION), IDENTITY_DELEGATION)) : null;
            this.name = name;
            description = text(object, DESCRIPTION, MAX_DESCRIPTION_LENGTH);
            schema = object(object.get(INPUT_SCHEMA), INPUT_SCHEMA).deepCopy();
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
                fields(property, Set.of("type", DESCRIPTION));
                String type = text(property, "type", 16);
                if (!Set.of("string", "number", "integer", "boolean").contains(type)) {
                    throw invalid();
                }
                if (property.has(DESCRIPTION)) {
                    text(property, DESCRIPTION, MAX_DESCRIPTION_LENGTH);
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
            JsonObject config = object(object.get(REQUEST_CONFIG), REQUEST_CONFIG);
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

        public String getPath() {
            return path;
        }

        public Optional<IdentityDelegation> getIdentityDelegation() {
            return Optional.ofNullable(identityDelegation);
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
