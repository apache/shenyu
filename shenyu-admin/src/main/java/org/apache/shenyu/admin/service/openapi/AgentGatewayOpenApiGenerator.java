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

package org.apache.shenyu.admin.service.openapi;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import io.swagger.v3.parser.OpenAPIV3Parser;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.apache.shenyu.admin.model.dto.AgentGatewayOpenApiRequest;
import org.apache.shenyu.admin.model.dto.AgentGatewayOpenApiRequest.Mapping;
import org.apache.shenyu.admin.model.result.AgentGatewayOpenApiPreview.Generated;
import org.apache.shenyu.admin.model.result.AgentGatewayOpenApiPreview.Item;
import org.apache.shenyu.common.dto.AgentGatewayHttpToolConfig;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/** Bounded, side-effect-free projection into the existing HTTP tool contract. */
public final class AgentGatewayOpenApiGenerator {

    private static final Set<String> METHODS = Set.of("get", "put", "post", "delete", "options", "head", "patch", "trace");

    private static final ObjectMapper JSON = new ObjectMapper().enable(JsonParser.Feature.STRICT_DUPLICATE_DETECTION)
            .enable(DeserializationFeature.FAIL_ON_TRAILING_TOKENS);

    /**
     * Generate only explicitly selected operations, keeping unsupported items visible.
     * @param request explicit source and mapping
     * @return generated items and the full source operation set
     */
    public Generated generate(final AgentGatewayOpenApiRequest request) {
        require(Objects.nonNull(request.mappings()) && request.mappings().size() <= 16, "Select at most 16 operations");
        JsonObject root = document(request.document());
        JsonObject paths = object(root.get("paths"), "paths");
        List<String> identities = new ArrayList<>();
        Map<String, Integer> operationIds = new HashMap<>();
        paths.entrySet().forEach(path -> object(path.getValue(), "path item").entrySet().forEach(operation -> {
            require(!object(path.getValue(), "path item").has("$ref"), "Referenced path items prevent a complete source inventory; provide inline paths");
            if (METHODS.contains(operation.getKey())) {
                identities.add(operation.getKey().toUpperCase(Locale.ROOT) + " " + path.getKey());
                JsonObject value = object(operation.getValue(), "operation");
                if (value.has("operationId")) {
                    operationIds.merge(string(value, "operationId"), 1, Integer::sum);
                }
            }
        }));
        Map<String, Item> items = new LinkedHashMap<>();
        Set<String> names = new HashSet<>();
        for (Mapping mapping : request.mappings()) {
            require(Objects.nonNull(mapping), "Mapping must not be null");
            require(!items.containsKey(mapping.identity()), "Duplicate operation selection: " + mapping.identity());
            Item item = generate(root, paths, mapping, operationIds);
            if (Objects.nonNull(item.after()) && !names.add(item.toolName())) {
                item = new Item(item.identity(), item.toolName(), "UNSUPPORTED", null, null, List.of("Duplicate selected tool name; provide distinct toolName values"));
                String duplicateName = item.toolName();
                items.replaceAll((key, previous) -> Objects.equals(previous.toolName(), duplicateName)
                        ? new Item(previous.identity(), previous.toolName(), "UNSUPPORTED", null, null, List.of("Duplicate selected tool name; provide distinct toolName values")) : previous);
            }
            items.put(mapping.identity(), item);
        }
        return new Generated(items, List.copyOf(identities));
    }

    private Item generate(final JsonObject root, final JsonObject paths, final Mapping mapping, final Map<String, Integer> operationIds) {
        String name = mapping.toolName();
        try {
            require(Set.of("GET", "POST").contains(mapping.method()), "Only GET and POST are supported");
            require(mapping.path().matches("/[A-Za-z0-9_/-]+") && !mapping.path().contains("//"), "Source path must be literal; templates are unsupported");
            JsonObject path = object(paths.get(mapping.path()), "Selected source path");
            require(!path.has("$ref"), "Referenced path items are unsupported");
            JsonObject operation = object(path.get(mapping.method().toLowerCase(Locale.ROOT)), "Selected operation");
            if (Objects.isNull(name) || name.isBlank()) {
                name = operation.has("operationId") ? string(operation, "operationId")
                        : mapping.method().toLowerCase(Locale.ROOT) + "_" + mapping.path().substring(1).replace('/', '_').replace('-', '_');
                require(!operation.has("operationId") || operationIds.get(name) == 1, "Duplicate operationId; provide an explicit unique toolName");
            }
            require(name.matches("[A-Za-z0-9_][A-Za-z0-9_.-]{0,63}"), "Invalid tool name; provide a 1-64 character toolName using letters, digits, _, . or -");
            JsonElement security = operation.has("security") ? operation.get("security") : root.get("security");
            require(Objects.isNull(security) || security.isJsonArray() && security.getAsJsonArray().isEmpty(), "OpenAPI security requirements need explicit gateway authentication mapping");
            require(!operation.has("callbacks") || object(operation.get("callbacks"), "callbacks").isEmpty(), "Callbacks are unsupported");
            JsonObject schema = new JsonObject();
            schema.addProperty("type", "object");
            schema.addProperty("additionalProperties", false);
            JsonObject properties = new JsonObject();
            JsonArray required = new JsonArray();
            JsonObject positions = new JsonObject();
            parameters(path, operation, properties, required, positions);
            if (operation.has("requestBody")) {
                body(operation.get("requestBody"), mapping, properties, required, positions);
            }
            schema.add("properties", properties);
            schema.add("required", required);
            JsonObject tool = plan(operation, name, mapping, schema, positions);
            JsonObject tools = new JsonObject();
            tools.add(name, tool);
            AgentGatewayHttpToolConfig.parse(tools, Set.of(name));
            return new Item(mapping.identity(), name, "GENERATED", null, tool.toString(), List.of());
        } catch (IllegalArgumentException | IllegalStateException error) {
            return new Item(mapping.identity(), name, "UNSUPPORTED", null, null, List.of(error.getMessage()));
        }
    }

    private JsonObject document(final String source) {
        require(Objects.nonNull(source) && source.getBytes(StandardCharsets.UTF_8).length <= 1048576, "OpenAPI JSON must be at most 1 MiB");
        try {
            // Detect duplicate keys before the OpenAPI parser can normalize or discard them.
            JSON.readTree(source);
            JsonObject root = object(com.google.gson.JsonParser.parseString(source), "OpenAPI document");
            require(string(root, "openapi").matches("3\\.0\\.[0-9]+"), "Only OpenAPI 3.0.x JSON is supported");
            ParseOptions options = new ParseOptions();
            options.setResolve(false);
            options.setResolveFully(false);
            options.setFlatten(false);
            require(Objects.nonNull(new OpenAPIV3Parser().readContents(source, null, options).getOpenAPI()), "Invalid OpenAPI 3.0 document");
            return root;
        } catch (Exception error) {
            throw new IllegalArgumentException("Invalid or unsupported OpenAPI JSON; require unique keys and OpenAPI 3.0.x", error);
        }
    }

    private void parameters(final JsonObject path, final JsonObject operation, final JsonObject properties, final JsonArray required, final JsonObject positions) {
        Map<String, JsonObject> merged = new LinkedHashMap<>();
        for (JsonObject owner : List.of(path, operation)) {
            if (!owner.has("parameters")) {
                continue;
            }
            require(owner.get("parameters").isJsonArray(), "parameters must be an array");
            Set<String> unique = new HashSet<>();
            for (JsonElement element : owner.getAsJsonArray("parameters")) {
                JsonObject parameter = object(element, "parameter");
                require(!parameter.has("$ref"), "Referenced parameters are unsupported");
                String key = string(parameter, "in") + ":" + string(parameter, "name");
                require(unique.add(key), "Duplicate parameter: " + key);
                merged.put(key, parameter);
            }
        }
        for (JsonObject parameter : merged.values()) {
            fields(parameter, Set.of("name", "in", "description", "required", "deprecated", "style", "explode", "allowReserved", "schema", "example", "examples"));
            require("query".equals(string(parameter, "in")), "Only query parameters are supported; path/header/cookie need a separate runtime contract");
            require(!parameter.has("style") || "form".equals(string(parameter, "style")), "Only form query serialization is supported");
            require(!flag(parameter, "allowReserved"), "allowReserved query serialization is unsupported");
            if (parameter.has("explode")) {
                flag(parameter, "explode");
            }
            JsonObject property = scalar(parameter.get("schema"));
            if (parameter.has("description")) {
                property.addProperty("description", string(parameter, "description"));
            }
            add(string(parameter, "name"), property, flag(parameter, "required"), "query", properties, required, positions);
        }
    }

    private void body(final JsonElement element, final Mapping mapping, final JsonObject properties, final JsonArray required, final JsonObject positions) {
        require("POST".equals(mapping.method()), "GET request bodies are unsupported");
        JsonObject body = object(element, "requestBody");
        fields(body, Set.of("description", "required", "content"));
        JsonObject content = object(body.get("content"), "requestBody.content");
        require(content.keySet().equals(Set.of("application/json")), "Only a single application/json request media type is supported");
        JsonObject media = object(content.get("application/json"), "application/json");
        fields(media, Set.of("schema", "example", "examples"));
        JsonObject schema = object(media.get("schema"), "body schema");
        fields(schema, Set.of("type", "description", "properties", "required", "additionalProperties"));
        require("object".equals(string(schema, "type")), "JSON body must be a flat object");
        require(!schema.has("additionalProperties") || !flag(schema, "additionalProperties"), "Open-ended body properties are unsupported");
        JsonObject bodyProperties = object(schema.get("properties"), "body properties");
        Set<String> bodyRequired = new HashSet<>();
        if (schema.has("required")) {
            require(schema.get("required").isJsonArray(), "body required must be an array");
            schema.getAsJsonArray("required").forEach(value -> {
                require(value.isJsonPrimitive() && value.getAsJsonPrimitive().isString() && bodyProperties.has(value.getAsString())
                        && bodyRequired.add(value.getAsString()), "Invalid or duplicate required body property");
            });
        }
        require(flag(body, "required") || bodyRequired.isEmpty(), "Optional bodies with required properties cannot be represented without changing requiredness");
        bodyProperties.entrySet().forEach(entry -> add(entry.getKey(), scalar(entry.getValue()), bodyRequired.contains(entry.getKey()), "body", properties, required, positions));
    }

    private JsonObject scalar(final JsonElement element) {
        JsonObject schema = object(element, "scalar schema");
        fields(schema, Set.of("type", "description"));
        require(Set.of("string", "integer", "number", "boolean").contains(string(schema, "type")), "Only flat string/integer/number/boolean arguments are supported");
        return schema.deepCopy();
    }

    private void add(final String name, final JsonObject property, final boolean mandatory, final String position,
                     final JsonObject properties, final JsonArray required, final JsonObject positions) {
        require(!properties.has(name), "Query/body argument name collision: " + name);
        require(name.matches("[A-Za-z_][A-Za-z0-9_]{0,63}"), "Invalid argument name: " + name);
        require(properties.size() < 16, "At most 16 scalar arguments are supported");
        properties.add(name, property);
        positions.addProperty(name, position);
        if (mandatory) {
            required.add(name);
        }
    }

    private JsonObject plan(final JsonObject operation, final String name, final Mapping mapping, final JsonObject schema, final JsonObject positions) {
        JsonObject tool = new JsonObject();
        String description = operation.has("description") ? string(operation, "description") : operation.has("summary") ? string(operation, "summary") : name;
        tool.addProperty("description", description);
        tool.add("inputSchema", schema);
        JsonObject template = new JsonObject();
        template.addProperty("method", mapping.method());
        template.addProperty("url", mapping.gatewayPath());
        JsonObject config = new JsonObject();
        config.add("requestTemplate", template);
        config.add("argsPosition", positions);
        tool.add("requestConfig", config);
        return tool;
    }

    private static JsonObject object(final JsonElement element, final String field) {
        require(Objects.nonNull(element) && element.isJsonObject(), field + " must be an object");
        return element.getAsJsonObject();
    }

    private static String string(final JsonObject object, final String field) {
        JsonElement value = object.get(field);
        require(Objects.nonNull(value) && value.isJsonPrimitive() && value.getAsJsonPrimitive().isString(), field + " must be a string");
        return value.getAsString();
    }

    private static boolean flag(final JsonObject object, final String field) {
        if (!object.has(field)) {
            return false;
        }
        JsonElement value = object.get(field);
        require(value.isJsonPrimitive() && value.getAsJsonPrimitive().isBoolean(), field + " must be a boolean");
        return value.getAsBoolean();
    }

    private static void fields(final JsonObject object, final Set<String> supported) {
        Set<String> unsupported = new HashSet<>(object.keySet());
        unsupported.removeAll(supported);
        require(unsupported.isEmpty(), "Unsupported fields/constraints: " + unsupported);
    }

    private static void require(final boolean valid, final String message) {
        if (!valid) {
            throw new IllegalArgumentException(message);
        }
    }
}
