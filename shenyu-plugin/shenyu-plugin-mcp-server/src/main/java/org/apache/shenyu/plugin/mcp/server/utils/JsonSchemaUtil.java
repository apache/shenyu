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

package org.apache.shenyu.plugin.mcp.server.utils;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.github.victools.jsonschema.generator.SchemaVersion;
import io.micrometer.common.util.StringUtils;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.plugin.mcp.server.model.McpServerToolParameter;
import org.springframework.ai.util.json.JsonParser;
import org.springframework.ai.util.json.schema.JsonSchemaGenerator.SchemaOption;

import java.util.List;
import java.util.stream.Stream;

public final class JsonSchemaUtil {

    public static String emptySchema(final SchemaOption... schemaOptions) {
        ObjectNode schema = JsonParser.getObjectMapper().createObjectNode();
        schema.put("$schema", SchemaVersion.DRAFT_2020_12.getIdentifier());
        schema.put("type", "object");
        processSchemaOptions(schemaOptions, schema);
        return schema.toPrettyString();
    }

    /**
     * Create a JSON schema from a list of parameters.
     *
     * @param parameters    the parameter list
     * @param schemaOptions optional schema options
     * @return the JSON schema as a string
     */
    public static String createParameterSchema(final List<McpServerToolParameter> parameters,
                                               final SchemaOption... schemaOptions) {
        if (CollectionUtils.isEmpty(parameters)) {
            return emptySchema(schemaOptions);
        }

        ObjectNode schema = JsonParser.getObjectMapper().createObjectNode();
        schema.put("$schema", SchemaVersion.DRAFT_2020_12.getIdentifier());
        schema.put("type", "object");

        ObjectNode properties = schema.putObject("properties");
        for (McpServerToolParameter parameter : parameters) {
            ObjectNode property = properties.putObject(parameter.getName());
            recursionConstructPropertiesNodes(parameter, property);
        }

        processSchemaOptions(schemaOptions, schema);
        return schema.toPrettyString();
    }

    /**
     * Recursively construct the properties nodes for the parameter.
     *
     * @param parameter the parameter
     * @param property  the property
     */
    public static void recursionConstructPropertiesNodes(final McpServerToolParameter parameter,
                                                         final ObjectNode property) {
        property.put("type", parameter.getType());
        // if the parameter is the item parameter of array, the description is null
        if (StringUtils.isNotBlank(parameter.getDescription())) {
            property.put("description", parameter.getDescription());
        }
        // add the properties schema for the object type parameter
        List<McpServerToolParameter> parameters = parameter.getParameters();
        if ("object".equals(parameter.getType()) && CollectionUtils.isNotEmpty(parameters)) {
            ObjectNode properties = property.putObject("properties");
            for (McpServerToolParameter itemParameter : parameters) {
                ObjectNode property1 = properties.putObject(itemParameter.getName());
                recursionConstructPropertiesNodes(itemParameter, property1);
            }
        }
        // add the items schema for the array type parameter
        if ("array".equals(parameter.getType()) && CollectionUtils.isNotEmpty(parameters)) {
            McpServerToolParameter itemParameter = parameters.get(0);
            ObjectNode items = property.putObject("items");
            recursionConstructPropertiesNodes(itemParameter, items);
        }
    }

    private static void processSchemaOptions(final SchemaOption[] schemaOptions, final ObjectNode schema) {
        if (Stream.of(schemaOptions)
                .noneMatch(option -> option == SchemaOption.ALLOW_ADDITIONAL_PROPERTIES_BY_DEFAULT)) {
            schema.put("additionalProperties", false);
        }

        if (Stream.of(schemaOptions).anyMatch(option -> option == SchemaOption.UPPER_CASE_TYPE_VALUES)) {
            convertTypeValuesToUpperCase(schema);
        }
    }

    public static void convertTypeValuesToUpperCase(final ObjectNode node) {
        if (node.isObject()) {
            node.fields().forEachRemaining(entry -> {
                JsonNode value = entry.getValue();
                if (value.isObject()) {
                    convertTypeValuesToUpperCase((ObjectNode) value);
                } else if (value.isArray()) {
                    value.elements().forEachRemaining(element -> {
                        if (element.isObject() || element.isArray()) {
                            convertTypeValuesToUpperCase((ObjectNode) element);
                        }
                    });
                } else if (value.isTextual() && entry.getKey().equals("type")) {
                    String oldValue = node.get("type").asText();
                    node.put("type", oldValue.toUpperCase());
                }
            });
        } else if (node.isArray()) {
            node.elements().forEachRemaining(element -> {
                if (element.isObject() || element.isArray()) {
                    convertTypeValuesToUpperCase((ObjectNode) element);
                }
            });
        }
    }
}
