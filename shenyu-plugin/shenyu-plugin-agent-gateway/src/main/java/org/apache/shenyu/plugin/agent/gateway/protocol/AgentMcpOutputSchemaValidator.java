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

package org.apache.shenyu.plugin.agent.gateway.protocol;

import com.fasterxml.jackson.databind.JsonNode;
import com.networknt.schema.OutputFormat;
import com.networknt.schema.Schema;
import com.networknt.schema.SchemaLocation;
import com.networknt.schema.SchemaRegistry;
import com.networknt.schema.SchemaRegistryConfig;
import com.networknt.schema.SpecificationVersion;
import com.networknt.schema.resource.SchemaLoader;

import java.util.Objects;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Request-owned validation against an unchanged, bounded Draft 2020-12 schema.
 * Unsupported or expensive schemas fail closed; no file or network resources load.
 */
final class AgentMcpOutputSchemaValidator {

    private static final String DIALECT = "https://json-schema.org/draft/2020-12/schema";

    private static final Set<String> META_SCHEMAS = Set.of(DIALECT,
            "https://json-schema.org/draft/2020-12/meta/core", "https://json-schema.org/draft/2020-12/meta/applicator",
            "https://json-schema.org/draft/2020-12/meta/unevaluated", "https://json-schema.org/draft/2020-12/meta/validation",
            "https://json-schema.org/draft/2020-12/meta/meta-data", "https://json-schema.org/draft/2020-12/meta/format-annotation",
            "https://json-schema.org/draft/2020-12/meta/content");

    private static final Set<String> KEYWORDS = Set.of("$schema", "$id", "$comment", "$defs", "type", "properties", "required",
            "additionalProperties", "unevaluatedProperties", "items", "prefixItems", "unevaluatedItems", "contains", "minContains",
            "maxContains", "minItems", "maxItems", "uniqueItems", "minLength", "maxLength", "minimum", "maximum",
            "exclusiveMinimum", "exclusiveMaximum", "multipleOf", "enum", "const", "allOf", "anyOf", "oneOf", "not",
            "if", "then", "else", "dependentRequired", "dependentSchemas", "propertyNames", "minProperties", "maxProperties",
            "title", "description", "default", "examples", "deprecated", "readOnly", "writeOnly", "format",
            "contentEncoding", "contentMediaType", "contentSchema");

    private static final Set<String> MAPS = Set.of("properties", "$defs", "dependentSchemas");

    private static final Set<String> CHILDREN = Set.of("additionalProperties", "unevaluatedProperties", "items", "unevaluatedItems",
            "contains", "propertyNames", "not", "if", "then", "else", "contentSchema");

    private static final Set<String> ARRAYS = Set.of("allOf", "anyOf", "oneOf", "prefixItems");

    private final Schema schema;

    AgentMcpOutputSchemaValidator(final JsonNode outputSchema) {
        if (Objects.isNull(outputSchema) || !outputSchema.isObject()) {
            throw invalid();
        }
        budget(outputSchema, 0, new AtomicInteger(), new AtomicLong(), 512, 65536);
        supported(outputSchema, 0);
        JsonNode owned = outputSchema.deepCopy();
        SchemaRegistry registry = localRegistry();
        if (!registry.getSchema(SchemaLocation.of(DIALECT)).validate(owned, OutputFormat.BOOLEAN)) {
            throw invalid();
        }
        schema = registry.getSchema(owned);
    }

    static SchemaRegistry localRegistry() {
        return SchemaRegistry.withDefaultDialect(SpecificationVersion.DRAFT_2020_12, builder -> builder
                .schemaCacheEnabled(false)
                .schemaLoader(SchemaLoader.builder().fetchRemoteResources(false)
                        .allow(iri -> META_SCHEMAS.contains(iri.toString())).build())
                .schemaRegistryConfig(SchemaRegistryConfig.builder().failFast(true).formatAssertionsEnabled(false).build()));
    }

    void validate(final JsonNode structuredContent) {
        if (Objects.isNull(structuredContent) || !structuredContent.isObject()) {
            throw invalid();
        }
        budget(structuredContent, 0, new AtomicInteger(), new AtomicLong(), 2048, 65536);
        JsonNode validationCopy = structuredContent.deepCopy();
        if (!schema.validate(validationCopy, OutputFormat.BOOLEAN) || !validationCopy.equals(structuredContent)) {
            throw invalid();
        }
    }

    private void supported(final JsonNode node, final int compositionDepth) {
        if (node.isBoolean()) {
            return;
        }
        if (!node.isObject()) {
            throw invalid();
        }
        node.fields().forEachRemaining(entry -> {
            String key = entry.getKey();
            JsonNode value = entry.getValue();
            if (!KEYWORDS.contains(key) || ("$schema".equals(key) && (!value.isTextual() || !DIALECT.equals(value.textValue())))) {
                throw invalid();
            }
            if (MAPS.contains(key)) {
                if (!value.isObject()) {
                    throw invalid();
                }
                value.elements().forEachRemaining(child -> supported(child, compositionDepth));
            } else if (CHILDREN.contains(key)) {
                supported(value, compositionDepth + ("not".equals(key) || "if".equals(key) ? 1 : 0));
            } else if (ARRAYS.contains(key)) {
                if (!value.isArray() || value.size() > 4 || compositionDepth >= 2) {
                    throw invalid();
                }
                value.elements().forEachRemaining(child -> supported(child, compositionDepth + 1));
            }
        });
        if (compositionDepth > 2) {
            throw invalid();
        }
    }

    private void budget(final JsonNode node, final int depth, final AtomicInteger nodes, final AtomicLong characters,
                        final int maximumNodes, final long maximumCharacters) {
        if (depth > 16 || nodes.incrementAndGet() > maximumNodes || node.isPojo() || node.isBinary() || node.isMissingNode()) {
            throw invalid();
        }
        if (node.isArray() && node.size() > 128) {
            throw invalid();
        }
        if (node.isObject()) {
            node.fieldNames().forEachRemaining(name -> {
                if (characters.addAndGet(name.length()) > maximumCharacters) {
                    throw invalid();
                }
            });
        } else if (node.isValueNode() && (node.asText().length() > 4096 || characters.addAndGet(node.asText().length()) > maximumCharacters)) {
            throw invalid();
        }
        for (JsonNode child : node) {
            budget(child, depth + 1, nodes, characters, maximumNodes, maximumCharacters);
        }
    }

    private IllegalArgumentException invalid() {
        return new IllegalArgumentException("Tool output schema or filtered result is unsupported or invalid");
    }
}
