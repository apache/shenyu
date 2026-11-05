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

package org.apache.shenyu.plugin.mcp.server.contract;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Independently runnable synthetic MCP contract regression cases for #7935.
 * These cases check authored facts, not ShenYu runtime contract pinning.
 */
class McpContractRegressionFixtureTest {

    private static final String ROOT = "/mcp-contract-regression/";

    private static final ObjectMapper MAPPER = new ObjectMapper();

    @Test
    void testStructuralChangesMatchExpectations() throws IOException {
        JsonNode baseline = read("before.tools-list.json");
        JsonNode cases = read("expected/structural_changes.json").path("cases");
        Map<String, String> variants = Map.of(
                "combined", "combined.after.tools-list.json",
                "annotation_only", "annotation_only.after.tools-list.json",
                "key_order_only", "key_order_only.after.tools-list.json",
                "unsupported", "unsupported.after.tools-list.json");

        for (Map.Entry<String, String> variant : variants.entrySet()) {
            JsonNode candidate = read(variant.getValue());
            Set<String> declared = new HashSet<>();
            for (JsonNode change : cases.path(variant.getKey())) {
                String pointer = change.path("json_pointer").asText();
                assertTrue(declared.add(pointer), "Duplicate expected pointer: " + pointer);
                JsonNode original = baseline.at(pointer);
                if ("add".equals(change.path("operation").asText())) {
                    assertTrue(original.isMissingNode(), pointer);
                } else {
                    assertEquals("replace", change.path("operation").asText());
                    assertEquals(change.path("before"), original, pointer);
                }
                assertEquals(change.path("after"), candidate.at(pointer), pointer);
            }
            Set<String> actual = new HashSet<>();
            diff(baseline, candidate, "", actual);
            assertEquals(declared, actual, variant.getKey());
        }
    }

    @Test
    void testCombinedDirectionAndWitnesses() throws IOException {
        JsonNode before = onlyTool(read("before.tools-list.json"));
        JsonNode after = onlyTool(read("combined.after.tools-list.json"));
        JsonNode expected = read("expected/compatibility.json");

        assertEquals(before.path("inputSchema").path("properties"),
                after.path("inputSchema").path("properties"));
        assertEquals(before.path("outputSchema").path("properties").path("count"),
                after.path("outputSchema").path("properties").path("count"));
        assertFalse(has(before.path("inputSchema").path("required"), "workspace_id"));
        assertTrue(has(after.path("inputSchema").path("required"), "workspace_id"));
        assertFalse(has(before.path("outputSchema").path("properties").path("status").path("enum"), "paused"));
        assertTrue(has(after.path("outputSchema").path("properties").path("status").path("enum"), "paused"));

        for (JsonNode witness : expected.path("witnesses")) {
            String schema = witness.path("schema").asText();
            JsonNode instance = witness.path("instance");
            assertEquals(witness.path("valid_before").asBoolean(),
                    acceptsFixtureSchema(before.path(schema), instance), witness.path("id").asText());
            assertEquals(witness.path("valid_after").asBoolean(),
                    acceptsFixtureSchema(after.path(schema), instance), witness.path("id").asText());
        }

        JsonNode inputs = expected.path("combined").path("inputs");
        JsonNode outputs = expected.path("combined").path("outputs");
        assertFalse(inputs.path("old_clients_to_new_tool").path("holds").asBoolean());
        assertTrue(inputs.path("new_clients_to_old_tool").path("holds").asBoolean());
        assertFalse(outputs.path("new_tool_to_strict_old_consumer").path("holds").asBoolean());
        assertTrue(outputs.path("old_tool_to_new_consumer").path("holds").asBoolean());
        assertEquals("BREAKING_CANDIDATE",
                outputs.path("new_tool_to_strict_old_consumer").path("classification").asText());
    }

    @Test
    void testAnnotationOnlyIsMetadataReview() throws IOException {
        JsonNode before = onlyTool(read("before.tools-list.json"));
        JsonNode after = onlyTool(read("annotation_only.after.tools-list.json"));
        JsonNode expected = read("expected/compatibility.json").path("annotation_only");
        assertEquals(before.path("inputSchema"), after.path("inputSchema"));
        assertEquals(before.path("outputSchema"), after.path("outputSchema"));
        assertTrue(before.path("annotations").path("readOnlyHint").asBoolean());
        assertFalse(after.path("annotations").path("readOnlyHint").asBoolean());
        assertTrue(expected.path("metadata_review_required").asBoolean());
    }

    @Test
    void testKeyOrderIsNotStructuralDrift() throws IOException {
        JsonNode before = read("before.tools-list.json");
        JsonNode after = read("key_order_only.after.tools-list.json");
        assertEquals(before, after);
        JsonNode expected = read("expected/compatibility.json").path("key_order_only");
        assertTrue(expected.path("parsed_json_equal").asBoolean());
        assertFalse(expected.path("semantic_equivalence_proof").asBoolean());
    }

    @Test
    void testUnsupportedCompositionRequiresReview() throws IOException {
        JsonNode candidate = onlyTool(read("unsupported.after.tools-list.json"));
        JsonNode expected = read("expected/compatibility.json").path("unsupported");
        JsonNode input = candidate.path("inputSchema");
        assertTrue(input.has("allOf"));
        assertTrue(input.path("allOf").get(0).has("not"));
        assertEquals("REQUIRES_REVIEW", expected.path("compatibility_direction").asText());
        assertTrue(expected.path("input_set_inclusion").isNull());
        assertFalse(expected.path("comparison_complete").asBoolean());
        assertThrows(IllegalArgumentException.class,
                () -> acceptsFixtureSchema(input, MAPPER.createObjectNode()));
    }

    private static JsonNode read(final String name) throws IOException {
        try (InputStream stream = McpContractRegressionFixtureTest.class.getResourceAsStream(ROOT + name)) {
            assertNotNull(stream, name);
            return MAPPER.readTree(stream);
        }
    }

    private static JsonNode onlyTool(final JsonNode tools) {
        assertEquals(1, tools.path("tools").size());
        return tools.path("tools").get(0);
    }

    private static boolean has(final JsonNode values, final String text) {
        for (JsonNode value : values) {
            if (text.equals(value.asText())) {
                return true;
            }
        }
        return false;
    }

    private static void diff(final JsonNode left, final JsonNode right, final String path, final Set<String> changed) {
        if (left.equals(right)) {
            return;
        }
        if (left.isObject() && right.isObject()) {
            Set<String> keys = new HashSet<>();
            left.fieldNames().forEachRemaining(keys::add);
            right.fieldNames().forEachRemaining(keys::add);
            for (String key : keys) {
                String next = path + "/" + key.replace("~", "~0").replace("/", "~1");
                if (!left.has(key) || !right.has(key)) {
                    changed.add(next);
                } else {
                    diff(left.get(key), right.get(key), next, changed);
                }
            }
        } else if ("/tools".equals(path) && left.isArray() && right.isArray() && left.size() == right.size()) {
            for (int i = 0; i < left.size(); i++) {
                diff(left.get(i), right.get(i), path + "/" + i, changed);
            }
        } else {
            changed.add(path);
        }
    }

    /**
     * A deliberately narrow evaluator for the basic object constraints in the four witnesses.
     * Never treat this as general-purpose JSON Schema compatibility analysis.
     */
    private static boolean acceptsFixtureSchema(final JsonNode schema, final JsonNode instance) {
        if (schema.has("allOf") || schema.has("anyOf") || schema.has("oneOf") || schema.has("not")) {
            throw new IllegalArgumentException("Composition requires review");
        }
        if (!"object".equals(schema.path("type").asText()) || !instance.isObject()) {
            return false;
        }
        for (JsonNode required : schema.path("required")) {
            if (!instance.has(required.asText())) {
                return false;
            }
        }
        JsonNode properties = schema.path("properties");
        Iterator<Map.Entry<String, JsonNode>> fields = instance.fields();
        while (fields.hasNext()) {
            Map.Entry<String, JsonNode> entry = fields.next();
            JsonNode rule = properties.path(entry.getKey());
            if (rule.isMissingNode()) {
                if (!schema.path("additionalProperties").asBoolean(true)) {
                    return false;
                }
                continue;
            }
            JsonNode value = entry.getValue();
            if ("string".equals(rule.path("type").asText())) {
                if (!value.isTextual() || value.asText().length() < rule.path("minLength").asInt(0)) {
                    return false;
                }
            } else if ("integer".equals(rule.path("type").asText())) {
                if (!value.isIntegralNumber() || value.asLong() < rule.path("minimum").asLong(Long.MIN_VALUE)) {
                    return false;
                }
            } else {
                throw new IllegalArgumentException("Unexpected fixture property type");
            }
            if (rule.has("enum") && !contains(rule.path("enum"), value)) {
                return false;
            }
        }
        return true;
    }

    private static boolean contains(final JsonNode values, final JsonNode sought) {
        for (JsonNode value : values) {
            if (value.equals(sought)) {
                return true;
            }
        }
        return false;
    }
}
