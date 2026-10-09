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

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class AgentToolDefinitionTest {

    @ParameterizedTest
    @ValueSource(strings = {"{}", "{\"type\":null}", "{\"type\":1}", "{\"type\":[]}", "{\"type\":\"array\"}",
            "{\"type\":\"object\",\"$ref\":\"https://example.com/schema\"}",
            "{\"type\":\"object\",\"$defs\":{\"nested\":{\"$dynamicRef\":\"https://example.com/schema\"}}}",
            "{\"type\":\"object\",\"properties\":{\"region\":{\"type\":\"string\",\"x-mcp-header\":\"Region\"}}}",
            "{\"type\":\"object\",\"allOf\":[{\"$ref\":\"file:///secret\"}]}",
            "{\"type\":\"object\",\"additionalProperties\":{\"$ref\":\"remote.json\"}}"})
    void shouldRejectUnsupportedDefinitions(final String schema) {
        assertThrows(IllegalArgumentException.class, () -> new AgentToolDefinition("tool", "Read a test record", parse(schema)));
    }

    @ParameterizedTest
    @ValueSource(strings = {"", " ", "\t"})
    void shouldRejectMissingDescription(final String description) {
        assertThrows(IllegalArgumentException.class, () -> new AgentToolDefinition("tool", description, parse("{\"type\":\"object\"}")));
    }

    @ParameterizedTest
    @ValueSource(strings = {"{\"type\":\"object\",\"$ref\":\"#/$defs/record\",\"$defs\":{\"record\":{\"type\":\"object\"}}}",
            "{\"type\":\"object\",\"properties\":{\"$ref\":{\"type\":\"string\"},\"x-mcp-header\":{\"type\":\"string\"}}}",
            "{\"type\":\"object\",\"default\":{\"$ref\":\"a literal, not a schema\"},\"enum\":[{\"x-mcp-header\":\"literal\"}]}"})
    void shouldNotTreatLiteralDataOrPropertyNamesAsSchemaKeywords(final String schema) {
        assertEquals("tool", new AgentToolDefinition("tool", "Read a test record", parse(schema)).getName());
    }

    @Test
    void shouldFreezeDefinitionAndReturnIndependentSchemaCopies() {
        JsonObject schema = parse("{\"type\":\"object\",\"properties\":{\"id\":{\"type\":\"string\"}}}");
        AgentToolDefinition definition = new AgentToolDefinition("tool", "Read a test record", schema);
        schema.addProperty("type", "array");
        definition.getInputSchema().getAsJsonObject("properties").addProperty("id", "changed");
        assertEquals("object", definition.getInputSchema().get("type").getAsString());
        assertEquals("string", definition.getInputSchema().getAsJsonObject("properties").getAsJsonObject("id").get("type").getAsString());
        assertEquals("Read a test record", definition.getDescription());
    }

    private JsonObject parse(final String schema) {
        return JsonParser.parseString(schema).getAsJsonObject();
    }
}
