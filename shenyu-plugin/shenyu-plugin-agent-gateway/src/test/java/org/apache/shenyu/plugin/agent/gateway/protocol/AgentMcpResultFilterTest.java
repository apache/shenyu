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

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.google.gson.JsonParser;
import org.apache.shenyu.common.dto.AgentGatewayResultFilterConfig;
import org.junit.jupiter.api.Test;

import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Behavior tests of the MCP result filter against the shared bounded schema
 * validator: deletion keeps the schema unchanged and validation stays fail-closed.
 */
class AgentMcpResultFilterTest {

    private final ObjectMapper mapper = new ObjectMapper();

    @Test
    void validatesUnchangedSchemaAfterDeletingOptionalFields() throws Exception {
        ObjectNode declaration = json("{\"type\":\"object\",\"properties\":{\"keep\":{\"type\":\"integer\"},\"secret\":{\"type\":\"string\"}},"
                + "\"required\":[\"keep\"],\"additionalProperties\":false}");
        ObjectNode original = declaration.deepCopy();
        AgentMcpResultFilter filter = new AgentMcpResultFilter(config(), "crm.profile", declaration);
        ObjectNode result = json("{\"structuredContent\":{\"keep\":3,\"secret\":\"private\"},\"isError\":false}");
        ObjectNode filtered = filter.apply(result);
        assertFalse(filtered.path("structuredContent").has("secret"));
        assertEquals(3, filtered.path("structuredContent").path("keep").asInt());
        assertEquals(original, declaration);
        assertTrue(result.path("structuredContent").has("secret"));
    }

    @Test
    void removingRequiredFieldFailsWithoutRawFallback() throws Exception {
        AgentMcpResultFilter filter = new AgentMcpResultFilter(config(), "crm.profile",
                json("{\"type\":\"object\",\"required\":[\"secret\"]}"));
        ObjectNode result = json("{\"structuredContent\":{\"secret\":\"private\"},\"isError\":false}");
        assertThrows(RuntimeException.class, () -> filter.apply(result));
        assertTrue(result.path("structuredContent").has("secret"));
    }

    @Test
    void defaultsCannotRestoreADeletedFieldOrSatisfyARequiredField() throws Exception {
        ObjectNode schema = json("{\"type\":\"object\",\"properties\":{\"secret\":{\"type\":\"string\",\"default\":\"private-default\"}}}");
        AgentMcpResultFilter optional = new AgentMcpResultFilter(config(), "crm.profile", schema);
        ObjectNode result = json("{\"structuredContent\":{\"secret\":\"private\",\"keep\":3}}");
        assertFalse(optional.apply(result).path("structuredContent").has("secret"));
        schema.putArray("required").add("secret");
        AgentMcpResultFilter required = new AgentMcpResultFilter(config(), "crm.profile", schema);
        assertThrows(RuntimeException.class, () -> required.apply(result));
    }

    @Test
    void ignoresUnsupportedSchemasWhenNoPlanIsConfigured() throws Exception {
        ObjectNode source = json("{\"structuredContent\":{\"secret\":\"private\"}}");
        AgentMcpResultFilter filter = new AgentMcpResultFilter(AgentGatewayResultFilterConfig.empty(), "crm.profile",
                json("{\"$ref\":\"https://untrusted.invalid/schema\"}"));
        assertTrue(source == filter.apply(source));
    }

    @Test
    void activeCopyPolicyRequiresStructuredContentForBusinessErrorsToo() throws Exception {
        AgentMcpResultFilter filter = new AgentMcpResultFilter(config(), "crm.profile", json("{\"type\":\"object\"}"));
        assertThrows(RuntimeException.class, () -> filter.apply(json("{\"isError\":false}")));
        assertThrows(RuntimeException.class, () -> filter.apply(json("{\"isError\":true}")));
    }

    private AgentGatewayResultFilterConfig config() {
        return AgentGatewayResultFilterConfig.parse(
                JsonParser.parseString("{\"crm.profile\":[\"/secret\"]}"), Set.of("crm.profile"));
    }

    private ObjectNode json(final String input) throws Exception {
        ObjectNode value = (ObjectNode) mapper.readTree(input);
        if (value.has("structuredContent") && !value.has("content")) {
            value.putArray("content");
        }
        return value;
    }
}
