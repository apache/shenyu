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
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.google.gson.JsonParser;
import com.networknt.schema.InvalidSchemaException;
import com.networknt.schema.SchemaLocation;
import com.sun.net.httpserver.HttpServer;
import org.apache.shenyu.common.dto.AgentGatewayResultFilterConfig;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.net.InetSocketAddress;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AgentMcpOutputSchemaValidatorTest {

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
    void schemaSnapshotIsIndependentOfMutableDefinition() throws Exception {
        ObjectNode declaration = json("{\"type\":\"object\",\"required\":[\"keep\"]}");
        AgentMcpOutputSchemaValidator validator = new AgentMcpOutputSchemaValidator(declaration);
        declaration.removeAll();
        assertThrows(RuntimeException.class, () -> validator.validate(json("{\"secret\":\"private\"}")));
        validator.validate(json("{\"keep\":1}"));
    }

    @Test
    void ignoresUnsupportedSchemasWhenNoPlanIsConfigured() throws Exception {
        ObjectNode source = json("{\"structuredContent\":{\"secret\":\"private\"}}");
        AgentMcpResultFilter filter = new AgentMcpResultFilter(AgentGatewayResultFilterConfig.empty(), "crm.profile",
                json("{\"$ref\":\"https://untrusted.invalid/schema\"}"));
        assertTrue(source == filter.apply(source));
    }

    @ParameterizedTest
    @ValueSource(strings = {"{\"$ref\":\"#\"}", "{\"$ref\":\"https://untrusted.invalid/schema\"}",
            "{\"$dynamicRef\":\"#anchor\"}", "{\"pattern\":\"(a+)+$\"}", "{\"patternProperties\":{}}",
            "{\"$schema\":\"https://untrusted.invalid/meta\"}", "{\"type\":\"invalid-type\"}",
            "{\"properties\":true}", "{\"allOf\":[true,true,true,true,true]}", "{\"unknownAssertion\":true}"})
    void rejectsUnsupportedOrMalformedSchemas(final String declaration) throws Exception {
        assertThrows(RuntimeException.class, () -> new AgentMcpOutputSchemaValidator(json(declaration)));
    }

    @Test
    void annotationDataDoesNotBecomeAReference() throws Exception {
        AgentMcpOutputSchemaValidator validator = new AgentMcpOutputSchemaValidator(json(
                "{\"type\":\"object\",\"properties\":{\"keep\":{\"const\":{\"$ref\":\"not-a-control-plane\"}}}}"));
        validator.validate(json("{\"keep\":{\"$ref\":\"not-a-control-plane\"}}"));
    }

    @Test
    void actualReferenceResolverCannotContactALocalHttpEndpoint() throws Exception {
        AtomicInteger requests = new AtomicInteger();
        HttpServer server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        server.createContext("/schema", exchange -> {
            requests.incrementAndGet();
            exchange.sendResponseHeaders(500, -1);
            exchange.close();
        });
        server.start();
        try {
            String uri = "http://127.0.0.1:" + server.getAddress().getPort() + "/schema";
            JsonNode declaration = json("{\"$ref\":\"" + uri + "\"}");
            InvalidSchemaException failure = assertThrows(InvalidSchemaException.class,
                    () -> AgentMcpOutputSchemaValidator.localRegistry().getSchema(declaration).validate(json("{}")));
            assertTrue(failure.getMessage().contains("not allowed"));
            assertEquals(0, requests.get());
        } finally {
            server.stop(0);
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"file:///does-not-exist/schema.json", "classpath:META-INF/shenyu/agent-gateway-version.properties",
            "https://untrusted.invalid/schema"})
    void actualLoaderRejectsNonBuiltinResources(final String uri) {
        InvalidSchemaException failure = assertThrows(InvalidSchemaException.class,
                () -> AgentMcpOutputSchemaValidator.localRegistry().getSchema(SchemaLocation.of(uri)));
        assertTrue(failure.getMessage().contains("not allowed"));
    }

    @Test
    void activeCopyPolicyRequiresStructuredContentForBusinessErrorsToo() throws Exception {
        AgentMcpResultFilter filter = new AgentMcpResultFilter(config(), "crm.profile", json("{\"type\":\"object\"}"));
        assertThrows(RuntimeException.class, () -> filter.apply(json("{\"isError\":false}")));
        assertThrows(RuntimeException.class, () -> filter.apply(json("{\"isError\":true}")));
    }

    @Test
    void limitsInstanceAndSchemaCostBeforeValidation() throws Exception {
        AgentMcpOutputSchemaValidator validator = new AgentMcpOutputSchemaValidator(json("{\"type\":\"object\"}"));
        ObjectNode large = json("{}");
        var array = large.putArray("values");
        for (int index = 0; index < 129; index++) {
            array.add(index);
        }
        assertThrows(RuntimeException.class, () -> validator.validate(large));
        ObjectNode longString = json("{}");
        longString.put("value", "x".repeat(4097));
        assertThrows(RuntimeException.class, () -> validator.validate(longString));
        assertThrows(RuntimeException.class, () -> new AgentMcpOutputSchemaValidator(json(
                "{\"allOf\":[{\"allOf\":[{\"allOf\":[true]}]}]}")));
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
