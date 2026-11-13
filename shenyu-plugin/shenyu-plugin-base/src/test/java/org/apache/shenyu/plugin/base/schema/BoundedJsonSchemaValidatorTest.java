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

package org.apache.shenyu.plugin.base.schema;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.networknt.schema.InvalidSchemaException;
import com.networknt.schema.SchemaLocation;
import com.sun.net.httpserver.HttpServer;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.net.InetSocketAddress;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Deterministic tests for the bounded Draft 2020-12 validator: unsupported or
 * expensive schemas fail closed, no remote resource is ever contacted, the
 * validated document is never mutated, and both object and value documents are
 * bounded before validation.
 */
class BoundedJsonSchemaValidatorTest {

    private final ObjectMapper mapper = new ObjectMapper();

    @Test
    void schemaSnapshotIsIndependentOfMutableDefinition() throws Exception {
        ObjectNode declaration = objectNode("{\"type\":\"object\",\"required\":[\"keep\"]}");
        BoundedJsonSchemaValidator validator = new BoundedJsonSchemaValidator(declaration);
        declaration.removeAll();
        assertThrows(RuntimeException.class, () -> validator.validateObject(json("{\"secret\":\"private\"}")));
        validator.validateObject(json("{\"keep\":1}"));
    }

    @ParameterizedTest
    @ValueSource(strings = {"{\"$ref\":\"#\"}", "{\"$ref\":\"https://untrusted.invalid/schema\"}",
            "{\"$dynamicRef\":\"#anchor\"}", "{\"pattern\":\"(a+)+$\"}", "{\"patternProperties\":{}}",
            "{\"$schema\":\"https://untrusted.invalid/meta\"}", "{\"type\":\"invalid-type\"}",
            "{\"properties\":true}", "{\"allOf\":[true,true,true,true,true]}", "{\"unknownAssertion\":true}"})
    void rejectsUnsupportedOrMalformedSchemas(final String declaration) throws Exception {
        assertThrows(RuntimeException.class, () -> new BoundedJsonSchemaValidator(json(declaration)));
    }

    @Test
    void annotationDataDoesNotBecomeAReference() throws Exception {
        BoundedJsonSchemaValidator validator = new BoundedJsonSchemaValidator(json(
                "{\"type\":\"object\",\"properties\":{\"keep\":{\"const\":{\"$ref\":\"not-a-control-plane\"}}}}"));
        validator.validateObject(json("{\"keep\":{\"$ref\":\"not-a-control-plane\"}}"));
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
                    () -> BoundedJsonSchemaValidator.localRegistry().getSchema(declaration).validate(json("{}")));
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
                () -> BoundedJsonSchemaValidator.localRegistry().getSchema(SchemaLocation.of(uri)));
        assertTrue(failure.getMessage().contains("not allowed"));
    }

    @Test
    void limitsInstanceAndSchemaCostBeforeValidation() throws Exception {
        BoundedJsonSchemaValidator validator = new BoundedJsonSchemaValidator(json("{\"type\":\"object\"}"));
        ObjectNode large = objectNode("{}");
        var array = large.putArray("values");
        for (int index = 0; index < 129; index++) {
            array.add(index);
        }
        assertThrows(RuntimeException.class, () -> validator.validateObject(large));
        ObjectNode longString = objectNode("{}");
        longString.put("value", "x".repeat(4097));
        assertThrows(RuntimeException.class, () -> validator.validateObject(longString));
        assertThrows(RuntimeException.class, () -> new BoundedJsonSchemaValidator(json(
                "{\"allOf\":[{\"allOf\":[{\"allOf\":[true]}]}]}")));
    }

    @Test
    void objectValidationRejectsNonObjectDocuments() throws Exception {
        BoundedJsonSchemaValidator validator = new BoundedJsonSchemaValidator(json("{\"type\":\"object\"}"));
        assertThrows(RuntimeException.class, () -> validator.validateObject(json("[1,2]")));
        assertThrows(RuntimeException.class, () -> validator.validateObject(mapper.readTree("\"text\"")));
        assertThrows(RuntimeException.class, () -> validator.validateObject(null));
    }

    @Test
    void valueValidationAcceptsAnyJsonDocumentTheSchemaDescribes() throws Exception {
        BoundedJsonSchemaValidator objectSchema = new BoundedJsonSchemaValidator(json("{\"type\":\"object\"}"));
        objectSchema.validateValue(json("{\"keep\":1}"));
        BoundedJsonSchemaValidator arraySchema = new BoundedJsonSchemaValidator(json("{\"type\":\"array\",\"items\":{\"type\":\"integer\"}}"));
        arraySchema.validateValue(json("[1,2,3]"));
        BoundedJsonSchemaValidator stringSchema = new BoundedJsonSchemaValidator(json("{\"type\":\"string\",\"minLength\":2}"));
        stringSchema.validateValue(mapper.readTree("\"ok\""));
        assertThrows(RuntimeException.class, () -> arraySchema.validateValue(json("[1,\"two\"]")));
        assertThrows(RuntimeException.class, () -> stringSchema.validateValue(mapper.readTree("\"x\"")));
    }

    private JsonNode json(final String input) throws Exception {
        return mapper.readTree(input);
    }

    private ObjectNode objectNode(final String input) throws Exception {
        return (ObjectNode) mapper.readTree(input);
    }
}
