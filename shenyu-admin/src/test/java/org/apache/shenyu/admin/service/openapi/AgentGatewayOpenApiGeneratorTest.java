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

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.apache.shenyu.admin.model.dto.AgentGatewayOpenApiRequest;
import org.apache.shenyu.admin.model.dto.AgentGatewayOpenApiRequest.Mapping;
import org.apache.shenyu.admin.model.result.AgentGatewayOpenApiPreview.Generated;
import org.apache.shenyu.admin.model.result.AgentGatewayOpenApiPreview.Item;
import org.apache.shenyu.admin.validation.validator.AgentGatewayRuleHandleValidator;
import org.apache.shenyu.common.dto.AgentGatewayMcpConfig;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/** Fail-closed source projection and the production Admin/gateway schema boundary. */
class AgentGatewayOpenApiGeneratorTest {

    private final AgentGatewayOpenApiGenerator generator = new AgentGatewayOpenApiGenerator();

    @Test
    void generatesOnlySelectedPlansAcceptedByBothProductionValidators() {
        AgentGatewayOpenApiRequest request = OpenApiImportFixture.request();
        Generated generated = generator.generate(new AgentGatewayOpenApiRequest(request.namespaceId(), request.ruleId(), request.sourceId(), request.document(), List.of(request.mappings().get(1))));
        assertEquals(1, generated.items().size());
        assertEquals(2, generated.sourceIdentities().size());
        Item item = generated.items().values().iterator().next();
        assertEquals("GENERATED", item.action());
        JsonObject plan = JsonParser.parseString(item.after()).getAsJsonObject();
        assertEquals("/orders/status", plan.getAsJsonObject("requestConfig").getAsJsonObject("requestTemplate").get("url").getAsString());
        assertEquals("query", plan.getAsJsonObject("requestConfig").getAsJsonObject("argsPosition").get("id").getAsString());
        assertEquals("body", plan.getAsJsonObject("requestConfig").getAsJsonObject("argsPosition").get("name").getAsString());
        assertEquals(JsonParser.parseString("[\"id\",\"name\"]"), plan.getAsJsonObject("inputSchema").get("required"));
        JsonObject handle = JsonParser.parseString("{\"trafficType\":\"mcp\",\"mcp\":{\"allowedTools\":[\"create\"],\"httpTools\":{}}}").getAsJsonObject();
        handle.getAsJsonObject("mcp").getAsJsonObject("httpTools").add("create", plan);
        AgentGatewayRuleHandleValidator.validate(handle.toString());
        assertEquals(1, AgentGatewayMcpConfig.parse(handle.get("mcp"), false).getHttpTools().getTools().size());
    }

    @ParameterizedTest
    @ValueSource(strings = {"\"enum\":[\"A\"]", "\"format\":\"uuid\"", "\"pattern\":\"a+\"", "\"minimum\":1", "\"nullable\":true",
        "\"default\":\"A\"", "\"maxLength\":3", "\"oneOf\":[{\"type\":\"string\"}]", "\"$ref\":\"http://127.0.0.1:1/schema\""})
    void rejectsConstraintsInsteadOfSilentlyDroppingThem(final String constraint) {
        JsonObject source = OpenApiImportFixture.document();
        source.getAsJsonObject("paths").getAsJsonObject("/backend/orders").getAsJsonArray("parameters").get(0).getAsJsonObject()
                .add("schema", JsonParser.parseString("{\"type\":\"string\"," + constraint + "}"));
        Item item = generator.generate(OpenApiImportFixture.request(source, List.of(OpenApiImportFixture.mapping("GET")))).items().values().iterator().next();
        assertEquals("UNSUPPORTED", item.action());
        assertFalse(item.diagnostics().isEmpty());
    }

    @Test
    void reportsDuplicatesAndAllowsExplicitNamesToResolveOperationIdAmbiguity() {
        JsonObject source = OpenApiImportFixture.document();
        source.getAsJsonObject("paths").getAsJsonObject("/backend/orders").getAsJsonObject("post").addProperty("operationId", "lookup");
        Generated ambiguous = generator.generate(OpenApiImportFixture.request(source, List.of(OpenApiImportFixture.mapping("GET"))));
        assertTrue(ambiguous.items().values().iterator().next().diagnostics().get(0).contains("Duplicate operationId"));
        Mapping named = new Mapping("GET", "/backend/orders", "/orders/status", "route", "unique");
        assertEquals("GENERATED", generator.generate(OpenApiImportFixture.request(source, List.of(named))).items().values().iterator().next().action());
        Mapping same = new Mapping("POST", "/backend/orders", "/orders/status", "route", "unique");
        assertTrue(generator.generate(OpenApiImportFixture.request(source, List.of(named, same))).items().values().stream().allMatch(item -> "UNSUPPORTED".equals(item.action())));
    }

    @Test
    void reportsQueryBodyCollisionsAndUnsupportedPaths() {
        JsonObject source = OpenApiImportFixture.document();
        source.getAsJsonObject("paths").getAsJsonObject("/backend/orders").getAsJsonObject("post").getAsJsonObject("requestBody")
                .getAsJsonObject("content").getAsJsonObject("application/json").getAsJsonObject("schema").getAsJsonObject("properties").add("id", JsonParser.parseString("{\"type\":\"string\"}"));
        assertTrue(generator.generate(OpenApiImportFixture.request(source, List.of(OpenApiImportFixture.mapping("POST"))))
                .items().values().iterator().next().diagnostics().get(0).contains("collision"));
        Mapping template = new Mapping("GET", "/backend/{id}", "/orders/status", "route", null);
        assertTrue(generator.generate(OpenApiImportFixture.request(source, List.of(template))).items().values().iterator().next().diagnostics().get(0).contains("literal"));
        Mapping external = new Mapping("GET", "/backend/orders", "https://example.org/orders", "route", null);
        assertEquals("UNSUPPORTED", generator.generate(OpenApiImportFixture.request(source, List.of(external))).items().values().iterator().next().action());
    }

    @Test
    void rejectsDuplicateJsonKeysAndOtherOpenApiVersions() {
        AgentGatewayOpenApiRequest request = OpenApiImportFixture.request();
        assertThrows(IllegalArgumentException.class, () -> generator.generate(new AgentGatewayOpenApiRequest("ns", "agent", "source",
                "{\"openapi\":\"3.0.3\",\"openapi\":\"3.1.0\",\"paths\":{}}", List.of())));
        assertThrows(IllegalArgumentException.class, () -> generator.generate(new AgentGatewayOpenApiRequest("ns", "agent", "source",
                request.document().replace("3.0.3", "3.1.0"), request.mappings())));
    }
}
