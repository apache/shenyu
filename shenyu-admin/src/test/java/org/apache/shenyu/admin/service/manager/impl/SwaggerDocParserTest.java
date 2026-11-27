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

package org.apache.shenyu.admin.service.manager.impl;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import org.apache.shenyu.admin.model.bean.DocInfo;
import org.apache.shenyu.admin.model.bean.DocParameter;
import org.apache.shenyu.common.utils.GsonUtils;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class SwaggerDocParserTest {

    public static final String DOC_INFO_JSON = "{\n"
            + "  \"openapi\": \"3.0.1\",\n"
            + "  \"info\": {\n"
            + "    \"title\": \"shenyu-examples-http-swagger3 API\",\n"
            + "    \"description\": \"shenyu-examples-http-swagger3 API\",\n"
            + "    \"version\": \"3.0.1\",\n"
            + "    \"contact\": {\n"
            + "      \"name\": \"ShenYu\",\n"
            + "      \"url\": \"https://github.com/apache/shenyu\",\n"
            + "      \"email\": \"dev@shenyu.apache.org\"\n"
            + "    }\n"
            + "  },\n"
            + "  \"servers\": [\n"
            + "    {\n"
            + "      \"url\": \"http://127.0.0.1:8190\",\n"
            + "      \"description\": \"Local server\"\n"
            + "    }\n"
            + "  ],\n"
            + "  \"tags\": [\n"
            + "    {\n"
            + "      \"name\": \"Order API\",\n"
            + "      \"description\": \"Order Controller\"\n"
            + "    }\n"
            + "  ],\n"
            + "  \"paths\": {\n"
            + "    \"/order/path/{id}/{name}\": {\n"
            + "      \"get\": {\n"
            + "        \"tags\": [\"Order API\"],\n"
            + "        \"summary\": \"Get path variable\",\n"
            + "        \"parameters\": [\n"
            + "          {\n"
            + "            \"name\": \"id\",\n"
            + "            \"in\": \"path\",\n"
            + "            \"required\": true,\n"
            + "            \"schema\": {\n"
            + "              \"type\": \"string\"\n"
            + "            }\n"
            + "          },\n"
            + "          {\n"
            + "            \"name\": \"name\",\n"
            + "            \"in\": \"path\",\n"
            + "            \"required\": true,\n"
            + "            \"schema\": {\n"
            + "              \"type\": \"string\"\n"
            + "            }\n"
            + "          }\n"
            + "        ],\n"
            + "        \"responses\": {\n"
            + "          \"200\": {\n"
            + "            \"description\": \"Successful response\",\n"
            + "            \"content\": {\n"
            + "              \"application/json\": {\n"
            + "                \"schema\": {\n"
            + "                  \"$ref\": \"#/components/schemas/OrderDTO\"\n"
            + "                }\n"
            + "              }\n"
            + "            }\n"
            + "          }\n"
            + "        }\n"
            + "      }\n"
            + "    }\n"
            + "  },\n"
            + "  \"components\": {\n"
            + "    \"schemas\": {\n"
            + "      \"OrderDTO\": {\n"
            + "        \"type\": \"object\",\n"
            + "        \"required\": [\"id\", \"name\"],\n"
            + "        \"properties\": {\n"
            + "          \"id\": {\n"
            + "            \"type\": \"string\",\n"
            + "            \"example\": \"100000\"\n"
            + "          },\n"
            + "          \"name\": {\n"
            + "            \"type\": \"string\",\n"
            + "            \"example\": \"jack\"\n"
            + "          }\n"
            + "        }\n"
            + "      }\n"
            + "    }\n"
            + "  }\n"
            + "}\n";

    @InjectMocks
    private SwaggerDocParser swaggerDocParser;

    @Test
    public void testParseJson() {
        JsonObject docRoot = GsonUtils.getInstance().fromJson(DOC_INFO_JSON, JsonObject.class);
        docRoot.addProperty("basePath", "/" + "testClusterName");
        DocInfo docInfo = swaggerDocParser.parseJson(docRoot);
        assert docInfo.getDocModuleList().get(0).getModule().equals("Order API");
    }

    @ParameterizedTest
    @CsvSource({
            "http://localhost:8190, /orders",
            "/, /orders",
            "/v1, /v1/orders",
            "http://localhost:8190/v1/, /v1/orders",
            "/services//v1/, /services//v1/orders"
    })
    public void testOpenApiOperationPath(final String serverUrl, final String expectedPath) {
        JsonObject docRoot = new JsonObject();
        docRoot.addProperty("openapi", "3.0.3");
        JsonObject server = new JsonObject();
        server.addProperty("url", serverUrl);
        JsonArray servers = new JsonArray();
        servers.add(server);
        docRoot.add("servers", servers);
        assertOperationPath(docRoot, "/orders", expectedPath);
    }

    @ParameterizedTest
    @CsvSource({
            "/, /orders, /orders",
            "/v2, /orders, /v2/orders",
            "/v2/, /orders, /v2/orders",
            "/v2/, /, /v2/",
            "/v2/, /orders//today, /v2/orders//today"
    })
    public void testSwaggerOperationPath(final String basePath, final String operationPath, final String expectedPath) {
        JsonObject docRoot = new JsonObject();
        docRoot.addProperty("swagger", "2.0");
        docRoot.addProperty("basePath", basePath);
        assertOperationPath(docRoot, operationPath, expectedPath);
    }

    @ParameterizedTest
    @CsvSource({
            "false, false, false, 201, createdId",
            "true, false, false, 201, createdId",
            "false, true, false, 201, createdId",
            "false, true, true, 201, okId",
            "false, false, false, 400, ''"
    })
    public void testImportsSchemaFromSuccessfulResponse(final boolean swagger, final boolean includeOk, final boolean okHasSchema,
                                                        final String status, final String expectedField) {
        JsonObject docRoot = new JsonObject();
        docRoot.addProperty(swagger ? "swagger" : "openapi", swagger ? "2.0" : "3.0.3");
        JsonObject schemas = new JsonObject();
        schemas.add("Created", responseModel("createdId"));
        schemas.add("Ok", responseModel("okId"));
        if (swagger) {
            docRoot.add("definitions", schemas);
        } else {
            JsonObject components = new JsonObject();
            components.add("schemas", schemas);
            docRoot.add("components", components);
        }
        JsonObject responses = new JsonObject();
        responses.add(status, responseWithSchema(swagger, "Created"));
        if (includeOk) {
            responses.add("200", okHasSchema ? responseWithSchema(swagger, "Ok") : new JsonObject());
        }
        JsonObject operation = new JsonObject();
        operation.add("responses", responses);
        JsonObject pathItem = new JsonObject();
        pathItem.add("post", operation);
        JsonObject paths = new JsonObject();
        paths.add("/orders", pathItem);
        docRoot.add("paths", paths);

        DocInfo result = swaggerDocParser.parseJson(docRoot);
        List<DocParameter> parameters = result.getDocModuleList().get(0).getDocItems().get(0).getResponseParameters();
        assertEquals(expectedField.isEmpty() ? List.of() : List.of(expectedField), parameters.stream().map(DocParameter::getName).toList());
    }

    private JsonObject responseModel(final String fieldName) {
        JsonObject field = new JsonObject();
        field.addProperty("type", "string");
        JsonObject properties = new JsonObject();
        properties.add(fieldName, field);
        JsonObject model = new JsonObject();
        model.addProperty("type", "object");
        model.add("properties", properties);
        return model;
    }

    private JsonObject responseWithSchema(final boolean swagger, final String name) {
        JsonObject schema = new JsonObject();
        schema.addProperty("$ref", (swagger ? "#/definitions/" : "#/components/schemas/") + name);
        JsonObject response = new JsonObject();
        response.addProperty("description", "Response");
        if (swagger) {
            response.add("schema", schema);
        } else {
            JsonObject mediaType = new JsonObject();
            mediaType.add("schema", schema);
            JsonObject content = new JsonObject();
            content.add("application/json", mediaType);
            response.add("content", content);
        }
        return response;
    }

    private void assertOperationPath(final JsonObject docRoot, final String operationPath, final String expectedPath) {
        JsonObject pathItem = new JsonObject();
        pathItem.add("get", new JsonObject());
        JsonObject paths = new JsonObject();
        paths.add(operationPath, pathItem);
        docRoot.add("paths", paths);
        DocInfo result = swaggerDocParser.parseJson(docRoot);
        assertEquals(expectedPath, result.getDocModuleList().get(0).getDocItems().get(0).getName());
    }
}
