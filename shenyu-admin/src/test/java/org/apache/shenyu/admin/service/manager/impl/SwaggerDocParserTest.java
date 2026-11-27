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
import org.apache.shenyu.admin.model.bean.DocItem;
import org.apache.shenyu.admin.model.bean.DocParameter;
import org.apache.shenyu.common.enums.SwaggerVersion;
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
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;

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

    private void assertOperationPath(final JsonObject docRoot, final String operationPath, final String expectedPath) {
        JsonObject pathItem = new JsonObject();
        pathItem.add("get", new JsonObject());
        JsonObject paths = new JsonObject();
        paths.add(operationPath, pathItem);
        docRoot.add("paths", paths);
        DocInfo result = swaggerDocParser.parseJson(docRoot);
        assertEquals(expectedPath, result.getDocModuleList().get(0).getDocItems().get(0).getName());
    }

    @Test
    void testOpenApiInlineOperationParameters() {
        JsonObject root = GsonUtils.getInstance().fromJson("""
                {
                  "openapi": "3.0.3",
                  "info": {"title": "Orders", "version": "1"},
                  "paths": {"/orders/{id}": {"get": {
                    "parameters": [
                      {"name": "id", "in": "path", "required": true, "schema": {"type": "integer"}},
                      {"name": "limit", "in": "query", "schema": {"type": "integer"}},
                      {"name": "X-Region", "in": "header", "required": true,
                       "description": "Region code", "schema": {"type": "string", "maxLength": 8}}
                    ],
                    "responses": {"200": {"description": "OK"}}
                  }}}
                }
                """, JsonObject.class);
        DocItem item = swaggerDocParser.parseJson(root).getDocModuleList().get(0).getDocItems().get(0);
        assertEquals(List.of("id", "limit"), item.getRequestParameters().stream().map(DocParameter::getName).toList());
        assertEquals(List.of("integer", "integer"), item.getRequestParameters().stream().map(DocParameter::getType).toList());
        assertTrue(item.getRequestParameters().get(0).isRequired());
        assertEquals(1, item.getRequestHeaders().size());
        DocParameter header = item.getRequestHeaders().get(0);
        assertEquals("X-Region", header.getName());
        assertEquals("string", header.getType());
        assertEquals("8", header.getMaxLength());
        assertEquals("Region code", header.getDescription());
        assertTrue(header.isRequired());
    }

    @Test
    void testOpenApiSchemaReferenceWithScalarSibling() {
        JsonObject root = GsonUtils.getInstance().fromJson("""
                {
                  "openapi": "3.1.0",
                  "info": {"title": "Orders", "version": "1"},
                  "paths": {"/orders": {"get": {
                    "parameters": [
                      {"name": "limit", "in": "query",
                       "schema": {"$ref": "#/components/schemas/Limit", "type": "number"}}
                    ],
                    "responses": {"200": {"description": "OK"}}
                  }}},
                  "components": {"schemas": {"Limit": {"type": "integer", "maximum": 100}}}
                }
                """, JsonObject.class);
        SwaggerDocParser parser = spy(new SwaggerDocParser());
        DocItem item = parser.parseJson(root).getDocModuleList().get(0).getDocItems().get(0);
        verify(parser).buildDocParameters("Limit", root, true, SwaggerVersion.V3);
        assertTrue(item.getRequestParameters().isEmpty());
    }

    @Test
    void testSwaggerBodyReferenceAndDirectHeader() {
        JsonObject root = GsonUtils.getInstance().fromJson("""
                {
                  "swagger": "2.0",
                  "info": {"title": "Orders", "version": "1"},
                  "paths": {"/orders": {"post": {
                    "parameters": [
                      {"name": "order", "in": "body", "schema": {"$ref": "#/definitions/Order"}},
                      {"name": "X-Region", "in": "header", "type": "string", "maxLength": 8}
                    ],
                    "responses": {"200": {"description": "OK"}}
                  }}},
                  "definitions": {"Order": {"type": "object", "properties": {"id": {"type": "integer"}}}}
                }
                """, JsonObject.class);
        DocItem item = swaggerDocParser.parseJson(root).getDocModuleList().get(0).getDocItems().get(0);
        assertEquals(List.of("id"), item.getRequestParameters().stream().map(DocParameter::getName).toList());
        assertEquals(1, item.getRequestHeaders().size());
        DocParameter header = item.getRequestHeaders().get(0);
        assertEquals("X-Region", header.getName());
        assertEquals("8", header.getMaxLength());
    }
}
