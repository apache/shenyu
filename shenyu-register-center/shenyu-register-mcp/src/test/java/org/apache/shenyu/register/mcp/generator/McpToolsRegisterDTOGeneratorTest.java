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

package org.apache.shenyu.register.mcp.generator;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import io.swagger.v3.oas.models.Operation;
import org.apache.shenyu.register.common.dto.McpToolsRegisterDTO;
import org.apache.shenyu.register.mcp.common.dto.ShenyuMcpRequestConfig;
import org.apache.shenyu.register.mcp.common.dto.ShenyuMcpTool;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

/**
 * Verify that each generated tool retains its selected OpenAPI operation.
 */
class McpToolsRegisterDTOGeneratorTest {

    private static final String ITEMS_PATH = "/catalog/items";

    private static final String ITEM_PATH = "/catalog/items/{itemId}";

    private static final String OPEN_API = """
            {"paths":{
              "/catalog/items":{"get":{"parameters":[
                {"name":"page","in":"query","schema":{"type":"integer"}}]}},
              "/catalog/items/{itemId}":{
                "get":{"parameters":[{"name":"itemId","in":"path","required":true,"schema":{"type":"string"}}]},
                "delete":{"parameters":[{"name":"itemId","in":"path","required":true,"schema":{"type":"string"}}]}}
            }}
            """;

    @ParameterizedTest
    @CsvSource({
        "listItems,/catalog/items,get,page,query",
        "getItem,/catalog/items/{itemId},get,itemId,path",
        "deleteItem,/catalog/items/{itemId},delete,itemId,path"
    })
    void selectsRequestedOperation(final String name, final String path, final String method,
                                   final String parameterName, final String position) {
        JsonObject openApi = JsonParser.parseString(OPEN_API).getAsJsonObject();
        JsonObject generated = generate(openApi, name, path, method);

        assertEquals(name, generated.get("name").getAsString());
        assertEquals("Catalog tool", generated.get("description").getAsString());
        assertRequest(generated, path, method, parameterName, position);
        JsonObject parameter = generated.getAsJsonArray("parameters").get(0).getAsJsonObject();
        assertEquals(parameterName, parameter.get("name").getAsString());
        assertEquals(parameterName, parameter.get("description").getAsString());
        assertEquals("page".equals(parameterName) ? "integer" : "string", parameter.get("type").getAsString());
        assertFalse(parameter.has("schema"));
        assertFalse(parameter.has("in"));
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void preservesMappingsWhenReusingDocumentInEitherOrder(final boolean reverse) {
        JsonObject openApi = JsonParser.parseString(OPEN_API).getAsJsonObject();
        if (reverse) {
            JsonObject paths = openApi.getAsJsonObject("paths");
            JsonObject methods = new JsonObject();
            methods.add("delete", paths.getAsJsonObject(ITEM_PATH).get("delete"));
            methods.add("get", paths.getAsJsonObject(ITEM_PATH).get("get"));
            JsonObject reordered = new JsonObject();
            reordered.add(ITEM_PATH, methods);
            reordered.add(ITEMS_PATH, paths.get(ITEMS_PATH));
            openApi.add("paths", reordered);
        }
        JsonObject original = openApi.deepCopy();
        for (int attempt = 0; attempt < 2; attempt++) {
            assertRequest(generate(openApi, "listItems", ITEMS_PATH, "get"), ITEMS_PATH, "get", "page", "query");
            assertRequest(generate(openApi, "getItem", ITEM_PATH, "get"), ITEM_PATH, "get", "itemId", "path");
            assertRequest(generate(openApi, "deleteItem", ITEM_PATH, "delete"), ITEM_PATH, "delete", "itemId", "path");
        }
        assertEquals(original, openApi, "generating tools must not alter the source document");
    }

    @Test
    void formatsParametersWithoutMutatingSource() {
        JsonObject openApi = JsonParser.parseString(OPEN_API).getAsJsonObject();
        JsonObject original = openApi.deepCopy();
        JsonObject generated = generate(openApi, "listItems", ITEMS_PATH, "get");

        assertEquals(original, openApi);
        assertEquals(generated, generate(openApi, "listItems", ITEMS_PATH, "get"));
    }

    @Test
    void preservesLegacySingleOperationRequestConfig() {
        JsonObject openApi = JsonParser.parseString(OPEN_API).getAsJsonObject();
        JsonObject paths = openApi.getAsJsonObject("paths");
        paths.remove(ITEM_PATH);
        ShenyuMcpRequestConfig config = requestConfig();

        JsonObject generated = McpRequestConfigGenerator.generateRequestConfig(openApi, config);

        assertEquals(ITEMS_PATH, generated.getAsJsonObject("requestTemplate").get("url").getAsString());
        assertEquals("get", generated.getAsJsonObject("requestTemplate").get("method").getAsString());
        assertEquals("query", generated.getAsJsonObject("argsPosition").get("page").getAsString());
        assertEquals(generated.get("argsPosition"), generated.getAsJsonObject("requestTemplate").get("argsPosition"));
        assertEquals("false", generated.getAsJsonObject("requestTemplate").get("argsToJsonBody").getAsString());
        JsonObject header = generated.getAsJsonObject("requestTemplate").getAsJsonArray("headers").get(0).getAsJsonObject();
        assertEquals("X-Catalog", header.get("key").getAsString());
        assertEquals("test", header.get("value").getAsString());
    }

    private JsonObject generate(final JsonObject openApi, final String name, final String path, final String method) {
        ShenyuMcpTool tool = new ShenyuMcpTool();
        tool.setToolName(name);
        tool.setMethod(method);
        tool.setOperation(new Operation().description("Catalog tool"));
        tool.setRequestConfig(requestConfig());
        McpToolsRegisterDTO dto = McpToolsRegisterDTOGenerator.generateRegisterDTO(tool, openApi, path, "catalog");
        assertEquals("catalog", dto.getNamespaceId());
        return JsonParser.parseString(dto.getMcpConfig()).getAsJsonObject();
    }

    private ShenyuMcpRequestConfig requestConfig() {
        ShenyuMcpRequestConfig config = new ShenyuMcpRequestConfig();
        config.setBodyToJson("false");
        config.getHeaders().put("X-Catalog", "test");
        return config;
    }

    private void assertRequest(final JsonObject generated, final String path, final String method,
                               final String parameterName, final String position) {
        JsonObject config = JsonParser.parseString(generated.get("requestConfig").getAsString()).getAsJsonObject();
        JsonObject template = config.getAsJsonObject("requestTemplate");
        assertEquals(path, template.get("url").getAsString());
        assertEquals(method, template.get("method").getAsString());
        JsonObject expectedPositions = new JsonObject();
        expectedPositions.addProperty(parameterName, position);
        assertEquals(expectedPositions, config.getAsJsonObject("argsPosition"));
        assertEquals(expectedPositions, template.getAsJsonObject("argsPosition"));
    }
}
