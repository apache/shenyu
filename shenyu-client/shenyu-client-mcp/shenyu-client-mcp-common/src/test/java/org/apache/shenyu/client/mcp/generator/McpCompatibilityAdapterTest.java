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

package org.apache.shenyu.client.mcp.generator;

import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.apache.shenyu.client.mcp.common.dto.ShenyuMcpTool;
import org.apache.shenyu.register.common.dto.McpToolsRegisterDTO;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;

/**
 * Preserve legacy client model deserialization and generator entry points after extraction.
 */
class McpCompatibilityAdapterTest {

    private final Gson gson = new Gson();

    @Test
    void preservesClientRequestConfigAfterDeserialization() {
        ShenyuMcpTool tool = gson.fromJson("{\"toolName\":\"orders\",\"requestConfig\":{\"headers\":{\"X-Test\":\"value\"},\"bodyToJson\":\"{}\"}}", ShenyuMcpTool.class);
        assertEquals("value", tool.getRequestConfig().getHeaders().get("X-Test"));
        assertEquals("{}", tool.getRequestConfig().getBodyToJson());
        assertSame(tool.getRequestConfig(), tool.getRequestConfig());
        ShenyuMcpTool restored = gson.fromJson(gson.toJson(tool), ShenyuMcpTool.class);
        assertEquals(tool.getRequestConfig().getHeaders(), restored.getRequestConfig().getHeaders());
        assertNull(new ShenyuMcpTool().getRequestConfig());
    }

    @Test
    void generatesTheExistingNestedStringWireFormat() {
        ShenyuMcpTool tool = gson.fromJson("{\"toolName\":\"orders\",\"method\":\"get\",\"operation\":{\"description\":\"Orders\"},"
                + "\"requestConfig\":{\"headers\":{\"X-Test\":\"value\"},\"bodyToJson\":\"{}\"}}", ShenyuMcpTool.class);
        JsonObject openApi = JsonParser.parseString("{\"paths\":{\"/orders\":{\"get\":{\"parameters\":["
                + "{\"name\":\"id\",\"in\":\"query\",\"schema\":{\"type\":\"string\"}}]}}}}").getAsJsonObject();
        McpToolsRegisterDTO registration = McpToolsRegisterDTOGenerator.generateRegisterDTO(tool, openApi, "/orders", "tenant-a");
        assertEquals("tenant-a", registration.getNamespaceId());
        JsonObject config = JsonParser.parseString(registration.getMcpConfig()).getAsJsonObject();
        assertEquals("orders", config.get("name").getAsString());
        assertEquals("Orders", config.get("description").getAsString());
        JsonObject parameter = config.getAsJsonArray("parameters").get(0).getAsJsonObject();
        assertEquals("string", parameter.get("type").getAsString());
        assertEquals("id", parameter.get("description").getAsString());
        assertFalse(parameter.has("schema"));
        assertFalse(parameter.has("in"));
        JsonObject request = JsonParser.parseString(config.get("requestConfig").getAsString()).getAsJsonObject();
        assertEquals("query", request.getAsJsonObject("argsPosition").get("id").getAsString());
        assertEquals("/orders", request.getAsJsonObject("requestTemplate").get("url").getAsString());
        assertEquals("get", request.getAsJsonObject("requestTemplate").get("method").getAsString());
        assertEquals(request.get("argsPosition"), request.getAsJsonObject("requestTemplate").get("argsPosition"));
    }
}
