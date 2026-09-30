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

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import io.swagger.v3.oas.annotations.OpenAPIDefinition;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.servers.Server;
import org.apache.shenyu.client.mcp.common.annotation.ShenyuMcpTool;
import org.apache.shenyu.client.mcp.common.constants.OpenApiConstants;
import org.apache.shenyu.client.mcp.utils.OpenApiConvertorUtil;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for {@link McpOpenApiGenerator}.
 */
public class McpOpenApiGeneratorTest {

    private static final String URL = "/mcp/test/tool";

    private static final String SERVER_URL = "http://localhost:8080";

    /**
     * A bare tool uses the default annotations, whose servers() and parameters are empty: the generated document
     * must still be produced instead of failing with an ArrayIndexOutOfBoundsException or a NullPointerException.
     */
    @Test
    void testGenerateOpenApiJsonWithDefaultAnnotations() {
        JsonObject json = McpOpenApiGenerator.generateOpenApiJson(getAnnotation(DefaultTool.class), buildTool(DefaultTool.class), URL);

        assertFalse(json.has(OpenApiConstants.OPEN_API_SERVER_KEY), "the optional server block is omitted when servers() is empty");
        assertEquals("3.0.0", json.get(OpenApiConstants.OPEN_API_VERSION_KEY).getAsString());
        assertTrue(json.has(OpenApiConstants.OPEN_API_PATH_KEY));
        assertTrue(json.getAsJsonObject(OpenApiConstants.OPEN_API_PATH_KEY).has(URL));
        JsonArray parameters = json.getAsJsonObject(OpenApiConstants.OPEN_API_PATH_KEY)
                .getAsJsonObject(URL).getAsJsonObject("get")
                .getAsJsonArray(OpenApiConstants.OPEN_API_PATH_OPERATION_METHOD_PARAMETERS_KEY);
        assertTrue(parameters.isEmpty(), "a tool without parameters produces an empty parameter list");
    }

    /**
     * The guard must not change the behaviour for a tool that configures a server.
     */
    @Test
    void testGenerateOpenApiJsonWithConfiguredServer() {
        JsonObject json = McpOpenApiGenerator.generateOpenApiJson(getAnnotation(ConfiguredServerTool.class),
                buildTool(ConfiguredServerTool.class), URL);

        assertTrue(json.has(OpenApiConstants.OPEN_API_SERVER_KEY));
        assertEquals(SERVER_URL, json.getAsJsonObject(OpenApiConstants.OPEN_API_SERVER_KEY)
                .get(OpenApiConstants.OPEN_API_SERVER_URL_KEY).getAsString());
    }

    /**
     * Parameters are still reported when the tool declares them.
     */
    @Test
    void testGenerateOpenApiJsonWithParameters() {
        JsonObject json = McpOpenApiGenerator.generateOpenApiJson(getAnnotation(ToolWithParameters.class),
                buildTool(ToolWithParameters.class), URL);

        JsonArray parameters = json.getAsJsonObject(OpenApiConstants.OPEN_API_PATH_KEY)
                .getAsJsonObject(URL).getAsJsonObject("get")
                .getAsJsonArray(OpenApiConstants.OPEN_API_PATH_OPERATION_METHOD_PARAMETERS_KEY);
        assertEquals(1, parameters.size());
        assertEquals("id", parameters.get(0).getAsJsonObject()
                .get(OpenApiConstants.OPEN_API_PATH_OPERATION_METHOD_PARAMETERS_NAME_KEY).getAsString());
    }

    private static ShenyuMcpTool getAnnotation(final Class<?> clazz) {
        return clazz.getAnnotation(ShenyuMcpTool.class);
    }

    /**
     * Build the tool the same way the client registration does: the operation is converted from the annotation.
     */
    private static org.apache.shenyu.client.mcp.common.dto.ShenyuMcpTool buildTool(final Class<?> clazz) {
        org.apache.shenyu.client.mcp.common.dto.ShenyuMcpTool tool = new org.apache.shenyu.client.mcp.common.dto.ShenyuMcpTool();
        tool.setOperation(OpenApiConvertorUtil.convertOperation(getAnnotation(clazz).operation()));
        tool.setMethod("get");
        tool.setToolName(getAnnotation(clazz).toolName());
        return tool;
    }

    @ShenyuMcpTool(toolName = "defaultTool", desc = "bare tool: servers() and parameters are unset")
    private static class DefaultTool {
    }

    @ShenyuMcpTool(definition = @OpenAPIDefinition(servers = @Server(url = SERVER_URL)), toolName = "configuredServerTool")
    private static class ConfiguredServerTool {
    }

    @ShenyuMcpTool(toolName = "toolWithParameters",
            operation = @Operation(parameters = @Parameter(name = "id", in = ParameterIn.QUERY, description = "the id")))
    private static class ToolWithParameters {
    }
}
