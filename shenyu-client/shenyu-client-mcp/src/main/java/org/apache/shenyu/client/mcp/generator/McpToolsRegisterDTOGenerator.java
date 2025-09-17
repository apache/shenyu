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
import io.swagger.v3.oas.annotations.Operation;
import org.apache.shenyu.client.mcp.common.annotation.ShenyuMcpClient;
import org.apache.shenyu.client.mcp.common.annotation.ShenyuMcpRequestConfig;
import org.apache.shenyu.client.mcp.common.constants.OpenApiConstants;
import org.apache.shenyu.register.common.dto.McpToolsRegisterDTO;

/**
 * the mcpToolsRegisterDTO generator.
 */
public class McpToolsRegisterDTOGenerator {

    public static McpToolsRegisterDTO generateRegisterDTO(final ShenyuMcpClient classMcpClient, final ShenyuMcpClient methodMcpClient,
                                                          final JsonObject openApiJsonObject, final String url, final String namespaceId) {
        JsonObject root = new JsonObject();

        Operation operation = methodMcpClient.operation();
        ShenyuMcpRequestConfig shenyuMcpRequestConfig = methodMcpClient.requestConfig();

        JsonObject paths = openApiJsonObject.getAsJsonObject(OpenApiConstants.OPEN_API_PATH_KEY);
        JsonObject path = paths.getAsJsonObject(url);
        JsonObject method = path.getAsJsonObject(operation.method());
        JsonArray parameters = method.getAsJsonArray(OpenApiConstants.OPEN_API_PATH_PATH_METHOD_PARAMETERS_KEY);

        root.addProperty("name", classMcpClient.toolName());
        root.add("parameters", parameters);

        JsonObject requestConfig = McpRequestConfigGenerator.generateRequestConfig(openApiJsonObject, shenyuMcpRequestConfig);
        root.addProperty("requestConfig", requestConfig.toString());

        root.addProperty("description", operation.description());

        McpToolsRegisterDTO mcpToolsRegisterDTO = new McpToolsRegisterDTO();
        mcpToolsRegisterDTO.setNamespaceId(namespaceId);
        mcpToolsRegisterDTO.setMcpConfig(root.toString());
        return mcpToolsRegisterDTO;
    }
}
