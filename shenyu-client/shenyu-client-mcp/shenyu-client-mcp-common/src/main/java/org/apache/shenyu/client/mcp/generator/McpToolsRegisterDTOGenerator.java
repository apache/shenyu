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
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.apache.shenyu.client.mcp.common.constants.OpenApiConstants;
import org.apache.shenyu.client.mcp.common.constants.ShenyuToolConfigConstants;
import org.apache.shenyu.client.mcp.common.dto.ShenyuMcpTool;
import org.apache.shenyu.register.common.dto.McpToolsRegisterDTO;

import java.util.Objects;

/**
 * the mcpToolsRegisterDTO generator.
 */
public class McpToolsRegisterDTOGenerator {

    public static McpToolsRegisterDTO generateRegisterDTO(final ShenyuMcpTool shenyuMcpTool, final JsonObject openApiJsonObject,
                                                          final String url, final String namespaceId) {
        JsonObject root = new JsonObject();

        JsonObject paths = openApiJsonObject.getAsJsonObject(OpenApiConstants.OPEN_API_PATH_KEY);
        JsonObject path = paths.getAsJsonObject(url);
        JsonObject method = path.getAsJsonObject(shenyuMcpTool.getMethod());
        JsonArray parameters = method.getAsJsonArray(OpenApiConstants.OPEN_API_PATH_OPERATION_METHOD_PARAMETERS_KEY);

        JsonObject requestConfig = McpRequestConfigGenerator.generateRequestConfig(openApiJsonObject, shenyuMcpTool.getRequestConfig());
        root.addProperty(ShenyuToolConfigConstants.REQUEST_CONFIG_KEY, requestConfig.toString());

        root.addProperty(ShenyuToolConfigConstants.NAME_KEY, shenyuMcpTool.getToolName());
        root.add(ShenyuToolConfigConstants.PARAMETERS_KEY, parameterFormatting(parameters));

        root.addProperty(ShenyuToolConfigConstants.DESCRIPTION_KEY, shenyuMcpTool.getOperation().getDescription());

        McpToolsRegisterDTO mcpToolsRegisterDTO = new McpToolsRegisterDTO();
        mcpToolsRegisterDTO.setNamespaceId(namespaceId);
        mcpToolsRegisterDTO.setMcpConfig(root.toString());
        return mcpToolsRegisterDTO;
    }

    public static JsonArray parameterFormatting(final JsonArray parameters) {

        parameters.forEach(parameter -> {
            JsonObject paramObj = parameter.getAsJsonObject();

            JsonElement schema = paramObj.remove(OpenApiConstants.OPEN_API_PATH_OPERATION_METHOD_PARAMETERS_SCHEMA_KEY);
            if (Objects.nonNull(schema) && schema.isJsonObject()) {
                JsonObject schemaObj = schema.getAsJsonObject();
                JsonElement typeElement = schemaObj.get(OpenApiConstants.OPEN_API_PATH_OPERATION_METHOD_PARAMETERS_SCHEMA_TYPE_KEY);

                if (Objects.nonNull(typeElement) && typeElement.isJsonPrimitive()) {
                    String type = typeElement.getAsString();
                    paramObj.addProperty(OpenApiConstants.OPEN_API_PATH_OPERATION_METHOD_PARAMETERS_SCHEMA_TYPE_KEY, type);
                }
            }
            if (!paramObj.has(OpenApiConstants.OPEN_API_PATH_OPERATION_METHOD_PARAMETERS_DESCRIPTION_KEY)) {
                paramObj.addProperty(OpenApiConstants.OPEN_API_PATH_OPERATION_METHOD_PARAMETERS_DESCRIPTION_KEY,
                        paramObj.get(OpenApiConstants.OPEN_API_PATH_OPERATION_METHOD_PARAMETERS_NAME_KEY).getAsString());
            }
            paramObj.remove(OpenApiConstants.OPEN_API_OPERATION_PATH_METHOD_PARAMETERS_IN_KEY);
        });
        return parameters;

    }
}
