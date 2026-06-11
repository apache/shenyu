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
import io.swagger.v3.oas.models.parameters.Parameter;
import org.apache.shenyu.client.mcp.common.annotation.ShenyuMcpTool;
import org.apache.shenyu.client.mcp.common.constants.OpenApiConstants;

import java.util.List;
import java.util.Objects;

/**
 * the openApi generator.
 */
public class McpOpenApiGenerator {
    public static JsonObject generateOpenApiJson(final ShenyuMcpTool classMcpClient,
                                                 final org.apache.shenyu.client.mcp.common.dto.ShenyuMcpTool shenyuMcpTool,
                                                 final String url) {
        JsonObject root = new JsonObject();
        root.addProperty(OpenApiConstants.OPEN_API_VERSION_KEY, "3.0.0");

        // Info
        JsonObject info = new JsonObject();
        OpenAPIDefinition definition = classMcpClient.definition();
        info.addProperty(OpenApiConstants.OPEN_API_INFO_TITLE_KEY, definition.info().title());
        info.addProperty(OpenApiConstants.OPEN_API_INFO_DESCRIPTION_KEY, definition.info().description());
        info.addProperty(OpenApiConstants.OPEN_API_INFO_VERSION_KEY, "");
        root.add(OpenApiConstants.OPEN_API_INFO_KEY, info);

        // Servers
        JsonObject server = new JsonObject();
        root.add(OpenApiConstants.OPEN_API_SERVER_KEY, server);
        server.addProperty(OpenApiConstants.OPEN_API_SERVER_URL_KEY, definition.servers()[0].url());

        // Paths
        JsonObject paths = new JsonObject();
        root.add(OpenApiConstants.OPEN_API_PATH_KEY, paths);

        String pathKey = url;
        JsonObject pathMap = new JsonObject();
        paths.add(pathKey, pathMap);

        String methodType = shenyuMcpTool.getMethod();
        JsonObject methodMap = new JsonObject();
        pathMap.add(methodType, methodMap);

        methodMap.addProperty(OpenApiConstants.OPEN_API_PATH_OPERATION_METHOD_SUMMARY_KEY, "");
        methodMap.addProperty(OpenApiConstants.OPEN_API_PATH_OPERATION_METHOD_DESCRIPTION_KEY, "");
        methodMap.addProperty(OpenApiConstants.OPEN_API_PATH_OPERATION_METHOD_OPERATION_ID_KEY, "");
        methodMap.add(OpenApiConstants.OPEN_API_PATH_OPERATION_METHOD_TAG_KEY, new JsonArray());

        JsonArray parameters = new JsonArray();
        methodMap.add(OpenApiConstants.OPEN_API_PATH_OPERATION_METHOD_PARAMETERS_KEY, parameters);

        List<io.swagger.v3.oas.models.parameters.Parameter> parameterList = shenyuMcpTool.getOperation().getParameters();

        if (!parameterList.isEmpty()) {

            for (Parameter parameter : parameterList) {
                JsonObject parameterObj = new JsonObject();
                parameterObj.addProperty(OpenApiConstants.OPEN_API_PATH_OPERATION_METHOD_PARAMETERS_NAME_KEY, parameter.getName());
                parameterObj.addProperty(OpenApiConstants.OPEN_API_OPERATION_PATH_METHOD_PARAMETERS_IN_KEY, parameter.getIn());
                parameterObj.addProperty(OpenApiConstants.OPEN_API_PATH_OPERATION_METHOD_PARAMETERS_DESCRIPTION_KEY, parameter.getDescription());
                parameterObj.addProperty(OpenApiConstants.OPEN_API_PATH_OPERATION_METHOD_PARAMETERS_REQUIRED_KEY, parameter.getRequired());

                if (Objects.nonNull(parameter.getSchema())) {
                    JsonObject schemaMap = new JsonObject();
                    schemaMap.addProperty(OpenApiConstants.OPEN_API_PATH_OPERATION_METHOD_PARAMETERS_SCHEMA_TYPE_KEY, parameter.getSchema().getType());
                    parameterObj.add(OpenApiConstants.OPEN_API_PATH_OPERATION_METHOD_PARAMETERS_SCHEMA_KEY, schemaMap);
                }
                parameters.add(parameterObj);
            }
        }

        return root;
    }
}