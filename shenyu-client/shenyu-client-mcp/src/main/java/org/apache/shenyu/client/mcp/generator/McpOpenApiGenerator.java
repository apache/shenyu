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
import org.apache.shenyu.client.mcp.common.annotation.ShenyuMcpClient;
import org.apache.shenyu.client.mcp.common.constants.OpenApiConstants;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

/**
 * the openApi generator.
 */
public class McpOpenApiGenerator {
    public static JsonObject generateOpenApiJson(final ShenyuMcpClient classMcpClient, final ShenyuMcpClient methodMcpClient, final String url) {
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

        Operation operation = methodMcpClient.operation();
        String methodType = operation.method();
        JsonObject methodMap = new JsonObject();
        pathMap.add(methodType, methodMap);

        methodMap.addProperty(OpenApiConstants.OPEN_API_PATH_PATH_METHOD_SUMMARY_KEY, "");
        methodMap.addProperty(OpenApiConstants.OPEN_API_PATH_PATH_METHOD_DESCRIPTION_KEY, "");
        methodMap.addProperty(OpenApiConstants.OPEN_API_PATH_PATH_METHOD_OPERATION_ID_KEY, "");
        methodMap.add(OpenApiConstants.OPEN_API_PATH_PATH_METHOD_TAG_KEY, new JsonArray());

        JsonArray parameters = new JsonArray();
        methodMap.add(OpenApiConstants.OPEN_API_PATH_PATH_METHOD_PARAMETERS_KEY, parameters);

        Parameter[] openApiParameters = operation.parameters();

        if (Objects.nonNull(openApiParameters)) {
            List<Parameter> parameterList = new ArrayList<>();

            parameterList.addAll(Arrays.asList(openApiParameters));

            for (Parameter parameter : parameterList) {
                JsonObject parameterObj = new JsonObject();
                parameterObj.addProperty(OpenApiConstants.OPEN_API_PATH_PATH_METHOD_PARAMETERS_NAME_KEY, parameter.name());
                parameterObj.addProperty(OpenApiConstants.OPEN_API_PATH_PATH_METHOD_PARAMETERS_IN_KEY, parameter.in().toString());
                parameterObj.addProperty(OpenApiConstants.OPEN_API_PATH_PATH_METHOD_PARAMETERS_DESCRIPTION_KEY, parameter.description());
                parameterObj.addProperty(OpenApiConstants.OPEN_API_PATH_PATH_METHOD_PARAMETERS_REQUIRED_KEY, parameter.required());

                JsonObject schema = new JsonObject();
                schema.addProperty(OpenApiConstants.OPEN_API_PATH_PATH_METHOD_PARAMETERS_SCHEMA_TYPE_KEY, parameter.schema().type());
                schema.addProperty(OpenApiConstants.OPEN_API_PATH_PATH_METHOD_PARAMETERS_SCHEMA_DEFAULT_VALUE_KEY, parameter.schema().defaultValue());
                parameterObj.add(OpenApiConstants.OPEN_API_PATH_PATH_METHOD_PARAMETERS_SCHEMA_KEY, schema);

                parameters.add(parameterObj);
            }
        }

        return root;
    }
}