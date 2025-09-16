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
import org.apache.shenyu.client.mcp.common.annotation.OpenApiConfig;
import org.apache.shenyu.client.mcp.common.annotation.OpenApiParameter;
import org.apache.shenyu.client.mcp.common.annotation.ShenyuMcpClient;
import org.apache.shenyu.client.mcp.common.constants.OpenApiConstants;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * the openApi generator
 */
public class McpOpenApiGenerator {
    public static JsonObject generateOpenApiJson(final ShenyuMcpClient apiDataList) {
        JsonObject root = new JsonObject();
        root.addProperty(OpenApiConstants.OPEN_API_VERSION_KEY, "3.0.0");

        // Info
        JsonObject info = new JsonObject();
        OpenApiConfig openApiConfig = apiDataList.openApi();
        info.addProperty(OpenApiConstants.OPEN_API_INFO_TITLE_KEY, openApiConfig.info().title());
        info.addProperty(OpenApiConstants.OPEN_API_INFO_DESCRIPTION_KEY, openApiConfig.info().description());
        info.addProperty(OpenApiConstants.OPEN_API_INFO_VERSION_KEY, "");
        root.add(OpenApiConstants.OPEN_API_INFO_KEY, info);

        // Servers
        JsonObject server = new JsonObject();
        root.add(OpenApiConstants.OPEN_API_SERVER_KEY, server);
        server.addProperty(OpenApiConstants.OPEN_API_SERVER_URL_KEY, openApiConfig.server().url());

        // Paths
        JsonObject paths = new JsonObject();
        root.add(OpenApiConstants.OPEN_API_PATH_KEY, paths);

        String pathKey = openApiConfig.path().path();
        JsonObject pathMap = new JsonObject();
        paths.add(pathKey, pathMap);

        String methodType = openApiConfig.path().type();
        JsonObject methodMap = new JsonObject();
        pathMap.add(methodType, methodMap);

        methodMap.addProperty(OpenApiConstants.OPEN_API_PATH_PATH_METHOD_SUMMARY_KEY, "");
        methodMap.addProperty(OpenApiConstants.OPEN_API_PATH_PATH_METHOD_DESCRIPTION_KEY, "");
        methodMap.addProperty(OpenApiConstants.OPEN_API_PATH_PATH_METHOD_OPERATION_ID_KEY, "");
        methodMap.add(OpenApiConstants.OPEN_API_PATH_PATH_METHOD_TAG_KEY, new JsonArray());

        JsonArray parameters = new JsonArray();
        methodMap.add(OpenApiConstants.OPEN_API_PATH_PATH_METHOD_PARAMETERS_KEY, parameters);

        OpenApiParameter[] openApiParameters = openApiConfig.path().parameters();
        OpenApiParameter openApiParameter = openApiConfig.path().parameter();

        if ((openApiParameters != null && openApiParameters.length > 0)
                || (openApiParameter != null && openApiParameter.name() != null && !openApiParameter.name().isEmpty())) {
            List<OpenApiParameter> parameterList = new ArrayList<>();
            if (openApiParameters != null) {
                parameterList.addAll(Arrays.asList(openApiParameters));
            }
            if (openApiParameter != null && openApiParameter.name() != null && !openApiParameter.name().isEmpty()) {
                parameterList.add(openApiParameter);
            }

            for (OpenApiParameter parameter : parameterList) {
                JsonObject parameterObj = new JsonObject();
                parameterObj.addProperty(OpenApiConstants.OPEN_API_PATH_PATH_METHOD_PARAMETERS_NAME_KEY, parameter.name());
                parameterObj.addProperty(OpenApiConstants.OPEN_API_PATH_PATH_METHOD_PARAMETERS_IN_KEY, parameter.in());
                parameterObj.addProperty(OpenApiConstants.OPEN_API_PATH_PATH_METHOD_PARAMETERS_DESCRIPTION_KEY, parameter.description());

                JsonObject schema = new JsonObject();
                schema.addProperty(OpenApiConstants.OPEN_API_PATH_PATH_METHOD_PARAMETERS_SCHEMA_TYPE_KEY, parameter.type());
                schema.addProperty(OpenApiConstants.OPEN_API_PATH_PATH_METHOD_PARAMETERS_SCHEMA_DEFAULT_VALUE_KEY, parameter.defaultValue());
                parameterObj.add(OpenApiConstants.OPEN_API_PATH_PATH_METHOD_PARAMETERS_SCHEMA_KEY, schema);

                parameters.add(parameterObj);
            }
        }

        return root;
    }
}