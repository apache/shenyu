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
import org.apache.shenyu.client.mcp.common.constants.RequestTemplateConstants;
import org.apache.shenyu.client.mcp.common.dto.ShenyuMcpRequestConfig;

import java.util.Objects;


/**
 * the mcpRequestConfig generator.
 */
public class McpRequestConfigGenerator {

    public static JsonObject generateRequestConfig(final JsonObject openApiJson, final ShenyuMcpRequestConfig shenyuMcpRequestConfig) {

        // requestConfig
        JsonObject root = new JsonObject();

        // requestTemplate
        JsonObject requestTemplate = new JsonObject();
        root.add(RequestTemplateConstants.REQUEST_TEMPLATE_KEY, requestTemplate);

        // url
        JsonObject paths = openApiJson.get(OpenApiConstants.OPEN_API_PATH_KEY).getAsJsonObject();
        String path = null;
        for (String methodKey : paths.keySet()) {
            path = methodKey;
            break;
        }
        requestTemplate.addProperty(RequestTemplateConstants.URL_KEY, path);

        // method
        JsonObject method = paths.get(path).getAsJsonObject();
        String methodType = null;
        for (String methodKey : method.keySet()) {
            methodType = methodKey;
            break;
        }
        requestTemplate.addProperty(RequestTemplateConstants.METHOD_KEY, methodType);

        // argsPosition
        JsonObject methodTypeJson = method.getAsJsonObject(methodType);
        JsonArray parameters = methodTypeJson.getAsJsonArray(OpenApiConstants.OPEN_API_PATH_OPERATION_METHOD_PARAMETERS_KEY);
        if (Objects.nonNull(parameters)) {
            JsonObject argsPosition = new JsonObject();

            for (JsonElement parameter : parameters) {
                JsonObject paramObj = parameter.getAsJsonObject();

                if (paramObj.has(OpenApiConstants.OPEN_API_PATH_OPERATION_METHOD_PARAMETERS_NAME_KEY)
                        && paramObj.has(OpenApiConstants.OPEN_API_OPERATION_PATH_METHOD_PARAMETERS_IN_KEY)) {

                    String name = paramObj.get(OpenApiConstants.OPEN_API_PATH_OPERATION_METHOD_PARAMETERS_NAME_KEY).getAsString();
                    String inValue = paramObj.get(OpenApiConstants.OPEN_API_OPERATION_PATH_METHOD_PARAMETERS_IN_KEY).getAsString();

                    argsPosition.addProperty(name, inValue);
                }
            }
            requestTemplate.add(RequestTemplateConstants.ARGS_POSITION_KEY, argsPosition);
        }

        // argsToJsonBody
        requestTemplate.addProperty(RequestTemplateConstants.BODY_JSON_KEY, shenyuMcpRequestConfig.getBodyToJson());

        // header
        JsonArray headers = new JsonArray();
        shenyuMcpRequestConfig.getHeaders().forEach((key, value) -> {
            JsonObject headerJson = new JsonObject();
            headerJson.addProperty("key", key);
            headerJson.addProperty("value", value);
            headers.add(headerJson);
        });
        requestTemplate.add(RequestTemplateConstants.HEADERS_KEY, headers);

        return root;
    }
}
