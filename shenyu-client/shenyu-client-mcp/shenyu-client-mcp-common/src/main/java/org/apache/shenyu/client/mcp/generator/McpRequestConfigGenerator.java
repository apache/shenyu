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
import org.apache.shenyu.client.mcp.common.annotation.ShenyuMcpRequestConfig;
import org.apache.shenyu.client.mcp.common.constants.OpenApiConstants;
import org.apache.shenyu.client.mcp.common.constants.RequestTemplateConstants;

import java.util.Arrays;


/**
 * the mcpRequestConfig generator.
 */
public class McpRequestConfigGenerator {

    public static JsonObject generateRequestConfig(final JsonObject openApiJson, final ShenyuMcpRequestConfig shenyuMcpRequestConfig) {

        JsonObject root = new JsonObject();

        // url
        JsonObject server = openApiJson.get(OpenApiConstants.OPEN_API_SERVER_KEY).getAsJsonObject();
        String serverAddress = server.get(OpenApiConstants.OPEN_API_SERVER_URL_KEY).getAsString();
        JsonObject paths = openApiJson.get(OpenApiConstants.OPEN_API_PATH_KEY).getAsJsonObject();
        String path = null;
        for (String methodKey : paths.keySet()) {
            path = methodKey;
            break;
        }
        String url = serverAddress + path;
        root.addProperty(RequestTemplateConstants.URL_KEY, url);

        // method
        JsonObject method = paths.get(path).getAsJsonObject();
        String methodType = null;
        for (String methodKey : method.keySet()) {
            methodType = methodKey;
            break;
        }
        root.addProperty(RequestTemplateConstants.METHOD_KEY, methodType);

        //bodyJson
        root.addProperty(RequestTemplateConstants.BODY_JSON_KEY, shenyuMcpRequestConfig.bodyJson());

        // requestTemplate
        JsonObject requestTemplate = new JsonObject();
        root.add(RequestTemplateConstants.REQUEST_TEMPLATE_KEY, requestTemplate);

        // header
        JsonArray headers = new JsonArray();
        Arrays.asList(shenyuMcpRequestConfig.headers()).forEach(header -> {
            JsonObject headerJson = new JsonObject();
            headerJson.addProperty(header.key(), header.value());
            headers.add(headerJson);
        });
        requestTemplate.add(RequestTemplateConstants.HEADERS_KEY, headers);

        return root;
    }
}
