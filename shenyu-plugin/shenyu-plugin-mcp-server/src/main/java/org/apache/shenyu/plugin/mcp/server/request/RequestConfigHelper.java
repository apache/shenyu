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

package org.apache.shenyu.plugin.mcp.server.request;

import com.google.gson.JsonObject;
import org.apache.shenyu.common.utils.GsonUtils;

/**
 * Helper class for parsing and handling requestConfig.
 */
public class RequestConfigHelper {
    
    private final JsonObject configJson;
    
    /**
     * Constructor.
     *
     * @param requestConfig the request config string
     */
    public RequestConfigHelper(final String requestConfig) {
        this.configJson = GsonUtils.getInstance().fromJson(requestConfig, JsonObject.class);
    }
    
    /**
     * Get the config json object.
     *
     * @return the config json object
     */
    public JsonObject getConfigJson() {
        return configJson;
    }
    
    /**
     * Get the request template json object.
     *
     * @return the request template json object
     */
    public JsonObject getRequestTemplate() {
        return configJson.getAsJsonObject("requestTemplate");
    }
    
    /**
     * Get the argument position mapping.
     *
     * @return the argument position json object
     */
    public JsonObject getArgsPosition() {
        return configJson.has("argsPosition") ? configJson.getAsJsonObject("argsPosition") : new JsonObject();
    }
    
    /**
     * Get the response template json object.
     *
     * @return the response template json object, or null if not present
     */
    public JsonObject getResponseTemplate() {
        return configJson.has("responseTemplate") ? configJson.getAsJsonObject("responseTemplate") : null;
    }
    
    /**
     * Get the url template string.
     *
     * @return the url template string
     */
    public String getUrlTemplate() {
        return getRequestTemplate().get("url").getAsString();
    }
    
    /**
     * Get the HTTP method string.
     *
     * @return the HTTP method string
     */
    public String getMethod() {
        JsonObject requestTemplate = getRequestTemplate();
        return requestTemplate.has("method") ? requestTemplate.get("method").getAsString() : "GET";
    }
    
    public boolean isArgsToJsonBody() {
        JsonObject requestTemplate = getRequestTemplate();
        return requestTemplate.has("argsToJsonBody") && requestTemplate.get("argsToJsonBody").getAsBoolean();
    }
    
    public boolean isArgsToUrlParam() {
        JsonObject requestTemplate = getRequestTemplate();
        return requestTemplate.has("argsToUrlParam") && requestTemplate.get("argsToUrlParam").getAsBoolean();
    }
    
    /**
     * Build the request path based on the URL template and argument positions.
     *
     * @param urlTemplate the URL template
     * @param argsPosition the argument position mapping
     * @param inputJson the input JSON object
     * @return the constructed request path
     */
    public static String buildPath(final String urlTemplate, final JsonObject argsPosition,
                                   final JsonObject inputJson) {
        // First, check if the input value is already a complete URL
        for (String key : argsPosition.keySet()) {
            if (inputJson.has(key)) {
                String value = inputJson.get(key).getAsString();
                // If the input value is already a complete URL, return it directly
                if (value.startsWith("http://") || value.startsWith("https://") || value.contains("?")) {
                    return value;
                }
            }
        }
        
        StringBuilder queryBuilder = new StringBuilder();
        
        // Check if the URL template already contains query parameters
        boolean hasExistingQuery = urlTemplate.contains("?");
        String basePath = hasExistingQuery ? urlTemplate.substring(0, urlTemplate.indexOf("?")) : urlTemplate;
        String existingQuery = hasExistingQuery ? urlTemplate.substring(urlTemplate.indexOf("?") + 1) : "";
        
        // handle new query parameters
        for (String key : argsPosition.keySet()) {
            String position = argsPosition.get(key).getAsString();
            if ("path".equals(position) && inputJson.has(key)) {
                // 处理路径参数
                String value = inputJson.get(key).getAsString();
                if (value.contains("?")) {
                    value = value.substring(0, value.indexOf("?"));
                }
                value = value.replace("\"", "").trim();
                basePath = basePath.replace("{{." + key + "}}", value);
            } else if ("query".equals(position) && inputJson.has(key)) {
                // handle query parameters
                if (!existingQuery.contains(key + "=")) {
                    if (!queryBuilder.isEmpty()) {
                        queryBuilder.append("&");
                    }
                    String value = inputJson.get(key).getAsString();
                    if (value.contains("?")) {
                        value = value.substring(0, value.indexOf("?"));
                    }
                    value = value.replace("\"", "").trim();
                    queryBuilder.append(key).append("=").append(value);
                }
            }
        }
        
        // clean up the base path by removing any placeholders that were not replaced
        basePath = basePath.replaceAll("\\{\\{\\.[^}]+}}", "");
        
        // build the final path
        StringBuilder finalPath = new StringBuilder(basePath);
        
        // add new query parameters if any
        if (!queryBuilder.isEmpty()) {
            if (hasExistingQuery) {
                finalPath.append("&").append(queryBuilder);
            } else {
                finalPath.append("?").append(queryBuilder);
            }
        }
        
        // add existing query parameters if any
        if (hasExistingQuery && !existingQuery.isEmpty()) {
            if (!queryBuilder.isEmpty()) {
                finalPath.append("&").append(existingQuery);
            } else {
                finalPath.append("?").append(existingQuery);
            }
        }
        
        String result = finalPath.toString();
        // remove trailing question mark if it exists
        if (result.endsWith("?")) {
            result = result.substring(0, result.length() - 1);
        }
        
        return result;
    }
    
    /**
     * Build the request body JSON object based on the argument positions and input
     * JSON.
     *
     * @param argsToJsonBody whether to convert arguments to JSON body
     * @param argsPosition the argument position mapping
     * @param inputJson the input JSON object
     * @return the constructed body JSON object
     */
    public static JsonObject buildBodyJson(final boolean argsToJsonBody, final JsonObject argsPosition,
                                           final JsonObject inputJson) {
        JsonObject bodyJson = new JsonObject();
        if (argsToJsonBody) {
            for (String key : argsPosition.keySet()) {
                String position = argsPosition.get(key).getAsString();
                if ("body".equals(position) && inputJson.has(key)) {
                    bodyJson.add(key, inputJson.get(key));
                }
            }
        }
        return bodyJson;
    }
}