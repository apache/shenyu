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
        if (isCompleteUrl(argsPosition, inputJson)) {
            return getCompleteUrl(argsPosition, inputJson);
        }

        StringBuilder queryBuilder = new StringBuilder();

        // Check whether the URL template already contains the query parameters
        boolean hasExistingQuery = urlTemplate.contains("?");
        String basePath = hasExistingQuery ? urlTemplate.substring(0, urlTemplate.indexOf("?")) : urlTemplate;
        String existingQuery = hasExistingQuery ? urlTemplate.substring(urlTemplate.indexOf("?") + 1) : "";

        // Handle new query parameters
        basePath = processArguments(argsPosition, inputJson, basePath, queryBuilder);

        // Clear the template variables that have not been replaced
        basePath = basePath.replaceAll("\\{\\{\\.[^}]+}}", "");

        // Build the final URL
        return buildFinalPath(basePath, queryBuilder, hasExistingQuery, existingQuery);
    }

    /**
     * Check if input contains a complete URL.
     *
     * @param argsPosition the argument position mapping
     * @param inputJson the input JSON object
     * @return true if input contains a complete URL
     */
    private static boolean isCompleteUrl(final JsonObject argsPosition, final JsonObject inputJson) {
        for (String key : argsPosition.keySet()) {
            if (inputJson.has(key)) {
                try {
                    String value = inputJson.get(key).getAsString();
                    if (value.startsWith("http://") || value.startsWith("https://") || value.contains("?")) {
                        return true;
                    }
                } catch (Exception exception) {
                    // Ignore exception
                }
            }
        }
        return false;
    }

    /**
     * Get the complete URL from input.
     *
     * @param argsPosition the argument position mapping
     * @param inputJson the input JSON object
     * @return the complete URL
     */
    private static String getCompleteUrl(final JsonObject argsPosition, final JsonObject inputJson) {
        for (String key : argsPosition.keySet()) {
            if (inputJson.has(key)) {
                try {
                    String value = inputJson.get(key).getAsString();
                    if (value.startsWith("http://") || value.startsWith("https://") || value.contains("?")) {
                        return value;
                    }
                } catch (Exception exception) {
                    // Ignore exception
                }
            }
        }
        return "";
    }

    /**
     * Process arguments for path and query parameters.
     *
     * @param argsPosition the argument position mapping
     * @param inputJson the input JSON object
     * @param basePath the base path to modify
     * @param queryBuilder the query builder to append to
     * @return the modified base path
     */
    private static String processArguments(final JsonObject argsPosition, final JsonObject inputJson,
                                       final String basePath, final StringBuilder queryBuilder) {
        String modifiedBasePath = basePath;
        for (String key : argsPosition.keySet()) {
            String position = argsPosition.get(key).getAsString();
            if ("path".equals(position) && inputJson.has(key)) {
                // Process path parameters
                String value = inputJson.get(key).getAsString();
                if (value.contains("?")) {
                    value = value.substring(0, value.indexOf("?"));
                }
                value = value.replace("\"", "").trim();
                modifiedBasePath = modifiedBasePath.replace("{{." + key + "}}", value);
            } else if ("query".equals(position) && inputJson.has(key)) {
                // Handle query parameters
                if (!modifiedBasePath.contains(key + "=")) {
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
        return modifiedBasePath;
    }

    /**
     * Build the final path with query parameters.
     *
     * @param basePath the base path
     * @param queryBuilder the query builder
     * @param hasExistingQuery whether there are existing query parameters
     * @param existingQuery the existing query string
     * @return the final path
     */
    private static String buildFinalPath(final String basePath, final StringBuilder queryBuilder,
                                       final boolean hasExistingQuery, final String existingQuery) {
        StringBuilder finalPath = new StringBuilder(basePath);

        // Add query parameters
        if (!queryBuilder.isEmpty()) {
            if (hasExistingQuery) {
                finalPath.append("&").append(queryBuilder);
            } else {
                finalPath.append("?").append(queryBuilder);
            }
        }

        // Add existing query parameters
        if (hasExistingQuery && !existingQuery.isEmpty()) {
            if (!queryBuilder.isEmpty()) {
                finalPath.append("&").append(existingQuery);
            } else {
                finalPath.append("?").append(existingQuery);
            }
        }

        String result = finalPath.toString();
        // Remove the question mark at the end
        if (result.endsWith("?")) {
            result = result.substring(0, result.length() - 1);
        }

        return result;
    }

}