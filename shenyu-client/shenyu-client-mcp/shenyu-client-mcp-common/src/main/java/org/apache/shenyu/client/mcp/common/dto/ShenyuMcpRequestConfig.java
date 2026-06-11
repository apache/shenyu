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

package org.apache.shenyu.client.mcp.common.dto;

import java.util.HashMap;
import java.util.Map;

/**
 * the shenyu mcp request config object.
 */
public class ShenyuMcpRequestConfig {

    private Map<String, String> headers = new HashMap<>();

    private String bodyToJson;

    public ShenyuMcpRequestConfig() {
    }

    /**
     * get headers.
     *
     * @return headers
     */
    public Map<String, String> getHeaders() {
        return headers;
    }

    /**
     * set headers.
     *
     * @param headers headers
     */
    public void setHeaders(final Map<String, String> headers) {
        this.headers = headers;
    }

    /**
     * get bodyToJson.
     *
     * @return bodyToJson
     */
    public String getBodyToJson() {
        return bodyToJson;
    }

    /**
     * set bodyToJson.
     *
     * @param bodyToJson bodyToJson
     */
    public void setBodyToJson(final String bodyToJson) {
        this.bodyToJson = bodyToJson;
    }
}
