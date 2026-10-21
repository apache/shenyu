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

import org.apache.shenyu.client.mcp.utils.OpenApiConvertorUtil;

import java.util.Objects;

/**
 * Client annotation adapter for the shared MCP tool model.
 */
public class ShenyuMcpTool extends org.apache.shenyu.register.mcp.common.dto.ShenyuMcpTool {

    public ShenyuMcpTool() {
    }

    public ShenyuMcpTool(final org.apache.shenyu.client.mcp.common.annotation.ShenyuMcpTool tool) {
        setOperation(OpenApiConvertorUtil.convertOperation(tool.operation()));
        setRequestConfig(OpenApiConvertorUtil.convertRequestConfig(tool.requestConfig()));
        setToolName(tool.toolName());
        setEnable(tool.enabled());
    }

    @Override
    public ShenyuMcpRequestConfig getRequestConfig() {
        org.apache.shenyu.register.mcp.common.dto.ShenyuMcpRequestConfig config = super.getRequestConfig();
        if (Objects.isNull(config) || config instanceof ShenyuMcpRequestConfig) {
            return (ShenyuMcpRequestConfig) config;
        }
        // Deserializers populate the inherited field using its shared model type.
        ShenyuMcpRequestConfig clientConfig = new ShenyuMcpRequestConfig();
        clientConfig.setHeaders(config.getHeaders());
        clientConfig.setBodyToJson(config.getBodyToJson());
        super.setRequestConfig(clientConfig);
        return clientConfig;
    }

    /**
     * Set the client request model.
     *
     * @param requestConfig the request model
     */
    public void setRequestConfig(final ShenyuMcpRequestConfig requestConfig) {
        super.setRequestConfig(requestConfig);
    }
}
