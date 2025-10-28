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

import io.swagger.v3.oas.models.Operation;
import org.apache.shenyu.client.mcp.utils.OpenApiConvertorUtil;

/**
 * the shenyu mcp tool object.
 */
public class ShenyuMcpTool {

    private io.swagger.v3.oas.models.Operation operation;

    private ShenyuMcpRequestConfig requestConfig;

    private String toolName;

    private Boolean enable = true;

    private String method;

    public ShenyuMcpTool() {
    }

    public ShenyuMcpTool(final org.apache.shenyu.client.mcp.common.annotation.ShenyuMcpTool shenyuMcpTool) {
        this.operation = OpenApiConvertorUtil.convertOperation(shenyuMcpTool.operation());
        this.requestConfig = OpenApiConvertorUtil.convertRequestConfig(shenyuMcpTool.requestConfig());
        this.toolName = shenyuMcpTool.toolName();
        this.enable = shenyuMcpTool.enabled();
    }

    /**
     * get operation.
     *
     * @return operation;
     */
    public Operation getOperation() {
        return operation;
    }

    /**
     * set operation.
     *
     * @param operation operation
     */
    public void setOperation(final Operation operation) {
        this.operation = operation;
    }

    /**
     * get requestConfig.
     *
     * @return requestConfig;
     */
    public ShenyuMcpRequestConfig getRequestConfig() {
        return requestConfig;
    }

    /**
     * set requestConfig.
     *
     * @param requestConfig requestConfig
     */
    public void setRequestConfig(final ShenyuMcpRequestConfig requestConfig) {
        this.requestConfig = requestConfig;
    }

    /**
     * get toolName.
     *
     * @return toolName;
     */
    public String getToolName() {
        return toolName;
    }

    /**
     * set toolName.
     *
     * @param toolName toolName
     */
    public void setToolName(final String toolName) {
        this.toolName = toolName;
    }

    /**
     * get enable.
     *
     * @return enable;
     */
    public Boolean getEnable() {
        return enable;
    }

    /**
     * set enable.
     *
     * @param enable enable
     */
    public void setEnable(final Boolean enable) {
        this.enable = enable;
    }


    /**
     * get method.
     *
     * @return method;
     */
    public String getMethod() {
        return method;
    }

    /**
     * set method.
     *
     * @param method method
     */
    public void setMethod(final String method) {
        this.method = method;
    }
}
