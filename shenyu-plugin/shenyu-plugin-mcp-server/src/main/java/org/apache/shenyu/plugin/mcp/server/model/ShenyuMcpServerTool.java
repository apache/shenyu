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

package org.apache.shenyu.plugin.mcp.server.model;

import com.google.common.collect.Lists;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * McpToolDescription represents a tool description in the context of a microservice.
 * It contains information about the tool's ID, name, version, description, parameters,
 * and return type.
 */
public class ShenyuMcpServerTool {
    
    /**
     * Name of the tool .
     */
    private String name;
    
    /**
     * Description of the tool .
     */
    private String description;
    
    /**
     * requestTemplate of the tool .
     */
    private String requestConfig;
    
    /**
     * Parameters of the tool .
     */
    private List<McpServerToolParameter> parameters = Lists.newArrayList();
    
    
    /**
     * Getter for name.
     *
     * @return name
     */
    public String getName() {
        return name;
    }
    
    /**
     * Setter for name.
     *
     * @param name name
     */
    public void setName(final String name) {
        this.name = name;
    }
    
    /**
     * Getter for description.
     *
     * @return description
     */
    public String getDescription() {
        return description;
    }
    
    /**
     * Setter for description.
     *
     * @param description description
     */
    public void setDescription(final String description) {
        this.description = description;
    }
    
    /**
     * Getter for requestConfig.
     *
     * @return requestConfig
     */
    public String getRequestConfig() {
        return requestConfig;
    }
    
    /**
     * Setter for requestConfig.
     *
     * @param requestConfig requestConfig
     */
    public void setRequestConfig(final String requestConfig) {
        this.requestConfig = requestConfig;
    }
    
    /**
     * Getter for parameters.
     *
     * @return parameters
     */
    public List<McpServerToolParameter> getParameters() {
        return parameters;
    }
    
    /**
     * Setter for parameters.
     *
     * @param parameters parameters
     */
    public void setParameters(final List<McpServerToolParameter> parameters) {
        this.parameters = parameters;
    }
    
    
    public static ShenyuMcpServerTool newInstance() {
        ShenyuMcpServerTool mcpServerPluginRuleHandle = new ShenyuMcpServerTool();
        mcpServerPluginRuleHandle.setName("");
        mcpServerPluginRuleHandle.setDescription("");
        mcpServerPluginRuleHandle.setParameters(new ArrayList<>(0));
        return mcpServerPluginRuleHandle;
    }
    
    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        ShenyuMcpServerTool that = (ShenyuMcpServerTool) o;
        return Objects.equals(name, that.name)
                && Objects.equals(description, that.description)
                && Objects.equals(requestConfig, that.requestConfig)
                && Objects.equals(parameters, that.parameters);
    }
    
    @Override
    public int hashCode() {
        return Objects.hash(name, description, requestConfig, parameters);
    }
    
    @Override
    public String toString() {
        return String.format("McpServerPluginRuleHandle: name: %s, description: %s, requestConfig: %s, parameters: %s",
                name,
                description,
                requestConfig,
                parameters);
    }
}
