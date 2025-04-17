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

package org.apache.shenyu.common.dto.convert.rule.impl;

import com.google.common.collect.Lists;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * McpToolDescription represents a tool description in the context of a microservice.
 * It contains information about the tool's ID, name, version, description, parameters,
 * and return type.
 */
public class McpServerPluginRuleHandle {
    
    /**
     * Name of the tool description.
     */
    private String name;
    
    /**
     * Description of the tool description.
     */
    private String description;
    
    /**
     * Request method of the tool description.
     */
    private String requestMethod;
    
    /**
     * Request path of the tool description.
     */
    private String requestPath;
    
    /**
     * Parameters of the tool description.
     */
    private List<McpParameter> parameters = Lists.newArrayList();
    
    
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
     * Getter for requestMethod.
     *
     * @return requestMethod
     */
    public String getRequestMethod() {
        return requestMethod;
    }
    
    /**
     * Setter for requestMethod.
     *
     * @param requestMethod requestMethod
     */
    public void setRequestMethod(final String requestMethod) {
        this.requestMethod = requestMethod;
    }
    
    /**
     * Getter for requestPath.
     *
     * @return requestPath
     */
    public String getRequestPath() {
        return requestPath;
    }
    
    /**
     * Setter for requestPath.
     *
     * @param requestPath requestPath
     */
    public void setRequestPath(final String requestPath) {
        this.requestPath = requestPath;
    }
    
    /**
     * Getter for parameters.
     *
     * @return parameters
     */
    public List<McpParameter> getParameters() {
        return parameters;
    }
    
    /**
     * Setter for parameters.
     *
     * @param parameters parameters
     */
    public void setParameters(final List<McpParameter> parameters) {
        this.parameters = parameters;
    }
    
    
    public static McpServerPluginRuleHandle newInstance() {
        McpServerPluginRuleHandle mcpServerPluginRuleHandle = new McpServerPluginRuleHandle();
        mcpServerPluginRuleHandle.setName("");
        mcpServerPluginRuleHandle.setDescription("");
        mcpServerPluginRuleHandle.setRequestMethod("GET");
        mcpServerPluginRuleHandle.setRequestPath("");
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
        McpServerPluginRuleHandle that = (McpServerPluginRuleHandle) o;
        return Objects.equals(name, that.name)
                && Objects.equals(description, that.description)
                && Objects.equals(requestMethod, that.requestMethod)
                && Objects.equals(requestPath, that.requestPath)
                && Objects.equals(parameters, that.parameters);
    }
    
    @Override
    public int hashCode() {
        return Objects.hash(name, description, requestMethod, requestPath, parameters);
    }
    
    @Override
    public String toString() {
        return String.format("McpServerPluginRuleHandle: name: %s, description: %s, requestMethod: %s, requestPath: %s, parameters: %s",
                name,
                description,
                requestMethod,
                requestPath,
                parameters);
    }
}
