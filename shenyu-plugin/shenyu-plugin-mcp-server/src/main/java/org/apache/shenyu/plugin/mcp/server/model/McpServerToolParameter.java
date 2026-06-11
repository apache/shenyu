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

import java.util.List;

/**
 * McpParameter represents a parameter in the context of a tool description.
 * It contains information about the parameter's name, type, description,
 * whether it is required, and its default value.
 */
public class McpServerToolParameter {
    
    /**
     * Parameter name.
     */
    private String name;

    /**
     * Parameter type.
     */
    private String type;

    /**
     * Parameter description.
     */
    private String description;

    /**
     * Whether the parameter is required.
     */
    private boolean required;

    /**
     * Default value of the parameter.
     */
    private String defaultValue;

    /**
     * the child parameters.
     */
    private List<McpServerToolParameter> parameters;

    /**
     * Constructor for McpParameter.
     */
    public McpServerToolParameter() {
    }

    /**
     * Constructor for McpParameter.
     *
     * @param name        name
     * @param type        type
     * @param description description
     * @param required    required
     * @param defaultValue defaultValue
     * @param parameters parameters
     */
    public McpServerToolParameter(final String name, final String type, final String description, final boolean required, final String defaultValue, final List<McpServerToolParameter> parameters) {
        this.name = name;
        this.type = type;
        this.description = description;
        this.required = required;
        this.defaultValue = defaultValue;
        this.parameters = parameters;
    }

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
     * Getter for type.
     *
     * @return type
     */ 
    public String getType() {
        return type;
    }

    /**
     * Setter for type.
     *
     * @param type type
     */ 
    public void setType(final String type) {
        this.type = type;
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
     * Getter for required.
     *
     * @return required
     */
    public boolean isRequired() {
        return required;
    }

    /**
     * Setter for required.
     *
     * @param required required
     */
    public void setRequired(final boolean required) {
        this.required = required;
    }

    /**
     * Getter for defaultValue.
     *
     * @return defaultValue
     */
    public String getDefaultValue() {
        return defaultValue;
    }

    /**
     * Setter for defaultValue.
     *
     * @param defaultValue defaultValue
     */
    public void setDefaultValue(final String defaultValue) {
        this.defaultValue = defaultValue;
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
}
