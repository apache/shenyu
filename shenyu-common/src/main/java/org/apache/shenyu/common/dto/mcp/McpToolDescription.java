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

package org.apache.shenyu.common.dto.mcp;

import com.google.common.collect.Lists;

import java.util.List;

/**
 * McpToolDescription represents a tool description in the context of a microservice.
 * It contains information about the tool's ID, name, version, description, parameters,
 * and return type.
 */
public class McpToolDescription {
    
    /**
     * ID of the tool description
     */
    private String id;
    
    /**
     * Name of the tool description
     */
    private String name;
    
    /**
     * Version of the tool description
     */
    private String version;
    
    /**
     * Description of the tool description
     */
    private String description;
    
    /**
     * Parameters of the tool description
     */
    private List<McpParameter> parameters = Lists.newArrayList();
    
    /**
     * Return type of the tool description
     */
    private String returnType;
    
    /**
     * Getter for id.
     *
     * @return id
     */
    public String getId() {
        return id;
    }
    
    /**
     * Setter for id.
     *
     * @param id id
     */
    public void setId(String id) {
        this.id = id;
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
    public void setName(String name) {
        this.name = name;
    }
    
    /**
     * Getter for version.
     *
     * @return version
     */
    public String getVersion() {
        return version;
    }
    
    /**
     * Setter for version.
     *
     * @param version version
     */
    public void setVersion(String version) {
        this.version = version;
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
    public void setDescription(String description) {
        this.description = description;
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
    public void setParameters(List<McpParameter> parameters) {
        this.parameters = parameters;
    }
    
    /**
     * Getter for returnType.
     *
     * @return returnType
     */
    public String getReturnType() {
        return returnType;
    }
    
    /**
     * Setter for returnType.
     *
     * @param returnType returnType
     */
    public void setReturnType(String returnType) {
        this.returnType = returnType;
    }
}
