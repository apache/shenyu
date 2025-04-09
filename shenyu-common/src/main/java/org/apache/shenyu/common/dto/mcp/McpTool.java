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

import java.util.List;
import java.util.Objects;

/**
 * McpTool represents a tool in the context of the Mcp system.
 * It contains information about the tool's name, description, endpoint,
 * HTTP method, return content type, and a list of parameters.
 */
public class McpTool {
    /**
     * The name of the tool.
     */
    private String name;
    
    /**
     * The description of the tool.
     */
    private String description;
    
    /**
     * The endpoint URL for the tool.
     */
    private String endpoint;
    
    /**
     * The HTTP method used by the tool (e.g., GET, POST).
     */
    private String httpMethod;
    
    /**
     * The content type of the response returned by the tool.
     */
    private String returnContentType;
    
    /**
     * The list of parameters required by the tool.
     */
    private List<McpToolParameter> parameters;
    
    /**
     * Creates a new builder instance.
     *
     * @return a new builder instance
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Gets the name of the tool.
     *
     * @return the tool name
     */
    public String getName() {
        return name;
    }

    /**
     * Sets the name of the tool.
     *
     * @param name the tool name to set
     */
    public void setName(final String name) {
        this.name = name;
    }

    /**
     * Gets the description of the tool.
     *
     * @return the tool description
     */
    public String getDescription() {
        return description;
    }

    /**
     * Sets the description of the tool.
     *
     * @param description the tool description to set
     */
    public void setDescription(final String description) {
        this.description = description;
    }

    /**
     * Gets the endpoint URL of the tool.
     *
     * @return the tool endpoint
     */
    public String getEndpoint() {
        return endpoint;
    }

    /**
     * Sets the endpoint URL of the tool.
     *
     * @param endpoint the tool endpoint to set
     */
    public void setEndpoint(final String endpoint) {
        this.endpoint = endpoint;
    }

    /**
     * Gets the HTTP method used by the tool.
     *
     * @return the HTTP method
     */
    public String getHttpMethod() {
        return httpMethod;
    }

    /**
     * Sets the HTTP method used by the tool.
     *
     * @param httpMethod the HTTP method to set
     */
    public void setHttpMethod(final String httpMethod) {
        this.httpMethod = httpMethod;
    }

    /**
     * Gets the return content type of the tool.
     *
     * @return the return content type
     */
    public String getReturnContentType() {
        return returnContentType;
    }

    /**
     * Sets the return content type of the tool.
     *
     * @param returnContentType the return content type to set
     */
    public void setReturnContentType(final String returnContentType) {
        this.returnContentType = returnContentType;
    }

    /**
     * Gets the list of parameters for the tool.
     *
     * @return the list of tool parameters
     */
    public List<McpToolParameter> getParameters() {
        return parameters;
    }

    /**
     * Sets the list of parameters for the tool.
     *
     * @param parameters the list of tool parameters to set
     */
    public void setParameters(final List<McpToolParameter> parameters) {
        this.parameters = parameters;
    }

    @Override
    public String toString() {
        return "McpTool{"
                + "name='" + name + '\''
                + ", description='" + description + '\''
                + ", endpoint='" + endpoint + '\''
                + ", httpMethod='" + httpMethod + '\''
                + ", returnContentType='" + returnContentType + '\''
                + ", parameters=" + parameters
                + '}';
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        McpTool mcpTool = (McpTool) o;
        return Objects.equals(name, mcpTool.name)
                && Objects.equals(description, mcpTool.description)
                && Objects.equals(endpoint, mcpTool.endpoint)
                && Objects.equals(httpMethod, mcpTool.httpMethod)
                && Objects.equals(returnContentType, mcpTool.returnContentType)
                && Objects.equals(parameters, mcpTool.parameters);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name, description, endpoint, httpMethod, returnContentType, parameters);
    }
    
    /**
     * Builder class for McpTool.
     */
    public static class Builder {

        /**
         * The name of the tool.
         */
        private String name;
        
        /**
         * The description of the tool.
         */
        private String description;
        
        /**
         * The endpoint of the tool.
         */
        private String endpoint;
        
        /**
         * The HTTP method of the tool.
         */
        private String httpMethod;
        
        /**
         * The return content type of the tool.
         */
        private String returnContentType;
        
        /**
         * The list of parameters for the tool.
         */
        private List<McpToolParameter> parameters;

        /**
         * Sets the name of the tool.
         *
         * @param name the tool name to set
         * @return the builder instance
         */
        public Builder name(final String name) {
            this.name = name;
            return this;
        }

        /**
         * Sets the description of the tool.
         *
         * @param description the tool description to set
         * @return the builder instance
         */
        public Builder description(final String description) {
            this.description = description;
            return this;
        }

        /**
         * Sets the endpoint of the tool.
         *
         * @param endpoint the tool endpoint to set
         * @return the builder instance
         */
        public Builder endpoint(final String endpoint) {
            this.endpoint = endpoint;
            return this;
        }

        /**
         * Sets the HTTP method of the tool.
         *
         * @param httpMethod the HTTP method to set
         * @return the builder instance
         */
        public Builder httpMethod(final String httpMethod) {
            this.httpMethod = httpMethod;
            return this;
        }

        /**
         * Sets the return content type of the tool.
         *
         * @param returnContentType the return content type to set
         * @return the builder instance
         */
        public Builder returnContentType(final String returnContentType) {
            this.returnContentType = returnContentType;
            return this;
        }

        /**
         * Sets the list of parameters for the tool.
         *
         * @param parameters the list of tool parameters to set
         * @return the builder instance
         */
        public Builder parameters(final List<McpToolParameter> parameters) {
            this.parameters = parameters;
            return this;
        }

        /**
         * Builds a new McpTool instance.
         *
         * @return a new McpTool instance
         */
        public McpTool build() {
            McpTool tool = new McpTool();
            tool.name = this.name;
            tool.description = this.description;
            tool.endpoint = this.endpoint;
            tool.httpMethod = this.httpMethod;
            tool.returnContentType = this.returnContentType;
            tool.parameters = this.parameters;
            return tool;
        }
    }


}
