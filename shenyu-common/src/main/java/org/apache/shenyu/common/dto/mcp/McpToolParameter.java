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

import java.util.Objects;

/**
 * This class represents a parameter for a tool in the Mcp system.
 * It contains information about the parameter's name, type, whether it is required,
 * and a description of the parameter.
 */
public class McpToolParameter {
    /**
     * The name of the parameter.
     */
    private String name;
    
    /**
     * The type of the parameter.
     */
    private String type;
    
    /**
     * Whether this parameter is required.
     */
    private boolean required;
    
    /**
     * The description of the parameter.
     */
    private String description;

    /**
     * Creates a new builder instance.
     *
     * @return a new builder instance
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Gets the name of the parameter.
     *
     * @return the parameter name
     */
    public String getName() {
        return name;
    }

    /**
     * Sets the name of the parameter.
     *
     * @param name the parameter name to set
     */
    public void setName(final String name) {
        this.name = name;
    }

    /**
     * Gets the type of the parameter.
     *
     * @return the parameter type
     */
    public String getType() {
        return type;
    }

    /**
     * Sets the type of the parameter.
     *
     * @param type the parameter type to set
     */
    public void setType(final String type) {
        this.type = type;
    }

    /**
     * Checks if the parameter is required.
     *
     * @return true if the parameter is required, false otherwise
     */
    public boolean isRequired() {
        return required;
    }

    /**
     * Sets whether the parameter is required.
     *
     * @param required the required status to set
     */
    public void setRequired(final boolean required) {
        this.required = required;
    }

    /**
     * Gets the description of the parameter.
     *
     * @return the parameter description
     */
    public String getDescription() {
        return description;
    }

    /**
     * Sets the description of the parameter.
     *
     * @param description the parameter description to set
     */
    public void setDescription(final String description) {
        this.description = description;
    }

    @Override
    public String toString() {
        return "McpToolParameter{"
                + "name='" + name + '\''
                + ", type='" + type + '\''
                + ", required=" + required
                + ", description='" + description + '\''
                + '}';
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        McpToolParameter that = (McpToolParameter) o;
        return required == that.required
                && Objects.equals(name, that.name)
                && Objects.equals(type, that.type)
                && Objects.equals(description, that.description);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name, type, required, description);
    }

    /**
     * Builder class for McpToolParameter.
     */
    public static class Builder {
        /**
         * The name of the parameter.
         */
        private String name;
        
        /**
         * The type of the parameter.
         */
        private String type;
        
        /**
         * Whether this parameter is required.
         */
        private boolean required;
        
        /**
         * The description of the parameter.
         */
        private String description;

        /**
         * Sets the name of the parameter.
         *
         * @param name the parameter name to set
         * @return the builder instance
         */
        public Builder name(final String name) {
            this.name = name;
            return this;
        }

        /**
         * Sets the type of the parameter.
         *
         * @param type the parameter type to set
         * @return the builder instance
         */
        public Builder type(final String type) {
            this.type = type;
            return this;
        }

        /**
         * Sets whether the parameter is required.
         *
         * @param required the required status to set
         * @return the builder instance
         */
        public Builder required(final boolean required) {
            this.required = required;
            return this;
        }

        /**
         * Sets the description of the parameter.
         *
         * @param description the parameter description to set
         * @return the builder instance
         */
        public Builder description(final String description) {
            this.description = description;
            return this;
        }

        /**
         * Builds a new McpToolParameter instance.
         *
         * @return a new McpToolParameter instance
         */
        public McpToolParameter build() {
            McpToolParameter parameter = new McpToolParameter();
            parameter.name = this.name;
            parameter.type = this.type;
            parameter.required = this.required;
            parameter.description = this.description;
            return parameter;
        }
    }
}
