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

package org.apache.shenyu.plugin.mcp.server.definition;

import org.apache.commons.lang3.StringUtils;
import org.springframework.ai.tool.definition.ToolDefinition;
import org.springframework.ai.tool.util.ToolUtils;
import org.springframework.util.Assert;

public class ShenyuToolDefinition implements ToolDefinition {
    
    private final String name;
    
    private final String description;
    
    private final String requestTemplate;
    
    private final String inputSchema;
    
    public ShenyuToolDefinition(final String name, final String description, final String requestTemplate, final String inputSchema) {
        Assert.hasText(name, "name cannot be null or empty");
        Assert.hasText(description, "description cannot be null or empty");
        Assert.hasText(requestTemplate, "requestTemplate cannot be null or empty");
        Assert.hasText(inputSchema, "inputSchema cannot be null or empty");
        this.name = name;
        this.description = description;
        this.inputSchema = inputSchema;
        this.requestTemplate = requestTemplate;
    }
    
    public static ShenyuToolDefinition.Builder builder() {
        return new ShenyuToolDefinition.Builder();
    }
    
    public String name() {
        return this.name;
    }
    
    public String description() {
        return this.description;
    }
    
    public String inputSchema() {
        return this.inputSchema;
    }
    
    public String requestTemplate() {
        return this.requestTemplate;
    }
    
    public static final class Builder {
        
        private String name;
        
        private String description;
        
        private String requestTemplate;
        
        private String inputSchema;
        
        private Builder() {
        }
        
        public ShenyuToolDefinition.Builder name(final String name) {
            this.name = name;
            return this;
        }
        
        public ShenyuToolDefinition.Builder description(final String description) {
            this.description = description;
            return this;
        }
        
        public ShenyuToolDefinition.Builder requestTemplate(final String requestTemplate) {
            this.requestTemplate = requestTemplate;
            return this;
        }
        
        public ShenyuToolDefinition.Builder inputSchema(final String inputSchema) {
            this.inputSchema = inputSchema;
            return this;
        }
        
        public ToolDefinition build() {
            if (!StringUtils.isNoneBlank(this.description)) {
                this.description = ToolUtils.getToolDescriptionFromName(this.name);
            }
            
            return new ShenyuToolDefinition(this.name,
                    this.description,
                    this.requestTemplate,
                    this.inputSchema);
        }
    }
}
