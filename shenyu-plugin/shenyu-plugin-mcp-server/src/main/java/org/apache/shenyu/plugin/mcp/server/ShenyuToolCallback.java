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

package org.apache.shenyu.plugin.mcp.server;

import org.springframework.ai.chat.model.ToolContext;
import org.springframework.ai.tool.ToolCallback;
import org.springframework.ai.tool.definition.ToolDefinition;

public class ShenyuToolCallback implements ToolCallback {
    
    private final ToolDefinition toolDefinition;
    
    public ShenyuToolCallback(final ToolDefinition toolDefinition) {
        this.toolDefinition = toolDefinition;
    }
    
    @Override
    public ToolDefinition getToolDefinition() {
        return this.toolDefinition;
    }
    
    @Override
    public String call(final String toolInput) {
        return toolInput;
    }
    
    @Override
    public String call(final String toolInput, final ToolContext tooContext) {
        return this.call(toolInput);
    }
}
