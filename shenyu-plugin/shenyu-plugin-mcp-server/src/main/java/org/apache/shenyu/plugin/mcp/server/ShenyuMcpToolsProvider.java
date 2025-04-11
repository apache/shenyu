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

import io.modelcontextprotocol.server.McpServerFeatures.SyncToolSpecification;
import io.modelcontextprotocol.server.McpSyncServer;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.springframework.ai.mcp.McpToolUtils;
import org.springframework.ai.tool.ToolCallback;
import org.springframework.ai.tool.ToolCallbacks;
import org.springframework.ai.tool.definition.DefaultToolDefinition;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;

@ConditionalOnBean(McpSyncServer.class)
public final class ShenyuMcpToolsProvider {
    
    public static void addSyncTools(final String name, final String description, final String inputSchema) {
        DefaultToolDefinition defaultToolDefinition = new DefaultToolDefinition(name, description, inputSchema);
        ShenyuToolCallback shenyuToolCallback = new ShenyuToolCallback(defaultToolDefinition);
        for (SyncToolSpecification syncToolSpecification : McpToolUtils.toSyncToolSpecifications(shenyuToolCallback)) {
            SpringBeanUtils
                    .getInstance()
                    .getBean(McpSyncServer.class)
                    .addTool(syncToolSpecification);
        }
    }
    
    public static void removeTools(final String name) {
        SpringBeanUtils
                .getInstance()
                .getBean(McpSyncServer.class)
                .removeTool(name);
    }
    
    public static void addSyncTools() {
        ShenyuPluginService pluginService = new ShenyuPluginService();
        for (SyncToolSpecification syncToolSpecification : McpToolUtils.toSyncToolSpecifications(ToolCallbacks.from(pluginService))) {
            SpringBeanUtils
                    .getInstance()
                    .getBean(McpSyncServer.class)
                    .addTool(syncToolSpecification);
        }
    }
    
    public void removeTools() {
        ShenyuPluginService pluginService = new ShenyuPluginService();
        for (ToolCallback toolCallback : ToolCallbacks.from(pluginService)) {
            SpringBeanUtils
                    .getInstance()
                    .getBean(McpSyncServer.class)
                    .removeTool(toolCallback.getToolDefinition().name());
        }
    }
    
}
