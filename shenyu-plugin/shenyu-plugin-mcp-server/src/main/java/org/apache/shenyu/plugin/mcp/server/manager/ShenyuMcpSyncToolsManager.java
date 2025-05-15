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

package org.apache.shenyu.plugin.mcp.server.manager;

import io.modelcontextprotocol.server.McpServerFeatures.SyncToolSpecification;
import io.modelcontextprotocol.server.McpSyncServer;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.mcp.server.callback.ShenyuToolCallback;
import org.apache.shenyu.plugin.mcp.server.definition.ShenyuToolDefinition;
import org.apache.shenyu.web.handler.ShenyuWebHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.ai.mcp.McpToolUtils;
import org.springframework.ai.tool.definition.ToolDefinition;

public class ShenyuMcpSyncToolsManager implements ShenyuMcpToolsManager {
    
    private static final Logger LOGGER = LoggerFactory.getLogger(ShenyuMcpSyncToolsManager.class);
    
    private final McpSyncServer mcpSyncServer;
    
    public ShenyuMcpSyncToolsManager(final McpSyncServer mcpSyncServer) {
        this.mcpSyncServer = mcpSyncServer;
    }
    
    @Override
    public void addTool(final String name,
                        final String description,
                        final String requestTemplate,
                        final String inputSchema) {
        // remove first for overwrite
        try {
            removeTool(name);
        } catch (Exception ignored) {
            // ignore
        }
        
        ToolDefinition shenyuToolDefinition =
                ShenyuToolDefinition.builder()
                        .name(name)
                        .description(description)
                        .requestTemplate(requestTemplate)
                        .inputSchema(inputSchema)
                        .build();
        LOGGER.debug("Adding tool, name: {}, description: {}, requestTemplate: {}, inputSchema: {}",
                name, description, requestTemplate, inputSchema);
        ShenyuToolCallback shenyuToolCallback = new ShenyuToolCallback(SpringBeanUtils.getInstance().getBean(ShenyuWebHandler.class), shenyuToolDefinition);
        for (SyncToolSpecification syncToolSpecification : McpToolUtils.toSyncToolSpecifications(shenyuToolCallback)) {
            mcpSyncServer.addTool(syncToolSpecification);
        }
    }
    
    @Override
    public void removeTool(final String name) {
        LOGGER.debug("Removing tool, name: {}", name);
        mcpSyncServer.removeTool(name);
    }
}
