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

import com.fasterxml.jackson.databind.ObjectMapper;
import io.modelcontextprotocol.server.McpAsyncServer;
import io.modelcontextprotocol.server.McpServer;
import io.modelcontextprotocol.server.McpServerFeatures.AsyncToolSpecification;
import io.modelcontextprotocol.spec.McpSchema;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.mcp.server.callback.ShenyuToolCallback;
import org.apache.shenyu.plugin.mcp.server.definition.ShenyuToolDefinition;
import org.apache.shenyu.plugin.mcp.server.tools.ShenyuDefaultTools;
import org.apache.shenyu.plugin.mcp.server.transport.ShenyuSseServerTransportProvider;
import org.apache.shenyu.web.handler.ShenyuWebHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.ai.mcp.McpToolUtils;
import org.springframework.ai.support.ToolCallbacks;
import org.springframework.ai.tool.definition.ToolDefinition;
import org.springframework.stereotype.Component;
import org.springframework.web.reactive.function.server.HandlerFunction;
import org.springframework.web.reactive.function.server.ServerRequest;
import org.springframework.web.reactive.function.server.ServerResponse;
import reactor.core.publisher.Mono;

import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Manager for MCP servers. Maps URIs to McpServer instances.
 */
@Component
public class ShenyuMcpServerManager {
    
    private static final Logger LOG = LoggerFactory.getLogger(ShenyuMcpServerManager.class);
    
    private static final String SLASH = "/";
    
    /**
     * Map to store URI to McpServer mapping.
     */
    private final Map<String, McpAsyncServer> mcpServerMap = new ConcurrentHashMap<>();
    
    /**
     * Map to store URI to McpServer mapping.
     */
    private final Map<String, ShenyuSseServerTransportProvider> mcpServerTransportMap = new ConcurrentHashMap<>();
    
    private final Map<String, HandlerFunction<ServerResponse>> routeMap = new ConcurrentHashMap<>();
    
    
    /**
     * Get or create a McpServer instance for the given URI.
     *
     * @param uri The URI to create or get a McpServer for
     * @return The McpServer for the URI
     */
    public ShenyuSseServerTransportProvider getOrCreateMcpServerTransport(final String uri) {
        return mcpServerTransportMap.computeIfAbsent(uri, this::createMcpServerTransport);
    }
    
    
    /**
     * Check if a McpServer exists for the given URI.
     *
     * @param uri The URI to check
     * @return true if a McpServer exists, false otherwise
     */
    public boolean hasMcpServer(final String uri) {
        return mcpServerMap.containsKey(uri);
    }
    
    /**
     * Check if a McpServer can route requests for the given URI.
     *
     * @param uri The URI to check
     * @return true if the URI is routable, false otherwise
     */
    public boolean canRoute(final String uri) {
        return routeMap.containsKey(uri);
    }
    
    /**
     * Remove a McpServer for the given URI.
     *
     * @param uri The URI to remove the McpServer for
     */
    public void removeMcpServer(final String uri) {
        LOG.info("Removed McpServer for URI: {}", uri);
        ShenyuSseServerTransportProvider transportProvider = mcpServerTransportMap.remove(uri);
        if (Objects.nonNull(transportProvider)) {
            transportProvider
                    .closeGracefully()
                    .doOnSuccess(aVoid -> LOG.info("Successfully closed McpServer for URI: {}", uri))
                    .doOnError(e -> LOG.error("Error closing McpServer for URI: {}", uri, e))
                    .subscribe();
        }
        mcpServerMap.remove(uri);
    }
    
    /**
     * Create a new McpServer for the given URI.
     *
     * @param uri The URI to create a McpServer for
     * @return The McpAsyncServer for the new McpServer
     */
    private ShenyuSseServerTransportProvider createMcpServerTransport(final String uri) {
        String path = StringUtils.removeEnd(uri, SLASH);
        LOG.info("Creating new McpServer for URI: {}", path);
        String messageEndpoint = path + "/message";
        ShenyuSseServerTransportProvider transportProvider = ShenyuSseServerTransportProvider.builder().objectMapper(new ObjectMapper()).sseEndpoint(path).messageEndpoint(messageEndpoint).build();
        
        LOG.debug("Registered RouterFunction for URI: {}", path);
        
        // Configure server capabilities with resource support
        var capabilities = McpSchema.ServerCapabilities.builder()
                // Tool support with list changes notifications
                .tools(true)
                // Logging support
                .logging().build();
        
        // Create the server with both tool and resource capabilities
        McpAsyncServer server = McpServer
                .async(transportProvider)
                .serverInfo("MCP Shenyu Server", "1.0.0")
                .capabilities(capabilities)
                .tools(McpToolUtils.toAsyncToolSpecifications(ToolCallbacks.from(new ShenyuDefaultTools())))
                .build();
        
        mcpServerMap.put(path, server);
        
        routeMap.put(path, transportProvider::handleSseConnection);
        routeMap.put(messageEndpoint, transportProvider::handleMessage);
        
        LOG.info("Created new McpServer for URI: {}", path);
        return transportProvider;
    }
    
    public Mono<ServerResponse> dispatch(final ServerRequest serverRequest) {
        HandlerFunction<ServerResponse> handler = routeMap.get(serverRequest.path());
        if (Objects.nonNull(handler)) {
            return handler.handle(serverRequest);
        } else {
            return ServerResponse.notFound().build();
        }
    }
    
    public void addTool(final String serverPath, final String name, final String description, final String requestTemplate, final String inputSchema) {
        // remove first for overwrite
        try {
            removeTool(serverPath, name);
        } catch (Exception ignored) {
            // ignore
        }
        
        ToolDefinition shenyuToolDefinition = ShenyuToolDefinition.builder().name(name).description(description).requestConfig(requestTemplate).inputSchema(inputSchema).build();
        LOG.debug("Adding tool, name: {}, description: {}, requestTemplate: {}, inputSchema: {}", name, description, requestTemplate, inputSchema);
        ShenyuToolCallback shenyuToolCallback = new ShenyuToolCallback(SpringBeanUtils.getInstance().getBean(ShenyuWebHandler.class), shenyuToolDefinition);
        for (AsyncToolSpecification asyncToolSpecification : McpToolUtils.toAsyncToolSpecifications(shenyuToolCallback)) {
            mcpServerMap.get(serverPath).addTool(asyncToolSpecification).block();
        }
    }
    
    public void removeTool(final String serverPath, final String name) {
        LOG.debug("Removing tool, name: {}", name);
        mcpServerMap.get(serverPath).removeTool(name).block();
    }
}