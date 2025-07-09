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
import com.google.common.collect.Lists;
import io.modelcontextprotocol.server.McpAsyncServer;
import io.modelcontextprotocol.server.McpServer;
import io.modelcontextprotocol.server.McpServerFeatures.AsyncToolSpecification;
import io.modelcontextprotocol.spec.McpSchema;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.plugin.mcp.server.callback.ShenyuToolCallback;
import org.apache.shenyu.plugin.mcp.server.definition.ShenyuToolDefinition;
import org.apache.shenyu.plugin.mcp.server.transport.ShenyuSseServerTransportProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.ai.mcp.McpToolUtils;
import org.springframework.ai.tool.definition.ToolDefinition;
import org.springframework.stereotype.Component;
import org.springframework.util.AntPathMatcher;
import org.springframework.web.reactive.function.server.HandlerFunction;

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
     * AntPathMatcher for pattern matching.
     */
    private final AntPathMatcher pathMatcher = new AntPathMatcher();

    /**
     * Map to store URI to McpServer mapping.
     */
    private final Map<String, McpAsyncServer> mcpServerMap = new ConcurrentHashMap<>();

    /**
     * Map to store URI to McpServer mapping.
     */
    private final Map<String, ShenyuSseServerTransportProvider> mcpServerTransportMap = new ConcurrentHashMap<>();

    private final Map<String, HandlerFunction<?>> routeMap = new ConcurrentHashMap<>();

    /**
     * Get or create a McpServer instance for the given URI.
     * Uses pattern matching to find the best matching server.
     *
     * @param uri The URI to create or get a McpServer for
     * @return The McpServer for the URI
     */
    public ShenyuSseServerTransportProvider getOrCreateMcpServerTransport(final String uri, final String messageEndpoint) {
        // First try exact match
        ShenyuSseServerTransportProvider transport = mcpServerTransportMap.get(uri);
        if (Objects.nonNull(transport)) {
            return transport;
        }

        // Then try to find existing transport that matches the pattern
        String basePath = extractBasePath(uri, messageEndpoint);
        transport = mcpServerTransportMap.get(basePath);
        if (Objects.nonNull(transport)) {
            LOG.debug("Found existing transport for base path '{}' for URI '{}'", basePath, uri);
            return transport;
        }

        // Create new transport for the base path
        return mcpServerTransportMap.computeIfAbsent(basePath, this::createMcpServerTransport);
    }

    /**
     * Extract the base path from a URI by removing the /message suffix and any
     * sub-paths.
     *
     * @param uri The URI to extract base path from
     * @return The base path
     */
    private String extractBasePath(final String uri, final String messageEndpoint) {
        String basePath = uri;

        // Remove /message suffix if present
        if (basePath.endsWith(messageEndpoint)) {
            basePath = basePath.substring(0, basePath.length() - "/message".length());
        }

        // For sub-paths, extract the main MCP server path
        // This assumes MCP server paths don't contain multiple levels
        // You may need to adjust this logic based on your specific URL structure
        String[] pathSegments = basePath.split("/");
        if (pathSegments.length > 2) {
            // Keep only the first two segments (empty + server-name)
            basePath = "/" + pathSegments[1];
        }

        return basePath;
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
     * Uses AntPathMatcher to support pattern matching.
     *
     * @param uri The URI to check
     * @return true if the URI is routable, false otherwise
     */
    public boolean canRoute(final String uri) {
        // First try exact match for performance
        if (routeMap.containsKey(uri)) {
            return true;
        }

        // Then try pattern matching for each registered pattern
        for (String pattern : routeMap.keySet()) {
            if (pathMatcher.match(pattern, uri)) {
                LOG.debug("URI '{}' matches pattern '{}'", uri, pattern);
                return true;
            }
        }

        return false;
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
        ShenyuSseServerTransportProvider transportProvider = ShenyuSseServerTransportProvider.builder()
                .objectMapper(new ObjectMapper()).sseEndpoint(path).messageEndpoint(messageEndpoint).build();

        LOG.debug("Registered RouterFunction for URI: {}", path);

        // Configure server capabilities with resource support
        var capabilities = McpSchema.ServerCapabilities.builder()
                // Tool support with list changes notifications
                .tools(true)
                // Logging support
                .logging().build();

        // Create the server with both tool and resource capabilities
        // Note: McpServer.async() automatically calls
        // transportProvider.setSessionFactory()
        McpAsyncServer server = McpServer
                .async(transportProvider)
                .serverInfo("MCP Shenyu Server", "1.0.0")
                .capabilities(capabilities)
                .tools(Lists.newArrayList())
                .build();

        // Verify that the session factory was properly set
        LOG.debug("MCP server created successfully for URI: {}, session factory should be available", path);

        mcpServerMap.put(path, server);

        // Register both exact paths and wildcard patterns for flexible matching
        routeMap.put(path, transportProvider::handleSseConnection);
        routeMap.put(messageEndpoint, transportProvider::handleMessage);

        // Add wildcard patterns to support sub-paths
        routeMap.put(path + "/**", transportProvider::handleSseConnection);
        routeMap.put(messageEndpoint + "/**", transportProvider::handleMessage);

        LOG.info("Created new McpServer for URI: {} with patterns", path);
        return transportProvider;
    }

    public void addTool(final String serverPath, final String name, final String description,
            final String requestTemplate, final String inputSchema) {
        // remove first for overwrite
        try {
            removeTool(serverPath, name);
        } catch (Exception ignored) {
            // ignore
        }

        ToolDefinition shenyuToolDefinition = ShenyuToolDefinition.builder().name(name).description(description)
                .requestConfig(requestTemplate).inputSchema(inputSchema).build();
        LOG.debug("Adding tool, name: {}, description: {}, requestTemplate: {}, inputSchema: {}", name, description,
                requestTemplate, inputSchema);
        ShenyuToolCallback shenyuToolCallback = new ShenyuToolCallback(shenyuToolDefinition);
        for (AsyncToolSpecification asyncToolSpecification : McpToolUtils
                .toAsyncToolSpecifications(shenyuToolCallback)) {
            mcpServerMap.get(serverPath).addTool(asyncToolSpecification).block();
        }
    }

    public void removeTool(final String serverPath, final String name) {
        LOG.debug("Removing tool, name: {}", name);
        mcpServerMap.get(serverPath).removeTool(name).block();
    }
}