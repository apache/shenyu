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
import io.modelcontextprotocol.spec.McpServerSession;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.plugin.mcp.server.callback.ShenyuToolCallback;
import org.apache.shenyu.plugin.mcp.server.definition.ShenyuToolDefinition;
import org.apache.shenyu.plugin.mcp.server.transport.ShenyuSseServerTransportProvider;
import org.apache.shenyu.plugin.mcp.server.transport.ShenyuStreamableHttpServerTransportProvider;
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
import java.util.Set;
import java.util.HashSet;
import java.util.Collections;

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
     * Map to store multiple McpServer instances for the same path (for protocol coexistence).
     * Key: serverPath, Value: Map of protocol -> McpAsyncServer
     */
    private final Map<String, Map<String, McpAsyncServer>> protocolServerMap = new ConcurrentHashMap<>();

    /**
     * Map to store URI to SSE transport provider mapping.
     */
    private final Map<String, ShenyuSseServerTransportProvider> mcpServerTransportMap = new ConcurrentHashMap<>();

    /**
     * Map to store URI to Streamable HTTP transport provider mapping.
     */
    private final Map<String, ShenyuStreamableHttpServerTransportProvider> streamableHttpTransportMap = new ConcurrentHashMap<>();

    private final Map<String, HandlerFunction<?>> routeMap = new ConcurrentHashMap<>();

    /**
     * Map to track which protocols (SSE/Streamable HTTP) are enabled for each server path.
     */
    private final Map<String, Set<String>> protocolMap = new ConcurrentHashMap<>();

    /**
     * Get or create a McpServer instance for the given URI.
     * Uses pattern matching to find the best matching server.
     *
     * @param uri The URI to create or get a McpServer for
     * @return The McpServer for the URI
     */
    public ShenyuSseServerTransportProvider getOrCreateMcpServerTransport(final String uri) {
        // First try exact match
        ShenyuSseServerTransportProvider transport = mcpServerTransportMap.get(uri);
        if (Objects.nonNull(transport)) {
            return transport;
        }

        // Then try to find existing transport that matches the pattern
        String basePath = extractBasePath(uri);
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
    private String extractBasePath(final String uri) {
        String basePath = uri;

        // Remove /message suffix if present
        if (basePath.endsWith("/message")) {
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
     * Get or create a Streamable HTTP transport provider for the given URI.
     *
     * @param uri The URI to create or get a transport provider for
     * @return The Streamable HTTP transport provider for the URI
     */
    public ShenyuStreamableHttpServerTransportProvider getOrCreateStreamableHttpTransport(final String uri) {
        // First try exact match
        ShenyuStreamableHttpServerTransportProvider transport = streamableHttpTransportMap.get(uri);
        if (Objects.nonNull(transport)) {
            return transport;
        }

        // Then try to find existing transport that matches the pattern
        String basePath = extractBasePath(uri);

        // Normalize the basePath by removing /streamablehttp suffix for consistency
        String normalizedBasePath = normalizeServerPath(basePath);

        transport = streamableHttpTransportMap.get(normalizedBasePath);
        if (Objects.nonNull(transport)) {
            LOG.debug("Found existing Streamable HTTP transport for normalized base path '{}' for URI '{}'", normalizedBasePath, uri);
            return transport;
        }

        // Create new transport for the base path
        return streamableHttpTransportMap.computeIfAbsent(basePath, this::createStreamableHttpTransport);
    }

    /**
     * Remove a McpServer for the given URI.
     *
     * @param uri The URI to remove the McpServer for
     */
    public void removeMcpServer(final String uri) {
        LOG.info("Removed McpServer for URI: {}", uri);

        // Remove SSE transport
        ShenyuSseServerTransportProvider sseTransportProvider = mcpServerTransportMap.remove(uri);
        if (Objects.nonNull(sseTransportProvider)) {
            sseTransportProvider
                    .closeGracefully()
                    .doOnSuccess(aVoid -> LOG.info("Successfully closed SSE transport for URI: {}", uri))
                    .doOnError(e -> LOG.error("Error closing SSE transport for URI: {}", uri, e))
                    .subscribe();
        }

        // Remove Streamable HTTP transport
        ShenyuStreamableHttpServerTransportProvider streamableHttpTransportProvider = streamableHttpTransportMap.remove(uri);
        if (Objects.nonNull(streamableHttpTransportProvider)) {
            streamableHttpTransportProvider
                    .closeGracefully()
                    .doOnSuccess(aVoid -> LOG.info("Successfully closed Streamable HTTP transport for URI: {}", uri))
                    .doOnError(e -> LOG.error("Error closing Streamable HTTP transport for URI: {}", uri, e))
                    .subscribe();
        }

        // Remove the server and protocol tracking
        mcpServerMap.remove(uri);
        removeProtocolTracking(uri);
    }

    /**
     * Create a new McpServer for the given URI.
     *
     * @param uri The URI to create a McpServer for
     * @return The McpAsyncServer for the new McpServer
     */
    private ShenyuSseServerTransportProvider createMcpServerTransport(final String uri) {
        String path = StringUtils.removeEnd(uri, SLASH);
        LOG.info("Creating new SSE transport for URI: {}", path);
        String messageEndpoint = path + "/message";
        ShenyuSseServerTransportProvider transportProvider = ShenyuSseServerTransportProvider.builder()
                .objectMapper(new ObjectMapper()).sseEndpoint(path).messageEndpoint(messageEndpoint).build();

        LOG.debug("Registered RouterFunction for URI: {}", path);

        // Always create a new server for each transport to ensure proper sessionFactory initialization
        LOG.info("Creating McpAsyncServer for SSE transport at path: {}", path);
        McpAsyncServer server = createSharedMcpServer(transportProvider, path);

        // Store the server with protocol-specific key to avoid conflicts
        String serverKey = path + "_sse";
        mcpServerMap.put(serverKey, server);

        // Store in protocol map for tracking
        protocolServerMap.computeIfAbsent(path, k -> new ConcurrentHashMap<>()).put("SSE", server);

        // Add default test tool for each server instance
        addDefaultTestTool(serverKey, server);

        // Register both exact paths and wildcard patterns for flexible matching
        routeMap.put(path, transportProvider::handleSseConnection);
        routeMap.put(messageEndpoint, transportProvider::handleMessage);

        // Add wildcard patterns to support sub-paths
        routeMap.put(path + "/**", transportProvider::handleSseConnection);
        routeMap.put(messageEndpoint + "/**", transportProvider::handleMessage);

        // Track protocol support
        protocolMap.computeIfAbsent(path, k -> Collections.synchronizedSet(new HashSet<>())).add("SSE");

        LOG.info("Created SSE transport for URI: {} with patterns", path);
        return transportProvider;
    }

    /**
     * Create a new Streamable HTTP transport for the given URI.
     *
     * @param uri The URI to create a transport for
     * @return The ShenyuStreamableHttpServerTransportProvider for the new transport
     */
    private ShenyuStreamableHttpServerTransportProvider createStreamableHttpTransport(final String uri) {
        String path = StringUtils.removeEnd(uri, SLASH);
        LOG.info("Creating new Streamable HTTP transport for URI: {}", path);

        // Remove /streamablehttp suffix to normalize with SSE path for tool sharing
        String normalizedPath = normalizeServerPath(path);

        ShenyuStreamableHttpServerTransportProvider transportProvider = ShenyuStreamableHttpServerTransportProvider.builder()
                .objectMapper(new ObjectMapper())
                .endpoint(path)  // Keep original path for routing
                .build();

        LOG.debug("Created Streamable HTTP transport for URI: {}", path);

        // Always create a new server for each transport to ensure proper sessionFactory initialization
        LOG.info("Creating McpAsyncServer for Streamable HTTP transport at normalized path: {}", normalizedPath);
        McpAsyncServer server = createSharedMcpServer(transportProvider, normalizedPath);

        // Store the server with protocol-specific key to avoid conflicts
        String serverKey = normalizedPath + "_streamable_http";
        mcpServerMap.put(serverKey, server);

        // Store in protocol map for tracking using normalized path for tool sharing
        protocolServerMap.computeIfAbsent(normalizedPath, k -> new ConcurrentHashMap<>()).put("Streamable HTTP", server);

        // Add default test tool for each server instance
        addDefaultTestTool(serverKey, server);

        // Register routes for Streamable HTTP unified endpoint using original path
        routeMap.put(path, transportProvider::handleUnifiedEndpoint);

        // Add wildcard patterns to support sub-paths using original path
        routeMap.put(path + "/**", transportProvider::handleUnifiedEndpoint);

        // Track protocol support using normalized path for proper protocol detection
        protocolMap.computeIfAbsent(normalizedPath, k -> Collections.synchronizedSet(new HashSet<>())).add("Streamable HTTP");

        LOG.info("Created Streamable HTTP transport for URI: {} with normalized path: {} for tool sharing", path, normalizedPath);
        return transportProvider;
    }

    /**
     * Create a shared McpAsyncServer that can be used by multiple transport providers.
     *
     * @param transportProvider The initial transport provider
     * @param path The server path
     * @return The created McpAsyncServer
     */
    private McpAsyncServer createSharedMcpServer(Object transportProvider, String path) {
        // Configure server capabilities with resource support
        var capabilities = McpSchema.ServerCapabilities.builder()
                // Tool support with list changes notifications
                .tools(true)
                // Logging support
                .logging().build();

        // Determine server info based on transport type
        String serverName;
        McpAsyncServer server;

        if (transportProvider instanceof ShenyuSseServerTransportProvider) {
            serverName = "MCP Shenyu Server (SSE)";
            ShenyuSseServerTransportProvider sseProvider = (ShenyuSseServerTransportProvider) transportProvider;
            server = McpServer
                    .async(sseProvider)
                    .serverInfo(serverName, "1.0.0")
                    .capabilities(capabilities)
                    .tools(Lists.newArrayList())
                    .build();
        } else if (transportProvider instanceof ShenyuStreamableHttpServerTransportProvider) {
            serverName = "MCP Shenyu Server (Streamable HTTP)";
            ShenyuStreamableHttpServerTransportProvider streamableProvider = (ShenyuStreamableHttpServerTransportProvider) transportProvider;
            server = McpServer
                    .async(streamableProvider)
                    .serverInfo(serverName, "1.0.0")
                    .capabilities(capabilities)
                    .tools(Lists.newArrayList())
                    .build();
        } else {
            throw new IllegalArgumentException("Unsupported transport provider type: " + transportProvider.getClass());
        }

        // Verify that the session factory was properly set
        LOG.debug("MCP server created successfully for path: {}, session factory should be available", path);

        return server;
    }

    /**
     * Add a default test tool to ensure tools/list returns at least one tool for testing.
     */
    private void addDefaultTestTool(String serverPath, McpAsyncServer server) {
        try {
            // Create a simple test tool
            ToolDefinition testToolDefinition = ShenyuToolDefinition.builder()
                    .name("shenyu_echo")
                    .description("A simple echo tool for testing MCP functionality")
                    .requestConfig("{\"method\":\"GET\",\"uri\":\"/echo\",\"timeout\":30000}")
                    .inputSchema("{\"type\":\"object\",\"properties\":{\"message\":{\"type\":\"string\",\"description\":\"Message to echo back\"}},\"required\":[\"message\"]}")
                    .build();

            ShenyuToolCallback testToolCallback = new ShenyuToolCallback(testToolDefinition);

            for (AsyncToolSpecification asyncToolSpecification : McpToolUtils.toAsyncToolSpecifications(testToolCallback)) {
                server.addTool(asyncToolSpecification).block();
            }

            LOG.info("Added default test tool 'shenyu_echo' to server for path: {}", serverPath);
        } catch (Exception e) {
            LOG.error("Failed to add default test tool for path: {}", serverPath, e);
        }
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

        // Add tool to all servers for this path (different protocols)
        Map<String, McpAsyncServer> protocolServers = protocolServerMap.get(serverPath);
        if (Objects.nonNull(protocolServers)) {
            for (Map.Entry<String, McpAsyncServer> entry : protocolServers.entrySet()) {
                try {
                    for (AsyncToolSpecification asyncToolSpecification : McpToolUtils.toAsyncToolSpecifications(shenyuToolCallback)) {
                        entry.getValue().addTool(asyncToolSpecification).block();
                    }
                    LOG.debug("Added tool '{}' to {} server for path: {}", name, entry.getKey(), serverPath);
                } catch (Exception e) {
                    LOG.error("Failed to add tool '{}' to {} server for path: {}", name, entry.getKey(), serverPath, e);
                }
            }
        } else {
            // Fallback to legacy method for backward compatibility
            for (AsyncToolSpecification asyncToolSpecification : McpToolUtils.toAsyncToolSpecifications(shenyuToolCallback)) {
                McpAsyncServer server = mcpServerMap.get(serverPath);
                if (Objects.nonNull(server)) {
                    server.addTool(asyncToolSpecification).block();
                }
            }
        }
    }

    public void removeTool(final String serverPath, final String name) {
        LOG.debug("Removing tool, name: {}", name);

        // Remove from all servers for this path (different protocols)
        Map<String, McpAsyncServer> protocolServers = protocolServerMap.get(serverPath);
        if (Objects.nonNull(protocolServers)) {
            for (Map.Entry<String, McpAsyncServer> entry : protocolServers.entrySet()) {
                try {
                    entry.getValue().removeTool(name).block();
                    LOG.debug("Removed tool '{}' from {} server for path: {}", name, entry.getKey(), serverPath);
                } catch (Exception e) {
                    LOG.error("Failed to remove tool '{}' from {} server for path: {}", name, entry.getKey(), serverPath, e);
                }
            }
        } else {
            // Fallback to legacy method for backward compatibility
            McpAsyncServer server = mcpServerMap.get(serverPath);
            if (Objects.nonNull(server)) {
                server.removeTool(name).block();
                LOG.debug("Removed tool '{}' from server for path: {}", name, serverPath);
            }
        }

        // Log server tool count
        logServerToolCount(serverPath);
    }

    /**
     * Log the current tool count for a server path.
     */
    private void logServerToolCount(String serverPath) {
        McpAsyncServer server = mcpServerMap.get(serverPath);
        if (Objects.nonNull(server)) {
            // Try to get tool count through reflection or other means
            // This is for debugging purposes
            LOG.info("Server at path '{}' has been updated. Server instance: {}", serverPath, server.getClass().getSimpleName());

            // Log protocol support information
            Set<String> protocols = getSupportedProtocols(serverPath);
            LOG.info("Server at path '{}' supports protocols: {}", serverPath, protocols);

            if (supportsBothProtocols(serverPath)) {
                LOG.info("Server at path '{}' supports BOTH SSE and Streamable HTTP protocols - tools are shared!", serverPath);
            }
        } else {
            LOG.warn("No server found for path: {}", serverPath);
        }
    }

    /**
     * Get all registered server paths.
     */
    public java.util.Set<String> getRegisteredServerPaths() {
        return mcpServerMap.keySet();
    }

    /**
     * Get supported protocols for a server path.
     *
     * @param serverPath The server path
     * @return Set of supported protocols
     */
    public Set<String> getSupportedProtocols(String serverPath) {
        Set<String> protocols = protocolMap.get(serverPath);
        return protocols != null ? new HashSet<>(protocols) : new HashSet<>();
    }

    /**
     * Check if a server path supports both SSE and Streamable HTTP protocols.
     *
     * @param serverPath The server path
     * @return true if both protocols are supported
     */
    public boolean supportsBothProtocols(String serverPath) {
        Set<String> protocols = protocolMap.get(serverPath);
        return protocols != null && protocols.contains("SSE") && protocols.contains("Streamable HTTP");
    }

    /**
     * Get protocol statistics for all servers.
     *
     * @return Map of server path to supported protocols
     */
    public Map<String, Set<String>> getProtocolStatistics() {
        Map<String, Set<String>> stats = new ConcurrentHashMap<>();
        protocolMap.forEach((path, protocols) -> stats.put(path, new HashSet<>(protocols)));
        return stats;
    }

    /**
     * Remove protocol tracking when a server is removed.
     *
     * @param serverPath The server path to remove protocol tracking for
     */
    private void removeProtocolTracking(String serverPath) {
        protocolMap.remove(serverPath);
        LOG.debug("Removed protocol tracking for server path: {}", serverPath);
    }

    /**
     * Get an existing sessionFactory from any transport provider for the given path.
     *
     * @param serverPath The server path
     * @return The sessionFactory if found, null otherwise
     */
    private McpServerSession.Factory getExistingSessionFactory(String serverPath) {
        // Try to get sessionFactory from SSE transport provider first
        ShenyuSseServerTransportProvider sseTransport = mcpServerTransportMap.get(serverPath);
        if (Objects.nonNull(sseTransport)) {
            // Use reflection to get the sessionFactory field since it's private
            try {
                java.lang.reflect.Field sessionFactoryField = ShenyuSseServerTransportProvider.class.getDeclaredField("sessionFactory");
                sessionFactoryField.setAccessible(true);
                McpServerSession.Factory sessionFactory = (McpServerSession.Factory) sessionFactoryField.get(sseTransport);
                if (Objects.nonNull(sessionFactory)) {
                    LOG.debug("Found sessionFactory from SSE transport for path: {}", serverPath);
                    return sessionFactory;
                }
            } catch (Exception e) {
                LOG.warn("Failed to get sessionFactory from SSE transport for path: {}", serverPath, e);
            }
        }

        // Try to get sessionFactory from Streamable HTTP transport provider
        ShenyuStreamableHttpServerTransportProvider streamableTransport = streamableHttpTransportMap.get(serverPath);
        if (Objects.nonNull(streamableTransport)) {
            try {
                java.lang.reflect.Field sessionFactoryField = ShenyuStreamableHttpServerTransportProvider.class.getDeclaredField("sessionFactory");
                sessionFactoryField.setAccessible(true);
                McpServerSession.Factory sessionFactory = (McpServerSession.Factory) sessionFactoryField.get(streamableTransport);
                if (Objects.nonNull(sessionFactory)) {
                    LOG.debug("Found sessionFactory from Streamable HTTP transport for path: {}", serverPath);
                    return sessionFactory;
                }
            } catch (Exception e) {
                LOG.warn("Failed to get sessionFactory from Streamable HTTP transport for path: {}", serverPath, e);
            }
        }

        LOG.warn("No sessionFactory found for path: {}", serverPath);
        return null;
    }

    /**
     * Create and set a sessionFactory for a transport provider using the existing server.
     * This is a workaround for situations where we need to share sessionFactory between transports.
     *
     * @param transportProvider The transport provider to set sessionFactory for
     * @param server The existing McpAsyncServer
     */
    private void createSessionFactoryForTransport(Object transportProvider, McpAsyncServer server) {
        try {
            // Create a new temporary server with the transport provider to get a sessionFactory
            // This is not ideal but necessary for the current architecture
            McpAsyncServer tempServer;

            if (transportProvider instanceof ShenyuSseServerTransportProvider) {
                tempServer = McpServer
                        .async((ShenyuSseServerTransportProvider) transportProvider)
                        .serverInfo("Temp Server for SessionFactory", "1.0.0")
                        .capabilities(McpSchema.ServerCapabilities.builder().tools(true).build())
                        .tools(Lists.newArrayList())
                        .build();
                LOG.debug("Created temporary server for SSE transport sessionFactory");
            } else if (transportProvider instanceof ShenyuStreamableHttpServerTransportProvider) {
                tempServer = McpServer
                        .async((ShenyuStreamableHttpServerTransportProvider) transportProvider)
                        .serverInfo("Temp Server for SessionFactory", "1.0.0")
                        .capabilities(McpSchema.ServerCapabilities.builder().tools(true).build())
                        .tools(Lists.newArrayList())
                        .build();
                LOG.debug("Created temporary server for Streamable HTTP transport sessionFactory");
            } else {
                LOG.error("Unsupported transport provider type for sessionFactory creation: {}", transportProvider.getClass());
                return;
            }

            // The sessionFactory should now be set in the transport provider
            LOG.info("Successfully created sessionFactory for transport provider");

        } catch (Exception e) {
            LOG.error("Failed to create sessionFactory for transport provider", e);
        }
    }

    /**
     * Normalize the server path by removing protocol-specific suffixes.
     * This ensures that different protocols (SSE and Streamable HTTP) can share the same server path.
     *
     * @param path The original path
     * @return The normalized path for tool sharing
     */
    private String normalizeServerPath(final String path) {
        String normalizedPath = path;

        // Remove /streamablehttp suffix to normalize with SSE path for tool sharing
        if (normalizedPath.endsWith("/streamablehttp")) {
            normalizedPath = normalizedPath.substring(0, normalizedPath.length() - "/streamablehttp".length());
            LOG.info("Normalized Streamable HTTP path from '{}' to '{}' for tool sharing with SSE", path, normalizedPath);
        }

        return normalizedPath;
    }

}