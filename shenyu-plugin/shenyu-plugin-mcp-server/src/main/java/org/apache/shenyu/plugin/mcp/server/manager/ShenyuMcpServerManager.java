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
 * Enhanced Manager for MCP servers supporting shared server instances across multiple transport protocols.
 * <p>
 * This manager implements a unified architecture where SSE and Streamable HTTP protocols share:
 * <ul>
 *   <li>Same McpAsyncServer instance per path</li>
 *   <li>Shared tool sets and capabilities</li>
 *   <li>Unified session management with protocol-specific transport layers</li>
 * </ul>
 * </p>
 *
 * <h3>Architecture Benefits:</h3>
 * <ul>
 *   <li><strong>Resource Efficiency:</strong> Single server instance handles multiple protocols</li>
 *   <li><strong>Consistency:</strong> Tools and capabilities automatically synchronized across protocols</li>
 *   <li><strong>Scalability:</strong> Easy to add new transport protocols</li>
 *   <li><strong>Maintainability:</strong> Centralized server and tool management</li>
 * </ul>
 */
@Component
public class ShenyuMcpServerManager {

    private static final Logger LOG = LoggerFactory.getLogger(ShenyuMcpServerManager.class);

    /**
     * AntPathMatcher for pattern matching.
     */
    private final AntPathMatcher pathMatcher = new AntPathMatcher();

    /**
     * Map to store normalized path to shared McpAsyncServer mapping.
     * Key: normalized server path, Value: shared McpAsyncServer instance
     */
    private final Map<String, McpAsyncServer> sharedServerMap = new ConcurrentHashMap<>();

    /**
     * Map to store normalized path to supported transport providers.
     * Key: normalized server path, Value: Map of protocol name -> transport provider
     */
    private final Map<String, Map<String, Object>> transportProviderMap = new ConcurrentHashMap<>();

    /**
     * Map to store URI to SSE transport provider mapping.
     */
    private final Map<String, ShenyuSseServerTransportProvider> sseTransportMap = new ConcurrentHashMap<>();

    /**
     * Map to store URI to Streamable HTTP transport provider mapping.
     */
    private final Map<String, ShenyuStreamableHttpServerTransportProvider> streamableHttpTransportMap = new ConcurrentHashMap<>();

    /**
     * Map to store route handlers for different endpoints.
     */
    private final Map<String, HandlerFunction<?>> routeMap = new ConcurrentHashMap<>();

    /**
     * Map to track which protocols are enabled for each normalized server path.
     */
    private final Map<String, Set<String>> protocolMap = new ConcurrentHashMap<>();

    /**
     * Composite transport provider that delegates to multiple transport implementations.
     * <p>
     * Enhanced with protocol-aware session management and improved error handling.
     * </p>
     */
    private static class CompositeTransportProvider implements io.modelcontextprotocol.spec.McpServerTransportProvider {

        private final Map<String, Object> transports = new ConcurrentHashMap<>();
        private volatile McpServerSession.Factory sessionFactory;

        // Track active sessions per protocol for isolation
        private final Map<String, Set<String>> protocolSessions = new ConcurrentHashMap<>();

        public void addTransport(String protocol, Object transportProvider) {
            transports.put(protocol, transportProvider);
            protocolSessions.put(protocol, Collections.synchronizedSet(new HashSet<>()));

            // Set session factory on the new transport if available
            if (sessionFactory != null && transportProvider instanceof io.modelcontextprotocol.spec.McpServerTransportProvider) {
                ((io.modelcontextprotocol.spec.McpServerTransportProvider) transportProvider).setSessionFactory(sessionFactory);
            }

            LOG.debug("Added transport '{}' to composite provider", protocol);
        }

        public Set<String> getSupportedProtocols() {
            return new HashSet<>(transports.keySet());
        }

        /**
         * Gets active session count for a specific protocol.
         *
         * @param protocol the protocol name
         * @return the number of active sessions
         */
        public int getActiveSessionCount(String protocol) {
            Set<String> sessions = protocolSessions.get(protocol);
            return sessions != null ? sessions.size() : 0;
        }

        @Override
        public void setSessionFactory(McpServerSession.Factory sessionFactory) {
            this.sessionFactory = sessionFactory;
            // Set session factory on all existing transports atomically
            synchronized (transports) {
                for (Object transport : transports.values()) {
                    if (transport instanceof io.modelcontextprotocol.spec.McpServerTransportProvider) {
                        try {
                            ((io.modelcontextprotocol.spec.McpServerTransportProvider) transport).setSessionFactory(sessionFactory);
                        } catch (Exception e) {
                            LOG.error("Failed to set session factory on transport: {}", transport.getClass().getSimpleName(), e);
                        }
                    }
                }
            }
            LOG.debug("Session factory set on composite transport with {} transports", transports.size());
        }

        @Override
        public reactor.core.publisher.Mono<Void> notifyClients(String method, Object params) {
            if (transports.isEmpty()) {
                LOG.debug("No transports available for client notification");
                return reactor.core.publisher.Mono.empty();
            }

            LOG.debug("Broadcasting notification '{}' to {} transports", method, transports.size());

            return reactor.core.publisher.Flux.fromIterable(transports.entrySet())
                    .flatMap(entry -> {
                        String protocol = entry.getKey();
                        Object transport = entry.getValue();

                        if (transport instanceof io.modelcontextprotocol.spec.McpServerTransportProvider) {
                            return ((io.modelcontextprotocol.spec.McpServerTransportProvider) transport)
                                    .notifyClients(method, params)
                                    .doOnSuccess(aVoid -> LOG.debug("Successfully notified {} clients", protocol))
                                    .doOnError(e -> LOG.warn("Failed to notify {} clients: {}", protocol, e.getMessage()))
                                    .onErrorComplete(); // Continue with other transports even if one fails
                        } else {
                            LOG.warn("Transport '{}' does not implement McpServerTransportProvider", protocol);
                            return reactor.core.publisher.Mono.empty();
                        }
                    })
                    .then()
                    .doOnSuccess(aVoid -> LOG.debug("Client notification broadcast completed"));
        }

        @Override
        public reactor.core.publisher.Mono<Void> closeGracefully() {
            if (transports.isEmpty()) {
                return reactor.core.publisher.Mono.empty();
            }

            LOG.info("Initiating graceful shutdown of {} transports", transports.size());

            return reactor.core.publisher.Flux.fromIterable(transports.entrySet())
                    .flatMap(entry -> {
                        String protocol = entry.getKey();
                        Object transport = entry.getValue();

                        if (transport instanceof io.modelcontextprotocol.spec.McpServerTransportProvider) {
                            return ((io.modelcontextprotocol.spec.McpServerTransportProvider) transport)
                                    .closeGracefully()
                                    .doOnSuccess(aVoid -> LOG.info("Successfully closed {} transport", protocol))
                                    .doOnError(e -> LOG.error("Error closing {} transport: {}", protocol, e.getMessage()))
                                    .onErrorComplete(); // Don't fail entire shutdown if one transport fails
                        } else {
                            LOG.warn("Transport '{}' does not implement graceful shutdown", protocol);
                            return reactor.core.publisher.Mono.empty();
                        }
                    })
                    .then()
                    .doOnSuccess(aVoid -> {
                        // Clear all tracking data after successful shutdown
                        transports.clear();
                        protocolSessions.clear();
                        LOG.info("Graceful shutdown completed - all transports and sessions cleared");
                    });
        }

        /**
         * Gets comprehensive status of all transports.
         *
         * @return status map with protocol statistics
         */
        public Map<String, Object> getTransportStatus() {
            Map<String, Object> status = new java.util.HashMap<>();
            status.put("totalTransports", transports.size());
            status.put("supportedProtocols", getSupportedProtocols());

            Map<String, Integer> sessionCounts = new java.util.HashMap<>();
            for (String protocol : transports.keySet()) {
                sessionCounts.put(protocol, getActiveSessionCount(protocol));
            }
            status.put("activeSessionsByProtocol", sessionCounts);

            return status;
        }
    }

    /**
     * Map to store composite transport providers for shared servers.
     */
    private final Map<String, CompositeTransportProvider> compositeTransportMap = new ConcurrentHashMap<>();

    /**
     * Get or create a shared MCP server for the given path, supporting multiple transport protocols.
     *
     * @param uri The URI to create or get a server for
     * @return The SSE transport provider for the URI
     */
    public ShenyuSseServerTransportProvider getOrCreateMcpServerTransport(final String uri, final String messageEndPoint) {
        String normalizedPath = normalizeServerPath(extractBasePath(uri));
        // First try exact match
        ShenyuSseServerTransportProvider transport = sseTransportMap.get(uri);
        if (Objects.nonNull(transport)) {
            return transport;
        }

        // Then try to find existing transport that matches the pattern
        String basePath = extractBasePath(uri, messageEndPoint);
        transport = sseTransportMap.get(basePath);
        if (transport == null) {
            transport = createSseTransport(normalizedPath, messageEndPoint);
            sseTransportMap.put(normalizedPath, transport);
            // Add to shared server infrastructure
            addTransportToSharedServer(normalizedPath, "SSE", transport);
        }
        // Create new transport for the base path
        return transport;
    }

    /**
     * Get or create a shared MCP server for Streamable HTTP transport.
     *
     * @param uri The URI to create or get a transport provider for
     * @return The Streamable HTTP transport provider for the URI
     */
    public ShenyuStreamableHttpServerTransportProvider getOrCreateStreamableHttpTransport(final String uri) {
        String normalizedPath = normalizeServerPath(extractBasePath(uri));

        // Get or create Streamable HTTP transport
        ShenyuStreamableHttpServerTransportProvider streamableTransport = streamableHttpTransportMap.get(normalizedPath);
        if (streamableTransport == null) {
            streamableTransport = createStreamableHttpTransport(normalizedPath, uri);
            streamableHttpTransportMap.put(normalizedPath, streamableTransport);

            // Add to shared server infrastructure
            addTransportToSharedServer(normalizedPath, "Streamable HTTP", streamableTransport);
        }

        return streamableTransport;
    }

    /**
     * Adds a transport provider to the shared server infrastructure.
     *
     * @param normalizedPath    the normalized server path
     * @param protocol          the protocol name
     * @param transportProvider the transport provider instance
     */
    private void addTransportToSharedServer(String normalizedPath, String protocol, Object transportProvider) {
        // Get or create shared server
        McpAsyncServer sharedServer = getOrCreateSharedServer(normalizedPath);

        // Add transport to composite provider
        CompositeTransportProvider compositeTransport = compositeTransportMap.get(normalizedPath);
        if (compositeTransport != null) {
            compositeTransport.addTransport(protocol, transportProvider);
        }

        // Track protocol support
        protocolMap.computeIfAbsent(normalizedPath, k -> Collections.synchronizedSet(new HashSet<>())).add(protocol);

        // Store transport provider reference
        transportProviderMap.computeIfAbsent(normalizedPath, k -> new ConcurrentHashMap<>()).put(protocol, transportProvider);

        LOG.info("Added {} transport to shared server at path: {}", protocol, normalizedPath);
    }

    /**
     * Gets or creates a shared McpAsyncServer instance for the given normalized path.
     *
     * @param normalizedPath the normalized server path
     * @return the shared McpAsyncServer instance
     */
    private McpAsyncServer getOrCreateSharedServer(String normalizedPath) {
        return sharedServerMap.computeIfAbsent(normalizedPath, path -> {
            LOG.info("Creating shared MCP server for path: {}", path);

            // Create composite transport provider
            CompositeTransportProvider compositeTransport = new CompositeTransportProvider();
            compositeTransportMap.put(path, compositeTransport);

            // Configure server capabilities
            var capabilities = McpSchema.ServerCapabilities.builder()
                    .tools(true)
                    .logging()
                    .build();

            // Create shared server with composite transport
            McpAsyncServer server = McpServer
                    .async(compositeTransport)
                    .serverInfo("MCP Shenyu Server (Multi-Protocol)", "1.0.0")
                    .capabilities(capabilities)
                    .tools(Lists.newArrayList())
                    .build();

            LOG.info("Created shared MCP server for path: {} with multi-protocol support", path);
            return server;
        });
    }

    /**
     * Creates SSE transport provider.
     */
    private ShenyuSseServerTransportProvider createSseTransport(String normalizedPath, final String messageEndPoint) {
        String messageEndpoint = normalizedPath + messageEndPoint;
        ShenyuSseServerTransportProvider transportProvider = ShenyuSseServerTransportProvider.builder()
                .objectMapper(new ObjectMapper())
                .sseEndpoint(normalizedPath)
                .messageEndpoint(messageEndpoint)
                .build();

        // Register routes
        routeMap.put(normalizedPath, transportProvider::handleSseConnection);
        routeMap.put(messageEndpoint, transportProvider::handleMessage);
        routeMap.put(normalizedPath + "/**", transportProvider::handleSseConnection);
        routeMap.put(messageEndpoint + "/**", transportProvider::handleMessage);

        LOG.debug("Created SSE transport for path: {}", normalizedPath);
        return transportProvider;
    }

    /**
     * Creates Streamable HTTP transport provider.
     */
    private ShenyuStreamableHttpServerTransportProvider createStreamableHttpTransport(String normalizedPath, String originalUri) {
        ShenyuStreamableHttpServerTransportProvider transportProvider = ShenyuStreamableHttpServerTransportProvider.builder()
                .objectMapper(new ObjectMapper())
                .endpoint(originalUri)  // Keep original URI for routing
                .build();

        // Register routes for original URI
        routeMap.put(originalUri, transportProvider::handleUnifiedEndpoint);
        routeMap.put(originalUri + "/**", transportProvider::handleUnifiedEndpoint);

        LOG.debug("Created Streamable HTTP transport for original URI: {} (normalized: {})", originalUri, normalizedPath);
        return transportProvider;
    }

    /**
     * Extract the base path from a URI by removing the /message suffix and any sub-paths.
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
        String[] pathSegments = basePath.split("/");
        if (pathSegments.length > 2) {
            // Keep only the first two segments (empty + server-name)
            basePath = "/" + pathSegments[1];
        }

        return basePath;
    }

    /**
     * Extract the base path from a URI by removing the /message suffix and any sub-paths.
     *
     * @param uri The URI to extract base path from
     * @return The base path
     */
    private String extractBasePath(final String uri, final String messageEndPoint) {
        String basePath = uri;

        // Remove /message suffix if present
        if (basePath.endsWith(messageEndPoint)) {
            basePath = basePath.substring(0, basePath.length() - messageEndPoint.length());
        }

        // For sub-paths, extract the main MCP server path
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
        String normalizedPath = normalizeServerPath(extractBasePath(uri));
        return sharedServerMap.containsKey(normalizedPath);
    }

    /**
     * Check if a McpServer can route requests for the given URI.
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
     * Remove a MCP server and all its associated transports.
     *
     * @param uri The URI to remove the server for
     */
    public void removeMcpServer(final String uri) {
        String normalizedPath = normalizeServerPath(extractBasePath(uri));
        LOG.info("Removing MCP server for URI: {} (normalized: {})", uri, normalizedPath);

        // Close composite transport gracefully
        CompositeTransportProvider compositeTransport = compositeTransportMap.remove(normalizedPath);
        if (compositeTransport != null) {
            compositeTransport.closeGracefully()
                    .doOnSuccess(aVoid -> LOG.info("Successfully closed composite transport for path: {}", normalizedPath))
                    .doOnError(e -> LOG.error("Error closing composite transport for path: {}", normalizedPath, e))
                    .subscribe();
        }

        // Remove individual transports
        ShenyuSseServerTransportProvider sseTransport = sseTransportMap.remove(normalizedPath);
        if (sseTransport != null) {
            sseTransport.closeGracefully().subscribe();
        }

        ShenyuStreamableHttpServerTransportProvider streamableTransport = streamableHttpTransportMap.remove(normalizedPath);
        if (streamableTransport != null) {
            streamableTransport.closeGracefully().subscribe();
        }

        // Remove server and tracking
        sharedServerMap.remove(normalizedPath);
        transportProviderMap.remove(normalizedPath);
        protocolMap.remove(normalizedPath);

        // Clean up routes - this is tricky as we need to remove all related routes
        routeMap.entrySet().removeIf(entry -> entry.getKey().startsWith(normalizedPath) || entry.getKey().startsWith(uri));

        LOG.info("Removed MCP server for path: {}", normalizedPath);
    }

    /**
     * Adds a tool to the shared server instance, automatically available across all protocols.
     *
     * @param serverPath      the server path
     * @param name            the tool name
     * @param description     the tool description
     * @param requestTemplate the request template
     * @param inputSchema     the input schema
     */
    public void addTool(final String serverPath, final String name, final String description,
                        final String requestTemplate, final String inputSchema) {
        String normalizedPath = normalizeServerPath(serverPath);

        // Remove existing tool first
        try {
            removeTool(serverPath, name);
        } catch (Exception ignored) {
            // ignore
        }

        ToolDefinition shenyuToolDefinition = ShenyuToolDefinition.builder()
                .name(name)
                .description(description)
                .requestConfig(requestTemplate)
                .inputSchema(inputSchema)
                .build();

        LOG.debug("Adding tool to shared server - name: {}, description: {}, path: {}", name, description, normalizedPath);

        ShenyuToolCallback shenyuToolCallback = new ShenyuToolCallback(shenyuToolDefinition);

        // Add tool to shared server (automatically available across all protocols)
        McpAsyncServer sharedServer = sharedServerMap.get(normalizedPath);
        if (sharedServer != null) {
            try {
                for (AsyncToolSpecification asyncToolSpecification : McpToolUtils.toAsyncToolSpecifications(shenyuToolCallback)) {
                    sharedServer.addTool(asyncToolSpecification).block();
                }

                Set<String> protocols = getSupportedProtocols(normalizedPath);
                LOG.info("Added tool '{}' to shared server for path: {} (available across protocols: {})",
                        name, normalizedPath, protocols);
            } catch (Exception e) {
                LOG.error("Failed to add tool '{}' to shared server for path: {}", name, normalizedPath, e);
            }
        } else {
            LOG.warn("No shared server found for path: {}", normalizedPath);
        }
    }

    /**
     * Removes a tool from the shared server instance.
     *
     * @param serverPath the server path
     * @param name       the tool name
     */
    public void removeTool(final String serverPath, final String name) {
        String normalizedPath = normalizeServerPath(serverPath);
        LOG.debug("Removing tool from shared server - name: {}, path: {}", name, normalizedPath);

        McpAsyncServer sharedServer = sharedServerMap.get(normalizedPath);
        if (sharedServer != null) {
            try {
                sharedServer.removeTool(name).block();

                Set<String> protocols = getSupportedProtocols(normalizedPath);
                LOG.info("Removed tool '{}' from shared server for path: {} (removed from protocols: {})",
                        name, normalizedPath, protocols);
            } catch (Exception e) {
                LOG.error("Failed to remove tool '{}' from shared server for path: {}", name, normalizedPath, e);
            }
        } else {
            LOG.warn("No shared server found for path: {}", normalizedPath);
        }
    }

    /**
     * Get supported protocols for a server path.
     *
     * @param serverPath The server path
     * @return Set of supported protocols
     */
    public Set<String> getSupportedProtocols(String serverPath) {
        String normalizedPath = normalizeServerPath(serverPath);
        Set<String> protocols = protocolMap.get(normalizedPath);
        return protocols != null ? new HashSet<>(protocols) : new HashSet<>();
    }

    /**
     * Check if a server path supports both SSE and Streamable HTTP protocols.
     *
     * @param serverPath The server path
     * @return true if both protocols are supported
     */
    public boolean supportsBothProtocols(String serverPath) {
        Set<String> protocols = getSupportedProtocols(serverPath);
        return protocols.contains("SSE") && protocols.contains("Streamable HTTP");
    }


    /**
     * Normalize the server path by removing protocol-specific suffixes.
     *
     * @param path The original path
     * @return The normalized path for shared server usage
     */
    private String normalizeServerPath(final String path) {
        if (path == null) {
            return null;
        }

        String normalizedPath = path;

        // Remove /streamablehttp suffix
        if (normalizedPath.endsWith("/streamablehttp")) {
            normalizedPath = normalizedPath.substring(0, normalizedPath.length() - "/streamablehttp".length());
            LOG.debug("Normalized Streamable HTTP path from '{}' to '{}' for shared server", path, normalizedPath);
        }

        return normalizedPath;
    }

}