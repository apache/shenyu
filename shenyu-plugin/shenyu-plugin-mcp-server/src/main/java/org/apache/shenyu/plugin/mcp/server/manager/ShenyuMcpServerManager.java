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

import java.time.Duration;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.Set;
import java.util.HashSet;
import java.util.Collections;

/**
 * Enhanced Manager for MCP servers supporting shared server instances across multiple transport protocols.
 * 
 * <p>This manager implements a unified architecture where SSE and Streamable HTTP protocols share
 * the same McpAsyncServer instance per path, with shared tool sets and capabilities.
 * Provides centralized server and tool management with protocol-specific transport layers.</p>
 *
 * @since 2.7.0.2
 */
@Component
public class ShenyuMcpServerManager {

    private static final Logger LOG = LoggerFactory.getLogger(ShenyuMcpServerManager.class);

    /**
     * Protocol names for transport identification.
     */
    private static final String SSE_PROTOCOL = "SSE";

    private static final String STREAMABLE_HTTP_PROTOCOL = "Streamable HTTP";

    /**
     * AntPathMatcher for pattern matching.
     */
    private final AntPathMatcher pathMatcher = new AntPathMatcher();

    /**
     * Shared ObjectMapper instance for JSON processing.
     */
    private final ObjectMapper objectMapper = new ObjectMapper();

    /**
     * Map to store normalized path to shared McpAsyncServer mapping.
     * Key: normalized server path, Value: shared McpAsyncServer instance
     */
    private final Map<String, McpAsyncServer> sharedServerMap = new ConcurrentHashMap<>();

    /**
     * Map to store route handlers for different endpoints.
     */
    private final Map<String, HandlerFunction<?>> routeMap = new ConcurrentHashMap<>();

    /**
     * Map to store composite transport providers for shared servers.
     */
    private final Map<String, CompositeTransportProvider> compositeTransportMap = new ConcurrentHashMap<>();

    /**
     * Get or create a shared MCP server for the given path, supporting multiple transport protocols.
     *
     * @param uri The URI to create or get a server for
     * @param messageEndPoint The message endpoint path
     * @return The SSE transport provider for the URI
     */
    public ShenyuSseServerTransportProvider getOrCreateMcpServerTransport(final String uri, final String messageEndPoint) {
        String normalizedPath = processPath(uri);
        return getOrCreateTransport(normalizedPath, SSE_PROTOCOL, 
            () -> createSseTransport(normalizedPath, messageEndPoint));
    }

    /**
     * Get or create a shared MCP server for Streamable HTTP transport.
     *
     * @param uri The URI to create or get a transport provider for
     * @return The Streamable HTTP transport provider for the URI
     */
    public ShenyuStreamableHttpServerTransportProvider getOrCreateStreamableHttpTransport(final String uri) {
        String normalizedPath = processPath(uri);
        return getOrCreateTransport(normalizedPath, STREAMABLE_HTTP_PROTOCOL, 
            () -> createStreamableHttpTransport(normalizedPath, uri));
    }

    /**
     * Generic method to get or create transport providers.
     *
     * @param normalizedPath the normalized path
     * @param protocol the protocol name
     * @param transportFactory the factory function to create transport if not exists
     * @param <T> the transport type
     * @return the transport provider
     */
    @SuppressWarnings("unchecked")
    private <T> T getOrCreateTransport(final String normalizedPath, final String protocol, 
                                      final java.util.function.Supplier<T> transportFactory) {
        CompositeTransportProvider compositeTransport = getOrCreateCompositeTransport(normalizedPath);
        
        T transport = switch (protocol) {
            case SSE_PROTOCOL -> (T) compositeTransport.getSseTransport();
            case STREAMABLE_HTTP_PROTOCOL -> (T) compositeTransport.getStreamableHttpTransport();
            default -> null;
        };
        
        if (Objects.isNull(transport)) {
            transport = transportFactory.get();
            addTransportToSharedServer(normalizedPath, protocol, transport);
        }
        
        return transport;
    }

    /**
     * Process URI to get normalized path.
     *
     * @param uri the URI to process
     * @return normalized path
     */
    private String processPath(final String uri) {
        return normalizeServerPath(extractBasePath(uri));
    }

    /**
     * Gets or creates a composite transport provider for the given normalized path.
     *
     * @param normalizedPath the normalized server path
     * @return the composite transport provider
     */
    private CompositeTransportProvider getOrCreateCompositeTransport(final String normalizedPath) {
        return compositeTransportMap.computeIfAbsent(normalizedPath, path -> {
            LOG.debug("Creating composite transport provider for path: {}", path);
            return new CompositeTransportProvider();
        });
    }

    /**
     * Adds a transport provider to the shared server infrastructure.
     *
     * @param normalizedPath    the normalized server path
     * @param protocol          the protocol name
     * @param transportProvider the transport provider instance
     */
    private void addTransportToSharedServer(final String normalizedPath, final String protocol, final Object transportProvider) {
        // Get or create shared server
        getOrCreateSharedServer(normalizedPath);

        // Add transport to composite provider
        CompositeTransportProvider compositeTransport = compositeTransportMap.get(normalizedPath);
        if (Objects.nonNull(compositeTransport)) {
            compositeTransport.addTransport(protocol, transportProvider);
        }

        LOG.info("Added {} transport to shared server at path: {}", protocol, normalizedPath);
    }

    /**
     * Gets or creates a shared McpAsyncServer instance for the given normalized path.
     *
     * @param normalizedPath the normalized server path
     * @return the shared McpAsyncServer instance
     */
    private McpAsyncServer getOrCreateSharedServer(final String normalizedPath) {
        return sharedServerMap.computeIfAbsent(normalizedPath, path -> {
            LOG.info("Creating shared MCP server for path: {}", path);

            // Get or create composite transport provider
            CompositeTransportProvider compositeTransport = getOrCreateCompositeTransport(path);

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
    private ShenyuSseServerTransportProvider createSseTransport(final String normalizedPath, final String messageEndPoint) {
        String messageEndpoint = normalizedPath + messageEndPoint;
        ShenyuSseServerTransportProvider transportProvider = ShenyuSseServerTransportProvider.builder()
                .objectMapper(objectMapper)
                .sseEndpoint(normalizedPath)
                .messageEndpoint(messageEndpoint)
                .build();

        // Register routes
        registerRoutes(normalizedPath, messageEndpoint, transportProvider::handleSseConnection, transportProvider::handleMessage);

        LOG.debug("Created SSE transport for path: {}", normalizedPath);
        return transportProvider;
    }

    /**
     * Creates Streamable HTTP transport provider.
     */
    private ShenyuStreamableHttpServerTransportProvider createStreamableHttpTransport(final String normalizedPath, final String originalUri) {
        ShenyuStreamableHttpServerTransportProvider transportProvider = ShenyuStreamableHttpServerTransportProvider.builder()
                .objectMapper(objectMapper)
                .endpoint(originalUri)
                .build();

        // Register routes for original URI
        registerRoutes(originalUri, null, transportProvider::handleUnifiedEndpoint, null);

        LOG.debug("Created Streamable HTTP transport for original URI: {} (normalized: {})", originalUri, normalizedPath);
        return transportProvider;
    }

    /**
     * Register routes for transport providers.
     *
     * @param primaryPath the primary path
     * @param secondaryPath the secondary path (can be null)
     * @param primaryHandler the primary handler
     * @param secondaryHandler the secondary handler (can be null)
     */
    private void registerRoutes(final String primaryPath, final String secondaryPath, 
                               final HandlerFunction<?> primaryHandler, final HandlerFunction<?> secondaryHandler) {
        routeMap.put(primaryPath, primaryHandler);
        routeMap.put(primaryPath + "/**", primaryHandler);
        
        if (Objects.nonNull(secondaryPath) && Objects.nonNull(secondaryHandler)) {
            routeMap.put(secondaryPath, secondaryHandler);
            routeMap.put(secondaryPath + "/**", secondaryHandler);
        }
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
     * Check if a McpServer exists for the given URI.
     *
     * @param uri The URI to check
     * @return true if a McpServer exists, false otherwise
     */
    public boolean hasMcpServer(final String uri) {
        String normalizedPath = processPath(uri);
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
        String normalizedPath = processPath(uri);
        LOG.info("Removing MCP server for URI: {} (normalized: {})", uri, normalizedPath);

        // Close composite transport gracefully
        CompositeTransportProvider compositeTransport = compositeTransportMap.remove(normalizedPath);
        if (Objects.nonNull(compositeTransport)) {
            compositeTransport.closeGracefully()
                    .doOnSuccess(aVoid -> LOG.info("Successfully closed composite transport for path: {}", normalizedPath))
                    .doOnError(e -> LOG.error("Error closing composite transport for path: {}", normalizedPath, e))
                    .subscribe();
        }

        // Remove server
        sharedServerMap.remove(normalizedPath);

        // Clean up routes
        routeMap.entrySet().removeIf(entry -> entry.getKey().startsWith(Objects.requireNonNull(normalizedPath)) || entry.getKey().startsWith(uri));

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
    public synchronized void addTool(final String serverPath, final String name, final String description,
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
        if (Objects.nonNull(sharedServer)) {
            try {
                for (AsyncToolSpecification asyncToolSpecification : McpToolUtils.toAsyncToolSpecifications(shenyuToolCallback)) {
                    // Use non-blocking approach with timeout to prevent hanging
                    sharedServer.addTool(asyncToolSpecification)
                            .timeout(Duration.ofSeconds(10))
                            .doOnSuccess(v -> LOG.debug("Successfully added tool '{}' to server for path: {}", name, normalizedPath))
                            .doOnError(e -> LOG.error("Failed to add tool '{}' to server for path: {}: {}", name, normalizedPath, e.getMessage()))
                            .block();
                }

                Set<String> protocols = getSupportedProtocols(normalizedPath);
                LOG.info("Added tool '{}' to shared server for path: {} (available across protocols: {})",
                        name, normalizedPath, protocols);
            } catch (Exception e) {
                LOG.error("Failed to add tool '{}' to shared server for path: {}:", name, normalizedPath, e);
                // Don't throw exception to prevent affecting other tools
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
        if (Objects.nonNull(sharedServer)) {
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
    public Set<String> getSupportedProtocols(final String serverPath) {
        String normalizedPath = normalizeServerPath(serverPath);
        CompositeTransportProvider compositeTransport = compositeTransportMap.get(normalizedPath);
        return Objects.nonNull(compositeTransport) ? compositeTransport.getSupportedProtocols() : new HashSet<>();
    }

    /**
     * Normalize the server path by removing protocol-specific suffixes.
     *
     * @param path The original path
     * @return The normalized path for shared server usage
     */
    private String normalizeServerPath(final String path) {
        if (Objects.isNull(path)) {
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

    /**
     * Composite transport provider that delegates to multiple transport implementations.
     * Enhanced with protocol-aware session management and improved error handling.
     * Provides unified access to different transport protocols.
     */
    private static class CompositeTransportProvider implements io.modelcontextprotocol.spec.McpServerTransportProvider {

        private final Map<String, Object> transports = new ConcurrentHashMap<>();

        private volatile McpServerSession.Factory sessionFactory;

        // Track active sessions per protocol for isolation
        private final Map<String, Set<String>> protocolSessions = new ConcurrentHashMap<>();

        public void addTransport(final String protocol, final Object transportProvider) {
            transports.put(protocol, transportProvider);
            protocolSessions.put(protocol, Collections.synchronizedSet(new HashSet<>()));

            // Set session factory on the new transport if available
            if (Objects.nonNull(sessionFactory) && transportProvider instanceof io.modelcontextprotocol.spec.McpServerTransportProvider) {
                ((io.modelcontextprotocol.spec.McpServerTransportProvider) transportProvider).setSessionFactory(sessionFactory);
            }

            LOG.debug("Added transport '{}' to composite provider", protocol);
        }

        /**
         * Gets the SSE transport provider.
         *
         * @return the SSE transport provider, or null if not available
         */
        public ShenyuSseServerTransportProvider getSseTransport() {
            Object transport = transports.get(SSE_PROTOCOL);
            return transport instanceof ShenyuSseServerTransportProvider 
                ? (ShenyuSseServerTransportProvider) transport : null;
        }

        /**
         * Gets the Streamable HTTP transport provider.
         *
         * @return the Streamable HTTP transport provider, or null if not available
         */
        public ShenyuStreamableHttpServerTransportProvider getStreamableHttpTransport() {
            Object transport = transports.get(STREAMABLE_HTTP_PROTOCOL);
            return transport instanceof ShenyuStreamableHttpServerTransportProvider 
                ? (ShenyuStreamableHttpServerTransportProvider) transport : null;
        }

        /**
         * Gets a transport provider by protocol name.
         *
         * @param protocol the protocol name
         * @return the transport provider, or null if not found
         */
        public Object getTransport(final String protocol) {
            return transports.get(protocol);
        }

        /**
         * Checks if a specific protocol is supported.
         *
         * @param protocol the protocol name
         * @return true if the protocol is supported, false otherwise
         */
        public boolean hasProtocol(final String protocol) {
            return transports.containsKey(protocol);
        }

        /**
         * Gets all supported protocols.
         *
         * @return a set of supported protocol names
         */
        public Set<String> getSupportedProtocols() {
            return new HashSet<>(transports.keySet());
        }

        @Override
        public void setSessionFactory(final McpServerSession.Factory sessionFactory) {
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
        public reactor.core.publisher.Mono<Void> notifyClients(final String method, final Object params) {
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
                                    // Continue with other transports even if one fails
                                    .onErrorComplete();
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
                                    // Don't fail entire shutdown if one transport fails
                                    .onErrorComplete();
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
    }

}