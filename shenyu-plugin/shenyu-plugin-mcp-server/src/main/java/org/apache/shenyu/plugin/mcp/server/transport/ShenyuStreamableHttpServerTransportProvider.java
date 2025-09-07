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

package org.apache.shenyu.plugin.mcp.server.transport;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.modelcontextprotocol.spec.McpError;
import io.modelcontextprotocol.spec.McpSchema;
import io.modelcontextprotocol.spec.McpServerSession;
import io.modelcontextprotocol.spec.McpServerTransport;
import io.modelcontextprotocol.spec.McpServerTransportProvider;
import io.modelcontextprotocol.util.Assert;
import org.apache.shenyu.plugin.mcp.server.holder.ShenyuMcpExchangeHolder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.web.reactive.function.server.ServerRequest;
import org.springframework.web.reactive.function.server.ServerResponse;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.Exceptions;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.io.IOException;
import java.util.Objects;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Streamable HTTP Server Transport Provider for MCP (Model Context Protocol).
 * Implements the Streamable HTTP MCP transport protocol with unified endpoint for all MCP operations.
 * Provides advanced session management with automatic recovery and reconnection capabilities.
 *
 * @see McpServerTransportProvider
 * @see McpServerSession
 * @see org.apache.shenyu.plugin.mcp.server.holder.ShenyuMcpExchangeHolder
 * @since 2.7.0.2
 */
public class ShenyuStreamableHttpServerTransportProvider implements McpServerTransportProvider {

    private static final Logger LOGGER = LoggerFactory.getLogger(ShenyuStreamableHttpServerTransportProvider.class);

    /**
     * Header name for MCP session identification.
     */
    private static final String SESSION_ID_HEADER = "Mcp-Session-Id";

    /**
     * JSON-RPC 2.0 version identifier.
     */
    private static final String JSONRPC_VERSION = "2.0";

    /**
     * MCP initialize method name.
     */
    private static final String INITIALIZE_METHOD = "initialize";

    /**
     * Default MCP protocol version.
     */
    private static final String DEFAULT_PROTOCOL_VERSION = "2025-03-26";

    /**
     * Server information constants.
     */
    private static final String SERVER_NAME = "ShenyuMcpServer";

    private static final String SERVER_VERSION = "1.0.0";

    private final ObjectMapper objectMapper;

    private McpServerSession.Factory sessionFactory;

    /**
     * Map of active client sessions, keyed by session ID.
     * This enables session reuse for subsequent requests with the same session ID.
     */
    private final ConcurrentHashMap<String, McpServerSession> sessions = new ConcurrentHashMap<>();

    /**
     * Map of session transports, keyed by session ID.
     * This enables access to transport-specific functionality and response correlation.
     */
    private final ConcurrentHashMap<String, StreamableHttpSessionTransport> sessionTransports = new ConcurrentHashMap<>();

    /**
     * Flag indicating if the transport provider is shutting down.
     */
    private volatile boolean isClosing;

    /**
     * Constructs a new Streamable HTTP server transport provider instance.
     *
     * @param objectMapper The ObjectMapper to use for JSON serialization/deserialization
     * @param endpoint     The endpoint path for the Streamable HTTP MCP transport
     * @throws IllegalArgumentException if objectMapper or endpoint is null
     */
    public ShenyuStreamableHttpServerTransportProvider(final ObjectMapper objectMapper, final String endpoint) {
        Assert.notNull(objectMapper, "ObjectMapper must not be null");
        Assert.notNull(endpoint, "Endpoint must not be null");
        this.objectMapper = objectMapper;
        LOGGER.debug("Created Streamable HTTP transport provider for endpoint: {}", endpoint);
    }

    /**
     * Creates a new builder instance for constructing transport providers.
     *
     * @return a new Builder instance
     */
    public static StreamableHttpProviderBuilder builder() {
        return new StreamableHttpProviderBuilder();
    }

    @Override
    public void setSessionFactory(final McpServerSession.Factory sessionFactory) {
        this.sessionFactory = sessionFactory;
        LOGGER.debug("Session factory configured for Streamable HTTP transport");
    }

    @Override
    public Mono<Void> notifyClients(final String method, final Object params) {
        if (sessions.isEmpty()) {
            LOGGER.debug("No active sessions available for client notification");
            return Mono.empty();
        }
        LOGGER.debug("Broadcasting notification '{}' to {} active sessions", method, sessions.size());
        return Flux.fromIterable(sessions.values())
                .flatMap(session -> session.sendNotification(method, params)
                        .doOnError(e -> LOGGER.warn("Failed to send notification to session {}: {}",
                                session.getId(), e.getMessage()))
                        .onErrorComplete())
                .then()
                .doOnSuccess(aVoid -> LOGGER.debug("Client notification broadcast completed"));
    }

    @Override
    public Mono<Void> closeGracefully() {
        isClosing = true;
        if (sessions.isEmpty()) {
            LOGGER.debug("No active sessions to close during graceful shutdown");
            return Mono.empty();
        }
        LOGGER.debug("Initiating graceful shutdown of {} active sessions", sessions.size());
        return Flux.fromIterable(sessions.values())
                .flatMap(McpServerSession::closeGracefully)
                .doOnComplete(() -> {
                    sessions.clear();
                    sessionTransports.clear();
                    LOGGER.debug("Graceful shutdown completed - all sessions and transports cleared");
                })
                .then();
    }

    /**
     * Unified endpoint for Streamable HTTP protocol. Handles GET (stream) and POST (message).
     *
     * @param request the server request
     * @return a Mono containing the server response
     */
    public Mono<ServerResponse> handleUnifiedEndpoint(final ServerRequest request) {
        if (isClosing) {
            return ServerResponse.status(HttpStatus.SERVICE_UNAVAILABLE).bodyValue("Server is shutting down");
        }
        if (Objects.isNull(sessionFactory)) {
            LOGGER.error("SessionFactory is null - MCP server not properly initialized");
            return ServerResponse.status(HttpStatus.INTERNAL_SERVER_ERROR).bodyValue("MCP server not properly initialized");
        }
        if ("OPTIONS".equalsIgnoreCase(request.methodName())) {
            // Handle CORS preflight requests
            return ServerResponse.ok()
                    .header("Access-Control-Allow-Origin", "*")
                    .header("Access-Control-Allow-Headers", "Content-Type, Mcp-Session-Id, Authorization")
                    .header("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
                    .header("Access-Control-Max-Age", "3600")
                    .build();
        } else if ("GET".equalsIgnoreCase(request.methodName())) {
            // Streamable HTTP protocol does not support GET requests, return 405 error
            return ServerResponse.status(HttpStatus.METHOD_NOT_ALLOWED)
                    .header("Access-Control-Allow-Origin", "*")
                    .header("Access-Control-Allow-Headers", "Content-Type, Mcp-Session-Id, Authorization")
                    .header("Access-Control-Allow-Methods", "POST, OPTIONS")
                    .header("Allow", "POST, OPTIONS")
                    .contentType(MediaType.APPLICATION_JSON)
                    .bodyValue(new java.util.HashMap<String, Object>() {{
                            put("error", new java.util.HashMap<String, Object>() {{
                                    put("code", -32601);
                                    put("message", "Streamable HTTP does not support GET requests. Please use POST requests for all MCP operations.");
                                }});
                        }});
        } else if ("POST".equalsIgnoreCase(request.methodName())) {
            // Extract ServerWebExchange from ServerRequest
            final ServerWebExchange exchange = request.exchange();
            return handleMessageEndpoint(exchange, request).flatMap(result -> {
                ServerResponse.BodyBuilder builder = ServerResponse.status(HttpStatus.valueOf(result.getStatusCode()))
                        .header("Access-Control-Allow-Origin", "*")
                        .header("Access-Control-Allow-Headers", "Content-Type, Mcp-Session-Id, Authorization")
                        .header("Access-Control-Allow-Methods", "GET, POST, OPTIONS");
                if (Objects.nonNull(result.getSessionId())) {
                    builder.header(SESSION_ID_HEADER, result.getSessionId());
                }
                builder.contentType(MediaType.APPLICATION_JSON);
                return builder.bodyValue(result.getResponseBodyAsJson());
            });
        }
        return ServerResponse.badRequest().bodyValue(new McpError("Unsupported HTTP method"));
    }

    /**
     * Handles POST requests for message processing in Streamable HTTP protocol.
     * This method processes all MCP operations including initialize, tools/list, tools/call,
     * and other custom operations. Enhanced to support requests without sessionId (creates
     * temporary sessions) and invalid sessionId (creates new session and re-stores it).
     * The method distinguishes between initialize requests (which create new sessions)
     * and regular requests (which require existing session correlation or temporary session creation).
     *
     * @param exchange the server web exchange
     * @param request  the server request containing the message
     * @return a Mono containing the message handling result
     */
    public Mono<MessageHandlingResult> handleMessageEndpoint(final ServerWebExchange exchange, final ServerRequest request) {
        LOGGER.debug("Processing Streamable HTTP message for path: {}", request.path());
        return request.bodyToMono(String.class)
                .flatMap(body -> {
                    LOGGER.debug("Received request body with length: {} chars", body.length());
                    try {
                        // Deserialize JSON-RPC request
                        final McpSchema.JSONRPCMessage message = McpSchema.deserializeJsonRpcMessage(objectMapper, body);
                        LOGGER.debug("Parsed JSON-RPC message of type: {}", message.getClass().getSimpleName());
                        // Handle initialize requests specially
                        if (isInitializeRequest(message)) {
                            return handleInitializeRequest(exchange, message);
                        }
                        // Handle regular requests with session management enhancement
                        return handleRegularRequestWithEnhancement(exchange, message, request);
                    } catch (IOException e) {
                        LOGGER.warn("Failed to parse JSON-RPC message: {}", e.getMessage());
                        final Object errorResponse = createJsonRpcError(null, -32700, "Parse error: Invalid JSON-RPC message");
                        return Mono.just(new MessageHandlingResult(400, errorResponse, null));
                    } catch (Exception e) {
                        LOGGER.error("Unexpected error handling message: {}", e.getMessage(), e);
                        final Object errorResponse = createJsonRpcError(null, -32603, "Internal error: " + e.getMessage());
                        return Mono.just(new MessageHandlingResult(500, errorResponse, null));
                    }
                });
    }

    /**
     * Checks if the given message is an initialize request.
     *
     * @param message the JSON-RPC message to check
     * @return true if this is an initialize request
     */
    private boolean isInitializeRequest(final McpSchema.JSONRPCMessage message) {
        if (message instanceof io.modelcontextprotocol.spec.McpSchema.JSONRPCRequest) {
            final String method = ((io.modelcontextprotocol.spec.McpSchema.JSONRPCRequest) message).method();
            return INITIALIZE_METHOD.equals(method);
        }
        return false;
    }

    /**
     * Handles initialize requests by creating new sessions and transport connections.
     *
     * @param exchange the server web exchange
     * @param message  the initialize request message
     * @return a Mono containing the initialization result
     */
    private Mono<MessageHandlingResult> handleInitializeRequest(final ServerWebExchange exchange,
                                                                final McpSchema.JSONRPCMessage message) {
        try {
            // Create new session and transport
            final StreamableHttpSessionTransport transport = new StreamableHttpSessionTransport();
            final McpServerSession session = sessionFactory.create(transport);
            final String newSessionId = session.getId();
            LOGGER.debug("Created new MCP session: {}", newSessionId);
            // Store session and transport for reuse
            sessions.put(newSessionId, session);
            sessionTransports.put(newSessionId, transport);
            // Configure exchange attributes
            configureExchangeForSession(exchange, newSessionId);
            // Extract and validate protocol version
            final Object messageId = extractMessageId(message);
            final String clientProtocolVersion = extractProtocolVersionFromInitialize(message);
            if (!isSupportedProtocolVersion(clientProtocolVersion)) {
                LOGGER.warn("Unsupported protocol version requested: {}", clientProtocolVersion);
                cleanupInvalidSession(newSessionId);
                final Object errorResponse = createJsonRpcError(messageId, -32600,
                        "Unsupported protocol version. Supported versions: ['" + DEFAULT_PROTOCOL_VERSION + "']");
                return Mono.just(new MessageHandlingResult(400, errorResponse, null));
            }
            // Create initialize response
            final Object initializeResponse = createInitializeResponse(messageId, clientProtocolVersion, newSessionId);
            LOGGER.debug("Initialize request processed successfully for session: {}", newSessionId);
            return Mono.just(new MessageHandlingResult(200, initializeResponse, newSessionId));
        } catch (Exception e) {
            LOGGER.error("Error handling initialize request: {}", e.getMessage(), e);
            final Object errorResponse = createJsonRpcError(extractMessageId(message), -32603,
                    "Internal error during initialization: " + e.getMessage());
            return Mono.just(new MessageHandlingResult(500, errorResponse, null));
        }
    }

    /**
     * Handles regular (non-initialize) requests with comprehensive session management.
     * Implements session management strategy for missing session ID (creates temporary session),
     * invalid session ID (creates new session), and valid session ID (processes normally).
     * Provides automatic session recovery and ServerWebExchange correlation for tool callbacks.
     *
     * @param exchange the server web exchange
     * @param message  the JSON-RPC message
     * @param request  the server request
     * @return a Mono containing the processing result
     */
    private Mono<MessageHandlingResult> handleRegularRequestWithEnhancement(final ServerWebExchange exchange,
                                                                            final McpSchema.JSONRPCMessage message,
                                                                            final ServerRequest request) {
        final String requestedSessionId = extractSessionId(request);
        final Object messageId = extractMessageId(message);
        if (Objects.isNull(requestedSessionId)) {
            LOGGER.info("No sessionId provided, creating temporary session for request");
            return createTemporarySessionAndProcess(exchange, message, messageId);
        }
        final McpServerSession existingSession = sessions.get(requestedSessionId);
        if (Objects.isNull(existingSession)) {
            LOGGER.info("SessionId {} not found, creating new session and re-storing", requestedSessionId);
            return createSessionAndRestoreId(exchange, message, requestedSessionId, messageId);
        }
        LOGGER.debug("Processing request for existing session: {}", requestedSessionId);
        final ServerWebExchange existingExchange = ShenyuMcpExchangeHolder.get(requestedSessionId);
        LOGGER.debug("Checking exchange mapping for session {}: existing={}", requestedSessionId,
                Objects.nonNull(existingExchange) ? "present (ID: " + System.identityHashCode(existingExchange) + ")" : "null");
        if (Objects.isNull(ShenyuMcpExchangeHolder.get(requestedSessionId))) {
            LOGGER.info("Exchange mapping lost for session {}, re-binding new exchange (ID: {})",
                    requestedSessionId, System.identityHashCode(exchange));
            configureExchangeForSession(exchange, requestedSessionId);
            LOGGER.info("Re-bound ServerWebExchange to session {} after reconnect", requestedSessionId);
        } else {
            LOGGER.debug("Exchange mapping already exists for session {}, using existing exchange", requestedSessionId);
        }
        return processWithExistingSession(existingSession, requestedSessionId, message, messageId)
                .map(result -> {
                    if (!requestedSessionId.equals(result.getSessionId())) {
                        LOGGER.info("Returning actual session ID {} instead of requested ID {}", result.getSessionId(), requestedSessionId);
                        return new MessageHandlingResult(result.getStatusCode(), result.getResponseBody(), result.getSessionId());
                    }
                    return result;
                });
    }

    /**
     * Creates a temporary session for stateless requests.
     * This method handles requests that arrive without a session ID by creating a temporary
     * session that is automatically cleaned up after the request completes. The session is
     * fully initialized using async methods to avoid blocking the Netty event loop.
     * Process Flow:
     * - Create session with auto-generated ID from MCP framework
     * - Bind ServerWebExchange to session ID before initialization
     * - Perform async session initialization (initialize + notification)
     * - Process the business request
     * - Clean up session and exchange binding after completion
     *
     * @param exchange  the server web exchange
     * @param message   the JSON-RPC message
     * @param messageId the message ID for correlation
     * @return a Mono containing the processing result
     */
    private Mono<MessageHandlingResult> createTemporarySessionAndProcess(final ServerWebExchange exchange,
                                                                         final McpSchema.JSONRPCMessage message,
                                                                         final Object messageId) {
        try {
            final StreamableHttpSessionTransport tempTransport = new StreamableHttpSessionTransport();
            final McpServerSession tempSession = sessionFactory.create(tempTransport);
            final String actualSessionId = tempSession.getId();
            LOGGER.info("Created temporary session: {}", actualSessionId);
            sessions.put(actualSessionId, tempSession);
            sessionTransports.put(actualSessionId, tempTransport);
            configureExchangeForSession(exchange, actualSessionId);
            LOGGER.debug("Bound exchange to temporary session: {} before handshake", actualSessionId);
            initializeSessionDirectly(tempSession, actualSessionId);
            tempTransport.resetCapturedMessage();
            return processWithExistingSession(tempSession, actualSessionId, message, messageId)
                    .doFinally(signalType -> {
                        LOGGER.debug("Cleaning up temporary session: {} (signal: {})", actualSessionId, signalType);
                        removeSession(actualSessionId);
                        ShenyuMcpExchangeHolder.remove(actualSessionId);
                    })
                    .map(result -> new MessageHandlingResult(result.getStatusCode(), result.getResponseBody(), null));
        } catch (Exception e) {
            LOGGER.error("Error creating temporary session: {}", e.getMessage(), e);
            final Object errorResponse = createJsonRpcError(messageId, -32603,
                    "Internal error creating temporary session: " + e.getMessage());
            return Mono.just(new MessageHandlingResult(500, errorResponse, null));
        }
    }

    /**
     * Creates a new session for session restoration scenarios.
     * This method handles scenarios where a client provides a session ID that no longer
     * exists on the server (e.g., server restart, session timeout, network disconnection).
     * A new session is created using the MCP framework, which generates its own session ID.
     * The client receives the new session ID for subsequent requests.
     * Important: The MCP framework generates its own session IDs, so the
     * client's requested session ID may differ from the actual session ID returned.
     * The response includes the actual session ID that should be used for future requests.
     *
     * @param exchange           the server web exchange
     * @param message            the JSON-RPC message
     * @param requestedSessionId the sessionId that the client requested
     * @param messageId          the message ID for correlation
     * @return a Mono containing the processing result
     */
    private Mono<MessageHandlingResult> createSessionAndRestoreId(final ServerWebExchange exchange,
                                                                  final McpSchema.JSONRPCMessage message,
                                                                  final String requestedSessionId,
                                                                  final Object messageId) {
        try {
            final StreamableHttpSessionTransport newTransport = new StreamableHttpSessionTransport(requestedSessionId);
            final McpServerSession newSession = sessionFactory.create(newTransport);
            final String actualSessionId = newSession.getId();
            LOGGER.info("Created new session - requested ID: {}, actual ID: {}", requestedSessionId, actualSessionId);
            sessions.put(actualSessionId, newSession);
            sessionTransports.put(actualSessionId, newTransport);
            configureExchangeForSession(exchange, actualSessionId);
            LOGGER.debug("Bound exchange to restored session: {} before handshake", actualSessionId);
            initializeSessionDirectly(newSession, actualSessionId);
            newTransport.resetCapturedMessage();
            return processWithExistingSession(newSession, actualSessionId, message, messageId)
                    .map(result -> {
                        if (!actualSessionId.equals(requestedSessionId)) {
                            LOGGER.info("Returning actual session ID {} instead of requested ID {}", actualSessionId, requestedSessionId);
                            return new MessageHandlingResult(result.getStatusCode(), result.getResponseBody(), actualSessionId);
                        }
                        return result;
                    });
        } catch (Exception e) {
            LOGGER.error("Error creating session with restored ID {}: {}", requestedSessionId, e.getMessage(), e);
            final Object errorResponse = createJsonRpcError(messageId, -32603,
                    "Internal error restoring session: " + e.getMessage());
            return Mono.just(new MessageHandlingResult(500, errorResponse, requestedSessionId));
        }
    }

    /**
     * Processes a message with an existing session.
     * This method contains the core message processing logic that is shared
     * between regular requests, temporary sessions, and restored sessions.
     *
     * @param session   the MCP server session
     * @param sessionId the session identifier
     * @param message   the JSON-RPC message
     * @param messageId the message ID for correlation
     * @return a Mono containing the processing result
     */
    private Mono<MessageHandlingResult> processWithExistingSession(final McpServerSession session,
                                                                   final String sessionId,
                                                                   final McpSchema.JSONRPCMessage message,
                                                                   final Object messageId) {
        // Verify exchange is available before processing
        final ServerWebExchange verifyExchange = ShenyuMcpExchangeHolder.get(sessionId);
        if (Objects.isNull(verifyExchange)) {
            LOGGER.error("CRITICAL: No exchange found in ShenyuMcpExchangeHolder for session {} when processing business request. This will cause ToolCallback to fail.", sessionId);
        } else {
            LOGGER.debug("Exchange verification passed for session {} (exchange ID: {})",
                    sessionId, System.identityHashCode(verifyExchange));
        }
        final StreamableHttpSessionTransport transport = getSessionTransport(sessionId);
        // Let MCP framework handle the message - framework will send response through transport
        return session.handle(message)
                .cast(Object.class)
                .doOnSuccess(result -> LOGGER.debug("Successfully processed message for session: {}", sessionId))
                .then(waitForTransportResponse(transport, sessionId, messageId))
                .onErrorResume(error -> {
                    LOGGER.error("Error processing message for session {}: {}", sessionId, error.getMessage(), error);
                    final Object errorResponse = createJsonRpcError(messageId, -32603,
                            "Internal error: " + error.getMessage());
                    return Mono.just(new MessageHandlingResult(500, errorResponse, sessionId));
                });
    }

    /**
     * Configures exchange attributes for the given session.
     *
     * @param exchange  the server web exchange
     * @param sessionId the session identifier
     */
    private void configureExchangeForSession(final ServerWebExchange exchange, final String sessionId) {
        if (Objects.isNull(exchange)) {
            LOGGER.error("Attempted to configure null exchange for session: {}", sessionId);
            return;
        }
        exchange.getAttributes().put("MCP_SESSION_ID", sessionId);
        ShenyuMcpExchangeHolder.put(sessionId, exchange);
        // Verify exchange was stored correctly
        final ServerWebExchange storedExchange = ShenyuMcpExchangeHolder.get(sessionId);
        if (Objects.isNull(storedExchange)) {
            LOGGER.error("Failed to store exchange in ShenyuMcpExchangeHolder for session: {}", sessionId);
        } else {
            LOGGER.info("Successfully configured and stored exchange for session: {} (exchange ID: {})",
                    sessionId, System.identityHashCode(exchange));
        }
    }

    /**
     * Cleans up an invalid session that failed initialization.
     *
     * @param sessionId the session ID to clean up
     */
    private void cleanupInvalidSession(final String sessionId) {
        sessions.remove(sessionId);
        sessionTransports.remove(sessionId);
        LOGGER.debug("Cleaned up invalid session: {}", sessionId);
    }

    /**
     * Extracts the protocol version from an initialize request.
     *
     * @param message the initialize request message
     * @return the requested protocol version or default version
     */
    private String extractProtocolVersionFromInitialize(final McpSchema.JSONRPCMessage message) {
        if (message instanceof io.modelcontextprotocol.spec.McpSchema.JSONRPCRequest) {
            final Object params = ((io.modelcontextprotocol.spec.McpSchema.JSONRPCRequest) message).params();
            if (params instanceof Map) {
                final Map<?, ?> paramsMap = (Map<?, ?>) params;
                final Object protocolVersion = paramsMap.get("protocolVersion");
                return Objects.nonNull(protocolVersion) ? protocolVersion.toString() : DEFAULT_PROTOCOL_VERSION;
            }
        }
        return DEFAULT_PROTOCOL_VERSION;
    }

    /**
     * Checks if the specified protocol version is supported.
     *
     * @param version the protocol version to check
     * @return true if the version is supported
     */
    private boolean isSupportedProtocolVersion(final String version) {
        return DEFAULT_PROTOCOL_VERSION.equals(version);
    }

    /**
     * Creates a standard initialize response for successful initialization.
     *
     * @param messageId       the original message ID for correlation
     * @param protocolVersion the negotiated protocol version
     * @param sessionId       the created session ID
     * @return the initialize response object
     */
    private Object createInitializeResponse(final Object messageId, final String protocolVersion, final String sessionId) {
        final Map<String, Object> capabilities = new java.util.HashMap<>();
        final Map<String, Object> toolsCapability = new java.util.HashMap<>();
        toolsCapability.put("listChanged", true);
        capabilities.put("tools", toolsCapability);
        final Map<String, Object> serverInfo = new java.util.HashMap<>();
        serverInfo.put("name", SERVER_NAME);
        serverInfo.put("version", SERVER_VERSION);
        final Map<String, Object> result = new java.util.HashMap<>();
        result.put("protocolVersion", protocolVersion);
        result.put("capabilities", capabilities);
        result.put("serverInfo", serverInfo);
        result.put("instructions", "Use available tools to interact with Shenyu gateway services");
        result.put("sessionId", sessionId);
        return createJsonRpcResponse(messageId, result);
    }

    /**
     * Extracts the message ID from a JSON-RPC message for response correlation.
     *
     * @param message the JSON-RPC message
     * @return the message ID, or null if not available
     */
    private Object extractMessageId(final McpSchema.JSONRPCMessage message) {
        if (message instanceof io.modelcontextprotocol.spec.McpSchema.JSONRPCRequest) {
            return ((io.modelcontextprotocol.spec.McpSchema.JSONRPCRequest) message).id();
        }
        return null;
    }

    /**
     * Creates a standard JSON-RPC 2.0 success response.
     *
     * @param id     the message ID for correlation
     * @param result the result object
     * @return the response object
     */
    private Object createJsonRpcResponse(final Object id, final Object result) {
        final Map<String, Object> response = new java.util.HashMap<>();
        response.put("jsonrpc", JSONRPC_VERSION);
        if (Objects.nonNull(id)) {
            response.put("id", id);
        }
        response.put("result", Objects.nonNull(result) ? result : new java.util.HashMap<>());
        return response;
    }

    /**
     * Creates a standard JSON-RPC 2.0 error response.
     *
     * @param id      the message ID for correlation
     * @param code    the error code
     * @param message the error message
     * @return the error response object
     */
    private Object createJsonRpcError(final Object id, final int code, final String message) {
        final Map<String, Object> error = new java.util.HashMap<>();
        error.put("code", code);
        error.put("message", message);
        final Map<String, Object> response = new java.util.HashMap<>();
        response.put("jsonrpc", JSONRPC_VERSION);
        if (Objects.nonNull(id)) {
            response.put("id", id);
        }
        response.put("error", error);
        return response;
    }

    /**
     * Extracts the session ID from the request headers or query parameters.
     *
     * @param request the server request
     * @return the session ID, or null if not found
     */
    private String extractSessionId(final ServerRequest request) {
        String sessionId = request.queryParam("sessionId").orElse(null);
        if (Objects.nonNull(sessionId)) {
            return sessionId;
        }
        return request.headers().firstHeader(SESSION_ID_HEADER);
    }

    /**
     * Removes a session from the active sessions map.
     * This method should be called when a session is closed or becomes invalid
     * to prevent memory leaks and ensure proper cleanup.
     *
     * @param sessionId the session ID to remove
     */
    public void removeSession(final String sessionId) {
        final McpServerSession removedSession = sessions.remove(sessionId);
        final StreamableHttpSessionTransport removedTransport = sessionTransports.remove(sessionId);
        if (Objects.nonNull(removedSession) || Objects.nonNull(removedTransport)) {
            LOGGER.debug("Removed session and transport: {}", sessionId);
        }
    }

    /**
     * Gets the session transport for a given session ID.
     *
     * @param sessionId the session identifier
     * @return the session transport, or null if not found
     */
    private StreamableHttpSessionTransport getSessionTransport(final String sessionId) {
        return sessionTransports.get(sessionId);
    }

    /**
     * Waits for and retrieves the transport response for correlation with HTTP response.
     * Checks if the transport has captured a response from the MCP framework and returns it as a MessageHandlingResult.
     * If no response is available, returns a default success response to prevent hanging.
     *
     * @param transport the session transport to check for responses
     * @param sessionId the session identifier for logging
     * @param messageId the original message ID for correlation
     * @return a Mono containing the message handling result
     */
    private Mono<MessageHandlingResult> waitForTransportResponse(final StreamableHttpSessionTransport transport,
                                                                 final String sessionId,
                                                                 final Object messageId) {
        return Mono.fromCallable(() -> {
            if (Objects.nonNull(transport) && transport.isResponseReady() && Objects.nonNull(transport.getLastSentMessage())) {
                final McpSchema.JSONRPCMessage sentMessage = transport.getLastSentMessage();
                LOGGER.debug("Retrieved captured response from transport for session: {}", sessionId);
                return new MessageHandlingResult(200, sentMessage, sessionId);
            } else {
                LOGGER.debug("No response captured from transport, returning default success for session: {}", sessionId);
                final Object successResponse = createJsonRpcResponse(messageId, new java.util.HashMap<>());
                return new MessageHandlingResult(200, successResponse, sessionId);
            }
        });
    }

    /**
     * Performs backend session initialization using async MCP protocol handshake.
     * Simulates complete initialize request-response cycle to ensure session's internal
     * state machine transitions correctly. Uses async operations to avoid blocking Netty threads.
     *
     * @param session   the MCP server session
     * @param sessionId the session identifier
     */
    private void initializeSessionDirectly(final McpServerSession session, final String sessionId) {
        try {
            // Core strategy: Simulate complete initialize request-response cycle
            // This ensures the session's internal state machine transitions correctly
            LOGGER.debug("Starting backend initialization for session: {}", sessionId);
            // Create a proper initialize request
            final String initRequestJson = createInitializeRequest();
            final McpSchema.JSONRPCMessage initRequest = McpSchema.deserializeJsonRpcMessage(objectMapper, initRequestJson);
            LOGGER.debug("Created initialize request for session: {}", sessionId);
            // Use subscribe instead of block to avoid blocking in Netty thread
            session.handle(initRequest)
                    .doOnSuccess(v -> {
                        LOGGER.debug("Initialize request processed successfully for session: {}", sessionId);
                        // Complete the handshake by sending a notification to finalize session state
                        try {
                            final StreamableHttpSessionTransport transport = sessionTransports.get(sessionId);
                            if (Objects.nonNull(transport) && transport.isResponseReady()) {
                                LOGGER.debug("Initialize response captured, session {} should be ready", sessionId);
                                // Complete the handshake by sending a notification to finalize session state
                                completeInitializationHandshakeAsync(session, sessionId);
                            } else {
                                LOGGER.warn("No initialize response captured for session: {}", sessionId);
                            }
                        } catch (Exception e) {
                            LOGGER.error("Error checking initialize response for session {}: {}", sessionId, e.getMessage());
                        }
                    })
                    .doOnError(e -> {
                        LOGGER.error("Failed to process initialize request for session {}: {}", sessionId, e.getMessage(), e);
                    })
                    // Use subscribe instead of block to avoid blocking
                    .subscribe();
            LOGGER.debug("Backend initialization completed for session: {}", sessionId);
            // Verify initialization success
            if (verifySessionInitialized(session, sessionId)) {
                LOGGER.info("Session {} successfully initialized and ready for business requests", sessionId);
            } else {
                LOGGER.warn("Session {} initialization may be incomplete - proceeding anyway", sessionId);
            }
        } catch (Exception e) {
            LOGGER.error("Unexpected error during session initialization for {}: {}", sessionId, e.getMessage(), e);
        }
    }

    /**
     * Creates a proper initialize request JSON for backend session initialization.
     *
     * @return the initialize request JSON string
     */
    private String createInitializeRequest() {
        final Map<String, Object> params = new java.util.HashMap<>();
        params.put("protocolVersion", DEFAULT_PROTOCOL_VERSION);
        params.put("capabilities", new java.util.HashMap<String, Object>() {{
                put("roots", new java.util.ArrayList<>());
            }});
        params.put("clientInfo", new java.util.HashMap<String, Object>() {{
                put("name", "ShenyuMcpClient");
                put("version", "1.0.0");
            }});
        final Map<String, Object> request = new java.util.HashMap<>();
        request.put("jsonrpc", JSONRPC_VERSION);
        request.put("id", "__backend_init");
        request.put("method", INITIALIZE_METHOD);
        request.put("params", params);
        try {
            return objectMapper.writeValueAsString(request);
        } catch (Exception e) {
            LOGGER.error("Failed to create initialize request JSON: {}", e.getMessage());
            return "{\"jsonrpc\":\"2.0\",\"id\":\"__backend_init\",\"method\":\"initialize\",\"params\":{\"protocolVersion\":\"" + DEFAULT_PROTOCOL_VERSION + "\"}}";
        }
    }

    /**
     * Verifies that a session has been properly initialized.
     *
     * @param session   the MCP server session to verify
     * @param sessionId the session identifier for logging
     * @return true if the session appears to be initialized
     */
    private boolean verifySessionInitialized(final McpServerSession session, final String sessionId) {
        try {
            // Strategy 1: Check for 'initialized' field
            try {
                final java.lang.reflect.Field initializedField = session.getClass().getDeclaredField("initialized");
                initializedField.setAccessible(true);
                final Object value = initializedField.get(session);
                final boolean isInitialized = Boolean.TRUE.equals(value);
                LOGGER.debug("Session {} initialized field value: {}", sessionId, isInitialized);
                return isInitialized;
            } catch (NoSuchFieldException | IllegalAccessException e) {
                LOGGER.debug("No 'initialized' field found or accessible in session: {}", e.getMessage());
            }
            // Strategy 2: Check for 'state' field
            try {
                final java.lang.reflect.Field stateField = session.getClass().getDeclaredField("state");
                stateField.setAccessible(true);
                final Object stateValue = stateField.get(session);
                if (Objects.nonNull(stateValue)) {
                    final String stateStr = stateValue.toString().toLowerCase();
                    final boolean isInitialized = stateStr.contains("init") && !stateStr.contains("uninit");
                    LOGGER.debug("Session {} state value: {} (initialized: {})", sessionId, stateValue, isInitialized);
                    return isInitialized;
                }
            } catch (NoSuchFieldException | IllegalAccessException e) {
                LOGGER.debug("No 'state' field found or accessible in session: {}", e.getMessage());
            }
            // Strategy 3: Check transport response
            final StreamableHttpSessionTransport transport = sessionTransports.get(sessionId);
            if (Objects.nonNull(transport)) {
                final boolean hasResponse = transport.isResponseReady();
                LOGGER.debug("Session {} transport has response ready: {}", sessionId, hasResponse);
                // If we have a response, the session likely processed the initialize request
                return hasResponse;
            }
            LOGGER.debug("Unable to verify initialization state for session: {}", sessionId);
            // Unable to verify, assume not initialized
            return false;
        } catch (Exception e) {
            LOGGER.error("Error verifying session initialization for {}: {}", sessionId, e.getMessage());
            return false;
        }
    }

    /**
     * Completes the handshake by sending a notification to finalize session state asynchronously.
     * Uses multiple strategies to ensure session state is properly set, including sending
     * initialized notifications and using reflection as fallback.
     *
     * @param session   the MCP server session
     * @param sessionId the session identifier
     */
    private void completeInitializationHandshakeAsync(final McpServerSession session, final String sessionId) {
        try {
            // Strategy 1: Send a "initialized" notification to complete the handshake
            try {
                final Map<String, Object> notification = new java.util.HashMap<>();
                notification.put("jsonrpc", JSONRPC_VERSION);
                notification.put("method", "notifications/initialized");
                notification.put("params", new java.util.HashMap<>());
                final String notificationJson = objectMapper.writeValueAsString(notification);
                final McpSchema.JSONRPCMessage notificationMessage = McpSchema.deserializeJsonRpcMessage(objectMapper, notificationJson);
                session.handle(notificationMessage)
                        .doOnSuccess(v -> {
                            LOGGER.debug("Initialized notification sent successfully for session: {}", sessionId);
                        })
                        .doOnError(e -> {
                            LOGGER.debug("Initialized notification failed for session {}: {}", sessionId, e.getMessage());
                        })
                        .onErrorComplete()
                        .subscribe();
            } catch (Exception e) {
                LOGGER.debug("Strategy 1 failed: {}", e.getMessage());
            }
            // Strategy 2: Force state transition using reflection as a fallback
            try {
                final java.lang.reflect.Field initializedField = session.getClass().getDeclaredField("initialized");
                initializedField.setAccessible(true);
                initializedField.set(session, true);
                LOGGER.debug("Successfully set session {} to initialized state via reflection", sessionId);
            } catch (Exception e) {
                LOGGER.debug("Strategy 2 failed: {}", e.getMessage());
            }
            // Strategy 3: Try to set state field
            try {
                final java.lang.reflect.Field stateField = session.getClass().getDeclaredField("state");
                stateField.setAccessible(true);
                final Object stateValue = stateField.get(session);
                if (Objects.nonNull(stateValue) && stateValue.getClass().isEnum()) {
                    // Try to find INITIALIZED enum value
                    for (Object enumConstant : stateValue.getClass().getEnumConstants()) {
                        if (enumConstant.toString().toLowerCase().contains("init") && !enumConstant.toString().toLowerCase().contains("uninit")) {
                            stateField.set(session, enumConstant);
                            LOGGER.debug("Successfully set session {} state to {} via reflection", sessionId, enumConstant);
                            break;
                        }
                    }
                }
            } catch (Exception e) {
                LOGGER.debug("Strategy 3 failed: {}", e.getMessage());
            }
        } catch (Exception e) {
            LOGGER.error("Error completing handshake for session {}: {}", sessionId, e.getMessage(), e);
        }
    }

    /**
     * Session transport implementation for Streamable HTTP with proper lifecycle management.
     * This transport handles the communication between the MCP framework and the Streamable HTTP
     * protocol. It captures responses from the MCP session and makes them available for
     * HTTP response correlation.
     */
    private class StreamableHttpSessionTransport implements McpServerTransport {

        private final String sessionId;

        private volatile boolean closed;

        private volatile McpSchema.JSONRPCMessage lastSentMessage;

        private volatile boolean responseReady;

        /**
         * Creates a new session transport with auto-generated session ID.
         */
        StreamableHttpSessionTransport() {
            this.sessionId = java.util.UUID.randomUUID().toString();
            LOGGER.debug("Created StreamableHttpSessionTransport with auto-generated sessionId: {}", this.sessionId);
        }

        /**
         * Creates a new session transport with the specified session ID.
         *
         * @param sessionId the session identifier, or null to auto-generate
         */
        StreamableHttpSessionTransport(final String sessionId) {
            this.sessionId = Objects.nonNull(sessionId) ? sessionId : java.util.UUID.randomUUID().toString();
            LOGGER.debug("Created StreamableHttpSessionTransport with sessionId: {}", this.sessionId);
        }

        /**
         * Gets the last message sent through this transport.
         * This is used for Streamable HTTP response correlation, allowing the
         * transport provider to capture and return responses sent by the MCP framework.
         *
         * @return the last sent message, or null if no message has been sent
         */
        public McpSchema.JSONRPCMessage getLastSentMessage() {
            return lastSentMessage;
        }

        /**
         * Checks if a response is ready for retrieval.
         *
         * @return true if response is available
         */
        public boolean isResponseReady() {
            return responseReady;
        }

        @Override
        public Mono<Void> sendMessage(final McpSchema.JSONRPCMessage message) {
            if (!closed) {
                this.lastSentMessage = message;
                this.responseReady = true;
                LOGGER.debug("Captured response message for session: {}", sessionId);
            }
            return Mono.empty();
        }

        @Override
        public <T> T unmarshalFrom(final Object data, final TypeReference<T> typeRef) {
            try {
                return new ObjectMapper().convertValue(data, typeRef);
            } catch (Exception e) {
                throw Exceptions.propagate(e);
            }
        }

        @Override
        public Mono<Void> closeGracefully() {
            return Mono.fromRunnable(() -> {
                if (!closed) {
                    closed = true;
                    removeSession(sessionId);
                    LOGGER.debug("Session transport closed gracefully: {}", sessionId);
                }
            });
        }

        @Override
        public void close() {
            if (!closed) {
                closed = true;
                removeSession(sessionId);
                LOGGER.debug("Session transport closed immediately: {}", sessionId);
            }
        }

        /**
         * Clears the captured message and resets the response flag so that a subsequent
         * business request is not confused with the internal initialize handshake that
         * is executed during short-reconnect or temporary session creation.
         */
        public void resetCapturedMessage() {
            this.lastSentMessage = null;
            this.responseReady = false;
        }
    }

}