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
import org.springframework.http.codec.ServerSentEvent;
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
 * Shenyu Streamable HTTP Server Transport Provider for MCP (Model Context Protocol).
 * <p>
 * This transport provider implements the Streamable HTTP MCP transport protocol, which is an 
 * upgraded version of SSE that supports unified endpoint with dynamic streaming capabilities.
 * Unlike traditional SSE that requires separate endpoints for different operations, Streamable HTTP
 * uses a single endpoint to handle all MCP operations through HTTP POST requests.
 * </p>
 *
 * <h3>Key Features:</h3>
 * <ul>
 *   <li><strong>Unified Endpoint:</strong> Single endpoint (/mcp) handles all MCP requests</li>
 *   <li><strong>Session Management:</strong> Persistent sessions with Mcp-Session-Id header correlation</li>
 *   <li><strong>Protocol Upgrade:</strong> Dynamic switching between regular HTTP and streaming responses</li>
 *   <li><strong>Recovery Support:</strong> Last-Event-ID header support for connection recovery</li>
 *   <li><strong>Session Reuse:</strong> Sessions can be reused across multiple requests</li>
 *   <li><strong>Error Handling:</strong> Comprehensive error handling with JSON-RPC 2.0 compliance</li>
 * </ul>
 *
 * <h3>Protocol Flow:</h3>
 * <ol>
 *   <li>Client sends initialize request via POST to unified endpoint</li>
 *   <li>Server creates new session and returns session ID in response headers</li>
 *   <li>Subsequent requests include session ID for correlation</li>
 *   <li>Server processes requests through existing session context</li>
 *   <li>Responses can be immediate JSON or streaming depending on operation</li>
 * </ol>
 *
 * <h3>Supported Operations:</h3>
 * <ul>
 *   <li>initialize - Creates new MCP session</li>
 *   <li>tools/list - Lists available tools</li>
 *   <li>tools/call - Executes tool calls</li>
 *   <li>Custom operations through extension mechanism</li>
 * </ul>
 *
 * @see McpServerTransportProvider
 * @see McpServerSession
 * @since 1.0.0
 */
public class ShenyuStreamableHttpServerTransportProvider implements McpServerTransportProvider {

    private static final Logger LOGGER = LoggerFactory.getLogger(ShenyuStreamableHttpServerTransportProvider.class);

    /**
     * Event type for JSON-RPC messages sent through SSE connection.
     */
    private static final String MESSAGE_EVENT_TYPE = "message";

    /**
     * Event type for progress updates during streaming operations.
     */
    private static final String PROGRESS_EVENT_TYPE = "progress";

    /**
     * Event type for operation completion events.
     */
    private static final String COMPLETE_EVENT_TYPE = "complete";

    /**
     * Default unified endpoint path for Streamable HTTP protocol.
     */
    private static final String DEFAULT_ENDPOINT = "/mcp";

    /**
     * Header name for MCP session identification.
     */
    private static final String SESSION_ID_HEADER = "Mcp-Session-Id";

    /**
     * Header name for last event ID (used for connection recovery).
     */
    private static final String LAST_EVENT_ID_HEADER = "Last-Event-ID";

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
    private final String endpoint;
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
        this.endpoint = endpoint;

        LOGGER.debug("Created Streamable HTTP transport provider for endpoint: {}", endpoint);
    }

    /**
     * Creates a new builder instance for constructing transport providers.
     *
     * @return a new Builder instance
     */
    public static Builder builder() {
        return new Builder();
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
                    .header("Access-Control-Allow-Headers", "Content-Type, Mcp-Session-Id, Authorization, Last-Event-ID")
                    .header("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
                    .header("Access-Control-Max-Age", "3600")
                    .build();
        } else if ("GET".equalsIgnoreCase(request.methodName())) {
            // Streamable HTTP protocol does not support GET requests, return 405 error
            return ServerResponse.status(HttpStatus.METHOD_NOT_ALLOWED)
                    .header("Access-Control-Allow-Origin", "*")
                    .header("Access-Control-Allow-Headers", "Content-Type, Mcp-Session-Id, Authorization, Last-Event-ID")
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
            //todo hear need a exchange
            return handleMessageEndpoint(null, request).flatMap(result -> {
                ServerResponse.BodyBuilder builder = ServerResponse.status(HttpStatus.valueOf(result.getStatusCode()))
                        .header("Access-Control-Allow-Origin", "*")
                        .header("Access-Control-Allow-Headers", "Content-Type, Mcp-Session-Id, Authorization, Last-Event-ID")
                        .header("Access-Control-Allow-Methods", "GET, POST, OPTIONS");

                if (result.getSessionId() != null) {
                    builder.header(SESSION_ID_HEADER, result.getSessionId());
                }

                // 统一使用 application/json 格式返回响应
                builder.contentType(MediaType.APPLICATION_JSON);
                return builder.bodyValue(result.getResponseBodyAsJson());
            });
        }
        return ServerResponse.badRequest().bodyValue(new McpError("Unsupported HTTP method"));
    }

    /**
     * Creates SSE Flux for writing to exchange response.
     * <p>
     * Note: Streamable HTTP protocol does not support GET streaming connections.
     * This method returns an error event instead of throwing an exception to maintain
     * compatibility with SSE infrastructure.
     * </p>
     *
     * @param request the server request
     * @return a Flux containing error event for unsupported operation
     */
    public Flux<ServerSentEvent<?>> createSseFlux(final ServerRequest request) {
        LOGGER.debug("Streamable HTTP does not support GET streaming for request: {}", request.path());

        // Return SSE event containing error information instead of throwing exception
        final ServerSentEvent<String> errorEvent = ServerSentEvent.<String>builder()
                .event("error")
                .data("{\"error\":{\"code\":-32601,\"message\":\"Streamable HTTP does not support GET streaming connection. Please use POST requests.\"}}")
                .build();

        return Flux.just(errorEvent);
    }

    /**
     * Handles POST requests for message processing in Streamable HTTP protocol.
     * <p>
     * This method processes all MCP operations including:
     * <ul>
     *   <li>initialize - Creates new sessions</li>
     *   <li>tools/list - Lists available tools</li>
     *   <li>tools/call - Executes tool operations</li>
     *   <li>Other custom operations</li>
     * </ul>
     * </p>
     * <p>
     * The method distinguishes between initialize requests (which create new sessions)
     * and regular requests (which require existing session correlation).
     * </p>
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
                            return handleInitializeRequest(exchange, message, request);
                        }

                        // Handle regular requests with existing session
                        return handleRegularRequest(message, request);

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
     * @param request  the server request
     * @return a Mono containing the initialization result
     */
    private Mono<MessageHandlingResult> handleInitializeRequest(final ServerWebExchange exchange,
                                                                final McpSchema.JSONRPCMessage message,
                                                                final ServerRequest request) {
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
     * Handles regular (non-initialize) requests using existing session context.
     *
     * @param message the JSON-RPC message
     * @param request the server request
     * @return a Mono containing the processing result
     */
    private Mono<MessageHandlingResult> handleRegularRequest(final McpSchema.JSONRPCMessage message,
                                                             final ServerRequest request) {

        final String sessionId = extractSessionId(request);
        if (sessionId == null) {
            LOGGER.warn("Session ID missing for non-initialize request");
            final Object errorResponse = createJsonRpcError(extractMessageId(message), -32600,
                    "Session ID required for non-initialize requests");
            return Mono.just(new MessageHandlingResult(400, errorResponse, null));
        }

        final McpServerSession session = sessions.get(sessionId);
        if (session == null) {
            LOGGER.warn("Session not found: {}", sessionId);
            final Object errorResponse = createJsonRpcError(extractMessageId(message), -32002,
                    "Session not found: " + sessionId);
            return Mono.just(new MessageHandlingResult(404, errorResponse, sessionId));
        }

        LOGGER.debug("Processing request for existing session: {}", sessionId);

        final StreamableHttpSessionTransport transport = getSessionTransport(sessionId);

        // Let MCP framework handle the message - framework will send response through transport
        return session.handle(message)
                .cast(Object.class)
                .doOnSuccess(result -> LOGGER.debug("Successfully processed message for session: {}", sessionId))
                .then(waitForTransportResponse(transport, sessionId, extractMessageId(message)))
                .onErrorResume(error -> {
                    LOGGER.error("Error processing message for session {}: {}", sessionId, error.getMessage(), error);
                    final Object errorResponse = createJsonRpcError(extractMessageId(message), -32603,
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
        exchange.getAttributes().put("MCP_SESSION_ID", sessionId);
        ShenyuMcpExchangeHolder.put(sessionId, exchange);
        LOGGER.debug("Configured exchange for session: {}", sessionId);
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
                return protocolVersion != null ? protocolVersion.toString() : DEFAULT_PROTOCOL_VERSION;
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
        if (id != null) {
            response.put("id", id);
        }
        response.put("result", result != null ? result : new java.util.HashMap<>());
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
        if (id != null) {
            response.put("id", id);
        }
        response.put("error", error);
        return response;
    }

    /**
     * Extracts the session ID from the request headers or query parameters.
     * <p>
     * Searches in the following order:
     * <ol>
     *   <li>Query parameter "sessionId"</li>
     *   <li>Header "Mcp-Session-Id"</li>
     *   <li>Authorization header (Bearer token)</li>
     * </ol>
     * </p>
     *
     * @param request the server request
     * @return the session ID, or null if not found
     */
    private String extractSessionId(final ServerRequest request) {
        // Try query parameters first
        String sessionId = request.queryParam("sessionId").orElse(null);
        if (sessionId != null) {
            return sessionId;
        }

        // Try Mcp-Session-Id header
        sessionId = request.headers().firstHeader(SESSION_ID_HEADER);
        if (sessionId != null) {
            return sessionId;
        }

        // Try Authorization header as fallback
        final String authHeader = request.headers().firstHeader("Authorization");
        if (authHeader != null && authHeader.startsWith("Bearer ")) {
            return authHeader.substring(7);
        }

        return null;
    }

    /**
     * Serializes an object to JSON string with error handling.
     *
     * @param obj the object to serialize
     * @return the JSON string, or empty object on error
     */
    private String objectToJson(final Object obj) {
        try {
            return objectMapper.writeValueAsString(obj);
        } catch (IOException e) {
            LOGGER.error("Failed to serialize object to JSON: {}", e.getMessage());
            return "{}";
        }
    }

    /**
     * Session transport implementation for Streamable HTTP with proper lifecycle management.
     * <p>
     * This transport handles the communication between the MCP framework and the Streamable HTTP
     * protocol. It captures responses from the MCP session and makes them available for
     * HTTP response correlation.
     * </p>
     * <p>
     * Key responsibilities:
     * <ul>
     *   <li>Message sending through sendMessage()</li>
     *   <li>Response capture and correlation</li>
     *   <li>Session lifecycle management</li>
     *   <li>Object unmarshalling support</li>
     * </ul>
     * </p>
     */
    private class StreamableHttpSessionTransport implements McpServerTransport {

        private final String sessionId;
        private volatile boolean closed = false;
        private volatile McpSchema.JSONRPCMessage lastSentMessage = null;
        private volatile boolean responseReady = false;

        /**
         * Creates a new session transport with auto-generated session ID.
         */
        public StreamableHttpSessionTransport() {
            this.sessionId = java.util.UUID.randomUUID().toString();
        }

        /**
         * Creates a new session transport with the specified session ID.
         *
         * @param sessionId the session identifier, or null to auto-generate
         */
        public StreamableHttpSessionTransport(final String sessionId) {
            this.sessionId = sessionId != null ? sessionId : java.util.UUID.randomUUID().toString();
        }

        /**
         * Gets the session identifier for this transport.
         *
         * @return the session ID
         */
        public String getSessionId() {
            return sessionId;
        }

        /**
         * Gets the last message sent through this transport.
         * <p>
         * This is used for Streamable HTTP response correlation, allowing the
         * transport provider to capture and return responses sent by the MCP framework.
         * </p>
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
         * Checks if this transport is closed.
         *
         * @return true if the transport is closed
         */
        public boolean isClosed() {
            return closed;
        }
    }

    /**
     * Result object for message handling operations.
     * <p>
     * This class encapsulates the result of processing a Streamable HTTP message,
     * including the HTTP status code, response body, and session correlation information.
     * </p>
     */
    public static class MessageHandlingResult {

        private final int statusCode;
        private final Object responseBody;
        private final String sessionId;

        /**
         * Creates a new message handling result.
         *
         * @param statusCode   the HTTP status code for the response
         * @param responseBody the response body object
         * @param sessionId    the session identifier for correlation (nullable)
         */
        public MessageHandlingResult(final int statusCode, final Object responseBody, final String sessionId) {
            this.statusCode = statusCode;
            this.responseBody = responseBody;
            this.sessionId = sessionId;
        }

        /**
         * Gets the HTTP status code for this result.
         *
         * @return the status code
         */
        public int getStatusCode() {
            return statusCode;
        }

        /**
         * Gets the response body object.
         *
         * @return the response body
         */
        public Object getResponseBody() {
            return responseBody;
        }

        /**
         * Gets the session identifier associated with this result.
         *
         * @return the session ID, or null if not available
         */
        public String getSessionId() {
            return sessionId;
        }

        /**
         * Converts the response body to a JSON string for HTTP response transmission.
         * <p>
         * If the response body is already a string, returns it as-is. Otherwise,
         * attempts to serialize it to JSON using ObjectMapper. On serialization failure,
         * returns a standard JSON-RPC error response.
         * </p>
         *
         * @return the response body as JSON string
         */
        public String getResponseBodyAsJson() {
            if (responseBody instanceof String) {
                return (String) responseBody;
            }

            try {
                return new ObjectMapper().writeValueAsString(responseBody);
            } catch (Exception e) {
                LOGGER.error("Failed to serialize response body to JSON: {}", e.getMessage());
                return "{\"jsonrpc\":\"2.0\",\"error\":{\"code\":-32603,\"message\":\"Internal error\"}}";
            }
        }
    }

    /**
     * Builder class for constructing instances of ShenyuStreamableHttpServerTransportProvider.
     * <p>
     * This builder provides a fluent interface for configuring transport provider instances
     * with proper validation and default values.
     * </p>
     *
     * <h3>Usage Example:</h3>
     * <pre>
     * ShenyuStreamableHttpServerTransportProvider provider = 
     *     ShenyuStreamableHttpServerTransportProvider.builder()
     *         .objectMapper(new ObjectMapper())
     *         .endpoint("/mcp")
     *         .build();
     * </pre>
     */
    public static class Builder {

        private ObjectMapper objectMapper;
        private String endpoint = DEFAULT_ENDPOINT;

        /**
         * Sets the ObjectMapper for JSON serialization/deserialization.
         *
         * @param objectMapper the ObjectMapper instance (required)
         * @return this builder for method chaining
         * @throws IllegalArgumentException if objectMapper is null
         */
        public Builder objectMapper(final ObjectMapper objectMapper) {
            Assert.notNull(objectMapper, "ObjectMapper must not be null");
            this.objectMapper = objectMapper;
            return this;
        }

        /**
         * Sets the endpoint path for the transport provider.
         *
         * @param endpoint the endpoint path (defaults to "/mcp" if not specified)
         * @return this builder for method chaining
         * @throws IllegalArgumentException if endpoint is null
         */
        public Builder endpoint(final String endpoint) {
            Assert.notNull(endpoint, "Endpoint must not be null");
            this.endpoint = endpoint;
            return this;
        }

        /**
         * Builds a new ShenyuStreamableHttpServerTransportProvider instance.
         *
         * @return the configured transport provider
         * @throws IllegalStateException if required configuration is missing
         */
        public ShenyuStreamableHttpServerTransportProvider build() {
            Assert.notNull(objectMapper, "ObjectMapper must be configured");
            return new ShenyuStreamableHttpServerTransportProvider(objectMapper, endpoint);
        }
    }

    /**
     * Removes a session from the active sessions map.
     * <p>
     * This method should be called when a session is closed or becomes invalid
     * to prevent memory leaks and ensure proper cleanup.
     * </p>
     *
     * @param sessionId the session ID to remove
     */
    public void removeSession(final String sessionId) {
        final McpServerSession removedSession = sessions.remove(sessionId);
        final StreamableHttpSessionTransport removedTransport = sessionTransports.remove(sessionId);

        if (removedSession != null || removedTransport != null) {
            LOGGER.debug("Removed session and transport: {}", sessionId);
        }
    }

    /**
     * Gets the number of active sessions currently managed by this transport provider.
     *
     * @return the count of active sessions
     */
    public int getActiveSessionCount() {
        return sessions.size();
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
     * <p>
     * This method checks if the transport has captured a response from the MCP framework
     * and returns it as a MessageHandlingResult. If no response is available, it returns
     * a default success response to prevent hanging.
     * </p>
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
            if (transport != null && transport.isResponseReady() && transport.getLastSentMessage() != null) {
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
}