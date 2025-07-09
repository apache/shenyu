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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.codec.ServerSentEvent;
import org.springframework.web.reactive.function.server.ServerRequest;
import org.springframework.web.reactive.function.server.ServerResponse;
import reactor.core.Exceptions;
import reactor.core.publisher.Flux;
import reactor.core.publisher.FluxSink;
import reactor.core.publisher.Mono;

import java.io.IOException;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

/**
 * ShenyuSseServerTransportProvider is a server transport provider for handling
 * Model Context Protocol (MCP) messages over Server-Sent Events (SSE) using
 * Spring WebFlux.
 * It allows clients to connect via SSE and receive real-time updates for MCP
 * messages.
 */
public class ShenyuSseServerTransportProvider implements McpServerTransportProvider {

    private static final Logger LOGGER = LoggerFactory.getLogger(ShenyuSseServerTransportProvider.class);

    /**
     * Event type for JSON-RPC messages sent through the SSE connection.
     */
    private static final String MESSAGE_EVENT_TYPE = "message";

    /**
     * Event type for sending the message endpoint URI to clients.
     */
    private static final String ENDPOINT_EVENT_TYPE = "endpoint";

    /**
     * Default SSE endpoint path as specified by the MCP transport specification.
     */
    private static final String DEFAULT_SSE_ENDPOINT = "/sse";

    private static final String DEFAULT_BASE_URL = "";

    private final ObjectMapper objectMapper;

    /**
     * Base URL for the message endpoint. This is used to construct the full URL for
     * clients to send their JSON-RPC messages.
     */
    private final String baseUrl;

    private final String messageEndpoint;

    private final String sseEndpoint;

    // private final RouterFunction<?> routerFunction;

    private McpServerSession.Factory sessionFactory;

    /**
     * Map of active client sessions, keyed by session ID.
     */
    private final ConcurrentHashMap<String, McpServerSession> sessions = new ConcurrentHashMap<>();

    /**
     * Flag indicating if the transport is shutting down.
     */
    private volatile boolean isClosing;

    /**
     * Constructs a new WebFlux SSE server transport provider instance.
     *
     * @param objectMapper    The ObjectMapper to use for JSON
     *                        serialization/deserialization of MCP messages. Must
     *                        not be null.
     * @param messageEndpoint The endpoint URI where clients should send their
     *                        JSON-RPC messages. This endpoint will be communicated
     *                        to clients during SSE connection setup. Must not be
     *                        null.
     * @throws IllegalArgumentException if either parameter is null
     */
    public ShenyuSseServerTransportProvider(final ObjectMapper objectMapper, final String messageEndpoint) {
        this(objectMapper, messageEndpoint, DEFAULT_SSE_ENDPOINT);
    }

    /**
     * Constructs a new WebFlux SSE server transport provider instance.
     *
     * @param objectMapper    The ObjectMapper to use for JSON
     *                        serialization/deserialization of MCP messages. Must
     *                        not be null.
     * @param messageEndpoint The endpoint URI where clients should send their
     *                        JSON-RPC messages. This endpoint will be communicated
     *                        to clients during SSE connection setup. Must not be
     *                        null.
     * @param sseEndpoint     The SSE endpoint path. Must not be null.
     * @throws IllegalArgumentException if either parameter is null
     */
    public ShenyuSseServerTransportProvider(final ObjectMapper objectMapper, final String messageEndpoint,
            final String sseEndpoint) {
        this(objectMapper, DEFAULT_BASE_URL, messageEndpoint, sseEndpoint);
    }

    /**
     * Constructs a new WebFlux SSE server transport provider instance with custom
     * base URL.
     *
     * @param objectMapper    The ObjectMapper to use for JSON
     *                        serialization/deserialization of MCP messages. Must
     *                        not be null.
     * @param baseUrl         The base URL for the message endpoint. Must not be
     *                        null.
     * @param messageEndpoint The endpoint URI where clients should send their
     *                        JSON-RPC messages. This endpoint will be communicated
     *                        to clients during SSE connection setup. Must not be
     *                        null.
     * @param sseEndpoint     The SSE endpoint path. Must not be null.
     * @throws IllegalArgumentException if any parameter is null
     */
    public ShenyuSseServerTransportProvider(final ObjectMapper objectMapper, final String baseUrl,
            final String messageEndpoint, final String sseEndpoint) {
        Assert.notNull(objectMapper, "ObjectMapper must not be null");
        Assert.notNull(baseUrl, "Message base path must not be null");
        Assert.notNull(messageEndpoint, "Message endpoint must not be null");
        Assert.notNull(sseEndpoint, "SSE endpoint must not be null");

        this.objectMapper = objectMapper;
        this.baseUrl = baseUrl;
        this.messageEndpoint = messageEndpoint;
        this.sseEndpoint = sseEndpoint;
        // this.routerFunction = RouterFunctions.route()
        // .GET(this.sseEndpoint, this::handleSseConnection)
        // .POST(this.messageEndpoint, this::handleMessage).build();
    }

    public static Builder builder() {
        return new Builder();
    }

    @Override
    public void setSessionFactory(final McpServerSession.Factory sessionFactory) {
        this.sessionFactory = sessionFactory;
    }

    /**
     * Checks if the transport is currently closing. This can be used to prevent new
     * connections or message processing while the transport is shutting down.
     *
     * @return true if the transport is closing, false otherwise
     */
    @Override
    public Mono<Void> notifyClients(final String method, final Object params) {
        if (sessions.isEmpty()) {
            LOGGER.debug("No active sessions to broadcast message to");
            return Mono.empty();
        }

        LOGGER.debug("Attempting to broadcast message to {} active sessions", sessions.size());

        return Flux.fromIterable(sessions.values())
                .flatMap(session -> session.sendNotification(method, params)
                        .doOnError(e -> LOGGER.error("Failed to send message to session {}: {}", session.getId(),
                                e.getMessage()))
                        .onErrorComplete())
                .then();
    }

    // FIXME: This javadoc makes claims about using isClosing flag but it's not
    // actually
    // doing that.

    /**
     * Closes all active sessions gracefully. This method should be called when the
     * transport is shutting down to ensure all sessions are closed properly.
     *
     * @return A Mono that completes when all sessions have been closed
     */
    @Override
    public Mono<Void> closeGracefully() {
        return Flux.fromIterable(sessions
                .values())
                .doFirst(() -> LOGGER.debug("Initiating graceful shutdown with {} active sessions", sessions.size()))
                .flatMap(McpServerSession::closeGracefully).then();
    }

    /**
     * Handles new SSE connection requests from clients. Creates a new session for
     * each
     * connection and sets up the SSE event stream.
     *
     * @param request The incoming server request
     * @return A Mono which emits a response with the SSE event stream
     */
    public Mono<ServerResponse> handleSseConnection(final ServerRequest request) {
        if (isClosing) {
            return ServerResponse.status(HttpStatus.SERVICE_UNAVAILABLE).bodyValue("Server is shutting down");
        }

        // Check if sessionFactory is available
        if (Objects.isNull(sessionFactory)) {
            LOGGER.error("SessionFactory is null - MCP server not properly initialized");
            return ServerResponse.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .bodyValue("MCP server not properly initialized");
        }

        return ServerResponse.ok().contentType(MediaType.TEXT_EVENT_STREAM)
                .body(Flux.<ServerSentEvent<?>>create(sink -> {
                    try {
                        WebFluxMcpSessionTransport sessionTransport = new WebFluxMcpSessionTransport(sink);

                        McpServerSession session = sessionFactory.create(sessionTransport);
                        String sessionId = session.getId();

                        LOGGER.debug("Created new SSE connection for session: {}", sessionId);
                        sessions.put(sessionId, session);

                        // Send initial endpoint event
                        LOGGER.debug("Sending initial endpoint event to session: {}", sessionId);
                        String endpointUrl = this.baseUrl + this.messageEndpoint + "?sessionId=" + sessionId;
                        LOGGER.debug("Endpoint URL: {}", endpointUrl);

                        ServerSentEvent<String> endpointEvent = ServerSentEvent.<String>builder()
                                .event(ENDPOINT_EVENT_TYPE)
                                .data(endpointUrl)
                                .build();

                        sink.next(endpointEvent);

                        sink.onCancel(() -> {
                            LOGGER.debug("Session {} cancelled", sessionId);
                            sessions.remove(sessionId);
                        });
                    } catch (Exception e) {
                        LOGGER.error("Error creating SSE session", e);
                        sink.error(e);
                    }
                }), ServerSentEvent.class);
    }

    /**
     * Creates SSE Flux directly for writing to exchange response.
     * This bypasses the ServerResponse wrapper for direct streaming.
     *
     * @param request The incoming server request
     * @return A Flux of ServerSentEvent objects
     */
    public Flux<ServerSentEvent<?>> createSseFlux(final ServerRequest request) {
        if (isClosing) {
            return Flux.error(new RuntimeException("Server is shutting down"));
        }

        // Check if sessionFactory is available
        if (Objects.isNull(sessionFactory)) {
            LOGGER.error("SessionFactory is null - MCP server not properly initialized");
            return Flux.error(new RuntimeException("MCP server not properly initialized"));
        }

        return Flux.<ServerSentEvent<?>>create(sink -> {
            try {
                WebFluxMcpSessionTransport sessionTransport = new WebFluxMcpSessionTransport(sink);

                McpServerSession session = sessionFactory.create(sessionTransport);
                String sessionId = session.getId();

                LOGGER.info("Created new SSE connection for session: {}", sessionId);
                sessions.put(sessionId, session);

                // Send initial endpoint event
                LOGGER.info("Sending initial endpoint event to session: {}", sessionId);
                String endpointUrl = this.baseUrl + this.messageEndpoint + "?sessionId=" + sessionId;
                LOGGER.info("Endpoint URL: {}", endpointUrl);

                ServerSentEvent<String> endpointEvent = ServerSentEvent.<String>builder()
                        .event(ENDPOINT_EVENT_TYPE)
                        .data(endpointUrl)
                        .build();

                LOGGER.info("Built endpoint event - Type: {}, Data: {}", endpointEvent.event(), endpointEvent.data());
                sink.next(endpointEvent);
                LOGGER.info("Successfully sent initial endpoint event for session: {}", sessionId);

                sink.onCancel(() -> {
                    LOGGER.info("Session {} cancelled by client", sessionId);
                    sessions.remove(sessionId);
                });

                sink.onDispose(() -> {
                    LOGGER.info("Session {} disposed", sessionId);
                    sessions.remove(sessionId);
                });

            } catch (Exception e) {
                LOGGER.error("Error creating SSE session", e);
                sink.error(e);
            }
        }).doOnSubscribe(subscription -> LOGGER.info("SSE Flux subscribed"))
                .doOnRequest(n -> LOGGER.debug("SSE Flux requested {} items", n));
    }

    /**
     * Handles incoming JSON-RPC messages from clients. This method processes the
     * message, validates the session ID, and routes the message to the appropriate
     * session handler.
     *
     * @param request The incoming server request containing the JSON-RPC message
     * @return A Mono which emits a response indicating the result of processing the
     *         message
     */
    public Mono<ServerResponse> handleMessage(final ServerRequest request) {
        if (isClosing) {
            return ServerResponse.status(HttpStatus.SERVICE_UNAVAILABLE).bodyValue("Server is shutting down");
        }

        if (request.queryParam("sessionId").isEmpty()) {
            return ServerResponse.badRequest().bodyValue(new McpError("Session ID missing in message endpoint"));
        }

        McpServerSession session = sessions.get(request.queryParam("sessionId").get());

        if (Objects.isNull(session)) {
            return ServerResponse.status(HttpStatus.NOT_FOUND)
                    .bodyValue(new McpError("Session not found: " + request.queryParam("sessionId").get()));
        }

        return request.bodyToMono(String.class).flatMap(body -> {
            try {
                McpSchema.JSONRPCMessage message = McpSchema.deserializeJsonRpcMessage(objectMapper, body);
                return session.handle(message).flatMap(response -> ServerResponse.ok().build()).onErrorResume(error -> {
                    LOGGER.error("Error processing  message: {}", error.getMessage());
                    // instead of signalling the error, just respond with 200 OK
                    // - the error is signalled on the SSE connection
                    // return ServerResponse.ok().build();
                    return ServerResponse.status(HttpStatus.INTERNAL_SERVER_ERROR)
                            .bodyValue(new McpError(error.getMessage()));
                });
            } catch (IllegalArgumentException | IOException e) {
                LOGGER.error("Failed to deserialize message: {}", e.getMessage());
                return ServerResponse.badRequest().bodyValue(new McpError("Invalid message format"));
            }
        });
    }

    /**
     * Creates a direct message handling result for writing to exchange response.
     * This bypasses the ServerResponse wrapper for direct response writing.
     *
     * @param request The incoming server request containing the JSON-RPC message
     * @return A Mono that provides the response body and status information
     */
    public Mono<MessageHandlingResult> handleMessageEndpoint(final ServerRequest request) {
        if (isClosing) {
            LOGGER.warn("Server is shutting down, rejecting message request");
            return Mono.just(new MessageHandlingResult(503, "Server is shutting down"));
        }

        if (request.queryParam("sessionId").isEmpty()) {
            LOGGER.warn("Session ID missing in message endpoint");
            return Mono.just(new MessageHandlingResult(400, "Session ID missing in message endpoint"));
        }

        String sessionId = request.queryParam("sessionId").get();
        McpServerSession session = sessions.get(sessionId);

        if (Objects.isNull(session)) {
            LOGGER.warn("Session not found: {}", sessionId);
            return Mono.just(new MessageHandlingResult(404, "Session not found: " + sessionId));
        }

        LOGGER.info("Processing message for session: {}", sessionId);

        return request.bodyToMono(String.class)
                .flatMap(body -> {
                    try {
                        LOGGER.debug("Received message body: {}", body);
                        McpSchema.JSONRPCMessage message = McpSchema.deserializeJsonRpcMessage(objectMapper, body);
                        LOGGER.info("Deserialized JSON-RPC message for session: {}", sessionId);

                        return session.handle(message)
                                .doOnSuccess(result -> LOGGER.info("Successfully processed message for session: {}",
                                        sessionId))
                                .map(response -> new MessageHandlingResult(200, "Message processed successfully"))
                                .onErrorResume(error -> {
                                    LOGGER.error("Error processing message for session {}: {}", sessionId,
                                            error.getMessage());
                                    return Mono.just(new MessageHandlingResult(500,
                                            "Error processing message: " + error.getMessage()));
                                });
                    } catch (IllegalArgumentException | IOException e) {
                        LOGGER.error("Failed to deserialize message for session {}: {}", sessionId, e.getMessage());
                        return Mono.just(new MessageHandlingResult(400, "Invalid message format: " + e.getMessage()));
                    }
                })
                .onErrorResume(error -> {
                    LOGGER.error("Unexpected error handling message for session {}: {}", sessionId, error.getMessage());
                    return Mono.just(new MessageHandlingResult(500, "Unexpected error: " + error.getMessage()));
                });
    }

    /**
     * Result class for direct message handling.
     */
    public static class MessageHandlingResult {

        private final int statusCode;

        private final String responseBody;

        public MessageHandlingResult(final int statusCode, final String responseBody) {
            this.statusCode = statusCode;
            this.responseBody = responseBody;
        }

        public int getStatusCode() {
            return statusCode;
        }

        public String getResponseBody() {
            return responseBody;
        }
    }

    private class WebFluxMcpSessionTransport implements McpServerTransport {

        private final FluxSink<ServerSentEvent<?>> sink;

        WebFluxMcpSessionTransport(final FluxSink<ServerSentEvent<?>> sink) {
            this.sink = sink;
        }

        @Override
        public Mono<Void> sendMessage(final McpSchema.JSONRPCMessage message) {
            return Mono.fromSupplier(() -> {
                try {
                    return objectMapper.writeValueAsString(message);
                } catch (IOException e) {
                    throw Exceptions.propagate(e);
                }
            }).doOnNext(jsonText -> {
                ServerSentEvent<Object> event = ServerSentEvent.builder().event(MESSAGE_EVENT_TYPE).data(jsonText)
                        .build();
                sink.next(event);
            }).doOnError(e -> {
                // TODO log with sessionid
                Throwable exception = Exceptions.unwrap(e);
                sink.error(exception);
            }).then();
        }

        @Override
        public <T> T unmarshalFrom(final Object data, final TypeReference<T> typeRef) {
            return objectMapper.convertValue(data, typeRef);
        }

        @Override
        public Mono<Void> closeGracefully() {
            return Mono.fromRunnable(sink::complete);
        }

        @Override
        public void close() {
            sink.complete();
        }

    }

    /**
     * Builder class for constructing instances of
     * {@link ShenyuSseServerTransportProvider}.
     * This builder allows for setting the ObjectMapper, base URL, message endpoint,
     * and SSE endpoint.
     */
    public static class Builder {

        private ObjectMapper objectMapper;

        private String baseUrl = DEFAULT_BASE_URL;

        private String messageEndpoint;

        private String sseEndpoint = DEFAULT_SSE_ENDPOINT;

        /**
         * Sets the ObjectMapper to use for JSON serialization/deserialization of MCP
         * messages.
         *
         * @param objectMapper The ObjectMapper instance. Must not be null.
         * @return this builder instance
         * @throws IllegalArgumentException if objectMapper is null
         */
        public Builder objectMapper(final ObjectMapper objectMapper) {
            Assert.notNull(objectMapper, "ObjectMapper must not be null");
            this.objectMapper = objectMapper;
            return this;
        }

        /**
         * Sets the project basePath as endpoint prefix where clients should send their
         * JSON-RPC messages.
         *
         * @param baseUrl the message basePath . Must not be null.
         * @return this builder instance
         * @throws IllegalArgumentException if basePath is null
         */
        public Builder basePath(final String baseUrl) {
            Assert.notNull(baseUrl, "basePath must not be null");
            this.baseUrl = baseUrl;
            return this;
        }

        /**
         * Sets the endpoint URI where clients should send their JSON-RPC messages.
         *
         * @param messageEndpoint The message endpoint URI. Must not be null.
         * @return this builder instance
         * @throws IllegalArgumentException if messageEndpoint is null
         */
        public Builder messageEndpoint(final String messageEndpoint) {
            Assert.notNull(messageEndpoint, "Message endpoint must not be null");
            this.messageEndpoint = messageEndpoint;
            return this;
        }

        /**
         * Sets the SSE endpoint path.
         *
         * @param sseEndpoint The SSE endpoint path. Must not be null.
         * @return this builder instance
         * @throws IllegalArgumentException if sseEndpoint is null
         */
        public Builder sseEndpoint(final String sseEndpoint) {
            Assert.notNull(sseEndpoint, "SSE endpoint must not be null");
            this.sseEndpoint = sseEndpoint;
            return this;
        }

        /**
         * Builds a new instance of {@link ShenyuSseServerTransportProvider} with the
         * configured settings.
         *
         * @return A new WebFluxSseServerTransportProvider instance
         * @throws IllegalStateException if required parameters are not set
         */
        public ShenyuSseServerTransportProvider build() {
            Assert.notNull(objectMapper, "ObjectMapper must be set");
            Assert.notNull(messageEndpoint, "Message endpoint must be set");

            return new ShenyuSseServerTransportProvider(objectMapper, baseUrl, messageEndpoint, sseEndpoint);
        }

    }

}
