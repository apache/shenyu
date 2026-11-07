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

import com.fasterxml.jackson.databind.ObjectMapper;
import io.modelcontextprotocol.json.McpJsonMapper;
import io.modelcontextprotocol.json.TypeRef;
import io.modelcontextprotocol.json.jackson.JacksonMcpJsonMapper;
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
import reactor.core.Exceptions;
import reactor.core.publisher.Flux;
import reactor.core.publisher.FluxSink;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Scheduler;
import reactor.core.scheduler.Schedulers;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.atomic.LongAdder;

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

    /**
     * Scheduler used for output-stall detection of live sessions.
     */
    private static final Scheduler STALL_SCHEDULER = Schedulers.boundedElastic();

    private final ObjectMapper objectMapper;

    private final McpJsonMapper jsonMapper;

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
     * Bounded admission policy applied to every SSE session of this provider.
     */
    private final SseSessionPolicy policy;

    /**
     * Live session admission states, keyed by session id, for aggregate gauges and
     * deterministic release on provider shutdown.
     */
    private final ConcurrentHashMap<String, SseSessionState> liveStates = new ConcurrentHashMap<>();

    /**
     * Events accepted into the bounded per-session buffers.
     */
    private final LongAdder acceptedSends = new LongAdder();

    /**
     * Events rejected because a session's bounded buffer refused them.
     */
    private final LongAdder rejectedSends = new LongAdder();

    /**
     * Terminal transitions per bounded reason.
     */
    private final Map<SseSessionState.TerminalReason, LongAdder> terminalCounts = new ConcurrentHashMap<>();

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
        this(objectMapper, baseUrl, messageEndpoint, sseEndpoint, SseSessionPolicy.defaults());
    }

    /**
     * Constructs a new WebFlux SSE server transport provider instance with custom base
     * URL and bounded-session policy.
     *
     * @param objectMapper    The ObjectMapper to use for JSON
     *                        serialization/deserialization of MCP messages. Must
     *                        not be null.
     * @param baseUrl         The base URL for the message endpoint. Must not be null.
     * @param messageEndpoint The endpoint URI where clients should send their
     *                        JSON-RPC messages. This endpoint will be communicated
     *                        to clients during SSE connection setup. Must not be
     *                        null.
     * @param sseEndpoint     The SSE endpoint path. Must not be null.
     * @param policy          The bounded admission policy for SSE sessions. Must not
     *                        be null.
     * @throws IllegalArgumentException if any parameter is null
     */
    public ShenyuSseServerTransportProvider(final ObjectMapper objectMapper, final String baseUrl,
            final String messageEndpoint, final String sseEndpoint, final SseSessionPolicy policy) {
        Assert.notNull(objectMapper, "ObjectMapper must not be null");
        Assert.notNull(baseUrl, "Message base path must not be null");
        Assert.notNull(messageEndpoint, "Message endpoint must not be null");
        Assert.notNull(sseEndpoint, "SSE endpoint must not be null");
        Assert.notNull(policy, "Policy must not be null");

        this.objectMapper = objectMapper;
        this.jsonMapper = new JacksonMcpJsonMapper(this.objectMapper);
        this.baseUrl = baseUrl;
        this.messageEndpoint = messageEndpoint;
        this.sseEndpoint = sseEndpoint;
        this.policy = policy;
        for (SseSessionState.TerminalReason reason : SseSessionState.TerminalReason.values()) {
            this.terminalCounts.put(reason, new LongAdder());
        }
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

    /**
     * Closes all active sessions gracefully. This method sets the isClosing flag
     * to prevent new connections, terminates every bounded session state and closes
     * all existing sessions properly.
     *
     * @return A Mono that completes when all sessions have been closed
     */
    @Override
    public Mono<Void> closeGracefully() {
        isClosing = true;
        return Flux.fromIterable(sessions
                .values())
                .doFirst(() -> {
                    LOGGER.debug("Initiating graceful shutdown with {} active sessions", sessions.size());
                    liveStates.values().forEach(state ->
                            state.terminate(SseSessionState.TerminalReason.PROVIDER_CLOSED));
                    liveStates.clear();
                })
                .flatMap(McpServerSession::closeGracefully)
                .then()
                .doFinally(signalType -> {
                    sessions.keySet().forEach(ShenyuMcpExchangeHolder::remove);
                    sessions.clear();
                    liveStates.clear();
                });
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
                .body(newSessionStream(), ServerSentEvent.class);
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

        return newSessionStream()
                .doOnSubscribe(subscription -> LOGGER.info("SSE Flux subscribed"))
                .doOnRequest(n -> LOGGER.debug("SSE Flux requested {} items", n));
    }

    /**
     * Opens one bounded SSE session stream shared by both public SSE entry points.
     *
     * <p>Each subscription owns exactly one MCP session, one admission state and one
     * stream sink. Every accepted event flows through a release hook that frees its
     * reservation when the consumer pulls it out of the transport buffer, so the
     * pending budget tracks the only unbounded segment of the pipeline; downstream
     * writes stay bounded by Reactor Netty's socket-writability water marks. A client
     * disconnect (cancel or dispose) triggers the idempotent terminal path.</p>
     *
     * @return a flux of server-sent events bounded by the session policy
     */
    private Flux<ServerSentEvent<?>> newSessionStream() {
        return Flux.defer(() -> {
            AtomicReference<SseSessionState> stateRef = new AtomicReference<>();
            return Flux.<ServerSentEvent<?>>create(sink -> {
                try {
                    AtomicReference<McpServerSession> sessionRef = new AtomicReference<>();
                    SseSessionState state = new SseSessionState(policy, STALL_SCHEDULER,
                            reason -> terminateSession(sessionRef.get(), sink, reason));

                    WebFluxMcpSessionTransport sessionTransport = new WebFluxMcpSessionTransport(sink, state);
                    McpServerSession session = sessionFactory.create(sessionTransport);
                    sessionRef.set(session);
                    String sessionId = session.getId();

                    LOGGER.debug("Created new SSE connection for session: {}", sessionId);
                    sessions.put(sessionId, session);
                    liveStates.put(sessionId, state);
                    stateRef.set(state);

                    // Send initial endpoint event before any JSON-RPC traffic; it is
                    // emitted without a reservation and never consumes session budget
                    String endpointUrl = this.baseUrl + this.messageEndpoint + "?sessionId=" + sessionId;
                    LOGGER.debug("Sending initial endpoint event to session: {} at {}", sessionId, endpointUrl);
                    sink.next(ServerSentEvent.<String>builder()
                            .event(ENDPOINT_EVENT_TYPE)
                            .data(endpointUrl)
                            .build());

                    sink.onDispose(() -> state.terminate(SseSessionState.TerminalReason.CLIENT_DISCONNECTED));
                } catch (Exception e) {
                    LOGGER.error("Error creating SSE session", e);
                    sink.error(e);
                }
            }).doOnNext(event -> {
                SseSessionState state = stateRef.get();
                if (Objects.nonNull(state)) {
                    state.release();
                }
            });
        });
    }

    /**
     * Terminates one SSE session exactly once: removes the session and its exchange
     * holder entry, records the bounded terminal reason and completes the SSE stream.
     * Completing the stream closes this transport, which is the SDK's own definition of
     * closing the session ({@code McpServerSession.closeGracefully()} delegates to the
     * transport); outstanding operations of the session are therefore finished by their
     * own request timeouts and are never replayed. Safe to race with the dispose path
     * and with provider shutdown because the admission state performs the transition
     * atomically.
     *
     * @param session the MCP session to close, null only on a programming error
     * @param sink the stream sink of the session
     * @param reason why the session is terminated
     */
    private void terminateSession(final McpServerSession session, final FluxSink<ServerSentEvent<?>> sink,
                                  final SseSessionState.TerminalReason reason) {
        Objects.requireNonNull(session, "session must not be null");
        String sessionId = session.getId();
        liveStates.remove(sessionId);
        sessions.remove(sessionId);
        ShenyuMcpExchangeHolder.remove(sessionId);
        terminalCounts.get(reason).increment();
        LOGGER.warn("SSE session terminated: {}", reason);
        sink.complete();
    }

    /**
     * Number of sends accepted into bounded session buffers.
     *
     * @return the accepted-send count
     */
    long getAcceptedSendCount() {
        return acceptedSends.sum();
    }

    /**
     * Number of sends rejected by bounded session buffers.
     *
     * @return the rejected-send count
     */
    long getRejectedSendCount() {
        return rejectedSends.sum();
    }

    /**
     * Number of terminal transitions observed for a bounded reason.
     *
     * @param reason the terminal reason
     * @return the terminal count for that reason
     */
    long getTerminalCount(final SseSessionState.TerminalReason reason) {
        LongAdder counter = terminalCounts.get(reason);
        return Objects.isNull(counter) ? 0L : counter.sum();
    }

    /**
     * Total pending events across all live sessions.
     *
     * @return the aggregate pending-event count
     */
    long getTotalPendingEvents() {
        return liveStates.values().stream().mapToInt(SseSessionState::getPendingEvents).sum();
    }

    /**
     * Total pending serialized bytes across all live sessions.
     *
     * @return the aggregate pending-byte count
     */
    long getTotalPendingBytes() {
        return liveStates.values().stream().mapToLong(SseSessionState::getPendingBytes).sum();
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
                McpSchema.JSONRPCMessage message = McpSchema.deserializeJsonRpcMessage(jsonMapper, body);
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
     * Checks whether an SSE session is currently active.
     *
     * @param sessionId the session identifier
     * @return true when the session exists
     */
    public boolean hasSession(final String sessionId) {
        return Objects.nonNull(sessionId) && sessions.containsKey(sessionId);
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
                        McpSchema.JSONRPCMessage message = McpSchema.deserializeJsonRpcMessage(jsonMapper, body);
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

        private final SseSessionState sessionState;

        WebFluxMcpSessionTransport(final FluxSink<ServerSentEvent<?>> sink, final SseSessionState sessionState) {
            this.sink = sink;
            this.sessionState = sessionState;
        }

        @Override
        public Mono<Void> sendMessage(final McpSchema.JSONRPCMessage message) {
            return Mono.fromSupplier(() -> {
                try {
                    return jsonMapper.writeValueAsString(message);
                } catch (IOException e) {
                    throw Exceptions.propagate(e);
                }
            }).doOnError(e -> {
                // a serialization failure poisons the stream exactly as before
                Throwable exception = Exceptions.unwrap(e);
                sink.error(exception);
            }).flatMap(this::admit).then();
        }

        /**
         * Admits one serialized message into the bounded session buffer: reserve the
         * event slot and payload bytes, then hand the event to the stream. Completion
         * of the returned Mono therefore means bounded acceptance, not network
         * delivery; the consumer's progress releases the reservation through the
         * stream's release hook.
         *
         * @param jsonText the serialized JSON-RPC payload
         * @return a mono that completes after the event was accepted
         */
        private Mono<Void> admit(final String jsonText) {
            int byteLength = jsonText.getBytes(StandardCharsets.UTF_8).length;
            SseSessionState.Reservation reservation = sessionState.tryReserve(byteLength);
            if (reservation != SseSessionState.Reservation.ACCEPTED) {
                rejectedSends.increment();
                if (reservation == SseSessionState.Reservation.OVERFLOW) {
                    // the consumer stopped making progress: terminate the session
                    sessionState.terminate(SseSessionState.TerminalReason.OUTPUT_OVERFLOW);
                }
                return Mono.error(new SseSessionRejectException(reservation, byteLength));
            }
            try {
                if (sink.isCancelled()) {
                    sessionState.release();
                    sessionState.terminate(SseSessionState.TerminalReason.CLIENT_DISCONNECTED);
                    return Mono.error(new IllegalStateException("SSE stream was cancelled before delivery"));
                }
                sink.next(ServerSentEvent.builder().event(MESSAGE_EVENT_TYPE).data(jsonText).build());
                acceptedSends.increment();
                return Mono.empty();
            } catch (RuntimeException ex) {
                sessionState.release();
                throw ex;
            }
        }

        @Override
        public <T> T unmarshalFrom(final Object data, final TypeRef<T> typeRef) {
            return jsonMapper.convertValue(data, typeRef);
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

        private SseSessionPolicy policy = SseSessionPolicy.defaults();

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
         * Sets the bounded admission policy for SSE sessions.
         *
         * @param policy the policy. Must not be null.
         * @return this builder instance
         * @throws IllegalArgumentException if policy is null
         */
        public Builder policy(final SseSessionPolicy policy) {
            Assert.notNull(policy, "Policy must not be null");
            this.policy = policy;
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

            return new ShenyuSseServerTransportProvider(objectMapper, baseUrl, messageEndpoint, sseEndpoint, policy);
        }

    }

}
