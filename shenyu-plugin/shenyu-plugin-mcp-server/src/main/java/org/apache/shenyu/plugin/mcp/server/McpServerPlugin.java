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

package org.apache.shenyu.plugin.mcp.server;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.utils.RequestUrlUtils;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.mcp.server.handler.McpServerPluginDataHandler;
import org.apache.shenyu.plugin.mcp.server.holder.ShenyuMcpExchangeHolder;
import org.apache.shenyu.plugin.mcp.server.manager.ShenyuMcpServerManager;
import org.apache.shenyu.plugin.mcp.server.model.ShenyuMcpServer;
import org.apache.shenyu.plugin.mcp.server.transport.ShenyuSseServerTransportProvider;
import org.apache.shenyu.plugin.mcp.server.transport.ShenyuStreamableHttpServerTransportProvider;
import org.apache.shenyu.plugin.mcp.server.transport.SseEventFormatter;
import org.apache.shenyu.plugin.mcp.server.transport.MessageHandlingResult;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.codec.HttpMessageReader;
import org.springframework.web.reactive.function.server.ServerRequest;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.nio.charset.StandardCharsets;

/**
 * MCP (Model Context Protocol) Server Plugin for Shenyu Gateway.
 *
 * <p>Provides MCP server functionality supporting both SSE and Streamable HTTP transport protocols.
 * Enables AI models to interact with Shenyu Gateway services through standardized MCP tool definitions.</p>
 *
 * @see org.apache.shenyu.plugin.base.AbstractShenyuPlugin
 * @see org.apache.shenyu.plugin.mcp.server.manager.ShenyuMcpServerManager
 * @since 2.7.0.2
 */
public class McpServerPlugin extends AbstractShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(McpServerPlugin.class);

    /**
     * Standard message endpoint path.
     */
    private static final String MESSAGE_ENDPOINT = "/message";

    /**
     * Streamable HTTP protocol path identifier.
     */
    private static final String STREAMABLE_HTTP_PATH = "/streamablehttp";

    /**
     * SSE protocol path identifier.
     */
    private static final String SSE_PATH = "/sse";

    /**
     * MCP tool call prevention attribute.
     */
    private static final String MCP_TOOL_CALL_ATTR = "MCP_TOOL_CALL";

    /**
     * MCP session ID attribute key.
     */
    private static final String MCP_SESSION_ID_ATTR = "MCP_SESSION_ID";

    /**
     * Session ID query parameter name.
     */
    private static final String SESSION_ID_PARAM = "sessionId";

    /**
     * Session ID header names (in order of preference).
     */
    private static final String[] SESSION_ID_HEADERS = {
            "X-Session-Id", "Mcp-Session-Id",
    };

    /**
     * Authorization header name.
     */
    private static final String AUTHORIZATION_HEADER = "Authorization";

    /**
     * Bearer token prefix.
     */
    private static final String BEARER_PREFIX = "Bearer ";

    private final ShenyuMcpServerManager shenyuMcpServerManager;

    private final List<HttpMessageReader<?>> messageReaders;

    /**
     * Constructs a new MCP server plugin.
     *
     * @param shenyuMcpServerManager the MCP server manager for handling transport providers
     * @param messageReaders         the HTTP message readers for request processing
     */
    public McpServerPlugin(final ShenyuMcpServerManager shenyuMcpServerManager,
                           final List<HttpMessageReader<?>> messageReaders) {
        this.shenyuMcpServerManager = shenyuMcpServerManager;
        this.messageReaders = messageReaders;
    }

    @Override
    protected String getRawPath(final ServerWebExchange exchange) {
        return RequestUrlUtils.getRewrittenRawPath(exchange);
    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange,
                                   final ShenyuPluginChain chain,
                                   final SelectorData selector,
                                   final RuleData rule) {

        final ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        Objects.requireNonNull(shenyuContext, "ShenyuContext must not be null");

        final String uri = exchange.getRequest().getURI().getRawPath();
        LOG.debug("Processing MCP request with URI: {}", uri);

        if (!shenyuMcpServerManager.canRoute(uri)) {
            LOG.debug("URI not handled by MCP server, continuing chain: {}", uri);
            return chain.execute(exchange);
        }

        LOG.debug("Handling MCP request for URI: {}", uri);

        // Create server request for processing
        final ServerRequest request = ServerRequest.create(exchange, messageReaders);

        // Route based on protocol type
        return routeByProtocol(exchange, chain, request, selector, uri);
    }

    @Override
    public String named() {
        return PluginEnum.MCP_SERVER.getName();
    }

    @Override
    public boolean skip(final ServerWebExchange exchange) {
        // Skip MCP plugin for MCP tool calls to prevent infinite loops
        final Boolean isMcpToolCall = exchange.getAttribute(MCP_TOOL_CALL_ATTR);
        if (Boolean.TRUE.equals(isMcpToolCall)) {
            LOG.debug("Skipping MCP plugin for tool call to prevent infinite loop");
            return true;
        }

        return skipExcept(exchange, RpcTypeEnum.HTTP);
    }

    @Override
    public int getOrder() {
        return PluginEnum.MCP_SERVER.getCode();
    }


    /**
     * Routes the request based on detected protocol type.
     *
     * @param exchange the server web exchange
     * @param chain    the plugin chain
     * @param request  the server request
     * @param uri      the request URI
     * @return a Mono representing the processing result
     */
    private Mono<Void> routeByProtocol(final ServerWebExchange exchange,
                                       final ShenyuPluginChain chain,
                                       final ServerRequest request,
                                       final SelectorData selector,
                                       final String uri) {

        if (isStreamableHttpProtocol(uri)) {
            return handleStreamableHttpRequest(exchange, chain, request, uri);
        } else if (isSseProtocol(uri)) {
            return handleSseRequest(exchange, chain, request, selector, uri);
        } else {
            // Default to SSE for backward compatibility
            LOG.debug("Using default SSE protocol for URI: {}", uri);
            return handleSseRequest(exchange, chain, request, selector, uri);
        }
    }

    /**
     * Extracts session ID from request parameters or headers.
     * Searches in the following order:
     * <ol>
     *   <li>Query parameter "sessionId"</li>
     *   <li>Header "X-Session-Id"</li>
     *   <li>Header "Mcp-Session-Id"</li>
     *   <li>Authorization header (Bearer token)</li>
     * </ol>
     *
     * @param exchange the ServerWebExchange containing the request
     * @return the sessionId if found, null otherwise
     */
    private String extractSessionId(final ServerWebExchange exchange) {
        // Try query parameters first
        String sessionId = exchange.getRequest().getQueryParams().getFirst(SESSION_ID_PARAM);
        if (Objects.nonNull(sessionId)) {
            LOG.debug("Found sessionId in query parameters: {}", sessionId);
            return sessionId;
        }

        // Try session ID headers
        for (String headerName : SESSION_ID_HEADERS) {
            sessionId = exchange.getRequest().getHeaders().getFirst(headerName);
            if (Objects.nonNull(sessionId)) {
                LOG.debug("Found sessionId in {} header: {}", headerName, sessionId);
                return sessionId;
            }
        }

        // Try Authorization header as fallback
        final String authHeader = exchange.getRequest().getHeaders().getFirst(AUTHORIZATION_HEADER);
        if (Objects.nonNull(authHeader) && authHeader.startsWith(BEARER_PREFIX)) {
            sessionId = authHeader.substring(BEARER_PREFIX.length());
            LOG.debug("Found sessionId in Authorization header: {}", sessionId);
            return sessionId;
        }

        LOG.debug("No sessionId found in request for path: {}", exchange.getRequest().getPath().value());
        return null;
    }

    /**
     * Checks if the URI indicates Streamable HTTP protocol.
     *
     * @param uri the URI to check
     * @return true if Streamable HTTP protocol should be used
     */
    private boolean isStreamableHttpProtocol(final String uri) {
        return uri.contains(STREAMABLE_HTTP_PATH) || uri.endsWith(STREAMABLE_HTTP_PATH);
    }

    /**
     * Checks if the URI indicates SSE protocol.
     *
     * @param uri the URI to check
     * @return true if SSE protocol should be used
     */
    private boolean isSseProtocol(final String uri) {
        return uri.contains(SSE_PATH) || uri.endsWith(SSE_PATH) || uri.endsWith(MESSAGE_ENDPOINT);
    }

    /**
     * Handles Streamable HTTP MCP requests with unified endpoint processing.
     *
     * @param exchange the server web exchange
     * @param chain    the plugin chain
     * @param request  the server request
     * @param uri      the request URI
     * @return a Mono representing the processing result
     */
    private Mono<Void> handleStreamableHttpRequest(final ServerWebExchange exchange,
                                                   final ShenyuPluginChain chain,
                                                   final ServerRequest request,
                                                   final String uri) {

        LOG.debug("Handling Streamable HTTP MCP request for URI: {}", uri);

        final ShenyuStreamableHttpServerTransportProvider transportProvider =
                shenyuMcpServerManager.getOrCreateStreamableHttpTransport(uri);

        setupSessionContext(exchange, chain);

        return processStreamableHttpEndpoint(exchange, transportProvider, request);
    }

    /**
     * Handles SSE MCP requests with endpoint-specific routing.
     *
     * @param exchange the server web exchange
     * @param chain    the plugin chain
     * @param request  the server request
     * @param uri      the request URI
     * @return a Mono representing the processing result
     */
    private Mono<Void> handleSseRequest(final ServerWebExchange exchange,
                                        final ShenyuPluginChain chain,
                                        final ServerRequest request,
                                        final SelectorData selector,
                                        final String uri) {

        LOG.debug("Handling SSE MCP request for URI: {}", uri);
        ShenyuMcpServer server = McpServerPluginDataHandler.CACHED_SERVER.get().obtainHandle(selector.getId());

        if (Objects.isNull(server)) {
            return chain.execute(exchange);
        }
        // Handle MCP SSE/message requests by delegating to the transport provider
        // directly
        String messageEndpoint = server.getMessageEndpoint();

        ShenyuSseServerTransportProvider transportProvider
                = shenyuMcpServerManager.getOrCreateMcpServerTransport(uri, messageEndpoint);

        if (uri.endsWith(messageEndpoint)) {
            setupSessionContext(exchange, chain);
            return handleMessageEndpoint(exchange, transportProvider, request);
        } else {
            return handleSseEndpoint(exchange, transportProvider, request);
        }
    }

    /**
     * Sets up session context and exchange correlation.
     *
     * @param exchange the server web exchange
     * @param chain    the plugin chain
     */
    private void setupSessionContext(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        final String sessionId = extractSessionId(exchange);
        if (Objects.nonNull(sessionId)) {
            exchange.getAttributes().put(MCP_SESSION_ID_ATTR, sessionId);
            exchange.getAttributes().put(Constants.CHAIN, chain);
            ShenyuMcpExchangeHolder.put(sessionId, exchange);
            LOG.debug("Set up session context for sessionId: {}", sessionId);
        }
    }

    /**
     * Processes Streamable HTTP unified endpoint requests.
          * Handles both GET (stream establishment) and POST (message processing) requests
     * according to the Streamable HTTP protocol specification.
     *
     * @param exchange          the server web exchange
     * @param transportProvider the Streamable HTTP transport provider
     * @param request           the server request
     * @return a Mono representing the processing result
     */
    private Mono<Void> processStreamableHttpEndpoint(final ServerWebExchange exchange,
                                                     final ShenyuStreamableHttpServerTransportProvider transportProvider,
                                                     final ServerRequest request) {

        LOG.debug("Processing Streamable HTTP endpoint for request: {}", request.path());

        final String method = exchange.getRequest().getMethod().name();

        if ("GET".equalsIgnoreCase(method)) {
            return handleStreamableHttpGetRequest(exchange);
        } else if ("POST".equalsIgnoreCase(method)) {
            return handleStreamableHttpPostRequest(exchange, transportProvider, request);
        } else {
            return handleUnsupportedMethod(exchange);
        }
    }

    /**
     * Handles Streamable HTTP GET requests (not supported by protocol).
     *
     * @param exchange the server web exchange
     * @return a Mono representing the error response
     */
    private Mono<Void> handleStreamableHttpGetRequest(final ServerWebExchange exchange) {
        LOG.debug("Rejecting Streamable HTTP GET request (protocol does not support GET)");

        setErrorResponse(exchange, HttpStatus.METHOD_NOT_ALLOWED,
                "POST, OPTIONS",
                createJsonError(-32601, "Streamable HTTP does not support GET requests. Please use POST requests for all MCP operations."));

        return writeJsonResponse(exchange);
    }

    /**
     * Handles Streamable HTTP POST requests for message processing.
     *
     * @param exchange          the server web exchange
     * @param transportProvider the transport provider
     * @param request           the server request
     * @return a Mono representing the processing result
     */
    private Mono<Void> handleStreamableHttpPostRequest(final ServerWebExchange exchange,
                                                       final ShenyuStreamableHttpServerTransportProvider transportProvider,
                                                       final ServerRequest request) {

        LOG.debug("Processing Streamable HTTP POST request for message handling");

        return transportProvider.handleMessageEndpoint(exchange, request)
                .flatMap(result -> processStreamableHttpResult(exchange, result))
                .doOnSuccess(aVoid -> LOG.debug("Streamable HTTP message processing completed"))
                .doOnError(error -> LOG.error("Error in Streamable HTTP message processing: {}", error.getMessage(), error));
    }

    /**
     * Processes the result of Streamable HTTP message handling.
     *
     * @param exchange the server web exchange
     * @param result   the message handling result
     * @return a Mono representing the response writing
     */
    private Mono<Void> processStreamableHttpResult(final ServerWebExchange exchange,
                                                   final MessageHandlingResult result) {

        LOG.debug("Processing Streamable HTTP result - Status: {}, SessionId: {}",
                result.getStatusCode(), result.getSessionId());

        // Configure response
        configureStreamableHttpResponse(exchange, result);

        // Write response body
        final String responseBodyJson = result.getResponseBodyAsJson();
        final byte[] responseBytes = responseBodyJson.getBytes(StandardCharsets.UTF_8);

        LOG.debug("Writing response body with {} bytes", responseBytes.length);

        return exchange.getResponse()
                .writeWith(Mono.just(exchange.getResponse().bufferFactory().wrap(responseBytes)))
                .doOnSuccess(aVoid -> LOG.debug("Response transmission completed successfully"))
                .doOnError(error -> LOG.error("Error writing response: {}", error.getMessage(), error));
    }

    /**
     * Configures response headers for Streamable HTTP.
     *
     * @param exchange the server web exchange
     * @param result   the message handling result
     */
    private void configureStreamableHttpResponse(final ServerWebExchange exchange,
                                                 final MessageHandlingResult result) {

        // Set response status
        exchange.getResponse().setStatusCode(HttpStatus.valueOf(result.getStatusCode()));

        // Add session ID header if available
        if (Objects.nonNull(result.getSessionId())) {
            exchange.getResponse().getHeaders().set("Mcp-Session-Id", result.getSessionId());
        } else {
            // Fallback to extracting from exchange
            final String sessionId = extractSessionId(exchange);
            if (Objects.nonNull(sessionId)) {
                exchange.getResponse().getHeaders().set("Mcp-Session-Id", sessionId);
            }
        }

        // Set standard headers
        setCorsHeaders(exchange);
        exchange.getResponse().getHeaders().set("Content-Type", "application/json");

        // Clean up potentially conflicting headers
        exchange.getResponse().getHeaders().remove("Transfer-Encoding");
        exchange.getResponse().getHeaders().remove("Content-Length");

        LOG.debug("Configured Streamable HTTP response headers");
    }

    /**
     * Handles unsupported HTTP methods.
     *
     * @param exchange the server web exchange
     * @return a Mono representing the error response
     */
    private Mono<Void> handleUnsupportedMethod(final ServerWebExchange exchange) {
        LOG.debug("Unsupported HTTP method: {}", exchange.getRequest().getMethod());

        setErrorResponse(exchange, HttpStatus.BAD_REQUEST,
                null,
                createJsonError(-32600, "Unsupported HTTP method"));

        return writeJsonResponse(exchange);
    }

    /**
     * Creates SSE response by setting up the SSE stream.
     *
     * @param exchange          the server web exchange
     * @param transportProvider the SSE transport provider
     * @param request           the server request
     * @return a Mono representing the SSE stream
     */
    private Mono<Void> handleSseEndpoint(final ServerWebExchange exchange,
                                         final ShenyuSseServerTransportProvider transportProvider,
                                         final ServerRequest request) {

        LOG.debug("Setting up SSE endpoint for request: {}", request.path());

        // Configure SSE headers
        configureSseHeaders(exchange);

        // Create and write SSE stream
        return exchange.getResponse()
                .writeWith(transportProvider
                        .createSseFlux(request)
                        .doOnNext(event -> {
                            String eventType = event.event();
                            LOG.debug("SSE Event - Type: {}", Objects.isNull(eventType) ? "data" : eventType);
                        })
                        .map(event -> SseEventFormatter.formatEvent(event, exchange))
                        .doOnSubscribe(subscription -> LOG.debug("SSE stream subscribed"))
                        .doOnComplete(() -> LOG.debug("SSE stream completed"))
                        .doOnError(error -> LOG.error("SSE stream error: {}", error.getMessage(), error)));
    }

    /**
     * Configures SSE response headers.
     *
     * @param exchange the server web exchange
     */
    private void configureSseHeaders(final ServerWebExchange exchange) {
        exchange.getResponse().getHeaders().set("Content-Type", "text/event-stream");
        exchange.getResponse().getHeaders().set("Cache-Control", "no-cache");
        exchange.getResponse().getHeaders().set("Connection", "keep-alive");
        setCorsHeaders(exchange);
        LOG.debug("Configured SSE headers");
    }

    /**
     * Handles message endpoint requests for SSE protocol.
     *
     * @param exchange          the server web exchange
     * @param transportProvider the SSE transport provider
     * @param request           the server request
     * @return a Mono representing the processing result
     */
    private Mono<Void> handleMessageEndpoint(final ServerWebExchange exchange,
                                             final ShenyuSseServerTransportProvider transportProvider,
                                             final ServerRequest request) {

        LOG.debug("Processing message endpoint request");

        return transportProvider.handleMessageEndpoint(request)
                .flatMap(result -> {
                    LOG.debug("Message handling result - Status: {}, Body length: {} chars",
                            result.getStatusCode(),
                            Objects.nonNull(result.getResponseBody()) ? result.getResponseBody().length() : 0);

                    // Configure response
                    exchange.getResponse().setStatusCode(HttpStatus.valueOf(result.getStatusCode()));
                    exchange.getResponse().getHeaders().add("Content-Type", "application/json");
                    setCorsHeaders(exchange);

                    // Create response body
                    final String responseBody = String.format("{\"message\":\"%s\"}", result.getResponseBody());
                    LOG.debug("Sending message response with length: {} chars", responseBody.length());

                    return exchange.getResponse()
                            .writeWith(Mono.just(exchange.getResponse().bufferFactory().wrap(responseBody.getBytes())));
                })
                .doOnSuccess(aVoid -> LOG.debug("Message response completed"))
                .doOnError(error -> LOG.error("Error in message response: {}", error.getMessage(), error));
    }

    /**
     * Sets CORS headers for cross-origin requests.
     *
     * @param exchange the server web exchange
     */
    private void setCorsHeaders(final ServerWebExchange exchange) {
        exchange.getResponse().getHeaders().set("Access-Control-Allow-Origin", "*");
        exchange.getResponse().getHeaders().set("Access-Control-Allow-Headers",
                "Content-Type, Mcp-Session-Id, Authorization, Last-Event-ID");
        exchange.getResponse().getHeaders().set("Access-Control-Allow-Methods",
                "GET, POST, OPTIONS");
    }

    /**
     * Sets up error response configuration.
     *
     * @param exchange    the server web exchange
     * @param status      the HTTP status
     * @param allowHeader the Allow header value (nullable)
     * @param errorBody   the error response body
     */
    private void setErrorResponse(final ServerWebExchange exchange,
                                  final HttpStatus status,
                                  final String allowHeader,
                                  final Map<String, Object> errorBody) {

        exchange.getResponse().setStatusCode(status);
        exchange.getResponse().getHeaders().add("Content-Type", "application/json");
        setCorsHeaders(exchange);

        if (Objects.nonNull(allowHeader)) {
            exchange.getResponse().getHeaders().add("Allow", allowHeader);
        }

        exchange.getAttributes().put("errorBody", errorBody);
    }

    /**
     * Writes JSON response to the exchange.
     *
     * @param exchange the server web exchange
     * @return a Mono representing the write operation
     */
    private Mono<Void> writeJsonResponse(final ServerWebExchange exchange) {
        @SuppressWarnings("unchecked") final Map<String, Object> errorBody = (Map<String, Object>) exchange.getAttributes().get("errorBody");

        if (Objects.isNull(errorBody)) {
            return Mono.empty();
        }

        try {
            final String errorResponse = new ObjectMapper()
                    .writeValueAsString(errorBody);
            return exchange.getResponse()
                    .writeWith(Mono.just(exchange.getResponse().bufferFactory().wrap(errorResponse.getBytes())));
        } catch (Exception e) {
            LOG.error("Error writing JSON response: {}", e.getMessage(), e);
            return Mono.empty();
        }
    }

    /**
     * Creates a standard JSON-RPC error object.
     *
     * @param code    the error code
     * @param message the error message
     * @return the error object as a Map
     */
    private Map<String, Object> createJsonError(final int code, final String message) {
        return Map.of(
                "error", Map.of(
                        "code", code,
                        "message", message
                )
        );
    }

}
