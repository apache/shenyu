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
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;

import org.apache.shenyu.plugin.mcp.server.holder.ShenyuMcpExchangeHolder;
import org.apache.shenyu.plugin.mcp.server.manager.ShenyuMcpServerManager;
import org.apache.shenyu.plugin.mcp.server.transport.ShenyuSseServerTransportProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.codec.HttpMessageReader;
import org.springframework.web.reactive.function.server.ServerRequest;
import org.springframework.web.server.ServerWebExchange;

import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Objects;

/**
 * McpServer Plugin.
 */
public class McpServerPlugin extends AbstractShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(McpServerPlugin.class);

    private final ShenyuMcpServerManager shenyuMcpServerManager;

    private final List<HttpMessageReader<?>> messageReaders;

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
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain,
            final SelectorData selector, final RuleData rule) {
        ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        Objects.requireNonNull(shenyuContext);

        String uri = exchange.getRequest().getURI().getRawPath();
        LOG.debug("Processing request with URI: {}", uri);

        // Check if this is an MCP request path
        LOG.info("Found matching MCP server for URI: {}", uri);

        // Get or create the MCP server
        // ShenyuSseServerTransportProvider serverTransport =
        // shenyuMcpServerManager.getOrCreateMcpServerTransport(uri);
        // if (serverTransport != null) {
        // LOG.info("Using MCP server for URI: {}", uri);
        //
        // ServerRequest request = ServerRequest.create(exchange, messageReaders);
        //
        // // Mark that this request is being handled by the MCP server
        // exchange.getAttributes().put("MCP_SERVER_HANDLED", true);
        //
        // return serverTransport.handleShenyuSseConnection(request);
        // }

        if (shenyuMcpServerManager.canRoute(uri)) {

            LOG.info("Handling MCP request for URI: {}", uri);

            // Extract sessionId from request if available
            String sessionId = extractSessionId(exchange);
            ShenyuMcpExchangeHolder.put(sessionId, exchange);
            LOG.debug("Extracted sessionId: {}", sessionId);

            ServerRequest request = ServerRequest.create(exchange, messageReaders);

            // Handle MCP SSE/message requests by delegating to the transport provider
            // directly
            ShenyuSseServerTransportProvider transportProvider = shenyuMcpServerManager
                    .getOrCreateMcpServerTransport(uri);

            // Mark exchange to prevent further plugin processing
            exchange.getAttributes().put("MCP_SERVER_HANDLED", true);
            shenyuContext.setRpcType(RpcTypeEnum.AI.getName());
            exchange.getAttributes().put(Constants.CONTEXT, shenyuContext);

            // Store sessionId in exchange attributes for later use
            if (Objects.nonNull(sessionId)) {
                exchange.getAttributes().put("MCP_SESSION_ID", sessionId);
                LOG.debug("Stored sessionId in exchange attributes: {}", sessionId);
            }

            if (uri.endsWith("/message")) {
                // Handle JSON-RPC message endpoint
                LOG.debug("Handling MCP message endpoint for URI: {} with sessionId: {}", uri, sessionId);
                return createDirectMessageResponse(exchange, transportProvider, request, sessionId);
            } else {
                // Handle SSE connection endpoint
                LOG.debug("Handling MCP SSE connection for URI: {} with sessionId: {}", uri, sessionId);

                // Create a custom SSE handler that bypasses the ServerResponse complexity
                return createDirectSseResponse(exchange, transportProvider, request, sessionId);
            }
        }

        // Continue the chain - the WebFlux infrastructure will route to the appropriate
        // handler
        return chain.execute(exchange);
    }

    @Override
    public String named() {
        return PluginEnum.MCP_SERVER.getName();
    }

    @Override
    public boolean skip(final ServerWebExchange exchange) {
        return skipExcept(exchange, RpcTypeEnum.HTTP);
    }

    @Override
    public int getOrder() {
        return PluginEnum.MCP_SERVER.getCode();
    }

    @Override
    protected Mono<Void> handleSelectorIfNull(final String pluginName, final ServerWebExchange exchange,
            final ShenyuPluginChain chain) {
        return WebFluxResultUtils.noSelectorResult(pluginName, exchange);
    }

    @Override
    protected Mono<Void> handleRuleIfNull(final String pluginName, final ServerWebExchange exchange,
            final ShenyuPluginChain chain) {
        return WebFluxResultUtils.noRuleResult(pluginName, exchange);
    }

    /**
     * Extract sessionId from request parameters or headers.
     * 
     * @param exchange The ServerWebExchange containing the request
     * @return The sessionId if found, null otherwise
     */
    private String extractSessionId(final ServerWebExchange exchange) {
        // First try to get sessionId from query parameters
        String sessionId = exchange.getRequest().getQueryParams().getFirst("sessionId");
        if (Objects.nonNull(sessionId)) {
            LOG.debug("Found sessionId in query parameters: {}", sessionId);
            return sessionId;
        }

        // Try to get sessionId from headers
        sessionId = exchange.getRequest().getHeaders().getFirst("X-Session-Id");
        if (Objects.nonNull(sessionId)) {
            LOG.debug("Found sessionId in X-Session-Id header: {}", sessionId);
            return sessionId;
        }

        // Try to get sessionId from Authorization header (if using Bearer token format)
        String authHeader = exchange.getRequest().getHeaders().getFirst("Authorization");
        if (Objects.nonNull(authHeader) && authHeader.startsWith("Bearer ")) {
            // Remove "Bearer " prefix
            sessionId = authHeader.substring(7);
            LOG.debug("Found sessionId in Authorization header: {}", sessionId);
            return sessionId;
        }

        // Try to get sessionId from exchange holder if it was stored by filter
        String path = exchange.getRequest().getPath().value();
        LOG.debug("No sessionId found in request parameters or headers for path: {}", path);
        return null;
    }

    /**
     * Create a direct SSE response by manually setting up the SSE stream.
     * This bypasses the ServerResponse complexity and writes directly to the
     * exchange.
     */
    private Mono<Void> createDirectSseResponse(final ServerWebExchange exchange,
            final ShenyuSseServerTransportProvider transportProvider,
            final ServerRequest request, final String sessionId) {
        LOG.debug("Creating direct SSE response for sessionId: {}", sessionId);

        // Set SSE headers
        exchange.getResponse().getHeaders().add("Content-Type", "text/event-stream");
        exchange.getResponse().getHeaders().add("Cache-Control", "no-cache");
        exchange.getResponse().getHeaders().add("Connection", "keep-alive");
        exchange.getResponse().getHeaders().add("Access-Control-Allow-Origin", "*");
        LOG.debug("Set SSE headers on exchange response");

        // Create SSE flux and write each event immediately to the exchange response
        return exchange.getResponse().writeWith(
                transportProvider.createSseFlux(request)
                        .doOnNext(event -> LOG.debug("SSE Event - Type: {}, Data: {}", event.event(), event.data()))
                        .map(event -> {
                            // Format SSE event according to standard
                            StringBuilder sseData = new StringBuilder();
                            if (Objects.nonNull(event.event())) {
                                sseData.append("event: ").append(event.event()).append("\n");
                            }
                            if (Objects.nonNull(event.data())) {
                                sseData.append("data: ").append(event.data()).append("\n");
                            }
                            /* End of event */
                            sseData.append("\n");

                            LOG.debug("Formatted SSE data: {}", sseData.toString().trim());
                            return exchange.getResponse().bufferFactory().wrap(sseData.toString().getBytes());
                        })
                        .doOnSubscribe(subscription -> LOG.debug("SSE stream subscribed"))
                        .doOnComplete(() -> LOG.debug("SSE stream completed"))
                        .doOnError(error -> LOG.error("SSE stream error: {}", error.getMessage())));
    }

    /**
     * Create a direct message response by manually setting up the response.
     * This bypasses the ServerResponse complexity and writes directly to the
     * exchange.
     */
    private Mono<Void> createDirectMessageResponse(final ServerWebExchange exchange,
            final ShenyuSseServerTransportProvider transportProvider,
            final ServerRequest request, final String sessionId) {
        LOG.debug("Creating direct message response for sessionId: {}", sessionId);

        return transportProvider.handleMessageDirect(request)
                .flatMap(result -> {
                    LOG.debug("Message handling result - Status: {}, Body: {}", result.getStatusCode(),
                            result.getResponseBody());

                    // Set response status
                    exchange.getResponse()
                            .setStatusCode(org.springframework.http.HttpStatus.valueOf(result.getStatusCode()));

                    // Set appropriate headers
                    exchange.getResponse().getHeaders().add("Content-Type", "application/json");
                    exchange.getResponse().getHeaders().add("Access-Control-Allow-Origin", "*");

                    // Write response body
                    String responseBody = String.format("{\"message\":\"%s\"}", result.getResponseBody());
                    LOG.debug("Sending response body: {}", responseBody);

                    return exchange.getResponse().writeWith(
                            reactor.core.publisher.Mono.just(
                                    exchange.getResponse().bufferFactory().wrap(responseBody.getBytes())));
                })
                .doOnSuccess(aVoid -> LOG.debug("Message response writing completed"))
                .doOnError(error -> LOG.error("Error writing message response: {}", error.getMessage()));
    }

}
