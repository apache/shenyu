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
import org.apache.shenyu.plugin.mcp.server.holder.ShenyuMcpExchangeHolder;
import org.apache.shenyu.plugin.mcp.server.manager.ShenyuMcpServerManager;
import org.apache.shenyu.plugin.mcp.server.transport.ShenyuSseServerTransportProvider;
import org.apache.shenyu.plugin.mcp.server.transport.SseEventFormatter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.codec.HttpMessageReader;
import org.springframework.web.reactive.function.server.ServerRequest;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Objects;

/**
 * MCP Server Plugin.
 * Handles MCP (Message Control Protocol) server-side functionality.
 */
public class McpServerPlugin extends AbstractShenyuPlugin {
    
    private static final Logger LOG = LoggerFactory.getLogger(McpServerPlugin.class);
    
    private static final String MESSAGE_ENDPOINT = "/message";
    
    private final ShenyuMcpServerManager shenyuMcpServerManager;
    
    private final List<HttpMessageReader<?>> messageReaders;
    
    public McpServerPlugin(final ShenyuMcpServerManager shenyuMcpServerManager, final List<HttpMessageReader<?>> messageReaders) {
        this.shenyuMcpServerManager = shenyuMcpServerManager;
        this.messageReaders = messageReaders;
    }
    
    @Override
    protected String getRawPath(final ServerWebExchange exchange) {
        return RequestUrlUtils.getRewrittenRawPath(exchange);
    }
    
    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        Objects.requireNonNull(shenyuContext, "ShenyuContext must not be null");
        
        String uri = exchange.getRequest().getURI().getRawPath();
        LOG.info("Processing request with URI: {}", uri);
        
        if (!shenyuMcpServerManager.canRoute(uri)) {
            // Continue the chain - the WebFlux infrastructure will route to the appropriate
            // handler
            return chain.execute(exchange);
        }
        
        LOG.info("Handling MCP request for URI: {}", uri);
        
        // Extract sessionId from request if available
        ServerRequest request = ServerRequest.create(exchange, messageReaders);
        
        // Handle MCP SSE/message requests by delegating to the transport provider
        // directly
        ShenyuSseServerTransportProvider transportProvider = shenyuMcpServerManager.getOrCreateMcpServerTransport(uri);
        
        // Mark exchange to prevent further plugin processing
        shenyuContext.setRpcType(RpcTypeEnum.AI.getName());
        exchange.getAttributes().put(Constants.CONTEXT, shenyuContext);
        
        if (uri.endsWith("/message")) {
            String sessionId = extractSessionId(exchange);
            if (Objects.nonNull(sessionId)) {
                exchange.getAttributes().put("MCP_SESSION_ID", sessionId);
                exchange.getAttributes().put(Constants.CHAIN, chain);
                // Store sessionId in exchange attributes for later use
                ShenyuMcpExchangeHolder.put(sessionId, exchange);
                LOG.debug("Extracted sessionId: {}", sessionId);
            }
            // Handle JSON-RPC message endpoint
            LOG.debug("Handling MCP message endpoint for URI: {} with sessionId: {}", uri, sessionId);
            return handleMessageEndpoint(exchange, transportProvider, request)
                    .doFinally((signalType) -> ShenyuMcpExchangeHolder.remove(sessionId));
        } else {
            // Handle SSE connection endpoint
            LOG.debug("Handling MCP SSE connection for URI: {} ", uri);
            
            // Create a custom SSE handler that bypasses the ServerResponse complexity
            return handleSseEndpoint(exchange, transportProvider, request);
        }
        
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
    
    /**
     * Extract sessionId from request parameters or headers.
     *
     * @param exchange The ServerWebExchange containing the request
     * @return The sessionId if found, null otherwise
     */
    private String extractSessionId(final ServerWebExchange exchange) {
        // Try query parameters
        String sessionId = exchange.getRequest().getQueryParams().getFirst("sessionId");
        if (Objects.nonNull(sessionId)) {
            LOG.debug("Found sessionId in query parameters: {}", sessionId);
            return sessionId;
        }
        
        // Try X-Session-Id header
        sessionId = exchange.getRequest().getHeaders().getFirst("X-Session-Id");
        if (Objects.nonNull(sessionId)) {
            LOG.debug("Found sessionId in X-Session-Id header: {}", sessionId);
            return sessionId;
        }
        
        // Try Authorization header
        String authHeader = exchange.getRequest().getHeaders().getFirst("Authorization");
        if (Objects.nonNull(authHeader) && authHeader.startsWith("Bearer ")) {
            sessionId = authHeader.substring(7);
            LOG.debug("Found sessionId in Authorization header: {}", sessionId);
            return sessionId;
        }
        
        LOG.debug("No sessionId found in request for path: {}", exchange.getRequest().getPath().value());
        return null;
    }
    
    /**
     * Create a direct SSE response by manually setting up the SSE stream.
     * This bypasses the ServerResponse complexity and writes directly to the
     * exchange.
     */
    private Mono<Void> handleSseEndpoint(final ServerWebExchange exchange, final ShenyuSseServerTransportProvider transportProvider, final ServerRequest request) {
        LOG.debug("Handling SSE endpoint for request: {}", request.path());
        
        // Set SSE headers
        configureSseHeaders(exchange);
        
        // Create SSE flux and write each event immediately to the exchange response
        return exchange.getResponse()
                .writeWith(transportProvider
                        .createSseFlux(request)
                        .doOnNext(event -> LOG.debug("SSE Event - Type: {}, Data: {}", event.event(), event.data()))
                        .map(event -> SseEventFormatter.formatEvent(event, exchange))
                        .doOnSubscribe(subscription -> LOG.debug("SSE stream subscribed"))
                        .doOnComplete(() -> LOG.debug("SSE stream completed"))
                        .doOnError(error -> LOG.error("SSE stream error: {}", error.getMessage())));
    }
    
    private void configureSseHeaders(final ServerWebExchange exchange) {
        exchange.getResponse().getHeaders().add("Content-Type", "text/event-stream");
        exchange.getResponse().getHeaders().add("Cache-Control", "no-cache");
        exchange.getResponse().getHeaders().add("Connection", "keep-alive");
        exchange.getResponse().getHeaders().add("Access-Control-Allow-Origin", "*");
        LOG.debug("Configured SSE headers on exchange response");
    }
    
    private Mono<Void> handleMessageEndpoint(final ServerWebExchange exchange, final ShenyuSseServerTransportProvider transportProvider, final ServerRequest request) {
        
        return transportProvider.handleMessageEndpoint(request).flatMap(result -> {
            LOG.debug("Message handling result - Status: {}, Body: {}", result.getStatusCode(), result.getResponseBody());
            
            // Set response status
            exchange.getResponse().setStatusCode(HttpStatus.valueOf(result.getStatusCode()));
            // Set appropriate headers
            exchange.getResponse().getHeaders().add("Content-Type", "application/json");
            exchange.getResponse().getHeaders().add("Access-Control-Allow-Origin", "*");
            
            // Write response body
            String responseBody = String.format("{\"message\":\"%s\"}", result.getResponseBody());
            LOG.debug("Sending response body: {}", responseBody);
            
            return exchange.getResponse().writeWith(Mono.just(exchange.getResponse().bufferFactory().wrap(responseBody.getBytes())));
        }).doOnSuccess(aVoid -> LOG.debug("Message response writing completed")).doOnError(error -> LOG.error("Error writing message response: {}", error.getMessage()));
    }
}
