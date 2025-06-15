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

package org.apache.shenyu.plugin.mcp.server.callback;

import com.google.common.collect.Maps;
import com.google.gson.JsonObject;
import io.modelcontextprotocol.server.McpSyncServerExchange;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.mcp.server.response.ShenyuMcpResponseDecorator;
import org.apache.shenyu.plugin.mcp.server.definition.ShenyuToolDefinition;
import org.apache.shenyu.plugin.mcp.server.holder.ShenyuMcpExchangeHolder;
import org.apache.shenyu.plugin.mcp.server.request.RequestConfig;
import org.apache.shenyu.plugin.mcp.server.request.RequestConfigHelper;
import org.apache.shenyu.plugin.mcp.server.request.BodyWriterExchange;
import org.apache.shenyu.plugin.mcp.server.session.McpSessionHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.ai.chat.model.ToolContext;
import org.springframework.ai.tool.ToolCallback;
import org.springframework.ai.tool.definition.ToolDefinition;
import org.springframework.http.HttpMethod;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpResponseDecorator;
import org.springframework.lang.NonNull;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;
import org.springframework.web.server.ServerWebExchange;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

public class ShenyuToolCallback implements ToolCallback {
    
    private static final Logger LOG = LoggerFactory.getLogger(ShenyuToolCallback.class);
    
    private static final int DEFAULT_TIMEOUT_SECONDS = 30;
    
    private final ToolDefinition toolDefinition;
    
    public ShenyuToolCallback(final ToolDefinition toolDefinition) {
        this.toolDefinition = Objects.requireNonNull(toolDefinition, "ToolDefinition cannot be null");
    }
    
    @NonNull
    @Override
    public ToolDefinition getToolDefinition() {
        return this.toolDefinition;
    }
    
    @NonNull
    @Override
    public String call(@NonNull final String input) {
        return call(input, new ToolContext(Maps.newHashMap()));
    }
    
    @NonNull
    @Override
    public String call(@NonNull final String input, final ToolContext toolContext) {
        Objects.requireNonNull(input, "Input cannot be null");
        Objects.requireNonNull(toolContext, "ToolContext cannot be null");
        
        try {
            // Get MCP session information
            final McpSyncServerExchange mcpSyncServerExchange = getMcpSyncServerExchange(toolContext);
            final String sessionId = getSessionId(mcpSyncServerExchange);
            
            // Get tool definition and request configuration
            final ShenyuToolDefinition shenyuToolDefinition = validateAndGetToolDefinition();
            final String configStr = validateAndGetRequestConfig(shenyuToolDefinition);
            
            // Get original exchange object
            final ServerWebExchange originExchange = ShenyuMcpExchangeHolder.get(sessionId);
            if (Objects.isNull(originExchange)) {
                throw new IllegalStateException("No exchange found for sessionId: " + sessionId);
            }
            ShenyuPluginChain chain = originExchange.getAttribute(Constants.CHAIN);
            Assert.notNull(chain, "ShenyuPluginChain cannot be null");
            
            final CompletableFuture<String> future = new CompletableFuture<>();
            
            // Build new exchange object
            final ServerWebExchange decoratedExchange = buildExchange(originExchange, future, sessionId, configStr, input);
            
            LOG.info("[MCP] Executing plugin chain for session: {}", sessionId);
            chain.execute(decoratedExchange).doOnSubscribe(s -> LOG.info("[MCP] Subscribed to plugin chain for session: {}", sessionId)).doOnError(e -> {
                LOG.error("[MCP] Error processing request for session: {}", sessionId, e);
                future.completeExceptionally(e);
            }).doOnSuccess(v -> LOG.info("[MCP] Plugin chain completed successfully for session: {}", sessionId)).doOnCancel(() -> {
                LOG.warn("[MCP] Plugin chain was cancelled for session: {}", sessionId);
                future.completeExceptionally(new RuntimeException("Request was cancelled"));
            }).subscribe();
            
            try {
                return future.get(DEFAULT_TIMEOUT_SECONDS, TimeUnit.SECONDS);
            } catch (Exception e) {
                LOG.error("[MCP] Error waiting for response for session: {}", sessionId, e);
                throw new RuntimeException("Error waiting for response: " + e.getMessage(), e);
            }
        } catch (Exception e) {
            LOG.error("Failed to process request: {}", e.getMessage(), e);
            throw new RuntimeException("Failed to process request: " + e.getMessage(), e);
        }
    }
    
    private McpSyncServerExchange getMcpSyncServerExchange(final ToolContext toolContext) {
        final McpSyncServerExchange exchange = McpSessionHelper.getMcpSyncServerExchange(toolContext);
        if (Objects.isNull(exchange)) {
            throw new IllegalStateException("Failed to get MCP sync server exchange");
        }
        return exchange;
    }
    
    private String getSessionId(final McpSyncServerExchange exchange) throws NoSuchFieldException, IllegalAccessException {
        final String sessionId = McpSessionHelper.getSessionId(exchange);
        if (!StringUtils.hasText(sessionId)) {
            throw new IllegalStateException("Session ID cannot be empty");
        }
        return sessionId;
    }
    
    private ShenyuToolDefinition validateAndGetToolDefinition() {
        if (!(this.toolDefinition instanceof ShenyuToolDefinition)) {
            throw new IllegalStateException("Invalid tool definition type");
        }
        return (ShenyuToolDefinition) this.toolDefinition;
    }
    
    private String validateAndGetRequestConfig(final ShenyuToolDefinition definition) {
        final String config = definition.requestConfig();
        if (!StringUtils.hasText(config)) {
            throw new IllegalStateException("Request configuration cannot be empty");
        }
        LOG.debug("Request configuration: {}", config);
        return config;
    }
    
    private ServerWebExchange buildExchange(final ServerWebExchange originExchange, final CompletableFuture<String> future, final String sessionId, final String configStr, final String input) {
        // Parse input parameters
        final JsonObject inputJson = GsonUtils.getInstance().fromJson(input, JsonObject.class);
        if (Objects.isNull(inputJson)) {
            throw new IllegalArgumentException("Invalid input JSON format");
        }
        LOG.debug("Input parameters: {}", inputJson);
        
        // Parse request configuration
        final RequestConfigHelper configHelper = new RequestConfigHelper(configStr);
        final JsonObject requestTemplate = configHelper.getRequestTemplate();
        final JsonObject argsPosition = configHelper.getArgsPosition();
        final String urlTemplate = configHelper.getUrlTemplate();
        final String method = configHelper.getMethod();
        final boolean argsToJsonBody = configHelper.isArgsToJsonBody();
        
        final String path = RequestConfigHelper.buildPath(urlTemplate, argsPosition, inputJson);
        final JsonObject bodyJson = RequestConfigHelper.buildBodyJson(argsToJsonBody, argsPosition, inputJson);
        
        final RequestConfig requestConfig = new RequestConfig(method, path, bodyJson, requestTemplate, argsToJsonBody);
        
        final ServerHttpRequest.Builder requestBuilder = originExchange
                .getRequest()
                .mutate()
                .method(HttpMethod.valueOf(requestConfig.getMethod()))
                .header("sessionId", sessionId)
                .header("Accept", "application/json");
        
        // Add custom headers
        if (requestConfig.getRequestTemplate().has("headers")) {
            for (final var headerElem : requestConfig.getRequestTemplate().getAsJsonArray("headers")) {
                final JsonObject headerObj = headerElem.getAsJsonObject();
                requestBuilder.header(headerObj.get("key").getAsString(), headerObj.get("value").getAsString());
            }
        }
        
        // Set Content-Type
        if (isRequestBodyMethod(requestConfig.getMethod())) {
            requestBuilder.header("Content-Type", "application/json");
        } else {
            requestBuilder.headers(httpHeaders -> httpHeaders.remove("Content-Type"));
        }
        
        // Set URI
        try {
            final URI oldUri = originExchange.getRequest().getURI();
            String newPath = requestConfig.getPath();
            final String newUriStr = oldUri.getScheme() + "://" + oldUri.getAuthority() + newPath;
            requestBuilder.uri(new URI(newUriStr));
        } catch (URISyntaxException e) {
            throw new RuntimeException("Invalid URI: " + e.getMessage(), e);
        }
        
        // Create response decorator
        final ServerHttpResponseDecorator responseDecorator = new ShenyuMcpResponseDecorator(originExchange.getResponse(), sessionId, future, configHelper.getResponseTemplate());
        
        // Build final exchange object
        final ServerWebExchange decoratedExchange = originExchange.mutate().request(requestBuilder.build()).response(responseDecorator).build();
        
        // Handle request body
        if (isRequestBodyMethod(requestConfig.getMethod()) && requestConfig.getBodyJson().size() > 0) {
            return new BodyWriterExchange(decoratedExchange, requestConfig.getBodyJson().toString());
        }
        
        // Set context and attributes
        final ShenyuContext shenyuContext = originExchange.getAttribute(Constants.CONTEXT);
        if (Objects.nonNull(shenyuContext)) {
            shenyuContext.setRpcType(RpcTypeEnum.HTTP.getName());
            shenyuContext.setPath(decoratedExchange.getRequest().getPath().value());
            shenyuContext.setRealUrl(decoratedExchange.getRequest().getPath().value());
            decoratedExchange.getAttributes().put(Constants.CONTEXT, shenyuContext);
        }
        
        return decoratedExchange;
    }
    
    private boolean isRequestBodyMethod(final String method) {
        return "POST".equalsIgnoreCase(method) || "PUT".equalsIgnoreCase(method) || "PATCH".equalsIgnoreCase(method);
    }
}
