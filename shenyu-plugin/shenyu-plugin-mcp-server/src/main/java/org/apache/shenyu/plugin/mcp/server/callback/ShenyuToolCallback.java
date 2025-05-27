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
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.mcp.server.decorator.ShenyuMcpResponseDecorator;
import org.apache.shenyu.plugin.mcp.server.definition.ShenyuToolDefinition;
import org.apache.shenyu.plugin.mcp.server.holder.ShenyuMcpExchangeHolder;
import org.apache.shenyu.plugin.mcp.server.utils.BodyWriterExchange;
import org.apache.shenyu.plugin.mcp.server.utils.McpSessionHelper;
import org.apache.shenyu.plugin.mcp.server.utils.RequestConfigHelper;
import org.apache.shenyu.web.handler.ShenyuWebHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.ai.chat.model.ToolContext;
import org.springframework.ai.tool.ToolCallback;
import org.springframework.ai.tool.definition.ToolDefinition;
import org.springframework.http.HttpMethod;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpResponseDecorator;
import org.springframework.lang.NonNull;
import org.springframework.web.server.ServerWebExchange;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.concurrent.CompletableFuture;

public class ShenyuToolCallback implements ToolCallback {

    private static final Logger LOG = LoggerFactory.getLogger(ShenyuToolCallback.class);

    private final ShenyuWebHandler shenyuWebHandler;

    private final ToolDefinition toolDefinition;

    public ShenyuToolCallback(final ShenyuWebHandler shenyuWebHandler, final ToolDefinition toolDefinition) {
        this.shenyuWebHandler = shenyuWebHandler;
        this.toolDefinition = toolDefinition;
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
        try {
            McpSyncServerExchange mcpSyncServerExchange = McpSessionHelper.getMcpSyncServerExchange(toolContext);
            String sessionId = McpSessionHelper.getSessionId(mcpSyncServerExchange);
            JsonObject inputJson = GsonUtils.getInstance().fromJson(input, JsonObject.class);
            LOG.debug("inputJson: {}", inputJson);

            final ShenyuToolDefinition shenyuToolDefinition = (ShenyuToolDefinition) this.toolDefinition;
            final String requestConfig = shenyuToolDefinition.requestConfig();
            LOG.debug("requestConfig: {}", requestConfig);

            RequestConfigHelper configHelper = new RequestConfigHelper(requestConfig);
            final JsonObject requestTemplate = configHelper.getRequestTemplate();
            final JsonObject argsPosition = configHelper.getArgsPosition();
            final JsonObject responseTemplate = configHelper.getResponseTemplate();
            final String urlTemplate = configHelper.getUrlTemplate();
            final String method = configHelper.getMethod();
            final boolean argsToJsonBody = configHelper.isArgsToJsonBody();

            ServerWebExchange exchange = createServerWebExchange(sessionId, inputJson, configHelper);

            LOG.debug("query params: {}", exchange.getRequest().getQueryParams());
            LOG.debug("argsPosition: {}", argsPosition);
            final String path = RequestConfigHelper.buildPath(urlTemplate, argsPosition, inputJson);
            LOG.debug("buildPath result: {}", path);

            LOG.debug("Starting request processing for session: {}", sessionId);
            LOG.debug("Request method: {}, path: {}", exchange.getRequest().getMethod(),
                    exchange.getRequest().getPath());
            LOG.debug("Request headers: {}", exchange.getRequest().getHeaders());
            LOG.debug("Request query params: {}", exchange.getRequest().getQueryParams());

            CompletableFuture<String> future = new CompletableFuture<>();

            ServerWebExchange mutatedExchange = buildServerWebExchange(sessionId, inputJson, requestTemplate,
                    argsPosition, method, argsToJsonBody, exchange, path);

            ServerHttpResponseDecorator responseDecorator = new ShenyuMcpResponseDecorator(
                    mutatedExchange.getResponse(),
                    sessionId, future, responseTemplate);

            ServerWebExchange decoratedExchange = mutatedExchange.mutate().response(responseDecorator).build();

            shenyuWebHandler.handle(decoratedExchange)
                    .doOnSubscribe(s -> LOG.debug("Subscribed to handle request for session: {}", sessionId))
                    .doOnError(e -> {
                        LOG.error("Error processing request for session: {}", sessionId, e);
                        future.completeExceptionally(e);
                    })
                    .doOnSuccess(v -> LOG.debug("Request handling completed successfully for session: {}", sessionId))
                    .doOnCancel(() -> {
                        LOG.warn("Request was cancelled for session: {}", sessionId);
                        future.completeExceptionally(new RuntimeException("Request was cancelled"));
                    })
                    .subscribe();

            // Return immediately and let the future complete asynchronously
            return future.thenApply(response -> {
                LOG.debug("Received response for session: {}, length: {}", sessionId, response.length());
                return response;
            }).exceptionally(e -> {
                LOG.error("Error processing request for session: {}", sessionId, e);
                throw new RuntimeException("Error processing request: " + e.getMessage(), e);
            }).join();
        } catch (Exception e) {
            LOG.error("Failed to process request", e);
            throw new RuntimeException("Failed to process request: " + e.getMessage(), e);
        }
    }

    private ServerWebExchange createServerWebExchange(final String sessionId, final JsonObject inputJson,
            final RequestConfigHelper configHelper) {
        final ServerWebExchange exchange = ShenyuMcpExchangeHolder.get(sessionId);
        final JsonObject requestTemplate = configHelper.getRequestTemplate();
        final JsonObject argsPosition = configHelper.getArgsPosition();
        final String urlTemplate = configHelper.getUrlTemplate();
        final String method = configHelper.getMethod();
        final boolean argsToJsonBody = configHelper.isArgsToJsonBody();

        final String path = RequestConfigHelper.buildPath(urlTemplate, argsPosition, inputJson);
        LOG.debug("Now Calling path:{}, input:{}", path, inputJson.toString());
        return buildServerWebExchange(sessionId, inputJson, requestTemplate, argsPosition, method, argsToJsonBody,
                exchange, path);
    }

    private ServerWebExchange buildServerWebExchange(final String sessionId,
            final JsonObject inputJson,
            final JsonObject requestTemplate,
            final JsonObject argsPosition,
            final String method,
            final boolean argsToJsonBody,
            final ServerWebExchange exchange,
            final String path) {
        final ServerHttpRequest.Builder requestBuilder = buildRequestBuilder(
                exchange, method, path, sessionId, requestTemplate);
        final JsonObject bodyJson = RequestConfigHelper.buildBodyJson(argsToJsonBody, argsPosition, inputJson);

        ServerWebExchange mutatedExchange = exchange.mutate().request(requestBuilder.build()).build();
        if ("POST".equalsIgnoreCase(method) && argsToJsonBody && bodyJson.size() > 0) {
            mutatedExchange = new BodyWriterExchange(mutatedExchange,
                    bodyJson.toString());
        }
        return mutatedExchange;
    }

    private ServerHttpRequest.Builder buildRequestBuilder(
            final ServerWebExchange exchange, final String method, final String path, final String sessionId,
            final JsonObject requestTemplate) {
        ServerHttpRequest.Builder requestBuilder = exchange.getRequest()
                .mutate()
                .method(HttpMethod.valueOf(method))
                .header("sessionId", sessionId)
                .header("Accept", "application/json");
        if (requestTemplate.has("headers")) {
            for (var headerElem : requestTemplate.getAsJsonArray("headers")) {
                JsonObject headerObj = headerElem.getAsJsonObject();
                String key = headerObj.get("key").getAsString();
                String value = headerObj.get("value").getAsString();
                requestBuilder.header(key, value);
            }
        } else {
            // Only set Content-Type for requests with a body
            if ("POST".equalsIgnoreCase(method) || "PUT".equalsIgnoreCase(method) || "PATCH".equalsIgnoreCase(method)) {
                requestBuilder.header("Content-Type", "application/json");
            } else {
                // For other methods, do not set Content-Type
                requestBuilder.headers(httpHeaders -> httpHeaders.remove("Content-Type"));
            }
        }
        try {
            URI oldUri = exchange.getRequest().getURI();
            String newUriStr = oldUri.getScheme() + "://" + oldUri.getAuthority() + path;
            URI newUri = new URI(newUriStr);
            requestBuilder.uri(newUri);
        } catch (URISyntaxException e) {
            throw new RuntimeException("Invalid URI: " + e.getMessage(), e);
        }
        return requestBuilder;
    }

}
