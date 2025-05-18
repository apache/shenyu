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
import io.modelcontextprotocol.server.McpAsyncServerExchange;
import io.modelcontextprotocol.server.McpSyncServerExchange;
import io.modelcontextprotocol.spec.McpServerSession;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.mcp.server.decorator.ShenyuMcpResponseDecorator;
import org.apache.shenyu.plugin.mcp.server.definition.ShenyuToolDefinition;
import org.apache.shenyu.plugin.mcp.server.holder.ShenyuMcpExchangeHolder;
import org.apache.shenyu.web.handler.ShenyuWebHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.ai.chat.model.ToolContext;
import org.springframework.ai.tool.ToolCallback;
import org.springframework.ai.tool.definition.ToolDefinition;
import org.springframework.http.server.reactive.ServerHttpResponseDecorator;
import org.springframework.lang.NonNull;
import org.springframework.web.server.ServerWebExchange;

import java.lang.reflect.Field;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;

public class ShenyuToolCallback implements ToolCallback {

    private static final Logger LOG = LoggerFactory.getLogger(ShenyuToolCallback.class);

    private final ShenyuWebHandler shenyuWebHandler;

    private final ToolDefinition toolDefinition;

    public ShenyuToolCallback(final ShenyuWebHandler shenyuWebHandler, final ToolDefinition toolDefinition) {
        this.shenyuWebHandler = shenyuWebHandler;
        this.toolDefinition = toolDefinition;
    }

    @Override
    public ToolDefinition getToolDefinition() {
        return this.toolDefinition;
    }

    @Override
    public String call(@NonNull final String input) {
        return call(input, new ToolContext(Maps.newHashMap()));
    }

    @Override
    public String call(@NonNull final String input, final ToolContext toolContext) {

        try {
            McpSyncServerExchange mcpSyncServerExchange = getMcpSyncServerExchange(toolContext);
            String sessionId = getSessionId(mcpSyncServerExchange);
            JsonObject inputJson = GsonUtils.getInstance().fromJson(input, JsonObject.class);
            ServerWebExchange exchange = createServerWebExchange(sessionId, inputJson);

            LOG.debug("Starting request processing for session: {}", sessionId);
            LOG.debug("Request method: {}, path: {}", exchange.getRequest().getMethod(),
                    exchange.getRequest().getPath());
            LOG.debug("Request headers: {}", exchange.getRequest().getHeaders());
            LOG.debug("Request query params: {}", exchange.getRequest().getQueryParams());

            CompletableFuture<String> future = new CompletableFuture<>();

            ServerHttpResponseDecorator responseDecorator = new ShenyuMcpResponseDecorator(exchange.getResponse(),
                    sessionId, future);

            ServerWebExchange decoratedExchange = exchange.mutate().response(responseDecorator).build();

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

    private static McpSyncServerExchange getMcpSyncServerExchange(final ToolContext toolContext) {
        if (Objects.isNull(toolContext)) {
            throw new IllegalArgumentException("ToolContext is required");
        }

        Map<String, Object> contextMap = toolContext.getContext();

        if (Objects.isNull(contextMap) || contextMap.isEmpty()) {
            throw new IllegalArgumentException("ToolContext is required");
        }

        McpSyncServerExchange mcpSyncServerExchange = (McpSyncServerExchange) contextMap.get("exchange");
        if (Objects.isNull(mcpSyncServerExchange)) {
            throw new IllegalArgumentException("McpSyncServerExchange is required in ToolContext");
        }
        return mcpSyncServerExchange;
    }

    private static String getSessionId(final McpSyncServerExchange mcpSyncServerExchange)
            throws NoSuchFieldException, IllegalAccessException {
        Field asyncExchangeField = mcpSyncServerExchange.getClass().getDeclaredField("exchange");
        asyncExchangeField.setAccessible(true);
        Object asyncExchange = asyncExchangeField.get(mcpSyncServerExchange);

        if (Objects.isNull(asyncExchange)) {
            throw new IllegalArgumentException("McpAsyncServerExchange is required in McpSyncServerExchange");
        }

        McpAsyncServerExchange mcpAsyncServerExchange = (McpAsyncServerExchange) asyncExchange;
        Field sessionField = mcpAsyncServerExchange.getClass().getDeclaredField("session");
        sessionField.setAccessible(true);
        Object session = sessionField.get(mcpAsyncServerExchange);

        if (Objects.isNull(session)) {
            throw new IllegalArgumentException("Session is required in McpAsyncServerExchange");
        }
        McpServerSession mcpServerSession = (McpServerSession) session;

        return mcpServerSession.getId();
    }

    private ServerWebExchange createServerWebExchange(final String sessionId, final JsonObject inputJson) {
        final ServerWebExchange exchange = ShenyuMcpExchangeHolder.get(sessionId);
        final ShenyuToolDefinition shenyuToolDefinition = (ShenyuToolDefinition) this.toolDefinition;
        final String requestConfig = shenyuToolDefinition.requestConfig();
        LOG.info("requestConfig: {}", requestConfig);

        final JsonObject configJson = GsonUtils.getInstance().fromJson(requestConfig, JsonObject.class);
        final JsonObject requestTemplate = configJson.getAsJsonObject("requestTemplate");
        final String urlTemplate = requestTemplate.get("url").getAsString();
        final String method = requestTemplate.has("method") ? requestTemplate.get("method").getAsString() : "GET";
        final JsonObject argsPosition = configJson.has("argsPosition") ? configJson.getAsJsonObject("argsPosition")
                : new JsonObject();

        final String path = buildPath(urlTemplate, argsPosition, inputJson, requestTemplate);
        final org.springframework.http.server.reactive.ServerHttpRequest.Builder requestBuilder = buildRequestBuilder(
                exchange, method, path, sessionId, requestTemplate);
        final boolean argsToJsonBody = requestTemplate.has("argsToJsonBody")
                && requestTemplate.get("argsToJsonBody").getAsBoolean();
        final JsonObject bodyJson = buildBodyJson(argsToJsonBody, argsPosition, inputJson);

        ServerWebExchange mutatedExchange = exchange.mutate().request(requestBuilder.build()).build();
        if ("POST".equalsIgnoreCase(method) && argsToJsonBody && bodyJson.size() > 0) {
            mutatedExchange = new org.apache.shenyu.plugin.mcp.server.utils.BodyWriterExchange(mutatedExchange,
                    bodyJson.toString());
        }
        return mutatedExchange;
    }

    private String buildPath(final String urlTemplate, final JsonObject argsPosition, final JsonObject inputJson,
            final JsonObject requestTemplate) {
        String path = urlTemplate;
        StringBuilder queryBuilder = new StringBuilder();
        for (String key : argsPosition.keySet()) {
            String position = argsPosition.get(key).getAsString();
            if ("path".equals(position) && inputJson.has(key)) {
                path = path.replace("{{." + key + "}}", inputJson.get(key).getAsString());
            } else if ("query".equals(position) && inputJson.has(key)) {
                if (queryBuilder.length() > 0) {
                    queryBuilder.append("&");
                }
                queryBuilder.append(key).append("=").append(inputJson.get(key).getAsString());
            }
        }
        path = path.replaceAll("\\{\\{\\.[^}]+}}", "");
        boolean argsToUrlParam = requestTemplate.has("argsToUrlParam")
                && requestTemplate.get("argsToUrlParam").getAsBoolean();
        if (argsToUrlParam && queryBuilder.length() > 0) {
            if (path.contains("?")) {
                path = path + "&" + queryBuilder;
            } else {
                path = path + "?" + queryBuilder;
            }
        }
        return path;
    }

    private org.springframework.http.server.reactive.ServerHttpRequest.Builder buildRequestBuilder(
            final ServerWebExchange exchange, final String method, final String path, final String sessionId,
            final JsonObject requestTemplate) {
        org.springframework.http.server.reactive.ServerHttpRequest.Builder requestBuilder = exchange.getRequest()
                .mutate()
                .method(org.springframework.http.HttpMethod.valueOf(method))
                .path(path)
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
            requestBuilder.header("Content-Type", "application/json");
        }
        return requestBuilder;
    }

    private JsonObject buildBodyJson(final boolean argsToJsonBody, final JsonObject argsPosition,
            final JsonObject inputJson) {
        JsonObject bodyJson = new JsonObject();
        if (argsToJsonBody) {
            for (String key : argsPosition.keySet()) {
                String position = argsPosition.get(key).getAsString();
                if ("body".equals(position) && inputJson.has(key)) {
                    bodyJson.add(key, inputJson.get(key));
                }
            }
        }
        return bodyJson;
    }

}
