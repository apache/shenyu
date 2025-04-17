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

import com.google.common.collect.Maps;
import com.google.gson.JsonObject;
import io.modelcontextprotocol.server.McpAsyncServerExchange;
import io.modelcontextprotocol.server.McpSyncServerExchange;
import io.modelcontextprotocol.spec.McpServerSession;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.web.handler.ShenyuWebHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.ai.chat.model.ToolContext;
import org.springframework.ai.tool.ToolCallback;
import org.springframework.ai.tool.definition.ToolDefinition;
import org.springframework.http.HttpMethod;
import org.springframework.http.server.reactive.ServerHttpResponseDecorator;
import org.springframework.lang.NonNull;
import org.springframework.web.server.ServerWebExchange;

import java.lang.reflect.Field;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

public class ShenyuToolCallback implements ToolCallback {

    private static final Logger LOG = LoggerFactory.getLogger(ShenyuToolCallback.class);
    
    private static final long TIMEOUT_SECONDS = 60;

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

        try {
            String sessionId = getSessionId(mcpSyncServerExchange);
            JsonObject inputJson = GsonUtils.getInstance().fromJson(input, JsonObject.class);
            ServerWebExchange exchange = createServerWebExchange(sessionId, inputJson);

            LOG.debug("Starting request processing for session: {}", sessionId);
            LOG.debug("Request method: {}, path: {}", exchange.getRequest().getMethod(),
                    exchange.getRequest().getPath());
            LOG.debug("Request headers: {}", exchange.getRequest().getHeaders());
            LOG.debug("Request query params: {}", exchange.getRequest().getQueryParams());

            CompletableFuture<String> future = new CompletableFuture<>();

            ServerHttpResponseDecorator responseDecorator = new ShenyuMcpResponseDecorator(exchange.getResponse(), sessionId, future);

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

            LOG.debug("Waiting for response for session: {}", sessionId);
            try {
                String response = future.get(TIMEOUT_SECONDS, TimeUnit.SECONDS);
                LOG.debug("Received response for session: {}, length: {}", sessionId, response.length());
                return response;
            } catch (TimeoutException e) {
                LOG.error("Request timed out after {} seconds for session: {}", TIMEOUT_SECONDS, sessionId);
                throw new RuntimeException("Request timed out after " + TIMEOUT_SECONDS + " seconds", e);
            }
        } catch (Exception e) {
            LOG.error("Failed to process request", e);
            throw new RuntimeException("Failed to process request: " + e.getMessage(), e);
        }
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
        ServerWebExchange exchange = ShenyuMcpExchangeHolder.get(sessionId);
        ShenyuToolDefinition shenyuToolDefinition = (ShenyuToolDefinition) this.toolDefinition;
        String requestMethod = StringUtils.isNoneBlank(shenyuToolDefinition.requestMethod()) ? shenyuToolDefinition.requestMethod() : "GET";
        String requestPath = StringUtils.isNoneBlank(shenyuToolDefinition.requestPath()) ? shenyuToolDefinition.requestPath() : this.toolDefinition.name();

        return exchange.mutate()
                .request(exchange.getRequest().mutate()
                        .method(HttpMethod.valueOf(requestMethod))
                        .path(requestPath)
                        .header("sessionId", sessionId)
                        .header("Accept", "application/json")
                        .header("Content-Type", "application/json")
                        .build())
                .build();
    }

}
