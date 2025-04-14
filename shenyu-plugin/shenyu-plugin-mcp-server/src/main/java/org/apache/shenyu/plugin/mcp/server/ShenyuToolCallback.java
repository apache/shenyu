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
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.web.handler.ShenyuWebHandler;
import org.springframework.ai.chat.model.ToolContext;
import org.springframework.ai.tool.ToolCallback;
import org.springframework.ai.tool.definition.ToolDefinition;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.HttpMethod;
import org.springframework.lang.NonNull;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.lang.reflect.Field;
import java.nio.charset.StandardCharsets;
import java.util.Map;

public class ShenyuToolCallback implements ToolCallback {
    
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
        
        if (toolContext == null) {
            throw new IllegalArgumentException("ToolContext is required");
        }
        
        Map<String, Object> contextMap = toolContext.getContext();
        
        if (contextMap == null || contextMap.isEmpty()) {
            throw new IllegalArgumentException("ToolContext is required");
        }
        
        McpSyncServerExchange mcpSyncServerExchange = (McpSyncServerExchange) contextMap.get("exchange");
        if (mcpSyncServerExchange == null) {
            throw new IllegalArgumentException("McpSyncServerExchange is required in ToolContext");
        }
        
        try {
            String sessionId = getSessionId(mcpSyncServerExchange);
            
            JsonObject inputJson = GsonUtils.getInstance().fromJson(input, JsonObject.class);
            ServerWebExchange exchange = createServerWebExchange(sessionId, inputJson);
            
            return shenyuWebHandler.handle(exchange).then(Mono.just(exchange)).map(ex -> {
                String responseBody = "Response processed";
                DataBuffer buffer = ex.getResponse().bufferFactory().wrap(responseBody.getBytes(StandardCharsets.UTF_8));
                return buffer.toString(StandardCharsets.UTF_8);
            }).block();
        } catch (Exception e) {
            throw new RuntimeException("Failed to get McpAsyncServerExchange: " + e.getMessage(), e);
        }
    }
    
    private static String getSessionId(final McpSyncServerExchange mcpSyncServerExchange) throws NoSuchFieldException, IllegalAccessException {
        Field asyncExchangeField = mcpSyncServerExchange.getClass().getDeclaredField("exchange");
        asyncExchangeField.setAccessible(true);
        Object asyncExchange = asyncExchangeField.get(mcpSyncServerExchange);
        
        if (asyncExchange == null) {
            throw new IllegalArgumentException("McpAsyncServerExchange is required in McpSyncServerExchange");
        }
        
        McpAsyncServerExchange mcpAsyncServerExchange = (McpAsyncServerExchange) asyncExchange;
        Field sessionField = mcpAsyncServerExchange.getClass().getDeclaredField("session");
        sessionField.setAccessible(true);
        Object session = sessionField.get(mcpAsyncServerExchange);
        
        if (null == session) {
            throw new IllegalArgumentException("Session is required in McpAsyncServerExchange");
        }
        McpServerSession mcpServerSession = (McpServerSession) session;
        
        return mcpServerSession.getId();
    }
    
    private ServerWebExchange createServerWebExchange(final String sessionId, final JsonObject inputJson) {
        ServerWebExchange exchange = ShenyuMcpExchangeHolder.get(sessionId);
        ShenyuToolDefinition shenyuToolDefinition = (ShenyuToolDefinition) this.toolDefinition;
        RuleData ruleData = shenyuToolDefinition.ruleData();
        String method = inputJson.has("method") ? inputJson.get("method").getAsString() : "GET";
        String path = inputJson.has("path") ? inputJson.get("path").getAsString() : this.toolDefinition.name();
        String body = inputJson.has("body") ? inputJson.get("body").getAsString() : "";
        return exchange.mutate().request(exchange.getRequest().mutate().method(HttpMethod.valueOf(method)).path(path).header("sessionId", sessionId).build()).build();
    }
    
}
