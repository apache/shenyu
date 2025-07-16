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

package org.apache.shenyu.plugin.mcp.server.session;

import io.modelcontextprotocol.server.McpAsyncServerExchange;
import io.modelcontextprotocol.server.McpSyncServerExchange;
import io.modelcontextprotocol.spec.McpServerSession;
import org.springframework.ai.chat.model.ToolContext;

import java.lang.reflect.Field;
import java.util.Map;
import java.util.Objects;

/**
 * Helper class for handling McpSession related operations.
 */
public class McpSessionHelper {
    
    /**
     * Get McpSyncServerExchange from ToolContext.
     *
     * @param toolContext the tool context
     * @return the McpSyncServerExchange instance
     */
    public static McpSyncServerExchange getMcpSyncServerExchange(final ToolContext toolContext) {
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
    
    /**
     * Get sessionId from McpSyncServerExchange.
     *
     * @param mcpSyncServerExchange the McpSyncServerExchange instance
     * @return the session id string
     * @throws NoSuchFieldException if field not found
     * @throws IllegalAccessException if field not accessible
     */
    public static String getSessionId(final McpSyncServerExchange mcpSyncServerExchange)
            throws NoSuchFieldException, IllegalAccessException {
        Field asyncExchangeField = mcpSyncServerExchange.getClass().getDeclaredField("exchange");
        asyncExchangeField.setAccessible(true);
        Object session = getSession(mcpSyncServerExchange, asyncExchangeField);
        if (Objects.isNull(session)) {
            throw new IllegalArgumentException("Session is required in McpAsyncServerExchange");
        }
        McpServerSession mcpServerSession = (McpServerSession) session;
        return mcpServerSession.getId();
    }
    
    /**
     * Get sessionId from McpSyncServerExchange.
     *
     * @param mcpSyncServerExchange the McpSyncServerExchange instance
     * @return the session id string
     * @throws NoSuchFieldException if field not found
     * @throws IllegalAccessException if field not accessible
     */
    public static McpServerSession getSession(final McpSyncServerExchange mcpSyncServerExchange)
            throws NoSuchFieldException, IllegalAccessException {
        Field asyncExchangeField = mcpSyncServerExchange.getClass().getDeclaredField("exchange");
        asyncExchangeField.setAccessible(true);
        Object session = getSession(mcpSyncServerExchange, asyncExchangeField);
        if (Objects.isNull(session)) {
            throw new IllegalArgumentException("Session is required in McpAsyncServerExchange");
        }
        return (McpServerSession) session;
    }
    
    private static Object getSession(final McpSyncServerExchange mcpSyncServerExchange, final Field asyncExchangeField) throws IllegalAccessException, NoSuchFieldException {
        Object asyncExchange = asyncExchangeField.get(mcpSyncServerExchange);
        if (Objects.isNull(asyncExchange)) {
            throw new IllegalArgumentException("McpAsyncServerExchange is required in McpSyncServerExchange");
        }
        McpAsyncServerExchange mcpAsyncServerExchange = (McpAsyncServerExchange) asyncExchange;
        Field sessionField = mcpAsyncServerExchange.getClass().getDeclaredField("session");
        sessionField.setAccessible(true);
        return sessionField.get(mcpAsyncServerExchange);
    }
}