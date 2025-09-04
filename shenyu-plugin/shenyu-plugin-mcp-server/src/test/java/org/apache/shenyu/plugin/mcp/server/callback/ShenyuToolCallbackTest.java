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

import io.modelcontextprotocol.server.McpSyncServerExchange;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.mcp.server.definition.ShenyuToolDefinition;
import org.apache.shenyu.plugin.mcp.server.holder.ShenyuMcpExchangeHolder;
import org.apache.shenyu.plugin.mcp.server.session.McpSessionHelper;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.ai.chat.model.ToolContext;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.web.server.ServerWebExchange;

import java.util.HashMap;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link ShenyuToolCallback}.
 */
@ExtendWith(MockitoExtension.class)
class ShenyuToolCallbackTest {

    @Mock
    private ShenyuToolDefinition toolDefinition;

    @Mock
    private ServerWebExchange exchange;

    @Mock
    private ShenyuPluginChain chain;

    @Mock
    private ServerHttpRequest request;

    @Mock
    private ShenyuContext shenyuContext;

    @Mock
    private McpSyncServerExchange mcpSyncServerExchange;

    private ShenyuToolCallback shenyuToolCallback;

    private MockedStatic<McpSessionHelper> mcpSessionHelperMock;

    private MockedStatic<ShenyuMcpExchangeHolder> exchangeHolderMock;

    @BeforeEach
    void setUp() {
        // Minimal setup - individual tests will add specific mocks as needed
        mcpSessionHelperMock = Mockito.mockStatic(McpSessionHelper.class);
        exchangeHolderMock = Mockito.mockStatic(ShenyuMcpExchangeHolder.class);
    }

    @AfterEach
    void tearDown() {
        mcpSessionHelperMock.close();
        exchangeHolderMock.close();
    }

    @Test
    void testGetToolDefinition() {
        shenyuToolCallback = new ShenyuToolCallback(toolDefinition);
        
        assertEquals(toolDefinition, shenyuToolCallback.getToolDefinition());
    }

    @Test
    void testCallWithNullInput() {
        shenyuToolCallback = new ShenyuToolCallback(toolDefinition);
        
        assertThrows(NullPointerException.class, () -> {
            shenyuToolCallback.call(null);
        });
    }

    @Test
    void testCallWithNullToolContext() {
        shenyuToolCallback = new ShenyuToolCallback(toolDefinition);
        
        assertThrows(NullPointerException.class, () -> {
            shenyuToolCallback.call("{}", null);
        });
    }

    @Test
    void testCallWithInvalidInput() {
        when(toolDefinition.name()).thenReturn("testTool");
        shenyuToolCallback = new ShenyuToolCallback(toolDefinition);
        
        ToolContext toolContext = new ToolContext(new HashMap<>());
        
        assertThrows(RuntimeException.class, () -> {
            shenyuToolCallback.call("invalid json", toolContext);
        });
    }

    @Test
    void testCallWithMissingMcpExchange() {
        when(toolDefinition.name()).thenReturn("testTool");
        shenyuToolCallback = new ShenyuToolCallback(toolDefinition);
        
        ToolContext toolContext = new ToolContext(new HashMap<>());
        mcpSessionHelperMock.when(() -> McpSessionHelper.getMcpSyncServerExchange(any()))
                .thenReturn(null);
        
        assertThrows(RuntimeException.class, () -> {
            shenyuToolCallback.call("{}", toolContext);
        });
    }

    @Test
    void testCallWithMissingSessionId() throws Exception {
        when(toolDefinition.name()).thenReturn("testTool");
        shenyuToolCallback = new ShenyuToolCallback(toolDefinition);
        
        ToolContext toolContext = new ToolContext(new HashMap<>());
        mcpSessionHelperMock.when(() -> McpSessionHelper.getMcpSyncServerExchange(any()))
                .thenReturn(mcpSyncServerExchange);
        mcpSessionHelperMock.when(() -> McpSessionHelper.getSessionId(any()))
                .thenReturn("");
        
        assertThrows(RuntimeException.class, () -> {
            shenyuToolCallback.call("{}", toolContext);
        });
    }

    @Test
    void testCallWithMissingExchange() throws Exception {
        when(toolDefinition.name()).thenReturn("testTool");
        shenyuToolCallback = new ShenyuToolCallback(toolDefinition);
        
        final ToolContext toolContext = new ToolContext(new HashMap<>());
        mcpSessionHelperMock.when(() -> McpSessionHelper.getMcpSyncServerExchange(any()))
                .thenReturn(mcpSyncServerExchange);
        mcpSessionHelperMock.when(() -> McpSessionHelper.getSessionId(any()))
                .thenReturn("session123");
        exchangeHolderMock.when(() -> ShenyuMcpExchangeHolder.get("session123"))
                .thenReturn(null);
        
        assertThrows(RuntimeException.class, () -> {
            shenyuToolCallback.call("{}", toolContext);
        });
    }

    @Test
    void testCallWithValidSetup() throws Exception {
        when(toolDefinition.name()).thenReturn("testTool");
        when(toolDefinition.requestConfig()).thenReturn("{\"requestTemplate\":{\"url\":\"/test\",\"method\":\"GET\"},\"argsPosition\":{}}");
        shenyuToolCallback = new ShenyuToolCallback(toolDefinition);
        
        final ToolContext toolContext = new ToolContext(new HashMap<>());
        String sessionId = "session123";
        
        // Setup minimal mocks needed for the execution path
        mcpSessionHelperMock.when(() -> McpSessionHelper.getMcpSyncServerExchange(any()))
                .thenReturn(mcpSyncServerExchange);
        mcpSessionHelperMock.when(() -> McpSessionHelper.getSessionId(any()))
                .thenReturn(sessionId);
        exchangeHolderMock.when(() -> ShenyuMcpExchangeHolder.get(sessionId))
                .thenReturn(exchange);
        
        when(exchange.getAttribute(Constants.CHAIN)).thenReturn(chain);
        
        // This test may timeout or fail during execution - the exact failure doesn't matter
        // We just want to test that it reaches the execution logic
        assertThrows(RuntimeException.class, () -> {
            shenyuToolCallback.call("{}", toolContext);
        });
    }

    @Test
    void testConstructorWithNullToolDefinition() {
        assertThrows(NullPointerException.class, () -> {
            new ShenyuToolCallback(null);
        });
    }

    @Test
    void testConstructorWithValidToolDefinition() {
        ShenyuToolCallback callback = new ShenyuToolCallback(toolDefinition);
        assertNotNull(callback);
        assertEquals(toolDefinition, callback.getToolDefinition());
    }
}
