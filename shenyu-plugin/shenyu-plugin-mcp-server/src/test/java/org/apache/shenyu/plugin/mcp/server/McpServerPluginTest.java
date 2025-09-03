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
import org.apache.shenyu.plugin.mcp.server.manager.ShenyuMcpServerManager;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.codec.HttpMessageReader;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.net.URI;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link McpServerPlugin}.
 */
@ExtendWith(MockitoExtension.class)
class McpServerPluginTest {

    @Mock
    private ShenyuMcpServerManager shenyuMcpServerManager;

    @Mock
    private List<HttpMessageReader<?>> messageReaders;

    @Mock
    private ServerWebExchange exchange;

    @Mock
    private ShenyuPluginChain chain;

    @Mock
    private ServerHttpRequest request;

    @Mock
    private SelectorData selector;

    @Mock
    private RuleData rule;

    @Mock
    private ShenyuContext shenyuContext;

    private McpServerPlugin mcpServerPlugin;

    @BeforeEach
    void setUp() {
        mcpServerPlugin = new McpServerPlugin(shenyuMcpServerManager, messageReaders);
    }

    @Test
    void testNamed() {
        assertEquals(PluginEnum.MCP_SERVER.getName(), mcpServerPlugin.named());
    }

    @Test
    void testGetOrder() {
        assertEquals(PluginEnum.MCP_SERVER.getCode(), mcpServerPlugin.getOrder());
    }

    @Test
    void testSkipWithMcpToolCall() {
        when(exchange.getAttribute("MCP_TOOL_CALL")).thenReturn(true);
        assertTrue(mcpServerPlugin.skip(exchange));
    }

    @Test
    void testSkipWithNonHttpRpcType() {
        when(exchange.getAttribute("MCP_TOOL_CALL")).thenReturn(null);
        when(exchange.getAttribute(Constants.CONTEXT)).thenReturn(shenyuContext);
        when(shenyuContext.getRpcType()).thenReturn(RpcTypeEnum.DUBBO.getName());
        
        assertTrue(mcpServerPlugin.skip(exchange));
    }

    @Test
    void testSkipWithHttpRpcType() {
        when(exchange.getAttribute("MCP_TOOL_CALL")).thenReturn(null);
        when(exchange.getAttribute(Constants.CONTEXT)).thenReturn(shenyuContext);
        when(shenyuContext.getRpcType()).thenReturn(RpcTypeEnum.HTTP.getName());
        
        assertFalse(mcpServerPlugin.skip(exchange));
    }

    @Test
    void testDoExecuteWhenCannotRoute() {
        when(exchange.getAttribute(Constants.CONTEXT)).thenReturn(shenyuContext);
        when(exchange.getRequest()).thenReturn(request);
        when(request.getURI()).thenReturn(URI.create("http://localhost:8080/test"));
        when(shenyuMcpServerManager.canRoute(anyString())).thenReturn(false);
        when(chain.execute(exchange)).thenReturn(Mono.empty());

        StepVerifier.create(mcpServerPlugin.doExecute(exchange, chain, selector, rule))
                .verifyComplete();
    }

    @Test
    void testDoExecuteWhenCanRoute() {
        when(exchange.getAttribute(Constants.CONTEXT)).thenReturn(shenyuContext);
        when(exchange.getRequest()).thenReturn(request);
        when(request.getURI()).thenReturn(URI.create("http://localhost:8080/mcp/sse"));
        when(shenyuMcpServerManager.canRoute(anyString())).thenReturn(false);
        when(chain.execute(exchange)).thenReturn(Mono.empty());

        StepVerifier.create(mcpServerPlugin.doExecute(exchange, chain, selector, rule))
                .verifyComplete();
    }

    @Test
    void testGetRawPath() {
        when(exchange.getRequest()).thenReturn(request);
        when(request.getURI()).thenReturn(URI.create("http://localhost:8080/test/path"));
        
        String rawPath = mcpServerPlugin.getRawPath(exchange);
        assertEquals("/test/path", rawPath);
    }
}
