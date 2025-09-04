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

package org.apache.shenyu.plugin.mcp.server.manager;

import org.apache.shenyu.plugin.mcp.server.transport.ShenyuSseServerTransportProvider;
import org.apache.shenyu.plugin.mcp.server.transport.ShenyuStreamableHttpServerTransportProvider;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test case for {@link ShenyuMcpServerManager}.
 */
@ExtendWith(MockitoExtension.class)
class ShenyuMcpServerManagerTest {

    private ShenyuMcpServerManager shenyuMcpServerManager;

    @BeforeEach
    void setUp() {
        shenyuMcpServerManager = new ShenyuMcpServerManager();
    }

    
    
    @Test
    void testGetOrCreateMcpServerTransport() {
        String uri = "/mcp/test";
        String messageEndpoint = "/message";
        
        ShenyuSseServerTransportProvider transport = shenyuMcpServerManager.getOrCreateMcpServerTransport(uri, messageEndpoint);
        
        assertNotNull(transport);
        assertTrue(shenyuMcpServerManager.hasMcpServer(uri));
    }

    @Test
    void testGetOrCreateStreamableHttpTransport() {
        String uri = "/mcp/test/streamablehttp";
        
        ShenyuStreamableHttpServerTransportProvider transport = shenyuMcpServerManager.getOrCreateStreamableHttpTransport(uri);
        
        assertNotNull(transport);
        assertTrue(shenyuMcpServerManager.hasMcpServer(uri));
    }

    @Test
    void testCanRouteWithExactMatch() {
        String uri = "/mcp/test";
        String messageEndpoint = "/message";
        
        shenyuMcpServerManager.getOrCreateMcpServerTransport(uri, messageEndpoint);
        
        assertTrue(shenyuMcpServerManager.canRoute(uri));
        assertTrue(shenyuMcpServerManager.canRoute(uri + messageEndpoint));
    }

    @Test
    void testCanRouteWithPatternMatch() {
        String uri = "/mcp/test";
        String messageEndpoint = "/message";
        
        shenyuMcpServerManager.getOrCreateMcpServerTransport(uri, messageEndpoint);
        
        assertTrue(shenyuMcpServerManager.canRoute(uri + "/anything"));
        assertTrue(shenyuMcpServerManager.canRoute(uri + messageEndpoint + "/anything"));
    }

    @Test
    void testCanRouteWithNoMatch() {
        assertFalse(shenyuMcpServerManager.canRoute("/unknown/path"));
    }

    @Test
    void testAddTool() {
        String serverPath = "/mcp/test";
        String messageEndpoint = "/message";
        
        // First create the server
        shenyuMcpServerManager.getOrCreateMcpServerTransport(serverPath, messageEndpoint);
        
        // Then add a tool
        String toolName = "testTool";
        String description = "A test tool";
        String requestTemplate = "{\"url\":\"/test\",\"method\":\"GET\"}";
        String inputSchema = "{\"type\":\"object\"}";
        
        // This should not throw an exception
        shenyuMcpServerManager.addTool(serverPath, toolName, description, requestTemplate, inputSchema);
    }

    @Test
    void testRemoveTool() {
        String serverPath = "/mcp/test";
        String messageEndpoint = "/message";
        String toolName = "testTool";
        
        // First create the server
        shenyuMcpServerManager.getOrCreateMcpServerTransport(serverPath, messageEndpoint);
        
        // This should not throw an exception even if tool doesn't exist
        shenyuMcpServerManager.removeTool(serverPath, toolName);
    }

    @Test
    void testRemoveMcpServer() {
        String uri = "/mcp/test";
        String messageEndpoint = "/message";
        
        shenyuMcpServerManager.getOrCreateMcpServerTransport(uri, messageEndpoint);
        assertTrue(shenyuMcpServerManager.hasMcpServer(uri));
        
        shenyuMcpServerManager.removeMcpServer(uri);
        assertFalse(shenyuMcpServerManager.hasMcpServer(uri));
    }

    @Test
    void testGetSupportedProtocols() {
        String uri = "/mcp";
        String messageEndpoint = "/mcp/message";
        
        shenyuMcpServerManager.getOrCreateMcpServerTransport(uri, messageEndpoint);
        
        Set<String> protocols = shenyuMcpServerManager.getSupportedProtocols(uri);
        assertNotNull(protocols);
        assertTrue(protocols.contains("SSE"));
    }

    @Test
    void testGetSupportedProtocolsForStreamableHttp() {
        String uri = "/mcp/test/streamablehttp";
        
        shenyuMcpServerManager.getOrCreateStreamableHttpTransport(uri);
        
        // Use base path since that's what the manager uses internally
        Set<String> protocols = shenyuMcpServerManager.getSupportedProtocols("/mcp");
        assertNotNull(protocols);
        assertTrue(protocols.contains("Streamable HTTP"));
    }

    @Test
    void testNormalizeServerPathWithStreamableHttp() {
        String uri = "/mcp/test/streamablehttp";
        
        shenyuMcpServerManager.getOrCreateStreamableHttpTransport(uri);
        
        // Both the original URI and normalized path should work
        assertTrue(shenyuMcpServerManager.hasMcpServer(uri));
        assertTrue(shenyuMcpServerManager.hasMcpServer("/mcp/test"));
    }
}
