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

package org.apache.shenyu.plugin.mcp.server.handler;

import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.mcp.server.manager.ShenyuMcpServerManager;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Arrays;
import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link McpServerPluginDataHandler}.
 */
@ExtendWith(MockitoExtension.class)
class McpServerPluginDataHandlerTest {

    @Mock
    private ShenyuMcpServerManager shenyuMcpServerManager;

    private McpServerPluginDataHandler dataHandler;

    @BeforeEach
    void setUp() {
        dataHandler = new McpServerPluginDataHandler(shenyuMcpServerManager);
    }

    @Test
    void testPluginNamed() {
        assertEquals(PluginEnum.MCP_SERVER.getName(), dataHandler.pluginNamed());
    }

    @Test
    void testHandlerSelectorWithNullData() {
        dataHandler.handlerSelector(null);
        verify(shenyuMcpServerManager, never()).getOrCreateMcpServerTransport(anyString(), anyString());
    }

    @Test
    void testHandlerSelectorWithNullId() {
        SelectorData selectorData = new SelectorData();
        selectorData.setId(null);
        
        dataHandler.handlerSelector(selectorData);
        verify(shenyuMcpServerManager, never()).getOrCreateMcpServerTransport(anyString(), anyString());
    }

    @Test
    void testHandlerSelectorWithEmptyConditions() {
        SelectorData selectorData = new SelectorData();
        selectorData.setId("selector1");
        selectorData.setConditionList(Collections.emptyList());
        
        dataHandler.handlerSelector(selectorData);
        verify(shenyuMcpServerManager, never()).getOrCreateMcpServerTransport(anyString(), anyString());
    }

    @Test
    void testHandlerSelectorWithValidData() {
        ConditionData condition = new ConditionData();
        condition.setParamType(ParamTypeEnum.URI.getName());
        condition.setParamValue("/mcp/test/**");
        
        SelectorData selectorData = new SelectorData();
        selectorData.setId("selector1");
        selectorData.setConditionList(Arrays.asList(condition));
        selectorData.setHandle("{\"messageEndpoint\":\"/message\"}");
        
        when(shenyuMcpServerManager.hasMcpServer(anyString())).thenReturn(false);
        when(shenyuMcpServerManager.getOrCreateMcpServerTransport(anyString(), anyString())).thenReturn(null);
        
        dataHandler.handlerSelector(selectorData);
        
        verify(shenyuMcpServerManager).getOrCreateMcpServerTransport(eq("/mcp/test/**"), eq("/message"));
    }

    @Test
    void testHandlerSelectorWithExistingServer() {
        ConditionData condition = new ConditionData();
        condition.setParamType(ParamTypeEnum.URI.getName());
        condition.setParamValue("/mcp/test/**");
        
        SelectorData selectorData = new SelectorData();
        selectorData.setId("selector1");
        selectorData.setConditionList(Arrays.asList(condition));
        selectorData.setHandle("{\"messageEndpoint\":\"/message\"}");
        
        when(shenyuMcpServerManager.hasMcpServer(anyString())).thenReturn(true);
        
        dataHandler.handlerSelector(selectorData);
        
        verify(shenyuMcpServerManager, never()).getOrCreateMcpServerTransport(anyString(), anyString());
    }

    @Test
    void testRemoveSelector() {
        ConditionData condition = new ConditionData();
        condition.setParamType(ParamTypeEnum.URI.getName());
        condition.setParamValue("/mcp/test/**");
        
        SelectorData selectorData = new SelectorData();
        selectorData.setId("selector1");
        selectorData.setConditionList(Arrays.asList(condition));
        
        when(shenyuMcpServerManager.hasMcpServer(anyString())).thenReturn(true);
        doNothing().when(shenyuMcpServerManager).removeMcpServer(anyString());
        
        dataHandler.removeSelector(selectorData);
        
        verify(shenyuMcpServerManager).removeMcpServer(eq("/mcp/test/**"));
    }

    @Test
    void testHandlerRuleWithValidData() {
        RuleData ruleData = new RuleData();
        ruleData.setId("rule1");
        ruleData.setSelectorId("selector1");
        ruleData.setName("testTool");
        ruleData.setHandle("{\"name\":\"testTool\",\"description\":\"A test tool\",\"requestConfig\":\"{\\\"url\\\":\\\"/test\\\",\\\"method\\\":\\\"GET\\\"}\",\"parameters\":[]}");
        
        // Mock the cached server
        McpServerPluginDataHandler.CACHED_SERVER.get().cachedHandle("selector1", 
            new org.apache.shenyu.plugin.mcp.server.model.ShenyuMcpServer());
        
        dataHandler.handlerRule(ruleData);
        
        // Verify that the method completes without exception
        // In a real scenario, you might want to verify the tool was added to the server
    }

    @Test
    void testHandlerRuleWithNullHandle() {
        RuleData ruleData = new RuleData();
        ruleData.setId("rule1");
        ruleData.setHandle(null);
        
        dataHandler.handlerRule(ruleData);
        
        verify(shenyuMcpServerManager, never()).addTool(anyString(), anyString(), anyString(), anyString(), anyString());
    }

    @Test
    void testRemoveRule() {
        RuleData ruleData = new RuleData();
        ruleData.setId("rule1");
        ruleData.setSelectorId("selector1");
        ruleData.setName("testTool");
        ruleData.setHandle("{\"name\":\"testTool\",\"description\":\"A test tool\"}");
        
        // Mock the cached server
        org.apache.shenyu.plugin.mcp.server.model.ShenyuMcpServer server = 
            new org.apache.shenyu.plugin.mcp.server.model.ShenyuMcpServer();
        server.setPath("/mcp/test");
        McpServerPluginDataHandler.CACHED_SERVER.get().cachedHandle("selector1", server);
        
        doNothing().when(shenyuMcpServerManager).removeTool(anyString(), anyString());
        
        dataHandler.removeRule(ruleData);
        
        verify(shenyuMcpServerManager).removeTool(eq("/mcp/test"), eq("testTool"));
    }

    @Test
    void testRemoveRuleWithNullHandle() {
        RuleData ruleData = new RuleData();
        ruleData.setId("rule1");
        ruleData.setHandle(null);
        
        dataHandler.removeRule(ruleData);
        
        verify(shenyuMcpServerManager, never()).removeTool(anyString(), anyString());
    }
}
