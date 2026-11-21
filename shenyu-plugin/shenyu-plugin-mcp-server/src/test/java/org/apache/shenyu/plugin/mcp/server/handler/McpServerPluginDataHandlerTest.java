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

import org.apache.shenyu.common.config.ShenyuConfig.RuleMatchCache;
import org.apache.shenyu.common.config.ShenyuConfig.SelectorMatchCache;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.cache.CommonPluginDataSubscriber;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.mcp.server.manager.ShenyuMcpServerManager;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
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
        McpServerPluginDataHandler.CACHED_SERVER.get().getAllCache().clear();
        McpServerPluginDataHandler.CACHED_TOOL.get().getAllCache().clear();
        BaseDataCache.getInstance().cleanRuleData();
        dataHandler = new McpServerPluginDataHandler(shenyuMcpServerManager);
    }

    @AfterEach
    void tearDown() {
        McpServerPluginDataHandler.CACHED_SERVER.get().getAllCache().clear();
        McpServerPluginDataHandler.CACHED_TOOL.get().getAllCache().clear();
        BaseDataCache.getInstance().cleanRuleData();
    }

    @Test
    void rulesArrivingBeforeSelectorsAreReplayedWithoutToggling() {
        RuleData rule = restartRule();
        dataHandler.handlerRule(rule);
        verify(shenyuMcpServerManager, never()).addTool(any(), any(), any(), any(), any());
        assertEquals("restart-tool", McpServerPluginDataHandler.CACHED_TOOL.get().obtainHandle(CacheKeyUtils.INST.getKey(rule)).getName());

        dataHandler.handlerSelector(restartSelector());

        verify(shenyuMcpServerManager).addTool(eq("/restart"), eq("restart-tool"), any(), any(), anyString());
        assertEquals("restart-tool", McpServerPluginDataHandler.CACHED_TOOL.get().obtainHandle(CacheKeyUtils.INST.getKey(rule)).getName());
    }

    @Test
    void deletedRulesAreNotReplayedWhenSelectorsArrive() {
        RuleData rule = restartRule();
        dataHandler.handlerRule(rule);
        dataHandler.removeRule(rule);
        dataHandler.handlerSelector(restartSelector());
        verify(shenyuMcpServerManager, never()).addTool(any(), any(), any(), any(), any());
        assertNull(McpServerPluginDataHandler.CACHED_TOOL.get().obtainHandle(CacheKeyUtils.INST.getKey(rule)));
    }

    @Test
    void disabledRulesAreNotReplayedWhenSelectorsArrive() {
        RuleData rule = restartRule();
        rule.setEnabled(false);
        dataHandler.handlerRule(rule);
        dataHandler.handlerSelector(restartSelector());
        verify(shenyuMcpServerManager, never()).addTool(any(), any(), any(), any(), any());
        assertNull(McpServerPluginDataHandler.CACHED_TOOL.get().obtainHandle(CacheKeyUtils.INST.getKey(rule)));
    }

    @Test
    void disablingAnActiveRuleRevokesTheRegisteredTool() {
        RuleData rule = restartRule();
        dataHandler.handlerRule(rule);
        dataHandler.handlerSelector(restartSelector());
        verify(shenyuMcpServerManager).addTool(eq("/restart"), eq("restart-tool"), any(), any(), anyString());

        RuleData disabled = restartRule();
        disabled.setEnabled(false);
        dataHandler.handlerRule(disabled);

        verify(shenyuMcpServerManager).removeTool("/restart", "restart-tool");
        assertNull(McpServerPluginDataHandler.CACHED_TOOL.get().obtainHandle(CacheKeyUtils.INST.getKey(rule)));
    }

    @Test
    void reEnablingARuleRegistersTheToolExactlyOnce() {
        dataHandler.handlerRule(restartRule());
        dataHandler.handlerSelector(restartSelector());

        RuleData disabled = restartRule();
        disabled.setEnabled(false);
        dataHandler.handlerRule(disabled);
        // duplicate updates for an already disabled rule must not revoke twice
        dataHandler.handlerRule(disabled);
        verify(shenyuMcpServerManager, times(1)).removeTool("/restart", "restart-tool");

        RuleData reEnabled = restartRule();
        reEnabled.setEnabled(true);
        dataHandler.handlerRule(reEnabled);
        verify(shenyuMcpServerManager, times(2)).addTool(eq("/restart"), eq("restart-tool"), any(), any(), anyString());
        assertEquals("restart-tool", McpServerPluginDataHandler.CACHED_TOOL.get().obtainHandle(CacheKeyUtils.INST.getKey(reEnabled)).getName());
    }

    @Test
    void disablingARenamedRuleRevokesTheRenamedTool() {
        dataHandler.handlerRule(restartRule());
        dataHandler.handlerSelector(restartSelector());

        RuleData renamed = restartRule();
        renamed.setHandle("{\"name\":\"renamed-tool\",\"parameters\":[]}");
        dataHandler.handlerRule(renamed);

        RuleData disabled = restartRule();
        disabled.setHandle("{\"name\":\"renamed-tool\",\"parameters\":[]}");
        disabled.setEnabled(false);
        dataHandler.handlerRule(disabled);

        verify(shenyuMcpServerManager).removeTool("/restart", "renamed-tool");
        verify(shenyuMcpServerManager, times(1)).removeTool("/restart", "restart-tool");
    }

    @Test
    void renamingACustomToolNameReplacesTheRegistration() {
        dataHandler.handlerRule(restartRule());
        dataHandler.handlerSelector(restartSelector());

        RuleData renamed = restartRule();
        renamed.setHandle("{\"name\":\"renamed-tool\",\"parameters\":[]}");
        dataHandler.handlerRule(renamed);

        verify(shenyuMcpServerManager).removeTool("/restart", "restart-tool");
        verify(shenyuMcpServerManager).addTool(eq("/restart"), eq("renamed-tool"), any(), any(), anyString());

        dataHandler.removeRule(renamed);
        verify(shenyuMcpServerManager).removeTool("/restart", "renamed-tool");
        verify(shenyuMcpServerManager, times(1)).removeTool("/restart", "restart-tool");
    }

    @Test
    void renamingThroughTheRuleNameReplacesTheRegistration() {
        RuleData rule = restartRule();
        rule.setHandle("{\"parameters\":[]}");
        dataHandler.handlerRule(rule);
        dataHandler.handlerSelector(restartSelector());

        RuleData renamed = restartRule();
        renamed.setName("renamed-tool");
        renamed.setHandle("{\"parameters\":[]}");
        dataHandler.handlerRule(renamed);

        verify(shenyuMcpServerManager).removeTool("/restart", "restart-tool");
        verify(shenyuMcpServerManager).addTool(eq("/restart"), eq("renamed-tool"), any(), any(), anyString());
    }

    @Test
    void sameNameUpdateReplacesWithoutRevocation() {
        dataHandler.handlerRule(restartRule());
        dataHandler.handlerSelector(restartSelector());

        RuleData updated = restartRule();
        updated.setHandle("{\"name\":\"restart-tool\",\"description\":\"updated\",\"parameters\":[]}");
        dataHandler.handlerRule(updated);

        verify(shenyuMcpServerManager, never()).removeTool(anyString(), anyString());
        verify(shenyuMcpServerManager, times(2)).addTool(eq("/restart"), eq("restart-tool"), any(), any(), anyString());
    }

    @Test
    void movingASelectorRevokesTheToolFromThePreviousServer() {
        dataHandler.handlerRule(restartRule());
        dataHandler.handlerSelector(restartSelector());
        when(shenyuMcpServerManager.hasMcpServer("/moved")).thenReturn(true);
        SelectorData moved = restartSelector();
        moved.getConditionList().get(0).setParamValue("/moved/**");

        dataHandler.handlerSelector(moved);

        verify(shenyuMcpServerManager).removeTool("/restart", "restart-tool");
        verify(shenyuMcpServerManager).addTool(eq("/moved"), eq("restart-tool"), any(), any(), anyString());
    }

    @Test
    void unchangedSelectorDoesNotRegisterToolsAgain() {
        when(shenyuMcpServerManager.hasMcpServer("/restart")).thenReturn(false, true);
        dataHandler.handlerRule(restartRule());
        dataHandler.handlerSelector(restartSelector());
        dataHandler.handlerSelector(restartSelector());
        verify(shenyuMcpServerManager).addTool(eq("/restart"), eq("restart-tool"), any(), any(), anyString());
    }

    @Test
    void movedSelectorReplaysRulesToAnExistingServer() {
        dataHandler.handlerRule(restartRule());
        dataHandler.handlerSelector(restartSelector());
        when(shenyuMcpServerManager.hasMcpServer("/moved")).thenReturn(true);
        SelectorData moved = restartSelector();
        moved.getConditionList().get(0).setParamValue("/moved/**");
        dataHandler.handlerSelector(moved);
        verify(shenyuMcpServerManager).removeMcpServer("/restart");
        verify(shenyuMcpServerManager).addTool(eq("/moved"), eq("restart-tool"), any(), any(), anyString());
    }

    @Test
    void replayUsesSnapshotOfTheReceivedRule() {
        RuleData rule = restartRule();
        dataHandler.handlerRule(rule);
        rule.setEnabled(false);
        rule.setSelectorId("another-selector");
        rule.setHandle(null);
        dataHandler.handlerSelector(restartSelector());
        verify(shenyuMcpServerManager).addTool(eq("/restart"), eq("restart-tool"), any(), any(), anyString());
    }

    @Test
    void testHandlerSelectorEndpointUpdateRebuildsServer() {
        // first sync creates the server with the default endpoint, and a rule registers its tool
        dataHandler.handlerSelector(endpointSelector("/message"));
        verify(shenyuMcpServerManager).getOrCreateMcpServerTransport("/endpoint", "/message");
        dataHandler.handlerRule(endpointRule());
        verify(shenyuMcpServerManager).addTool(eq("/endpoint"), eq("endpoint-tool"), any(), any(), anyString());

        // handle-only update: same path, new endpoint — the server must be rebuilt
        // around the new endpoint and the tools replayed onto it (#8044)
        org.mockito.Mockito.reset(shenyuMcpServerManager);
        when(shenyuMcpServerManager.hasMcpServer(anyString())).thenReturn(false);
        dataHandler.handlerSelector(endpointSelector("/rpc"));
        verify(shenyuMcpServerManager).removeMcpServer("/endpoint");
        verify(shenyuMcpServerManager).getOrCreateMcpServerTransport("/endpoint", "/rpc");
        verify(shenyuMcpServerManager).addTool(eq("/endpoint"), eq("endpoint-tool"), any(), any(), anyString());
    }

    @Test
    void testEndpointUpdateRebuildsEvenWhenTeardownFindsNoServer() {
        // first sync creates the server under the default endpoint
        dataHandler.handlerSelector(endpointSelector("/message"));
        verify(shenyuMcpServerManager).getOrCreateMcpServerTransport("/endpoint", "/message");

        // stale-window sync: the manager no longer knows a server under the previous
        // path, yet one still exists under the current path advertising the old
        // endpoint; the rebuild must not depend on that lookup (#8044)
        org.mockito.Mockito.reset(shenyuMcpServerManager);
        when(shenyuMcpServerManager.hasMcpServer(anyString())).thenReturn(false, true);
        dataHandler.handlerSelector(endpointSelector("/rpc"));
        verify(shenyuMcpServerManager).removeMcpServer("/endpoint");
        verify(shenyuMcpServerManager).getOrCreateMcpServerTransport("/endpoint", "/rpc");
    }

    @Test
    void testHandlerSelectorUnchangedEndpointDoesNotRebuildServer() {
        dataHandler.handlerSelector(endpointSelector("/message"));
        verify(shenyuMcpServerManager).getOrCreateMcpServerTransport("/endpoint", "/message");

        dataHandler.handlerSelector(endpointSelector("/message"));
        verify(shenyuMcpServerManager, never()).removeMcpServer(anyString());
        verify(shenyuMcpServerManager, never()).getOrCreateMcpServerTransport("/endpoint", "/rpc");
    }

    private SelectorData endpointSelector(final String messageEndpoint) {
        ConditionData condition = new ConditionData();
        condition.setParamType(ParamTypeEnum.URI.getName());
        condition.setParamValue("/endpoint/**");
        SelectorData selector = new SelectorData();
        selector.setId("endpoint-selector");
        selector.setConditionList(Collections.singletonList(condition));
        selector.setHandle("{\"messageEndpoint\":\"" + messageEndpoint + "\"}");
        return selector;
    }

    private RuleData endpointRule() {
        RuleData rule = new RuleData();
        rule.setId("endpoint-rule");
        rule.setSelectorId("endpoint-selector");
        rule.setName("endpoint-tool");
        rule.setHandle("{\"name\":\"endpoint-tool\",\"parameters\":[]}");
        return rule;
    }

    private RuleData restartRule() {
        RuleData rule = new RuleData();
        rule.setId("restart-rule");
        rule.setSelectorId("restart-selector");
        rule.setName("restart-tool");
        rule.setHandle("{\"name\":\"restart-tool\",\"parameters\":[]}");
        return rule;
    }

    private SelectorData restartSelector() {
        ConditionData condition = new ConditionData();
        condition.setParamType(ParamTypeEnum.URI.getName());
        condition.setParamValue("/restart/**");
        SelectorData selector = new SelectorData();
        selector.setId("restart-selector");
        selector.setConditionList(Collections.singletonList(condition));
        selector.setHandle("{\"messageEndpoint\":\"/message\"}");
        return selector;
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
    void testHandlerSelectorWithoutUriCondition() {
        ConditionData condition = new ConditionData();
        condition.setParamType(ParamTypeEnum.HEADER.getName());
        condition.setParamValue("x-session-id");

        SelectorData selectorData = new SelectorData();
        selectorData.setId("selector-no-uri");
        selectorData.setConditionList(Arrays.asList(condition));

        dataHandler.handlerSelector(selectorData);

        verify(shenyuMcpServerManager, never()).getOrCreateMcpServerTransport(anyString(), anyString());
        verify(shenyuMcpServerManager, never()).getOrCreateStreamableHttpTransport(anyString());
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
        
        verify(shenyuMcpServerManager).getOrCreateMcpServerTransport(eq("/mcp/test"), eq("/message"));
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
        
        verify(shenyuMcpServerManager).removeMcpServer(eq("/mcp/test"));
    }

    @Test
    void testHandlerSelectorPathUpdateRemovesPreviousServer() {
        org.apache.shenyu.plugin.mcp.server.model.ShenyuMcpServer previousServer =
                new org.apache.shenyu.plugin.mcp.server.model.ShenyuMcpServer();
        previousServer.setPath("/mcp/old");
        McpServerPluginDataHandler.CACHED_SERVER.get().cachedHandle("selector1", previousServer);
        when(shenyuMcpServerManager.hasMcpServer(anyString())).thenReturn(false);

        ConditionData condition = new ConditionData();
        condition.setParamType(ParamTypeEnum.URI.getName());
        condition.setParamValue("/mcp/new/**");
        SelectorData selectorData = new SelectorData();
        selectorData.setId("selector1");
        selectorData.setConditionList(Arrays.asList(condition));
        selectorData.setHandle("{\"messageEndpoint\":\"/message\"}");

        dataHandler.handlerSelector(selectorData);

        verify(shenyuMcpServerManager).removeMcpServer("/mcp/old");
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
    void testHandlerRuleWithBlankServerPath() {
        RuleData ruleData = new RuleData();
        ruleData.setId("rule-blank-path");
        ruleData.setSelectorId("selector-blank-path");
        ruleData.setName("testTool");
        ruleData.setHandle("{\"name\":\"testTool\",\"description\":\"A test tool\",\"requestConfig\":\"{\\\"url\\\":\\\"/test\\\",\\\"method\\\":\\\"GET\\\"}\",\"parameters\":[]}");

        org.apache.shenyu.plugin.mcp.server.model.ShenyuMcpServer server =
                new org.apache.shenyu.plugin.mcp.server.model.ShenyuMcpServer();
        server.setPath("");
        McpServerPluginDataHandler.CACHED_SERVER.get().cachedHandle("selector-blank-path", server);

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

    @Test
    void testRemoveRuleRemovesToolByConfiguredName() {
        RuleData ruleData = new RuleData();
        ruleData.setId("rule1");
        ruleData.setSelectorId("selector1");
        ruleData.setName("testTool");
        ruleData.setHandle("{\"name\":\"customTool\",\"description\":\"A test tool\",\"requestConfig\":\"{\\\"url\\\":\\\"/test\\\",\\\"method\\\":\\\"GET\\\"}\",\"parameters\":[]}");

        org.apache.shenyu.plugin.mcp.server.model.ShenyuMcpServer server = new org.apache.shenyu.plugin.mcp.server.model.ShenyuMcpServer();
        server.setPath("/mcp/test");
        McpServerPluginDataHandler.CACHED_SERVER.get().cachedHandle("selector1", server);

        dataHandler.handlerRule(ruleData);
        verify(shenyuMcpServerManager).addTool(eq("/mcp/test"), eq("customTool"), anyString(), anyString(), anyString());

        doNothing().when(shenyuMcpServerManager).removeTool(anyString(), anyString());
        dataHandler.removeRule(ruleData);

        verify(shenyuMcpServerManager).removeTool(eq("/mcp/test"), eq("customTool"));
        verify(shenyuMcpServerManager, never()).removeTool(eq("/mcp/test"), eq("testTool"));
    }

    @Test
    void movedRuleKeepsTheToolRegisteredOnTheNewSelectorThroughScopedRefresh() {
        // selectors A (/a) and B (/b) exist, and the rule's tool is registered under A
        dataHandler.handlerSelector(selectorAt("/a/**", "selector-a"));
        dataHandler.handlerSelector(selectorAt("/b/**", "selector-b"));
        final RuleData original = toolRule("selector-a");
        dataHandler.handlerRule(original);
        verify(shenyuMcpServerManager).addTool(eq("/a"), eq("moved-tool"), any(), any(), anyString());
        BaseDataCache.getInstance().cacheRuleData(original);

        // the rule keeps its id but moves to selector B; the scoped refresh must revoke the
        // old registration before installing the new one, otherwise removeRule(A) removes by
        // rule id and unregisters the tool handlerRule(B) just registered
        final CommonPluginDataSubscriber subscriber = new CommonPluginDataSubscriber(
                List.of(dataHandler), new SelectorMatchCache(), new RuleMatchCache());
        final RuleData moved = toolRule("selector-b");
        subscriber.refreshRuleDataScoped(List.of(moved));

        verify(shenyuMcpServerManager).removeTool("/a", "moved-tool");
        verify(shenyuMcpServerManager).addTool(eq("/b"), eq("moved-tool"), any(), any(), anyString());
        verify(shenyuMcpServerManager, never()).removeTool("/b", "moved-tool");
        assertEquals(List.of(moved), BaseDataCache.getInstance().obtainRuleData("selector-b"));
        assertNotNull(McpServerPluginDataHandler.CACHED_TOOL.get().obtainHandle(CacheKeyUtils.INST.getKey(moved)));
    }

    @Test
    void failedRuleRemovalIsRetriedThroughScopedRefresh() {
        dataHandler.handlerSelector(selectorAt("/a/**", "selector-a"));
        final RuleData stale = toolRule("selector-a");
        stale.setSort(1);
        dataHandler.handlerRule(stale);
        BaseDataCache.getInstance().cacheRuleData(stale);
        final RuleData replacement = toolRule("selector-a");
        replacement.setId("replacement-rule");
        replacement.setName("replacement-tool");
        replacement.setHandle("{\"name\":\"replacement-tool\",\"parameters\":[]}");
        replacement.setSort(2);
        final CommonPluginDataSubscriber subscriber = new CommonPluginDataSubscriber(
                List.of(dataHandler), new SelectorMatchCache(), new RuleMatchCache());
        doThrow(new IllegalStateException("removal failed")).doNothing()
                .when(shenyuMcpServerManager).removeTool("/a", "moved-tool");

        assertThrows(IllegalStateException.class, () -> subscriber.refreshRuleDataScoped(List.of(replacement)));
        assertEquals(List.of(stale, replacement), BaseDataCache.getInstance().obtainRuleData("selector-a"));

        subscriber.refreshRuleDataScoped(List.of(replacement));

        verify(shenyuMcpServerManager, times(2)).removeTool("/a", "moved-tool");
        verify(shenyuMcpServerManager).addTool(eq("/a"), eq("replacement-tool"), any(), any(), anyString());
        verify(shenyuMcpServerManager, never()).removeTool("/a", "replacement-tool");
        assertEquals(List.of(replacement), BaseDataCache.getInstance().obtainRuleData("selector-a"));
        assertNull(McpServerPluginDataHandler.CACHED_TOOL.get().obtainHandle(CacheKeyUtils.INST.getKey(stale)));
        assertNotNull(McpServerPluginDataHandler.CACHED_TOOL.get().obtainHandle(CacheKeyUtils.INST.getKey(replacement)));
    }

    private SelectorData selectorAt(final String uriPattern, final String selectorId) {
        ConditionData condition = new ConditionData();
        condition.setParamType(ParamTypeEnum.URI.getName());
        condition.setParamValue(uriPattern);
        SelectorData selector = new SelectorData();
        selector.setId(selectorId);
        selector.setConditionList(Collections.singletonList(condition));
        selector.setHandle("{\"messageEndpoint\":\"/message\"}");
        return selector;
    }

    private RuleData toolRule(final String selectorId) {
        RuleData rule = new RuleData();
        rule.setId("moved-rule");
        rule.setSelectorId(selectorId);
        rule.setPluginName(dataHandler.pluginNamed());
        rule.setNamespaceId("ns1");
        rule.setName("moved-tool");
        rule.setHandle("{\"name\":\"moved-tool\",\"parameters\":[]}");
        return rule;
    }
}
