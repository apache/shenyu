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

package org.apache.shenyu.plugin.base.cache;

import com.google.common.collect.Lists;
import org.apache.shenyu.common.config.ShenyuConfig.RuleMatchCache;
import org.apache.shenyu.common.config.ShenyuConfig.SelectorMatchCache;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginHandlerEventEnum;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.ConfigurableApplicationContext;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * Test cases for CommonPluginDataSubscriber.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class CommonPluginDataSubscriberTest {

    private final String mockName1 = "MOCK_NAME_1";

    private final String mockName2 = "MOCK_NAME_2";

    private final String mockPluginName1 = "MOCK_PLUGIN_NAME_1";

    private final String mockPluginName2 = "MOCK_PLUGIN_NAME_2";

    private final String mockSelectorId1 = "MOCK_SELECTOR_ID_1";

    private final String mockSelectorId2 = "MOCK_SELECTOR_ID_2";

    private CommonPluginDataSubscriber commonPluginDataSubscriber;

    @Mock
    private ApplicationEventPublisher eventPublisher;
    
    private BaseDataCache baseDataCache;

    @Mock
    private PluginDataHandler handler;

    @BeforeEach
    public void setup() {
        this.mockShenyuTrieConfig();
        ArrayList<PluginDataHandler> pluginDataHandlerList = Lists.newArrayList();
        commonPluginDataSubscriber = new CommonPluginDataSubscriber(pluginDataHandlerList, eventPublisher, new SelectorMatchCache(), new RuleMatchCache());
        baseDataCache = BaseDataCache.getInstance();
        clearCaches();
        when(handler.pluginNamed()).thenReturn("divide");
        commonPluginDataSubscriber.putExtendPluginDataHandler(List.of(handler));
    }

    @Test
    public void testOnSubscribe() {
        baseDataCache.cleanPluginData();

        PluginData pluginData = PluginData.builder().name(mockName1).build();
        commonPluginDataSubscriber.onSubscribe(pluginData);
        assertNotNull(baseDataCache.obtainPluginData(pluginData.getName()));
        assertEquals(pluginData, baseDataCache.obtainPluginData(pluginData.getName()));
    }

    @Test
    public void testUnSubscribe() {
        baseDataCache.cleanPluginData();
        PluginData pluginData = PluginData.builder().name(mockName1).build();
        baseDataCache.cachePluginData(pluginData);
        assertNotNull(baseDataCache.obtainPluginData(pluginData.getName()));
        
        commonPluginDataSubscriber.unSubscribe(pluginData);
        assertNull(baseDataCache.obtainPluginData(pluginData.getName()));
    }

    @Test
    public void testRefreshPluginDataAll() {
        baseDataCache.cleanPluginData();
        PluginData firstCachedPluginData = PluginData.builder().name(mockName1).build();
        PluginData secondCachedPluginData = PluginData.builder().name(mockName2).build();
        baseDataCache.cachePluginData(firstCachedPluginData);
        baseDataCache.cachePluginData(secondCachedPluginData);
        assertNotNull(baseDataCache.obtainPluginData(firstCachedPluginData.getName()));
        assertNotNull(baseDataCache.obtainPluginData(secondCachedPluginData.getName()));

        commonPluginDataSubscriber.refreshPluginDataAll();
        assertNull(baseDataCache.obtainPluginData(firstCachedPluginData.getName()));
        assertNull(baseDataCache.obtainPluginData(secondCachedPluginData.getName()));
    }

    @Test
    public void testRefreshPluginDataSelf() {
        baseDataCache.cleanPluginData();
        PluginData firstCachedPluginData = PluginData.builder().name(mockName1).build();
        PluginData secondCachedPluginData = PluginData.builder().name(mockName2).build();
        baseDataCache.cachePluginData(firstCachedPluginData);
        baseDataCache.cachePluginData(secondCachedPluginData);
        assertNotNull(baseDataCache.obtainPluginData(firstCachedPluginData.getName()));
        assertNotNull(baseDataCache.obtainPluginData(secondCachedPluginData.getName()));

        commonPluginDataSubscriber.refreshPluginDataSelf(Lists.newArrayList(firstCachedPluginData));
        assertNull(baseDataCache.obtainPluginData(firstCachedPluginData.getName()));
        assertNotNull(baseDataCache.obtainPluginData(secondCachedPluginData.getName()));
    }

    @Test
    public void testOnSelectorSubscribe() {
        baseDataCache.cleanSelectorData();

        SelectorData selectorData = SelectorData.builder().id("1").enabled(true).pluginName(mockPluginName1).sort(1).build();
        commonPluginDataSubscriber.onSelectorSubscribe(selectorData);
        List<SelectorData> obtainSelectorData = baseDataCache.obtainSelectorData(selectorData.getPluginName());
        assertEquals(Lists.newArrayList(selectorData), obtainSelectorData);
    }

    @Test
    public void testUnSelectorSubscribe() {
        final String path = "/selector";
        final String emptyRulePath = "/empty-rule";
        final String unrelatedRulePath = "/unrelated-rule";
        final MatchDataCache matchDataCache = MatchDataCache.getInstance();
        baseDataCache.cleanSelectorData();
        matchDataCache.cleanSelectorData();
        matchDataCache.cleanRuleDataData();

        final SelectorData selectorData = SelectorData.builder().id(mockSelectorId1).enabled(true).pluginName(mockPluginName1).build();
        final RuleData ruleData = RuleData.builder().id("1").selectorId(mockSelectorId1).pluginName(mockPluginName1).build();
        final RuleData emptyRuleData = RuleData.builder().pluginName(mockPluginName1).build();
        final RuleData unrelatedRuleData = RuleData.builder().id("2").selectorId(mockSelectorId2).pluginName(mockPluginName1).build();
        baseDataCache.cacheSelectData(selectorData);
        matchDataCache.cacheSelectorData(path, selectorData, 100, 100);
        matchDataCache.cacheRuleData(path, ruleData, 100, 100);
        matchDataCache.cacheRuleData(emptyRulePath, emptyRuleData, 100, 100);
        matchDataCache.cacheRuleData(unrelatedRulePath, unrelatedRuleData, 100, 100);

        try {
            assertNotNull(baseDataCache.obtainSelectorData(selectorData.getPluginName()));
            assertEquals(selectorData, matchDataCache.obtainSelectorData(mockPluginName1, path));
            assertEquals(ruleData, matchDataCache.obtainRuleData(mockPluginName1, path));

            commonPluginDataSubscriber.unSelectorSubscribe(selectorData);

            assertNull(baseDataCache.obtainSelectorData(selectorData.getPluginName()));
            assertNull(matchDataCache.obtainSelectorData(mockPluginName1, path));
            assertNull(matchDataCache.obtainRuleData(mockPluginName1, path));
            assertNull(matchDataCache.obtainRuleData(mockPluginName1, emptyRulePath));
            assertEquals(unrelatedRuleData, matchDataCache.obtainRuleData(mockPluginName1, unrelatedRulePath));
        } finally {
            matchDataCache.cleanSelectorData();
            matchDataCache.cleanRuleDataData();
        }
    }

    @Test
    public void testRefreshSelectorDataAll() {
        baseDataCache.cleanSelectorData();
        SelectorData firstCachedSelectorData = SelectorData.builder().id("1").enabled(true).pluginName(mockPluginName1).build();
        SelectorData secondCachedSelectorData = SelectorData.builder().id("2").enabled(true).pluginName(mockPluginName2).build();
        baseDataCache.cacheSelectData(firstCachedSelectorData);
        baseDataCache.cacheSelectData(secondCachedSelectorData);
        assertNotNull(baseDataCache.obtainSelectorData(firstCachedSelectorData.getPluginName()));
        assertNotNull(baseDataCache.obtainSelectorData(secondCachedSelectorData.getPluginName()));

        commonPluginDataSubscriber.refreshSelectorDataAll();
        assertNull(baseDataCache.obtainSelectorData(firstCachedSelectorData.getPluginName()));
        assertNull(baseDataCache.obtainSelectorData(secondCachedSelectorData.getPluginName()));
    }

    @Test
    public void testRefreshSelectorDataSelf() {
        baseDataCache.cleanSelectorData();
        SelectorData firstCachedSelectorData = SelectorData.builder().id("1").pluginName(mockPluginName1).build();
        SelectorData secondCachedSelectorData = SelectorData.builder().id("2").pluginName(mockPluginName2).build();
        baseDataCache.cacheSelectData(firstCachedSelectorData);
        baseDataCache.cacheSelectData(secondCachedSelectorData);
        assertNotNull(baseDataCache.obtainSelectorData(firstCachedSelectorData.getPluginName()));
        assertNotNull(baseDataCache.obtainSelectorData(secondCachedSelectorData.getPluginName()));

        commonPluginDataSubscriber.refreshSelectorDataSelf(Lists.newArrayList(firstCachedSelectorData));
        assertNull(baseDataCache.obtainSelectorData(firstCachedSelectorData.getPluginName()));
        assertEquals(Lists.newArrayList(secondCachedSelectorData), baseDataCache.obtainSelectorData(secondCachedSelectorData.getPluginName()));
    }

    @Test
    public void testOnRuleSubscribe() {
        baseDataCache.cleanRuleData();

        RuleData ruleData = RuleData.builder().id("1").selectorId(mockSelectorId1).enabled(true).pluginName(mockPluginName1).sort(1).build();
        commonPluginDataSubscriber.onRuleSubscribe(ruleData);
        assertNotNull(baseDataCache.obtainRuleData(ruleData.getSelectorId()));
        assertEquals(Lists.newArrayList(ruleData), baseDataCache.obtainRuleData(ruleData.getSelectorId()));
    }

    @Test
    public void testUnRuleSubscribe() {
        baseDataCache.cleanRuleData();
        RuleData ruleData = RuleData.builder().id("1").selectorId(mockSelectorId1).pluginName(mockPluginName1).sort(1).build();
        baseDataCache.cacheRuleData(ruleData);
        assertNotNull(baseDataCache.obtainRuleData(ruleData.getSelectorId()));

        commonPluginDataSubscriber.unRuleSubscribe(ruleData);
        assertNull(baseDataCache.obtainRuleData(ruleData.getSelectorId()));
    }

    @Test
    public void testRefreshRuleDataAll() {
        baseDataCache.cleanRuleData();
        RuleData firstCachedRuleData = RuleData.builder().id("1").selectorId(mockSelectorId1).pluginName(mockPluginName1).build();
        RuleData secondCachedRuleData = RuleData.builder().id("2").selectorId(mockSelectorId2).pluginName(mockPluginName2).build();
        baseDataCache.cacheRuleData(firstCachedRuleData);
        baseDataCache.cacheRuleData(secondCachedRuleData);
        assertNotNull(baseDataCache.obtainRuleData(firstCachedRuleData.getSelectorId()));
        assertNotNull(baseDataCache.obtainRuleData(firstCachedRuleData.getSelectorId()));

        commonPluginDataSubscriber.refreshRuleDataAll();
        assertNull(baseDataCache.obtainRuleData(firstCachedRuleData.getSelectorId()));
        assertNull(baseDataCache.obtainRuleData(firstCachedRuleData.getSelectorId()));
    }

    @Test
    public void testRefreshRuleDataSelf() {
        baseDataCache.cleanRuleData();
        RuleData firstCachedRuleData = RuleData.builder().id("1").selectorId(mockSelectorId1).pluginName(mockPluginName1).build();
        RuleData secondCachedRuleData = RuleData.builder().id("2").selectorId(mockSelectorId2).pluginName(mockPluginName2).build();
        baseDataCache.cacheRuleData(firstCachedRuleData);
        baseDataCache.cacheRuleData(secondCachedRuleData);
        assertNotNull(baseDataCache.obtainRuleData(firstCachedRuleData.getSelectorId()));
        assertNotNull(baseDataCache.obtainRuleData(firstCachedRuleData.getSelectorId()));

        commonPluginDataSubscriber.refreshRuleDataSelf(Lists.newArrayList(firstCachedRuleData));
        assertNull(baseDataCache.obtainRuleData(firstCachedRuleData.getSelectorId()));
        assertEquals(Lists.newArrayList(secondCachedRuleData), baseDataCache.obtainRuleData(secondCachedRuleData.getSelectorId()));
    }

    private void mockShenyuTrieConfig() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setApplicationContext(context);
    }

    @AfterEach
    public void clearCaches() {
        baseDataCache.cleanPluginData();
        baseDataCache.cleanSelectorData();
        baseDataCache.cleanRuleData();
        MatchDataCache.getInstance().cleanSelectorData();
        MatchDataCache.getInstance().cleanRuleDataData();
    }

    @Test
    public void pluginRefreshPublishesTheCompleteBatch() {
        final PluginData a = plugin("jwt", 1);
        final PluginData old = plugin("divide", 2);
        final PluginData updated = plugin("divide", 3);
        final PluginData added = plugin("rewrite", 4);
        baseDataCache.cachePluginData(a);
        baseDataCache.cachePluginData(old);
        final var previous = baseDataCache.getPluginMap();
        doAnswer(invocation -> {
            assertSame(previous, baseDataCache.getPluginMap());
            assertSame(old, baseDataCache.obtainPluginData("divide"));
            assertNull(baseDataCache.obtainPluginData("rewrite"));
            verifyNoInteractions(eventPublisher);
            return null;
        }).when(handler).handlerPlugin(updated);
        doAnswer(invocation -> {
            assertSame(updated, baseDataCache.obtainPluginData("divide"));
            assertSame(added, baseDataCache.obtainPluginData("rewrite"));
            return null;
        }).when(eventPublisher).publishEvent(any(PluginHandlerEvent.class));
        commonPluginDataSubscriber.onPluginRefresh(List.of(updated, added));
        assertNotSame(previous, baseDataCache.getPluginMap());
        assertEquals(3, baseDataCache.getPluginMap().size());
        assertSame(a, baseDataCache.obtainPluginData("jwt"));
        assertSame(old, previous.get("divide"));
        verify(eventPublisher).publishEvent(org.mockito.ArgumentMatchers.argThat((PluginHandlerEvent event) ->
                event.getSource() == updated && event.getPluginStateEnums() == PluginHandlerEventEnum.ENABLED));
        verify(eventPublisher).publishEvent(org.mockito.ArgumentMatchers.argThat((PluginHandlerEvent event) ->
                event.getSource() == updated && event.getPluginStateEnums() == PluginHandlerEventEnum.SORTED));
    }

    @Test
    public void selectorHandlerSeesThePublishedBatch() {
        final SelectorData first = selector("a", 1);
        final SelectorData second = selector("b", 2);
        doAnswer(invocation -> {
            assertEquals(List.of(first, second), baseDataCache.obtainSelectorData("divide"));
            return null;
        }).when(handler).handlerSelector(first);
        commonPluginDataSubscriber.onSelectorRefresh(List.of(first, second));
        verify(handler).handlerSelector(second);
    }

    @Test
    public void ruleHandlerSeesThePublishedBatch() {
        final RuleData first = rule("a", 1);
        final RuleData second = rule("b", 2);
        doAnswer(invocation -> {
            assertEquals(List.of(first, second), baseDataCache.obtainRuleData("selector"));
            return null;
        }).when(handler).handlerRule(first);
        commonPluginDataSubscriber.onRuleRefresh(List.of(first, second));
        verify(handler).handlerRule(second);
    }

    @Test
    public void emptyBatchesRetainAllMaps() {
        baseDataCache.cachePluginData(plugin("divide", 1));
        baseDataCache.cacheSelectData(selector("a", 1));
        baseDataCache.cacheRuleData(rule("a", 1));
        final var plugins = baseDataCache.getPluginMap();
        final var selectors = baseDataCache.getSelectorMap();
        final var rules = baseDataCache.getRuleMap();
        commonPluginDataSubscriber.onPluginRefresh(List.of());
        commonPluginDataSubscriber.onSelectorRefresh(List.of());
        commonPluginDataSubscriber.onRuleRefresh(List.of());
        assertSame(plugins, baseDataCache.getPluginMap());
        assertSame(selectors, baseDataCache.getSelectorMap());
        assertSame(rules, baseDataCache.getRuleMap());
        verifyNoInteractions(eventPublisher);
    }

    @Test
    public void selectorHandlerFailureDoesNotRollBackPublishedBatch() {
        final SelectorData old = selector("b", 1);
        final SelectorData updated = selector("b", 2);
        final SelectorData added = selector("c", 3);
        baseDataCache.cacheSelectData(old);
        doThrow(new IllegalStateException("handler failed")).when(handler).handlerSelector(updated);
        assertThrows(IllegalStateException.class, () -> commonPluginDataSubscriber.onSelectorRefresh(List.of(updated, added)));
        assertEquals(List.of(updated, added), baseDataCache.obtainSelectorData("divide"));
    }

    @Test
    public void ruleHandlerFailureDoesNotRollBackPublishedBatch() {
        final RuleData old = rule("b", 1);
        final RuleData updated = rule("b", 2);
        final RuleData added = rule("c", 3);
        baseDataCache.cacheRuleData(old);
        doThrow(new IllegalStateException("handler failed")).when(handler).handlerRule(updated);
        assertThrows(IllegalStateException.class, () -> commonPluginDataSubscriber.onRuleRefresh(List.of(updated, added)));
        assertEquals(List.of(updated, added), baseDataCache.obtainRuleData("selector"));
    }

    @Test
    public void refreshInvalidatesMatchingAndNegativeCacheEntries() {
        final SelectorMatchCache selectorConfig = new SelectorMatchCache();
        final RuleMatchCache ruleConfig = new RuleMatchCache();
        selectorConfig.getCache().setEnabled(true);
        ruleConfig.getCache().setEnabled(true);
        commonPluginDataSubscriber = new CommonPluginDataSubscriber(List.of(handler), eventPublisher, selectorConfig, ruleConfig);
        final MatchDataCache matches = MatchDataCache.getInstance();
        matches.cacheSelectorData("/selector", selector("selector", 1), 100, 100);
        matches.cacheSelectorData("/empty", SelectorData.builder().pluginName("divide").build(), 100, 100);
        matches.cacheRuleData("/selector", rule("a", 1), 100, 100);
        matches.cacheRuleData("/empty", RuleData.builder().pluginName("divide").build(), 100, 100);
        commonPluginDataSubscriber.onSelectorRefresh(List.of(selector("selector", 2)));
        assertNull(matches.obtainSelectorData("divide", "/selector"));
        assertNull(matches.obtainSelectorData("divide", "/empty"));
        assertNull(matches.obtainRuleData("divide", "/selector"));
        assertNull(matches.obtainRuleData("divide", "/empty"));

        matches.cacheRuleData("/rule", rule("a", 1), 100, 100);
        matches.cacheRuleData("/empty", RuleData.builder().pluginName("divide").build(), 100, 100);
        commonPluginDataSubscriber.onRuleRefresh(List.of(rule("a", 2)));
        assertNull(matches.obtainRuleData("divide", "/rule"));
        assertNull(matches.obtainRuleData("divide", "/empty"));

        matches.cacheSelectorData("/selector", selector("a", 1), 100, 100);
        matches.cacheRuleData("/rule", rule("a", 1), 100, 100);
        commonPluginDataSubscriber.onPluginRefresh(List.of(plugin("divide", 1)));
        assertNull(matches.obtainSelectorData("divide", "/selector"));
        assertNull(matches.obtainRuleData("divide", "/rule"));
    }

    @Test
    public void disabledPluginEventIsPublishedAfterTheBatch() {
        final PluginData disabled = PluginData.builder().name("divide").enabled(false).sort(1).build();
        doAnswer(invocation -> {
            assertSame(disabled, baseDataCache.obtainPluginData("divide"));
            return null;
        }).when(eventPublisher).publishEvent(any(PluginHandlerEvent.class));
        commonPluginDataSubscriber.onPluginRefresh(List.of(disabled));
        verify(eventPublisher).publishEvent(org.mockito.ArgumentMatchers.argThat((PluginHandlerEvent event) ->
                event.getSource() == disabled && event.getPluginStateEnums() == PluginHandlerEventEnum.DISABLED));
    }

    private PluginData plugin(final String name, final int sort) {
        return PluginData.builder().name(name).sort(sort).enabled(true).build();
    }

    private SelectorData selector(final String id, final int sort) {
        return SelectorData.builder().id(id).pluginName("divide").sort(sort).build();
    }

    private RuleData rule(final String id, final int sort) {
        return RuleData.builder().id(id).pluginName("divide").selectorId("selector").sort(sort).build();
    }
}
