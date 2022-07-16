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
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.context.ApplicationEventPublisher;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

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

    @BeforeEach
    public void setup() {
        ArrayList<PluginDataHandler> pluginDataHandlerList = Lists.newArrayList();
        commonPluginDataSubscriber = new CommonPluginDataSubscriber(pluginDataHandlerList, eventPublisher);
        baseDataCache = BaseDataCache.getInstance();
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

        SelectorData selectorData = SelectorData.builder().id("1").pluginName(mockPluginName1).sort(1).build();
        commonPluginDataSubscriber.onSelectorSubscribe(selectorData);
        List<SelectorData> obtainSelectorData = baseDataCache.obtainSelectorData(selectorData.getPluginName());
        assertEquals(Lists.newArrayList(selectorData), obtainSelectorData);
    }

    @Test
    public void testUnSelectorSubscribe() {
        baseDataCache.cleanSelectorData();
        SelectorData selectorData = SelectorData.builder().id("1").pluginName(mockPluginName1).build();
        baseDataCache.cacheSelectData(selectorData);
        assertNotNull(baseDataCache.obtainSelectorData(selectorData.getPluginName()));

        commonPluginDataSubscriber.unSelectorSubscribe(selectorData);
        assertEquals(Lists.newArrayList(), baseDataCache.obtainSelectorData(selectorData.getPluginName()));
    }

    @Test
    public void testRefreshSelectorDataAll() {
        baseDataCache.cleanSelectorData();
        SelectorData firstCachedSelectorData = SelectorData.builder().id("1").pluginName(mockPluginName1).build();
        SelectorData secondCachedSelectorData = SelectorData.builder().id("2").pluginName(mockPluginName2).build();
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
        assertEquals(Lists.newArrayList(), baseDataCache.obtainSelectorData(firstCachedSelectorData.getPluginName()));
        assertEquals(Lists.newArrayList(secondCachedSelectorData), baseDataCache.obtainSelectorData(secondCachedSelectorData.getPluginName()));
    }

    @Test
    public void testOnRuleSubscribe() {
        baseDataCache.cleanRuleData();

        RuleData ruleData = RuleData.builder().id("1").selectorId(mockSelectorId1).pluginName(mockPluginName1).sort(1).build();
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
        assertEquals(Lists.newArrayList(), baseDataCache.obtainRuleData(ruleData.getSelectorId()));
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
        assertEquals(Lists.newArrayList(), baseDataCache.obtainRuleData(firstCachedRuleData.getSelectorId()));
        assertEquals(Lists.newArrayList(secondCachedRuleData), baseDataCache.obtainRuleData(secondCachedRuleData.getSelectorId()));
    }
}
