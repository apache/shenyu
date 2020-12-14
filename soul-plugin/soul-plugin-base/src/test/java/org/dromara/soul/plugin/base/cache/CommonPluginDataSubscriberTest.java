/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License,  Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,  software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,  either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.dromara.soul.plugin.base.cache;

import com.google.common.collect.Lists;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.plugin.base.handler.PluginDataHandler;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

/**
 * Test cases for CommonPluginDataSubscriber.
 *
 * @author BetterWp
 */
public final class CommonPluginDataSubscriberTest {

    private final String mockName1 = "MOCK_NAME_1";

    private final String mockName2 = "MOCK_NAME_2";

    private final String mockPluginName1 = "MOCK_PLUGIN_NAME_1";

    private final String mockPluginName2 = "MOCK_PLUGIN_NAME_2";

    private final String mockSelectorId1 = "MOCK_SELECTOR_ID_1";

    private final String mockSelectorId2 = "MOCK_SELECTOR_ID_2";

    private CommonPluginDataSubscriber commonPluginDataSubscriber;
    
    private BaseDataCache baseDataCache;

    @Before
    public void setup() {
        ArrayList<PluginDataHandler> pluginDataHandlerList = Lists.newArrayList();
        commonPluginDataSubscriber = new CommonPluginDataSubscriber(pluginDataHandlerList);
        baseDataCache = BaseDataCache.getInstance();
    }

    @Test
    public void testOnSubscribe() {
        baseDataCache.cleanPluginData();

        PluginData pluginData = PluginData.builder().name(mockName1).build();
        commonPluginDataSubscriber.onSubscribe(pluginData);
        Assert.assertNotNull(baseDataCache.obtainPluginData(pluginData.getName()));
        Assert.assertEquals(pluginData, baseDataCache.obtainPluginData(pluginData.getName()));
    }

    @Test
    public void testUnSubscribe() {
        baseDataCache.cleanPluginData();
        PluginData pluginData = PluginData.builder().name(mockName1).build();
        baseDataCache.cachePluginData(pluginData);
        Assert.assertNotNull(baseDataCache.obtainPluginData(pluginData.getName()));
        
        commonPluginDataSubscriber.unSubscribe(pluginData);
        Assert.assertNull(baseDataCache.obtainPluginData(pluginData.getName()));
    }

    @Test
    public void testRefreshPluginDataAll() {
        baseDataCache.cleanPluginData();
        PluginData pluginData1 = PluginData.builder().name(mockName1).build();
        PluginData pluginData2 = PluginData.builder().name(mockName2).build();
        baseDataCache.cachePluginData(pluginData1);
        baseDataCache.cachePluginData(pluginData2);
        Assert.assertNotNull(baseDataCache.obtainPluginData(pluginData1.getName()));
        Assert.assertNotNull(baseDataCache.obtainPluginData(pluginData2.getName()));

        commonPluginDataSubscriber.refreshPluginDataAll();
        Assert.assertNull(baseDataCache.obtainPluginData(pluginData1.getName()));
        Assert.assertNull(baseDataCache.obtainPluginData(pluginData2.getName()));
    }

    @Test
    public void testRefreshPluginDataSelf() {
        baseDataCache.cleanPluginData();
        PluginData pluginData1 = PluginData.builder().name(mockName1).build();
        PluginData pluginData2 = PluginData.builder().name(mockName2).build();
        baseDataCache.cachePluginData(pluginData1);
        baseDataCache.cachePluginData(pluginData2);
        Assert.assertNotNull(baseDataCache.obtainPluginData(pluginData1.getName()));
        Assert.assertNotNull(baseDataCache.obtainPluginData(pluginData2.getName()));

        commonPluginDataSubscriber.refreshPluginDataSelf(Lists.newArrayList(pluginData1));
        Assert.assertNull(baseDataCache.obtainPluginData(pluginData1.getName()));
        Assert.assertNotNull(baseDataCache.obtainPluginData(pluginData2.getName()));
    }

    @Test
    public void testOnSelectorSubscribe() {
        baseDataCache.cleanSelectorData();

        SelectorData selectorData = SelectorData.builder().id("1").pluginName(mockPluginName1).sort(1).build();
        commonPluginDataSubscriber.onSelectorSubscribe(selectorData);
        List<SelectorData> obtainSelectorData = baseDataCache.obtainSelectorData(selectorData.getPluginName());
        Assert.assertEquals(Lists.newArrayList(selectorData), obtainSelectorData);
    }

    @Test
    public void testUnSelectorSubscribe() {
        baseDataCache.cleanSelectorData();
        SelectorData selectorData = SelectorData.builder().id("1").pluginName(mockPluginName1).build();
        baseDataCache.cacheSelectData(selectorData);
        Assert.assertNotNull(baseDataCache.obtainSelectorData(selectorData.getPluginName()));

        commonPluginDataSubscriber.unSelectorSubscribe(selectorData);
        Assert.assertEquals(Lists.newArrayList(), baseDataCache.obtainSelectorData(selectorData.getPluginName()));
    }

    @Test
    public void testRefreshSelectorDataAll() {
        baseDataCache.cleanSelectorData();
        SelectorData selectorData1 = SelectorData.builder().id("1").pluginName(mockPluginName1).build();
        SelectorData selectorData2 = SelectorData.builder().id("2").pluginName(mockPluginName2).build();
        baseDataCache.cacheSelectData(selectorData1);
        baseDataCache.cacheSelectData(selectorData2);
        Assert.assertNotNull(baseDataCache.obtainSelectorData(selectorData1.getPluginName()));
        Assert.assertNotNull(baseDataCache.obtainSelectorData(selectorData2.getPluginName()));

        commonPluginDataSubscriber.refreshSelectorDataAll();
        Assert.assertNull(baseDataCache.obtainSelectorData(selectorData1.getPluginName()));
        Assert.assertNull(baseDataCache.obtainSelectorData(selectorData2.getPluginName()));
    }

    @Test
    public void testRefreshSelectorDataSelf() {
        baseDataCache.cleanSelectorData();
        SelectorData selectorData1 = SelectorData.builder().id("1").pluginName(mockPluginName1).build();
        SelectorData selectorData2 = SelectorData.builder().id("2").pluginName(mockPluginName2).build();
        baseDataCache.cacheSelectData(selectorData1);
        baseDataCache.cacheSelectData(selectorData2);
        Assert.assertNotNull(baseDataCache.obtainSelectorData(selectorData1.getPluginName()));
        Assert.assertNotNull(baseDataCache.obtainSelectorData(selectorData2.getPluginName()));

        commonPluginDataSubscriber.refreshSelectorDataSelf(Lists.newArrayList(selectorData1));
        Assert.assertEquals(Lists.newArrayList(), baseDataCache.obtainSelectorData(selectorData1.getPluginName()));
        Assert.assertEquals(Lists.newArrayList(selectorData2), baseDataCache.obtainSelectorData(selectorData2.getPluginName()));
    }

    @Test
    public void testOnRuleSubscribe() {
        baseDataCache.cleanRuleData();

        RuleData ruleData = RuleData.builder().id("1").selectorId(mockSelectorId1).pluginName(mockPluginName1).sort(1).build();
        commonPluginDataSubscriber.onRuleSubscribe(ruleData);
        Assert.assertNotNull(baseDataCache.obtainRuleData(ruleData.getSelectorId()));
        Assert.assertEquals(Lists.newArrayList(ruleData), baseDataCache.obtainRuleData(ruleData.getSelectorId()));
    }

    @Test
    public void testUnRuleSubscribe() {
        baseDataCache.cleanRuleData();
        RuleData ruleData = RuleData.builder().id("1").selectorId(mockSelectorId1).pluginName(mockPluginName1).sort(1).build();
        baseDataCache.cacheRuleData(ruleData);
        Assert.assertNotNull(baseDataCache.obtainRuleData(ruleData.getSelectorId()));

        commonPluginDataSubscriber.unRuleSubscribe(ruleData);
        Assert.assertEquals(Lists.newArrayList(), baseDataCache.obtainRuleData(ruleData.getSelectorId()));
    }

    @Test
    public void testRefreshRuleDataAll() {
        baseDataCache.cleanRuleData();
        RuleData ruleData1 = RuleData.builder().id("1").selectorId(mockSelectorId1).pluginName(mockPluginName1).build();
        RuleData ruleData2 = RuleData.builder().id("2").selectorId(mockSelectorId2).pluginName(mockPluginName2).build();
        baseDataCache.cacheRuleData(ruleData1);
        baseDataCache.cacheRuleData(ruleData2);
        Assert.assertNotNull(baseDataCache.obtainRuleData(ruleData1.getSelectorId()));
        Assert.assertNotNull(baseDataCache.obtainRuleData(ruleData1.getSelectorId()));

        commonPluginDataSubscriber.refreshRuleDataAll();
        Assert.assertNull(baseDataCache.obtainRuleData(ruleData1.getSelectorId()));
        Assert.assertNull(baseDataCache.obtainRuleData(ruleData1.getSelectorId()));
    }

    @Test
    public void testRefreshRuleDataSelf() {
        baseDataCache.cleanRuleData();
        RuleData ruleData1 = RuleData.builder().id("1").selectorId(mockSelectorId1).pluginName(mockPluginName1).build();
        RuleData ruleData2 = RuleData.builder().id("2").selectorId(mockSelectorId2).pluginName(mockPluginName2).build();
        baseDataCache.cacheRuleData(ruleData1);
        baseDataCache.cacheRuleData(ruleData2);
        Assert.assertNotNull(baseDataCache.obtainRuleData(ruleData1.getSelectorId()));
        Assert.assertNotNull(baseDataCache.obtainRuleData(ruleData1.getSelectorId()));

        commonPluginDataSubscriber.refreshRuleDataSelf(Lists.newArrayList(ruleData1));
        Assert.assertEquals(Lists.newArrayList(), baseDataCache.obtainRuleData(ruleData1.getSelectorId()));
        Assert.assertEquals(Lists.newArrayList(ruleData2), baseDataCache.obtainRuleData(ruleData2.getSelectorId()));
    }
}
