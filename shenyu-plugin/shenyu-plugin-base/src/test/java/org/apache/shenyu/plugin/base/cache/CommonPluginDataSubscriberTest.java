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

package org.apache.shenyu.plugin.base.cache;

import com.google.common.collect.Lists;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

/**
 * Test cases for CommonPluginDataSubscriber.
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

        PluginData pluginData = new PluginData();
        pluginData.setName(mockName1);
        commonPluginDataSubscriber.onSubscribe(pluginData);
        Assert.assertNotNull(baseDataCache.obtainPluginData(pluginData.getName()));
        Assert.assertEquals(pluginData, baseDataCache.obtainPluginData(pluginData.getName()));
    }

    @Test
    public void testUnSubscribe() {
        baseDataCache.cleanPluginData();
        PluginData pluginData = new PluginData();
        pluginData.setName(mockName1);
        baseDataCache.cachePluginData(pluginData);
        Assert.assertNotNull(baseDataCache.obtainPluginData(pluginData.getName()));

        commonPluginDataSubscriber.unSubscribe(pluginData);
        Assert.assertNull(baseDataCache.obtainPluginData(pluginData.getName()));
    }

    @Test
    public void testRefreshPluginDataAll() {
        baseDataCache.cleanPluginData();
        final PluginData firstCachedPluginData = new PluginData();
        firstCachedPluginData.setName(mockName1);
        final PluginData secondCachedPluginData = new PluginData();
        secondCachedPluginData.setName(mockName2);
        baseDataCache.cachePluginData(firstCachedPluginData);
        baseDataCache.cachePluginData(secondCachedPluginData);
        Assert.assertNotNull(baseDataCache.obtainPluginData(firstCachedPluginData.getName()));
        Assert.assertNotNull(baseDataCache.obtainPluginData(secondCachedPluginData.getName()));

        commonPluginDataSubscriber.refreshPluginDataAll();
        Assert.assertNull(baseDataCache.obtainPluginData(firstCachedPluginData.getName()));
        Assert.assertNull(baseDataCache.obtainPluginData(secondCachedPluginData.getName()));
    }

    @Test
    public void testRefreshPluginDataSelf() {
        baseDataCache.cleanPluginData();
        final PluginData firstCachedPluginData = new PluginData();
        firstCachedPluginData.setName(mockName1);
        final PluginData secondCachedPluginData = new PluginData();
        secondCachedPluginData.setName(mockName2);
        baseDataCache.cachePluginData(firstCachedPluginData);
        baseDataCache.cachePluginData(secondCachedPluginData);
        Assert.assertNotNull(baseDataCache.obtainPluginData(firstCachedPluginData.getName()));
        Assert.assertNotNull(baseDataCache.obtainPluginData(secondCachedPluginData.getName()));

        commonPluginDataSubscriber.refreshPluginDataSelf(Lists.newArrayList(firstCachedPluginData));
        Assert.assertNull(baseDataCache.obtainPluginData(firstCachedPluginData.getName()));
        Assert.assertNotNull(baseDataCache.obtainPluginData(secondCachedPluginData.getName()));
    }

    @Test
    public void testOnSelectorSubscribe() {
        baseDataCache.cleanSelectorData();
        final SelectorData selectorData = new SelectorData();
        selectorData.setId("1");
        selectorData.setPluginName(mockPluginName1);
        selectorData.setSort(1);
        commonPluginDataSubscriber.onSelectorSubscribe(selectorData);
        List<SelectorData> obtainSelectorData = baseDataCache.obtainSelectorData(selectorData.getPluginName());
        Assert.assertEquals(Lists.newArrayList(selectorData), obtainSelectorData);
    }

    @Test
    public void testUnSelectorSubscribe() {
        baseDataCache.cleanSelectorData();
        final SelectorData selectorData = new SelectorData();
        selectorData.setId("1");
        selectorData.setPluginName(mockPluginName1);
        baseDataCache.cacheSelectData(selectorData);
        Assert.assertNotNull(baseDataCache.obtainSelectorData(selectorData.getPluginName()));

        commonPluginDataSubscriber.unSelectorSubscribe(selectorData);
        Assert.assertEquals(Lists.newArrayList(), baseDataCache.obtainSelectorData(selectorData.getPluginName()));
    }

    @Test
    public void testRefreshSelectorDataAll() {
        baseDataCache.cleanSelectorData();
        final SelectorData firstCachedSelectorData = new SelectorData();
        firstCachedSelectorData.setId("1");
        firstCachedSelectorData.setPluginName(mockPluginName1);
        final SelectorData secondCachedSelectorData = new SelectorData();
        secondCachedSelectorData.setId("2");
        secondCachedSelectorData.setPluginName(mockPluginName2);
        baseDataCache.cacheSelectData(firstCachedSelectorData);
        baseDataCache.cacheSelectData(secondCachedSelectorData);
        Assert.assertNotNull(baseDataCache.obtainSelectorData(firstCachedSelectorData.getPluginName()));
        Assert.assertNotNull(baseDataCache.obtainSelectorData(secondCachedSelectorData.getPluginName()));

        commonPluginDataSubscriber.refreshSelectorDataAll();
        Assert.assertNull(baseDataCache.obtainSelectorData(firstCachedSelectorData.getPluginName()));
        Assert.assertNull(baseDataCache.obtainSelectorData(secondCachedSelectorData.getPluginName()));
    }

    @Test
    public void testRefreshSelectorDataSelf() {
        baseDataCache.cleanSelectorData();
        final SelectorData firstCachedSelectorData = new SelectorData();
        firstCachedSelectorData.setId("1");
        firstCachedSelectorData.setPluginName(mockPluginName1);
        final SelectorData secondCachedSelectorData = new SelectorData();
        secondCachedSelectorData.setId("2");
        secondCachedSelectorData.setPluginName(mockPluginName2);
        baseDataCache.cacheSelectData(firstCachedSelectorData);
        baseDataCache.cacheSelectData(secondCachedSelectorData);
        Assert.assertNotNull(baseDataCache.obtainSelectorData(firstCachedSelectorData.getPluginName()));
        Assert.assertNotNull(baseDataCache.obtainSelectorData(secondCachedSelectorData.getPluginName()));

        commonPluginDataSubscriber.refreshSelectorDataSelf(Lists.newArrayList(firstCachedSelectorData));
        Assert.assertEquals(Lists.newArrayList(), baseDataCache.obtainSelectorData(firstCachedSelectorData.getPluginName()));
        Assert.assertEquals(Lists.newArrayList(secondCachedSelectorData), baseDataCache.obtainSelectorData(secondCachedSelectorData.getPluginName()));
    }

    @Test
    public void testOnRuleSubscribe() {
        baseDataCache.cleanRuleData();
        final RuleData ruleData = new RuleData();
        ruleData.setId("1");
        ruleData.setSelectorId(mockSelectorId1);
        ruleData.setPluginName(mockPluginName1);
        ruleData.setSort(1);
        commonPluginDataSubscriber.onRuleSubscribe(ruleData);
        Assert.assertNotNull(baseDataCache.obtainRuleData(ruleData.getSelectorId()));
        Assert.assertEquals(Lists.newArrayList(ruleData), baseDataCache.obtainRuleData(ruleData.getSelectorId()));
    }

    @Test
    public void testUnRuleSubscribe() {
        baseDataCache.cleanRuleData();
        final RuleData ruleData = new RuleData();
        ruleData.setId("1");
        ruleData.setSelectorId(mockSelectorId1);
        ruleData.setPluginName(mockPluginName1);
        ruleData.setSort(1);
        baseDataCache.cacheRuleData(ruleData);
        Assert.assertNotNull(baseDataCache.obtainRuleData(ruleData.getSelectorId()));

        commonPluginDataSubscriber.unRuleSubscribe(ruleData);
        Assert.assertEquals(Lists.newArrayList(), baseDataCache.obtainRuleData(ruleData.getSelectorId()));
    }

    @Test
    public void testRefreshRuleDataAll() {
        baseDataCache.cleanRuleData();
        final RuleData firstCachedRuleData = new RuleData();
        firstCachedRuleData.setId("1");
        firstCachedRuleData.setSelectorId(mockSelectorId1);
        firstCachedRuleData.setPluginName(mockPluginName1);
        final RuleData secondCachedRuleData = new RuleData();
        secondCachedRuleData.setId("2");
        secondCachedRuleData.setSelectorId(mockSelectorId2);
        secondCachedRuleData.setPluginName(mockPluginName2);
        baseDataCache.cacheRuleData(firstCachedRuleData);
        baseDataCache.cacheRuleData(secondCachedRuleData);
        Assert.assertNotNull(baseDataCache.obtainRuleData(firstCachedRuleData.getSelectorId()));
        Assert.assertNotNull(baseDataCache.obtainRuleData(firstCachedRuleData.getSelectorId()));

        commonPluginDataSubscriber.refreshRuleDataAll();
        Assert.assertNull(baseDataCache.obtainRuleData(firstCachedRuleData.getSelectorId()));
        Assert.assertNull(baseDataCache.obtainRuleData(firstCachedRuleData.getSelectorId()));
    }

    @Test
    public void testRefreshRuleDataSelf() {
        baseDataCache.cleanRuleData();
        final RuleData firstCachedRuleData = new RuleData();
        firstCachedRuleData.setId("1");
        firstCachedRuleData.setSelectorId(mockSelectorId1);
        firstCachedRuleData.setPluginName(mockPluginName1);
        final RuleData secondCachedRuleData = new RuleData();
        secondCachedRuleData.setId("2");
        secondCachedRuleData.setSelectorId(mockSelectorId2);
        secondCachedRuleData.setPluginName(mockPluginName2);
        baseDataCache.cacheRuleData(firstCachedRuleData);
        baseDataCache.cacheRuleData(secondCachedRuleData);
        Assert.assertNotNull(baseDataCache.obtainRuleData(firstCachedRuleData.getSelectorId()));
        Assert.assertNotNull(baseDataCache.obtainRuleData(firstCachedRuleData.getSelectorId()));

        commonPluginDataSubscriber.refreshRuleDataSelf(Lists.newArrayList(firstCachedRuleData));
        Assert.assertEquals(Lists.newArrayList(), baseDataCache.obtainRuleData(firstCachedRuleData.getSelectorId()));
        Assert.assertEquals(Lists.newArrayList(secondCachedRuleData), baseDataCache.obtainRuleData(secondCachedRuleData.getSelectorId()));
    }
}
