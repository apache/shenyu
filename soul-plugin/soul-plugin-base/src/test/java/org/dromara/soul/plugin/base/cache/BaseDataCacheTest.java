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
import lombok.SneakyThrows;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.junit.Assert;
import org.junit.Test;

import java.lang.reflect.Field;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Test cases for BaseDataCache.
 *
 * @author BetterWp
 */
@SuppressWarnings("unchecked")
public final class BaseDataCacheTest {

    private final String pluginMapStr = "PLUGIN_MAP";

    private final String selectorMapStr = "SELECTOR_MAP";

    private final String ruleMapStr = "RULE_MAP";

    private final String mockName1 = "MOCK_NAME_1";
    
    private final String mockName2 = "MOCK_NAME_2";
    
    private final String mockPluginName1 = "MOCK_PLUGIN_NAME_1";
    
    private final String mockPluginName2 = "MOCK_PLUGIN_NAME_2";
    
    private final String mockSelectorId1 = "MOCK_SELECTOR_ID_1";
    
    private final String mockSelectorId2 = "MOCK_SELECTOR_ID_2";

    @Test
    public void testGetInstance() {
        BaseDataCache baseDataCache = BaseDataCache.getInstance();
        Assert.assertNotNull(baseDataCache);
    }

    @Test
    public void testCachePluginData() {
        PluginData pluginData = PluginData.builder().name(mockName1).build();
        ConcurrentHashMap<String, PluginData> pluginMap = getFieldByName(pluginMapStr);
        Assert.assertNull(pluginMap.get(mockName1));

        BaseDataCache.getInstance().cachePluginData(pluginData);
        Assert.assertNotNull(pluginMap.get(mockName1));
        Assert.assertEquals(pluginData, pluginMap.get(mockName1));
    }

    @Test
    public void testRemovePluginData() {
        PluginData pluginData = PluginData.builder().name(mockName1).build();
        ConcurrentHashMap<String, PluginData> pluginMap = getFieldByName(pluginMapStr);
        pluginMap.put(mockName1, pluginData);
        Assert.assertNotNull(pluginMap.get(mockName1));

        BaseDataCache.getInstance().removePluginData(pluginData);
        Assert.assertNull(pluginMap.get(mockName1));
    }

    @Test
    public void testCleanPluginData() {
        PluginData pluginData1 = PluginData.builder().name(mockName1).build();
        PluginData pluginData2 = PluginData.builder().name(mockName2).build();
        ConcurrentHashMap<String, PluginData> pluginMap = getFieldByName(pluginMapStr);
        pluginMap.put(mockName1, pluginData1);
        pluginMap.put(mockName2, pluginData2);
        Assert.assertNotNull(pluginMap.get(mockName1));
        Assert.assertNotNull(pluginMap.get(mockName2));

        BaseDataCache.getInstance().cleanPluginData();
        Assert.assertNull(pluginMap.get(mockName1));
        Assert.assertNull(pluginMap.get(mockName2));
    }

    @Test
    public void testCleanPluginDataSelf() {
        PluginData pluginData1 = PluginData.builder().name(mockName1).build();
        PluginData pluginData2 = PluginData.builder().name(mockName2).build();
        ConcurrentHashMap<String, PluginData> pluginMap = getFieldByName(pluginMapStr);
        pluginMap.put(mockName1, pluginData1);
        pluginMap.put(mockName2, pluginData2);
        Assert.assertNotNull(pluginMap.get(mockName1));
        Assert.assertNotNull(pluginMap.get(mockName2));

        List<PluginData> pluginDataList = Lists.newArrayList(pluginData1);
        BaseDataCache.getInstance().cleanPluginDataSelf(pluginDataList);
        Assert.assertNull(pluginMap.get(mockName1));
        Assert.assertNotNull(pluginMap.get(mockName2));
    }

    @Test
    public void testObtainPluginData() {
        PluginData pluginData = PluginData.builder().name(mockName1).build();
        ConcurrentHashMap<String, PluginData> pluginMap = getFieldByName(pluginMapStr);
        pluginMap.put(mockName1, pluginData);
        Assert.assertNotNull(pluginMap.get(mockName1));
        Assert.assertEquals(pluginData, BaseDataCache.getInstance().obtainPluginData(mockName1));
    }

    @Test
    public void testCacheSelectData() {
        SelectorData selectorData1 = SelectorData.builder().id("1").pluginName(mockPluginName1).sort(1).build();
        BaseDataCache.getInstance().cacheSelectData(selectorData1);
        ConcurrentHashMap<String, List<SelectorData>> selectorMap = getFieldByName(selectorMapStr);
        Assert.assertEquals(Lists.newArrayList(selectorData1), selectorMap.get(mockPluginName1));

        SelectorData selectorData2 = SelectorData.builder().id("2").pluginName(mockPluginName1).sort(2).build();
        BaseDataCache.getInstance().cacheSelectData(selectorData2);
        Assert.assertEquals(Lists.newArrayList(selectorData1, selectorData2), selectorMap.get(mockPluginName1));
    }

    @Test
    public void testRemoveSelectData() {
        SelectorData selectorData = SelectorData.builder().id("1").pluginName(mockPluginName1).build();
        ConcurrentHashMap<String, List<SelectorData>> selectorMap = getFieldByName(selectorMapStr);
        selectorMap.put(mockPluginName1, Lists.newArrayList(selectorData));

        BaseDataCache.getInstance().removeSelectData(selectorData);
        Assert.assertEquals(Lists.newArrayList(), selectorMap.get(mockPluginName1));
    }

    @Test
    public void testCleanSelectorData() {
        SelectorData selectorData1 = SelectorData.builder().id("1").pluginName(mockPluginName1).build();
        SelectorData selectorData2 = SelectorData.builder().id("2").pluginName(mockPluginName2).build();
        ConcurrentHashMap<String, List<SelectorData>> selectorMap = getFieldByName(selectorMapStr);
        selectorMap.put(mockPluginName1, Lists.newArrayList(selectorData1));
        selectorMap.put(mockPluginName2, Lists.newArrayList(selectorData2));

        BaseDataCache.getInstance().cleanSelectorData();
        Assert.assertNull(selectorMap.get(mockPluginName1));
        Assert.assertNull(selectorMap.get(mockPluginName2));
    }

    @Test
    public void testCleanSelectorDataSelf() {
        SelectorData selectorData1 = SelectorData.builder().id("1").pluginName(mockPluginName1).build();
        SelectorData selectorData2 = SelectorData.builder().id("2").pluginName(mockPluginName2).build();
        ConcurrentHashMap<String, List<SelectorData>> selectorMap = getFieldByName(selectorMapStr);
        selectorMap.put(mockPluginName1, Lists.newArrayList(selectorData1));
        selectorMap.put(mockPluginName2, Lists.newArrayList(selectorData2));

        BaseDataCache.getInstance().cleanSelectorDataSelf(Lists.newArrayList(selectorData1));
        Assert.assertEquals(Lists.newArrayList(), selectorMap.get(mockPluginName1));
        Assert.assertEquals(Lists.newArrayList(selectorData2), selectorMap.get(mockPluginName2));
    }

    @Test
    public void testObtainSelectorData() {
        SelectorData selectorData = SelectorData.builder().id("1").pluginName(mockPluginName1).build();
        ConcurrentHashMap<String, List<SelectorData>> selectorMap = getFieldByName(selectorMapStr);
        selectorMap.put(mockPluginName1, Lists.newArrayList(selectorData));

        List<SelectorData> selectorDataList = BaseDataCache.getInstance().obtainSelectorData(mockPluginName1);
        Assert.assertEquals(Lists.newArrayList(selectorData), selectorDataList);
    }

    @Test
    public void testCacheRuleData() {
        RuleData ruleData1 = RuleData.builder().id("1").selectorId(mockSelectorId1).sort(1).build();
        BaseDataCache.getInstance().cacheRuleData(ruleData1);
        ConcurrentHashMap<String, List<RuleData>> ruleMap = getFieldByName(ruleMapStr);
        Assert.assertEquals(Lists.newArrayList(ruleData1), ruleMap.get(mockSelectorId1));

        RuleData ruleData2 = RuleData.builder().id("2").selectorId(mockSelectorId1).sort(2).build();
        BaseDataCache.getInstance().cacheRuleData(ruleData2);
        Assert.assertEquals(Lists.newArrayList(ruleData1, ruleData2), ruleMap.get(mockSelectorId1));
    }

    @Test
    public void testRemoveRuleData() {
        RuleData ruleData = RuleData.builder().id("1").selectorId(mockSelectorId1).build();
        ConcurrentHashMap<String, List<RuleData>> ruleMap = getFieldByName(ruleMapStr);
        ruleMap.put(mockSelectorId1, Lists.newArrayList(ruleData));

        BaseDataCache.getInstance().removeRuleData(ruleData);
        Assert.assertEquals(Lists.newArrayList(), ruleMap.get(mockSelectorId1));
    }

    @Test
    public void testCleanRuleData() {
        RuleData ruleData1 = RuleData.builder().id("1").selectorId(mockSelectorId1).build();
        RuleData ruleData2 = RuleData.builder().id("2").selectorId(mockSelectorId2).build();
        ConcurrentHashMap<String, List<RuleData>> ruleMap = getFieldByName(ruleMapStr);
        ruleMap.put(mockSelectorId1, Lists.newArrayList(ruleData1));
        ruleMap.put(mockSelectorId2, Lists.newArrayList(ruleData2));

        BaseDataCache.getInstance().cleanRuleData();
        Assert.assertNull(ruleMap.get(mockSelectorId1));
        Assert.assertNull(ruleMap.get(mockSelectorId2));
    }

    @Test
    public void testCleanRuleDataSelf() {
        RuleData ruleData1 = RuleData.builder().id("1").selectorId(mockSelectorId1).build();
        RuleData ruleData2 = RuleData.builder().id("2").selectorId(mockSelectorId2).build();
        ConcurrentHashMap<String, List<RuleData>> ruleMap = getFieldByName(ruleMapStr);
        ruleMap.put(mockSelectorId1, Lists.newArrayList(ruleData1));
        ruleMap.put(mockSelectorId2, Lists.newArrayList(ruleData2));

        BaseDataCache.getInstance().cleanRuleDataSelf(Lists.newArrayList(ruleData1));
        Assert.assertEquals(Lists.newArrayList(), ruleMap.get(mockSelectorId1));
        Assert.assertEquals(Lists.newArrayList(ruleData2), ruleMap.get(mockSelectorId2));
    }

    @Test
    public void testObtainRuleData() {
        RuleData ruleData = RuleData.builder().id("1").selectorId(mockSelectorId1).build();
        ConcurrentHashMap<String, List<RuleData>> ruleMap = getFieldByName(ruleMapStr);
        ruleMap.put(mockSelectorId1, Lists.newArrayList(ruleData));

        List<RuleData> ruleDataList = BaseDataCache.getInstance().obtainRuleData(mockSelectorId1);
        Assert.assertEquals(Lists.newArrayList(ruleData), ruleDataList);
    }

    @SneakyThrows
    private ConcurrentHashMap getFieldByName(final String name) {
        BaseDataCache baseDataCache = BaseDataCache.getInstance();
        Field pluginMapField = baseDataCache.getClass().getDeclaredField(name);
        pluginMapField.setAccessible(true);
        return (ConcurrentHashMap) pluginMapField.get(baseDataCache);
    }
}
