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
import org.junit.Assert;
import org.junit.Test;

import java.lang.reflect.Field;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Test cases for BaseDataCache.
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
    public void testCachePluginData() throws NoSuchFieldException, IllegalAccessException {
        PluginData pluginData = PluginData.builder().name(mockName1).build();
        ConcurrentHashMap<String, PluginData> pluginMap = getFieldByName(pluginMapStr);
        Assert.assertNull(pluginMap.get(mockName1));

        BaseDataCache.getInstance().cachePluginData(pluginData);
        Assert.assertNotNull(pluginMap.get(mockName1));
        Assert.assertEquals(pluginData, pluginMap.get(mockName1));
    }

    @Test
    public void testRemovePluginData() throws NoSuchFieldException, IllegalAccessException {
        PluginData pluginData = PluginData.builder().name(mockName1).build();
        ConcurrentHashMap<String, PluginData> pluginMap = getFieldByName(pluginMapStr);
        pluginMap.put(mockName1, pluginData);
        Assert.assertNotNull(pluginMap.get(mockName1));

        BaseDataCache.getInstance().removePluginData(pluginData);
        Assert.assertNull(pluginMap.get(mockName1));
    }

    @Test
    public void testCleanPluginData() throws NoSuchFieldException, IllegalAccessException {
        PluginData firstCachedPluginData = PluginData.builder().name(mockName1).build();
        PluginData secondCachedPluginData = PluginData.builder().name(mockName2).build();
        ConcurrentHashMap<String, PluginData> pluginMap = getFieldByName(pluginMapStr);
        pluginMap.put(mockName1, firstCachedPluginData);
        pluginMap.put(mockName2, secondCachedPluginData);
        Assert.assertNotNull(pluginMap.get(mockName1));
        Assert.assertNotNull(pluginMap.get(mockName2));

        BaseDataCache.getInstance().cleanPluginData();
        Assert.assertNull(pluginMap.get(mockName1));
        Assert.assertNull(pluginMap.get(mockName2));
    }

    @Test
    public void testCleanPluginDataSelf() throws NoSuchFieldException, IllegalAccessException {
        PluginData firstCachedPluginData = PluginData.builder().name(mockName1).build();
        PluginData secondCachedPluginData = PluginData.builder().name(mockName2).build();
        ConcurrentHashMap<String, PluginData> pluginMap = getFieldByName(pluginMapStr);
        pluginMap.put(mockName1, firstCachedPluginData);
        pluginMap.put(mockName2, secondCachedPluginData);
        Assert.assertNotNull(pluginMap.get(mockName1));
        Assert.assertNotNull(pluginMap.get(mockName2));

        List<PluginData> pluginDataList = Lists.newArrayList(firstCachedPluginData);
        BaseDataCache.getInstance().cleanPluginDataSelf(pluginDataList);
        Assert.assertNull(pluginMap.get(mockName1));
        Assert.assertNotNull(pluginMap.get(mockName2));
    }

    @Test
    public void testObtainPluginData() throws NoSuchFieldException, IllegalAccessException {
        PluginData pluginData = PluginData.builder().name(mockName1).build();
        ConcurrentHashMap<String, PluginData> pluginMap = getFieldByName(pluginMapStr);
        pluginMap.put(mockName1, pluginData);
        Assert.assertNotNull(pluginMap.get(mockName1));
        Assert.assertEquals(pluginData, BaseDataCache.getInstance().obtainPluginData(mockName1));
    }

    @Test
    public void testCacheSelectData() throws NoSuchFieldException, IllegalAccessException {
        SelectorData firstCachedSelectorData = SelectorData.builder().id("1").pluginName(mockPluginName1).sort(1).build();
        BaseDataCache.getInstance().cacheSelectData(firstCachedSelectorData);
        ConcurrentHashMap<String, List<SelectorData>> selectorMap = getFieldByName(selectorMapStr);
        Assert.assertEquals(Lists.newArrayList(firstCachedSelectorData), selectorMap.get(mockPluginName1));

        SelectorData secondCachedSelectorData = SelectorData.builder().id("2").pluginName(mockPluginName1).sort(2).build();
        BaseDataCache.getInstance().cacheSelectData(secondCachedSelectorData);
        Assert.assertEquals(Lists.newArrayList(firstCachedSelectorData, secondCachedSelectorData), selectorMap.get(mockPluginName1));
    }

    @Test
    public void testRemoveSelectData() throws NoSuchFieldException, IllegalAccessException {
        SelectorData selectorData = SelectorData.builder().id("1").pluginName(mockPluginName1).build();
        ConcurrentHashMap<String, List<SelectorData>> selectorMap = getFieldByName(selectorMapStr);
        selectorMap.put(mockPluginName1, Lists.newArrayList(selectorData));

        BaseDataCache.getInstance().removeSelectData(selectorData);
        Assert.assertEquals(Lists.newArrayList(), selectorMap.get(mockPluginName1));
    }

    @Test
    public void testCleanSelectorData() throws NoSuchFieldException, IllegalAccessException {
        SelectorData firstCachedSelectorData = SelectorData.builder().id("1").pluginName(mockPluginName1).build();
        SelectorData secondCachedSelectorData = SelectorData.builder().id("2").pluginName(mockPluginName2).build();
        ConcurrentHashMap<String, List<SelectorData>> selectorMap = getFieldByName(selectorMapStr);
        selectorMap.put(mockPluginName1, Lists.newArrayList(firstCachedSelectorData));
        selectorMap.put(mockPluginName2, Lists.newArrayList(secondCachedSelectorData));

        BaseDataCache.getInstance().cleanSelectorData();
        Assert.assertNull(selectorMap.get(mockPluginName1));
        Assert.assertNull(selectorMap.get(mockPluginName2));
    }

    @Test
    public void testCleanSelectorDataSelf() throws NoSuchFieldException, IllegalAccessException {
        SelectorData firstCachedSelectorData = SelectorData.builder().id("1").pluginName(mockPluginName1).build();
        SelectorData secondCachedSelectorData = SelectorData.builder().id("2").pluginName(mockPluginName2).build();
        ConcurrentHashMap<String, List<SelectorData>> selectorMap = getFieldByName(selectorMapStr);
        selectorMap.put(mockPluginName1, Lists.newArrayList(firstCachedSelectorData));
        selectorMap.put(mockPluginName2, Lists.newArrayList(secondCachedSelectorData));

        BaseDataCache.getInstance().cleanSelectorDataSelf(Lists.newArrayList(firstCachedSelectorData));
        Assert.assertEquals(Lists.newArrayList(), selectorMap.get(mockPluginName1));
        Assert.assertEquals(Lists.newArrayList(secondCachedSelectorData), selectorMap.get(mockPluginName2));
    }

    @Test
    public void testObtainSelectorData() throws NoSuchFieldException, IllegalAccessException {
        SelectorData selectorData = SelectorData.builder().id("1").pluginName(mockPluginName1).build();
        ConcurrentHashMap<String, List<SelectorData>> selectorMap = getFieldByName(selectorMapStr);
        selectorMap.put(mockPluginName1, Lists.newArrayList(selectorData));

        List<SelectorData> selectorDataList = BaseDataCache.getInstance().obtainSelectorData(mockPluginName1);
        Assert.assertEquals(Lists.newArrayList(selectorData), selectorDataList);
    }

    @Test
    public void testCacheRuleData() throws NoSuchFieldException, IllegalAccessException {
        RuleData firstCachedRuleData = RuleData.builder().id("1").selectorId(mockSelectorId1).sort(1).build();
        BaseDataCache.getInstance().cacheRuleData(firstCachedRuleData);
        ConcurrentHashMap<String, List<RuleData>> ruleMap = getFieldByName(ruleMapStr);
        Assert.assertEquals(Lists.newArrayList(firstCachedRuleData), ruleMap.get(mockSelectorId1));

        RuleData secondCachedRuleData = RuleData.builder().id("2").selectorId(mockSelectorId1).sort(2).build();
        BaseDataCache.getInstance().cacheRuleData(secondCachedRuleData);
        Assert.assertEquals(Lists.newArrayList(firstCachedRuleData, secondCachedRuleData), ruleMap.get(mockSelectorId1));
    }

    @Test
    public void testRemoveRuleData() throws NoSuchFieldException, IllegalAccessException {
        RuleData ruleData = RuleData.builder().id("1").selectorId(mockSelectorId1).build();
        ConcurrentHashMap<String, List<RuleData>> ruleMap = getFieldByName(ruleMapStr);
        ruleMap.put(mockSelectorId1, Lists.newArrayList(ruleData));

        BaseDataCache.getInstance().removeRuleData(ruleData);
        Assert.assertEquals(Lists.newArrayList(), ruleMap.get(mockSelectorId1));
    }

    @Test
    public void testCleanRuleData() throws NoSuchFieldException, IllegalAccessException {
        RuleData firstCachedRuleData = RuleData.builder().id("1").selectorId(mockSelectorId1).build();
        RuleData secondCachedRuleData = RuleData.builder().id("2").selectorId(mockSelectorId2).build();
        ConcurrentHashMap<String, List<RuleData>> ruleMap = getFieldByName(ruleMapStr);
        ruleMap.put(mockSelectorId1, Lists.newArrayList(firstCachedRuleData));
        ruleMap.put(mockSelectorId2, Lists.newArrayList(secondCachedRuleData));

        BaseDataCache.getInstance().cleanRuleData();
        Assert.assertNull(ruleMap.get(mockSelectorId1));
        Assert.assertNull(ruleMap.get(mockSelectorId2));
    }

    @Test
    public void testCleanRuleDataSelf() throws NoSuchFieldException, IllegalAccessException {
        RuleData firstCachedRuleData = RuleData.builder().id("1").selectorId(mockSelectorId1).build();
        RuleData secondCachedRuleData = RuleData.builder().id("2").selectorId(mockSelectorId2).build();
        ConcurrentHashMap<String, List<RuleData>> ruleMap = getFieldByName(ruleMapStr);
        ruleMap.put(mockSelectorId1, Lists.newArrayList(firstCachedRuleData));
        ruleMap.put(mockSelectorId2, Lists.newArrayList(secondCachedRuleData));

        BaseDataCache.getInstance().cleanRuleDataSelf(Lists.newArrayList(firstCachedRuleData));
        Assert.assertEquals(Lists.newArrayList(), ruleMap.get(mockSelectorId1));
        Assert.assertEquals(Lists.newArrayList(secondCachedRuleData), ruleMap.get(mockSelectorId2));
    }

    @Test
    public void testObtainRuleData() throws NoSuchFieldException, IllegalAccessException {
        RuleData ruleData = RuleData.builder().id("1").selectorId(mockSelectorId1).build();
        ConcurrentHashMap<String, List<RuleData>> ruleMap = getFieldByName(ruleMapStr);
        ruleMap.put(mockSelectorId1, Lists.newArrayList(ruleData));

        List<RuleData> ruleDataList = BaseDataCache.getInstance().obtainRuleData(mockSelectorId1);
        Assert.assertEquals(Lists.newArrayList(ruleData), ruleDataList);
    }

    private ConcurrentHashMap getFieldByName(final String name) throws NoSuchFieldException, IllegalAccessException {
        BaseDataCache baseDataCache = BaseDataCache.getInstance();
        Field pluginMapField = baseDataCache.getClass().getDeclaredField(name);
        pluginMapField.setAccessible(true);
        return (ConcurrentHashMap) pluginMapField.get(baseDataCache);
    }
}
