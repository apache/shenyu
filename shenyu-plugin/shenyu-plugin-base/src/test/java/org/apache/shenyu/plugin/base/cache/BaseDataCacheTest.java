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
import lombok.SneakyThrows;
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
    public void testCachePluginData() {
        final PluginData pluginData = new PluginData();
        pluginData.setName(mockName1);

        ConcurrentHashMap<String, PluginData> pluginMap = getFieldByName(pluginMapStr);
        Assert.assertNull(pluginMap.get(mockName1));

        BaseDataCache.getInstance().cachePluginData(pluginData);
        Assert.assertNotNull(pluginMap.get(mockName1));
        Assert.assertEquals(pluginData, pluginMap.get(mockName1));
    }

    @Test
    public void testRemovePluginData() {
        final PluginData pluginData = new PluginData();
        pluginData.setName(mockName1);

        ConcurrentHashMap<String, PluginData> pluginMap = getFieldByName(pluginMapStr);
        pluginMap.put(mockName1, pluginData);
        Assert.assertNotNull(pluginMap.get(mockName1));

        BaseDataCache.getInstance().removePluginData(pluginData);
        Assert.assertNull(pluginMap.get(mockName1));
    }

    @Test
    public void testCleanPluginData() {
        ConcurrentHashMap<String, PluginData> pluginMap = getFieldByName(pluginMapStr);
        final PluginData firstCachedPluginData = new PluginData();
        firstCachedPluginData.setName(mockName1);
        pluginMap.put(mockName1, firstCachedPluginData);
        final PluginData secondCachedPluginData = new PluginData();
        secondCachedPluginData.setName(mockName2);
        pluginMap.put(mockName2, secondCachedPluginData);
        Assert.assertNotNull(pluginMap.get(mockName1));
        Assert.assertNotNull(pluginMap.get(mockName2));

        BaseDataCache.getInstance().cleanPluginData();
        Assert.assertNull(pluginMap.get(mockName1));
        Assert.assertNull(pluginMap.get(mockName2));
    }

    @Test
    public void testCleanPluginDataSelf() {
        ConcurrentHashMap<String, PluginData> pluginMap = getFieldByName(pluginMapStr);
        final PluginData firstCachedPluginData = new PluginData();
        firstCachedPluginData.setName(mockName1);
        pluginMap.put(mockName1, firstCachedPluginData);
        final PluginData secondCachedPluginData = new PluginData();
        secondCachedPluginData.setName(mockName2);
        pluginMap.put(mockName2, secondCachedPluginData);
        Assert.assertNotNull(pluginMap.get(mockName1));
        Assert.assertNotNull(pluginMap.get(mockName2));

        List<PluginData> pluginDataList = Lists.newArrayList(firstCachedPluginData);
        BaseDataCache.getInstance().cleanPluginDataSelf(pluginDataList);
        Assert.assertNull(pluginMap.get(mockName1));
        Assert.assertNotNull(pluginMap.get(mockName2));
    }

    @Test
    public void testObtainPluginData() {
        final PluginData pluginData = new PluginData();
        pluginData.setName(mockName1);
        ConcurrentHashMap<String, PluginData> pluginMap = getFieldByName(pluginMapStr);
        pluginMap.put(mockName1, pluginData);
        Assert.assertNotNull(pluginMap.get(mockName1));
        Assert.assertEquals(pluginData, BaseDataCache.getInstance().obtainPluginData(mockName1));
    }

    @Test
    public void testCacheSelectData() {
        final SelectorData firstCachedSelectorData = new SelectorData();
        firstCachedSelectorData.setId("1");
        firstCachedSelectorData.setPluginName(mockPluginName1);
        firstCachedSelectorData.setSort(1);
        BaseDataCache.getInstance().cacheSelectData(firstCachedSelectorData);
        ConcurrentHashMap<String, List<SelectorData>> selectorMap = getFieldByName(selectorMapStr);
        Assert.assertEquals(Lists.newArrayList(firstCachedSelectorData), selectorMap.get(mockPluginName1));
        final SelectorData secondCachedSelectorData = new SelectorData();
        firstCachedSelectorData.setId("2");
        firstCachedSelectorData.setPluginName(mockPluginName1);
        firstCachedSelectorData.setSort(2);
        BaseDataCache.getInstance().cacheSelectData(secondCachedSelectorData);
        Assert.assertEquals(Lists.newArrayList(firstCachedSelectorData, secondCachedSelectorData), selectorMap.get(mockPluginName1));
    }

    @Test
    public void testRemoveSelectData() {
        final SelectorData selectorData = new SelectorData();
        selectorData.setId("1");
        selectorData.setPluginName(mockPluginName1);
        ConcurrentHashMap<String, List<SelectorData>> selectorMap = getFieldByName(selectorMapStr);
        selectorMap.put(mockPluginName1, Lists.newArrayList(selectorData));

        BaseDataCache.getInstance().removeSelectData(selectorData);
        Assert.assertEquals(Lists.newArrayList(), selectorMap.get(mockPluginName1));
    }

    @Test
    public void testCleanSelectorData() {
        final SelectorData firstCachedSelectorData = new SelectorData();
        firstCachedSelectorData.setId("1");
        firstCachedSelectorData.setPluginName(mockPluginName1);
        final SelectorData secondCachedSelectorData = new SelectorData();
        secondCachedSelectorData.setId("2");
        secondCachedSelectorData.setPluginName(mockPluginName2);
        ConcurrentHashMap<String, List<SelectorData>> selectorMap = getFieldByName(selectorMapStr);
        selectorMap.put(mockPluginName1, Lists.newArrayList(firstCachedSelectorData));
        selectorMap.put(mockPluginName2, Lists.newArrayList(secondCachedSelectorData));

        BaseDataCache.getInstance().cleanSelectorData();
        Assert.assertNull(selectorMap.get(mockPluginName1));
        Assert.assertNull(selectorMap.get(mockPluginName2));
    }

    @Test
    public void testCleanSelectorDataSelf() {
        final SelectorData firstCachedSelectorData = new SelectorData();
        firstCachedSelectorData.setId("1");
        firstCachedSelectorData.setPluginName(mockPluginName1);
        final SelectorData secondCachedSelectorData = new SelectorData();
        secondCachedSelectorData.setId("2");
        secondCachedSelectorData.setPluginName(mockPluginName2);
        ConcurrentHashMap<String, List<SelectorData>> selectorMap = getFieldByName(selectorMapStr);
        selectorMap.put(mockPluginName1, Lists.newArrayList(firstCachedSelectorData));
        selectorMap.put(mockPluginName2, Lists.newArrayList(secondCachedSelectorData));

        BaseDataCache.getInstance().cleanSelectorDataSelf(Lists.newArrayList(firstCachedSelectorData));
        Assert.assertEquals(Lists.newArrayList(), selectorMap.get(mockPluginName1));
        Assert.assertEquals(Lists.newArrayList(secondCachedSelectorData), selectorMap.get(mockPluginName2));
    }

    @Test
    public void testObtainSelectorData() {
        final SelectorData selectorData = new SelectorData();
        selectorData.setId("1");
        selectorData.setPluginName(mockPluginName1);
        ConcurrentHashMap<String, List<SelectorData>> selectorMap = getFieldByName(selectorMapStr);
        selectorMap.put(mockPluginName1, Lists.newArrayList(selectorData));

        List<SelectorData> selectorDataList = BaseDataCache.getInstance().obtainSelectorData(mockPluginName1);
        Assert.assertEquals(Lists.newArrayList(selectorData), selectorDataList);
    }

    @Test
    public void testCacheRuleData() {
        final RuleData firstCachedRuleData = new RuleData();
        firstCachedRuleData.setId("1");
        firstCachedRuleData.setSelectorId(mockSelectorId1);
        firstCachedRuleData.setSort(1);
        BaseDataCache.getInstance().cacheRuleData(firstCachedRuleData);
        ConcurrentHashMap<String, List<RuleData>> ruleMap = getFieldByName(ruleMapStr);
        Assert.assertEquals(Lists.newArrayList(firstCachedRuleData), ruleMap.get(mockSelectorId1));
        final RuleData secondCachedRuleData = new RuleData();
        secondCachedRuleData.setId("2");
        secondCachedRuleData.setSelectorId(mockSelectorId1);
        secondCachedRuleData.setSort(2);
        BaseDataCache.getInstance().cacheRuleData(secondCachedRuleData);
        Assert.assertEquals(Lists.newArrayList(firstCachedRuleData, secondCachedRuleData), ruleMap.get(mockSelectorId1));
    }

    @Test
    public void testRemoveRuleData() {
        final RuleData ruleData = new RuleData();
        ruleData.setId("1");
        ruleData.setSelectorId(mockSelectorId1);
        ConcurrentHashMap<String, List<RuleData>> ruleMap = getFieldByName(ruleMapStr);
        ruleMap.put(mockSelectorId1, Lists.newArrayList(ruleData));

        BaseDataCache.getInstance().removeRuleData(ruleData);
        Assert.assertEquals(Lists.newArrayList(), ruleMap.get(mockSelectorId1));
    }

    @Test
    public void testCleanRuleData() {
        final RuleData firstCachedRuleData = new RuleData();
        firstCachedRuleData.setId("1");
        firstCachedRuleData.setSelectorId(mockSelectorId1);
        final RuleData secondCachedRuleData = new RuleData();
        secondCachedRuleData.setId("2");
        secondCachedRuleData.setSelectorId(mockSelectorId2);
        ConcurrentHashMap<String, List<RuleData>> ruleMap = getFieldByName(ruleMapStr);
        ruleMap.put(mockSelectorId1, Lists.newArrayList(firstCachedRuleData));
        ruleMap.put(mockSelectorId2, Lists.newArrayList(secondCachedRuleData));

        BaseDataCache.getInstance().cleanRuleData();
        Assert.assertNull(ruleMap.get(mockSelectorId1));
        Assert.assertNull(ruleMap.get(mockSelectorId2));
    }

    @Test
    public void testCleanRuleDataSelf() {
        final RuleData firstCachedRuleData = new RuleData();
        firstCachedRuleData.setId("1");
        firstCachedRuleData.setSelectorId(mockSelectorId1);
        final RuleData secondCachedRuleData = new RuleData();
        secondCachedRuleData.setId("2");
        secondCachedRuleData.setSelectorId(mockSelectorId2);
        ConcurrentHashMap<String, List<RuleData>> ruleMap = getFieldByName(ruleMapStr);
        ruleMap.put(mockSelectorId1, Lists.newArrayList(firstCachedRuleData));
        ruleMap.put(mockSelectorId2, Lists.newArrayList(secondCachedRuleData));

        BaseDataCache.getInstance().cleanRuleDataSelf(Lists.newArrayList(firstCachedRuleData));
        Assert.assertEquals(Lists.newArrayList(), ruleMap.get(mockSelectorId1));
        Assert.assertEquals(Lists.newArrayList(secondCachedRuleData), ruleMap.get(mockSelectorId2));
    }

    @Test
    public void testObtainRuleData() {
        final RuleData ruleData = new RuleData();
        ruleData.setId("1");
        ruleData.setSelectorId(mockSelectorId1);
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
