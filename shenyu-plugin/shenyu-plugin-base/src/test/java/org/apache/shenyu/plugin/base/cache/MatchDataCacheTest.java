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
import org.apache.shenyu.common.cache.MemorySafeLRUMap;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

import static org.junit.jupiter.api.Assertions.assertEquals;

@SuppressWarnings("unchecked")
public final class MatchDataCacheTest {

    private final String selectorMapStr = "SELECTOR_DATA_MAP";

    private final String ruleMapStr = "RULE_DATA_MAP";

    private final String mockPluginName1 = "MOCK_PLUGIN_NAME_1";

    private final String path1 = "/http/abc";

    @Test
    public void testCacheSelectorData() throws NoSuchFieldException, IllegalAccessException {
        SelectorData firstCachedSelectorData = SelectorData.builder().id("1").pluginName(mockPluginName1).sort(1).build();
        MatchDataCache.getInstance().cacheSelectorData(path1, firstCachedSelectorData, 5 * 1024);
        ConcurrentHashMap<String, MemorySafeLRUMap<String, List<SelectorData>>> selectorMap = getFieldByName(selectorMapStr);
        assertEquals(Lists.newArrayList(firstCachedSelectorData), selectorMap.get(mockPluginName1).get(path1));

        SelectorData secondCachedSelectorData = SelectorData.builder().id("2").pluginName(mockPluginName1).sort(2).build();
        MatchDataCache.getInstance().cacheSelectorData(path1, secondCachedSelectorData, 5 * 1024);
        assertEquals(Lists.newArrayList(firstCachedSelectorData, secondCachedSelectorData), selectorMap.get(mockPluginName1).get(path1));
    }

    @Test
    public void testCacheRuleData() throws NoSuchFieldException, IllegalAccessException {
        RuleData firstCachedRuleData = RuleData.builder().id("1").pluginName(mockPluginName1).sort(1).build();
        MatchDataCache.getInstance().cacheRuleData(path1, firstCachedRuleData, 5 * 1024);
        ConcurrentHashMap<String, MemorySafeLRUMap<String, List<RuleData>>> ruleMap = getFieldByName(ruleMapStr);
        assertEquals(Lists.newArrayList(firstCachedRuleData), ruleMap.get(mockPluginName1).get(path1));

        RuleData secondCachedRuleData = RuleData.builder().id("2").pluginName(mockPluginName1).sort(2).build();
        MatchDataCache.getInstance().cacheRuleData(path1, secondCachedRuleData, 5 * 1024);
        assertEquals(Lists.newArrayList(firstCachedRuleData, secondCachedRuleData), ruleMap.get(mockPluginName1).get(path1));
    }

    @Test
    public void testObtainSelectorData() throws NoSuchFieldException, IllegalAccessException {
        SelectorData firstCachedSelectorData = SelectorData.builder().id("1").pluginName(mockPluginName1).sort(1).build();
        ConcurrentHashMap<String, MemorySafeLRUMap<String, List<SelectorData>>> selectorMap = getFieldByName(selectorMapStr);
        selectorMap.put(mockPluginName1, new MemorySafeLRUMap<>(5 * 1024, 16));
        List<SelectorData> selectorDataListTemp = Lists.newArrayList(firstCachedSelectorData);
        selectorMap.get(mockPluginName1).put(path1, selectorDataListTemp);
        List<SelectorData> firstSelectorDataList = MatchDataCache.getInstance().obtainSelectorData(mockPluginName1, path1);
        assertEquals(Lists.newArrayList(firstCachedSelectorData), firstSelectorDataList);

        SelectorData secondCachedSelectorData = SelectorData.builder().id("2").pluginName(mockPluginName1).sort(2).build();
        selectorDataListTemp.add(secondCachedSelectorData);
        List<SelectorData> secondSelectorDataList = MatchDataCache.getInstance().obtainSelectorData(mockPluginName1, path1);
        assertEquals(selectorDataListTemp, secondSelectorDataList);
    }

    @Test
    public void testObtainRuleData() throws NoSuchFieldException, IllegalAccessException {
        RuleData firstCachedRuleData = RuleData.builder().id("1").pluginName(mockPluginName1).sort(1).build();
        ConcurrentHashMap<String, MemorySafeLRUMap<String, List<RuleData>>> ruleMap = getFieldByName(ruleMapStr);
        ruleMap.put(mockPluginName1, new MemorySafeLRUMap<>(5 * 1024, 16));
        List<RuleData> ruleDataListTemp = Lists.newArrayList(firstCachedRuleData);
        ruleMap.get(mockPluginName1).put(path1, ruleDataListTemp);
        List<RuleData> firstRuleDataList = MatchDataCache.getInstance().obtainRuleData(mockPluginName1, path1);
        assertEquals(Lists.newArrayList(firstCachedRuleData), firstRuleDataList);

        RuleData secondCachedRuleData = RuleData.builder().id("2").pluginName(mockPluginName1).sort(2).build();
        ruleDataListTemp.add(secondCachedRuleData);
        List<RuleData> secondRuleDataList = MatchDataCache.getInstance().obtainRuleData(mockPluginName1, path1);
        assertEquals(ruleDataListTemp, secondRuleDataList);
    }

    @Test
    public void testRemoveSelectorData() throws NoSuchFieldException, IllegalAccessException {
        SelectorData firstCachedSelectorData = SelectorData.builder().id("1").pluginName(mockPluginName1).sort(1).build();
        MatchDataCache.getInstance().cacheSelectorData(path1, firstCachedSelectorData, 5 * 1024);

        SelectorData secondCachedSelectorData = SelectorData.builder().id("2").pluginName(mockPluginName1).sort(2).build();
        MatchDataCache.getInstance().cacheSelectorData(path1, secondCachedSelectorData, 5 * 1024);

        MatchDataCache.getInstance().removeSelectorData(firstCachedSelectorData);
        ConcurrentHashMap<String, MemorySafeLRUMap<String, List<SelectorData>>> selectorMap = getFieldByName(selectorMapStr);
        assertEquals(Lists.newArrayList(secondCachedSelectorData), selectorMap.get(mockPluginName1).get(path1));
    }

    @Test
    public void testRemoveRuleData() throws NoSuchFieldException, IllegalAccessException {
        RuleData firstCachedRuleData = RuleData.builder().id("1").pluginName(mockPluginName1).sort(1).build();
        MatchDataCache.getInstance().cacheRuleData(path1, firstCachedRuleData, 5 * 1024);

        RuleData secondCachedRuleData = RuleData.builder().id("2").pluginName(mockPluginName1).sort(2).build();
        MatchDataCache.getInstance().cacheRuleData(path1, secondCachedRuleData, 5 * 1024);

        MatchDataCache.getInstance().removeRuleData(firstCachedRuleData);
        ConcurrentHashMap<String, MemorySafeLRUMap<String, List<RuleData>>> ruleMap = getFieldByName(ruleMapStr);
        assertEquals(Lists.newArrayList(secondCachedRuleData), ruleMap.get(mockPluginName1).get(path1));
    }

    @SuppressWarnings("rawtypes")
    private ConcurrentHashMap getFieldByName(final String name) throws NoSuchFieldException, IllegalAccessException {
        MatchDataCache matchDataCache = MatchDataCache.getInstance();
        Field pluginMapField = matchDataCache.getClass().getDeclaredField(name);
        pluginMapField.setAccessible(true);
        return (ConcurrentHashMap) pluginMapField.get(matchDataCache);
    }
}
