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

import org.apache.shenyu.common.cache.WindowTinyLFUMap;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.util.concurrent.ConcurrentHashMap;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

@SuppressWarnings("unchecked")
public final class MatchDataCacheTest {

    private final String selectorMapStr = "SELECTOR_DATA_MAP";

    private final String ruleMapStr = "RULE_DATA_MAP";

    private final String mockPluginName1 = "MOCK_PLUGIN_NAME_1";

    private final String path1 = "/http/abc";

    @Test
    public void testCacheSelectorData() throws NoSuchFieldException, IllegalAccessException {
        SelectorData firstCachedSelectorData = SelectorData.builder().id("1").pluginName(mockPluginName1).sort(1).build();
        MatchDataCache.getInstance().cacheSelectorData(path1, firstCachedSelectorData, 100, 100);
        ConcurrentHashMap<String, WindowTinyLFUMap<String, SelectorData>> selectorMap = getFieldByName(selectorMapStr);
        assertEquals(firstCachedSelectorData, selectorMap.get(mockPluginName1).get(path1));
        selectorMap.clear();
    }

    @Test
    public void testObtainSelectorData() throws NoSuchFieldException, IllegalAccessException {
        SelectorData firstSelectorData = SelectorData.builder().id("1").pluginName(mockPluginName1).sort(1).build();
        ConcurrentHashMap<String, WindowTinyLFUMap<String, SelectorData>> selectorMap = getFieldByName(selectorMapStr);
        selectorMap.put(mockPluginName1, new WindowTinyLFUMap<>(100, 100, Boolean.FALSE));
        selectorMap.get(mockPluginName1).put(path1, firstSelectorData);
        SelectorData firstSelectorDataCache = MatchDataCache.getInstance().obtainSelectorData(mockPluginName1, path1);
        assertEquals(firstSelectorData, firstSelectorDataCache);
        selectorMap.clear();
    }

    @Test
    public void testRemoveSelectorData() throws NoSuchFieldException, IllegalAccessException {
        SelectorData firstCachedSelectorData = SelectorData.builder().id("1").pluginName(mockPluginName1).sort(1).build();
        MatchDataCache.getInstance().cacheSelectorData(path1, firstCachedSelectorData, 100, 100);
        MatchDataCache.getInstance().removeSelectorData(firstCachedSelectorData.getPluginName());
        ConcurrentHashMap<String, WindowTinyLFUMap<String, SelectorData>> selectorMap = getFieldByName(selectorMapStr);
        assertNull(selectorMap.get(mockPluginName1));
        selectorMap.clear();
    }

    @SuppressWarnings("rawtypes")
    private ConcurrentHashMap getFieldByName(final String name) throws NoSuchFieldException, IllegalAccessException {
        MatchDataCache matchDataCache = MatchDataCache.getInstance();
        Field pluginMapField = matchDataCache.getClass().getDeclaredField(name);
        pluginMapField.setAccessible(true);
        return (ConcurrentHashMap) pluginMapField.get(matchDataCache);
    }
    
    @Test
    public void testCacheRuleData() throws NoSuchFieldException, IllegalAccessException {
        RuleData cacheRuleData = RuleData.builder().id("1").pluginName(mockPluginName1).sort(1).build();
        MatchDataCache.getInstance().cacheRuleData(path1, cacheRuleData, 100, 100);
        ConcurrentHashMap<String, WindowTinyLFUMap<String, RuleData>> ruleMap = getFieldByName(ruleMapStr);
        assertEquals(cacheRuleData, ruleMap.get(mockPluginName1).get(path1));
        ruleMap.clear();
    }
    
    @Test
    public void testObtainRuleData() throws NoSuchFieldException, IllegalAccessException {
        RuleData cacheRuleData = RuleData.builder().id("1").pluginName(mockPluginName1).sort(1).build();
        ConcurrentHashMap<String, WindowTinyLFUMap<String, RuleData>> ruleMap = getFieldByName(ruleMapStr);
        ruleMap.put(mockPluginName1, new WindowTinyLFUMap<>(100, 100, Boolean.FALSE));
        ruleMap.get(mockPluginName1).put(path1, cacheRuleData);
        RuleData firstRuleDataCache = MatchDataCache.getInstance().obtainRuleData(mockPluginName1, path1);
        assertEquals(cacheRuleData, firstRuleDataCache);
        ruleMap.clear();
    }
    
    @Test
    public void testRemoveRuleData() throws NoSuchFieldException, IllegalAccessException {
        RuleData cacheRuleData = RuleData.builder().id("1").pluginName(mockPluginName1).sort(1).build();
        MatchDataCache.getInstance().cacheRuleData(path1, cacheRuleData, 100, 100);
        MatchDataCache.getInstance().removeRuleData(cacheRuleData.getPluginName());
        ConcurrentHashMap<String, WindowTinyLFUMap<String, RuleData>> ruleMap = getFieldByName(ruleMapStr);
        assertNull(ruleMap.get(mockPluginName1));
        ruleMap.clear();
    }
}
