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

import com.google.common.collect.Maps;
import org.apache.shenyu.common.cache.WindowTinyLFUMap;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.utils.MapUtils;

import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentMap;


/**
 * The match data cache.
 */
public final class MatchDataCache {

    private static final MatchDataCache INSTANCE = new MatchDataCache();

    /**
     * pluginName -> LRUMap.
     */
    private static final ConcurrentMap<String, Map<String, SelectorData>> SELECTOR_DATA_MAP = Maps.newConcurrentMap();
    
    /**
     * plugin name -> LRU Map.
     * LRU Map: path -> rule data.
     */
    private static final ConcurrentMap<String, Map<String, RuleData>> RULE_DATA_MAP = Maps.newConcurrentMap();

    private MatchDataCache() {
    }

    /**
     * Gets instance.
     *
     * @return the instance
     */
    public static MatchDataCache getInstance() {
        return INSTANCE;
    }

    /**
     * Remove selector data.
     *
     * @param pluginName the pluginName
     */
    public void removeSelectorData(final String pluginName) {
        SELECTOR_DATA_MAP.remove(pluginName);
    }

    /**
     * Clean selector data.
     */
    public void cleanSelectorData() {
        SELECTOR_DATA_MAP.clear();
    }

    /**
     * Cache selector data.
     *
     * @param path         the path
     * @param selectorData the selector data
     * @param initialCapacity initialCapacity
     * @param maximumSize maximumSize
     */
    public void cacheSelectorData(final String path, final SelectorData selectorData, final int initialCapacity, final long maximumSize) {
        MapUtils.computeIfAbsent(SELECTOR_DATA_MAP, selectorData.getPluginName(), map ->
                new WindowTinyLFUMap<>(initialCapacity, maximumSize, Boolean.FALSE)).put(path, selectorData);
    }

    /**
     * Obtain selector data.
     *
     * @param pluginName the pluginName
     * @param path       the path
     * @return the selector data
     */
    public SelectorData obtainSelectorData(final String pluginName, final String path) {
        final Map<String, SelectorData> lruMap = SELECTOR_DATA_MAP.get(pluginName);
        return Optional.ofNullable(lruMap).orElse(Maps.newHashMap()).get(path);
    }
    
    /**
     * cache rule data.
     *
     * @param path path
     * @param ruleData rule data
     * @param initialCapacity initial capacity
     * @param maximumSize maximum size
     */
    public void cacheRuleData(final String path, final RuleData ruleData, final int initialCapacity, final long maximumSize) {
        MapUtils.computeIfAbsent(RULE_DATA_MAP, ruleData.getPluginName(), map ->
                new WindowTinyLFUMap<>(initialCapacity, maximumSize, Boolean.FALSE)).put(path, ruleData);
    }
    
    /**
     * remove rule data.
     *
     * @param pluginName pluginName
     */
    public void removeRuleData(final String pluginName) {
        RULE_DATA_MAP.remove(pluginName);
    }
    
    /**
     * clear the cache.
     */
    public void cleanRuleDataData() {
        RULE_DATA_MAP.clear();
    }
    
    /**
     * get rule data.
     *
     * @param pluginName pluginName
     * @param path path
     * @return ruleData
     */
    public RuleData obtainRuleData(final String pluginName, final String path) {
        final Map<String, RuleData> lruMap = RULE_DATA_MAP.get(pluginName);
        return Optional.ofNullable(lruMap).orElse(Maps.newHashMap()).get(path);
    }
    
}
