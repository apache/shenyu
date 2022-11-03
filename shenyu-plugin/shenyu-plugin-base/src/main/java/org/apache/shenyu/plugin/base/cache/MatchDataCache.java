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
import org.apache.shenyu.common.cache.MemorySafeWindowTinyLFUMap;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;

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
     * @param maxMemory    the max memory
     */
    public void cacheSelectorData(final String path, final SelectorData selectorData, final Integer maxMemory) {
        SELECTOR_DATA_MAP.computeIfAbsent(selectorData.getPluginName(), map -> new MemorySafeWindowTinyLFUMap<>(maxMemory, 1 << 16)).put(path, selectorData);
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
     * remove rule data from RULE_DATA_MAP.
     *
     * @param pluginName plugin name
     */
    public void removeRuleData(final String pluginName) {
        RULE_DATA_MAP.remove(pluginName);
    }

    /**
     * clean rule data.
     */
    public void cleanRuleData() {
        RULE_DATA_MAP.clear();
    }

    /**
     * cache rule data to RULE_DATA_MAP, and put (key=pluginName, value=ruleData) to {@linkplain MemorySafeWindowTinyLFUMap}.
     * {@linkplain MemorySafeWindowTinyLFUMap} memory decide by yourself, you can config in shenyu-bootstrap application.yml.
     *
     * @param path uri path
     * @param ruleData rule data
     * @param maxMemory maxMemory
     */
    public void cacheRuleData(final String path, final RuleData ruleData, final Integer maxMemory) {
        RULE_DATA_MAP.computeIfAbsent(ruleData.getPluginName(), map -> new MemorySafeWindowTinyLFUMap<>(maxMemory, 1 << 16)).put(path, ruleData);
    }

    /**
     * get rule data from RULE_DATA_MAP.
     *
     * @param pluginName plugin name
     * @param path path
     * @return {@linkplain RuleData}
     */
    public RuleData obtainRuleData(final String pluginName, final String path) {
        final Map<String, RuleData> lruMap = RULE_DATA_MAP.get(pluginName);
        return Optional.ofNullable(lruMap).orElse(Maps.newHashMap()).get(path);
    }
}
