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
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.cache.WindowTinyLFUMap;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.utils.MapUtils;

import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentMap;


/**
 * The match data cache.
 */
public final class MatchDataCache {

    private static final MatchDataCache INSTANCE = new MatchDataCache();

    private static final MatchDataCache PUBLISHED = new MatchDataCache();

    private static final ReadOnly READ_ONLY = new ReadOnly();

    /**
     * pluginName -> LRUMap.
     */
    private final ConcurrentMap<String, Map<String, SelectorData>> selectorDataMap = Maps.newConcurrentMap();
    
    /**
     * plugin name -> LRU Map.
     * LRU Map: path -> rule data.
     */
    private final ConcurrentMap<String, Map<String, RuleData>> ruleDataMap = Maps.newConcurrentMap();

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
     * Access isolated derived matches for the frozen configuration contract.
     * @return shared access without exposing the backing maps
     */
    public ReadOnly readOnly() {
        return READ_ONLY;
    }

    /**
     * Remove selector data.
     *
     * @param pluginName the pluginName
     */
    public void removeSelectorData(final String pluginName) {
        if (this == INSTANCE) {
            PUBLISHED.removeSelectorData(pluginName);
        }
        selectorDataMap.remove(pluginName);
    }
    
    /**
     * remove selector data.
     *
     * @param pluginName plugin name
     * @param selectorId selector id
     */
    public void removeSelectorData(final String pluginName, final String selectorId) {
        if (this == INSTANCE) {
            PUBLISHED.removeSelectorData(pluginName, selectorId);
        }
        if (StringUtils.isBlank(pluginName)) {
            // a dangling selector carries no plugin name, so its entries may live under any plugin bucket
            selectorDataMap.values().forEach(pathSelectorCache ->
                    pathSelectorCache.entrySet().removeIf(entry -> selectorId.equals(entry.getValue().getId())));
            return;
        }
        Map<String, SelectorData> pathSelectorCache = selectorDataMap.get(pluginName);
        if (Objects.isNull(pathSelectorCache) || pathSelectorCache.isEmpty()) {
            return;
        }
        pathSelectorCache.entrySet().removeIf(entry -> selectorId.equals(entry.getValue().getId()));
    }

    /**
     * remove empty selector data.
     *
     * @param pluginName plugin name
     */
    public void removeEmptySelectorData(final String pluginName) {
        if (this == INSTANCE) {
            PUBLISHED.removeEmptySelectorData(pluginName);
        }
        if (StringUtils.isBlank(pluginName)) {
            return;
        }
        Map<String, SelectorData> pathSelectorCache = selectorDataMap.get(pluginName);
        if (Objects.isNull(pathSelectorCache) || pathSelectorCache.isEmpty()) {
            return;
        }
        pathSelectorCache.entrySet().removeIf(entry -> Objects.isNull(entry.getValue().getId()));
    }

    /**
     * Clean selector data.
     */
    public void cleanSelectorData() {
        if (this == INSTANCE) {
            PUBLISHED.cleanSelectorData();
        }
        selectorDataMap.clear();
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
        cacheSelectorData(path, selectorData, initialCapacity, maximumSize, new ShenyuConfig.MatchCacheConfig().getTtl());
    }

    /**
     * Cache selector data with a bounded lifetime.
     *
     * @param path the path
     * @param selectorData the selector data
     * @param initialCapacity initial capacity
     * @param maximumSize maximum size
     * @param ttl lifetime in milliseconds
     */
    public void cacheSelectorData(final String path, final SelectorData selectorData, final int initialCapacity, final long maximumSize, final long ttl) {
        MapUtils.computeIfAbsent(selectorDataMap, selectorData.getPluginName(), map ->
                new WindowTinyLFUMap<>(initialCapacity, ttl, maximumSize, Boolean.FALSE)).put(path, selectorData);
    }

    /**
     * Obtain selector data.
     *
     * @param pluginName the pluginName
     * @param path       the path
     * @return the selector data
     */
    public SelectorData obtainSelectorData(final String pluginName, final String path) {
        final Map<String, SelectorData> lruMap = selectorDataMap.get(pluginName);
        return Objects.isNull(lruMap) ? null : lruMap.get(path);
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
        cacheRuleData(path, ruleData, initialCapacity, maximumSize, new ShenyuConfig.MatchCacheConfig().getTtl());
    }

    /**
     * Cache rule data with a bounded lifetime.
     *
     * @param path the path
     * @param ruleData the rule data
     * @param initialCapacity initial capacity
     * @param maximumSize maximum size
     * @param ttl lifetime in milliseconds
     */
    public void cacheRuleData(final String path, final RuleData ruleData, final int initialCapacity, final long maximumSize, final long ttl) {
        MapUtils.computeIfAbsent(ruleDataMap, ruleData.getPluginName(), map ->
                new WindowTinyLFUMap<>(initialCapacity, ttl, maximumSize, Boolean.FALSE)).put(path, ruleData);
    }
    
    /**
     * remove rule data.
     *
     * @param pluginName pluginName
     */
    public void removeRuleData(final String pluginName) {
        if (this == INSTANCE) {
            PUBLISHED.removeRuleData(pluginName);
        }
        ruleDataMap.remove(pluginName);
    }
    
    /**
     * remove rule data.
     *
     * @param pluginName pluginName
     * @param ruleId ruleId
     */
    public void removeRuleData(final String pluginName, final String ruleId) {
        if (this == INSTANCE) {
            PUBLISHED.removeRuleData(pluginName, ruleId);
        }
        if (StringUtils.isBlank(pluginName)) {
            // a rule of a deleted plugin carries no plugin name, so its entries may live under any plugin bucket
            ruleDataMap.values().forEach(pathRuleDataCache ->
                    pathRuleDataCache.entrySet().removeIf(entry -> ruleId.equals(entry.getValue().getId())));
            return;
        }
        Map<String, RuleData> pathRuleDataCache = ruleDataMap.get(pluginName);
        if (Objects.isNull(pathRuleDataCache) || pathRuleDataCache.isEmpty()) {
            return;
        }
        pathRuleDataCache.entrySet().removeIf(entry -> ruleId.equals(entry.getValue().getId()));
    }

    /**
     * remove rule data by selector.
     *
     * @param pluginName pluginName
     * @param selectorId selectorId
     */
    public void removeRuleDataBySelector(final String pluginName, final String selectorId) {
        if (this == INSTANCE) {
            PUBLISHED.removeRuleDataBySelector(pluginName, selectorId);
        }
        if (StringUtils.isBlank(pluginName)) {
            // a dangling selector carries no plugin name, so its rule entries may live under any plugin bucket
            ruleDataMap.values().forEach(pathRuleDataCache ->
                    pathRuleDataCache.entrySet().removeIf(entry -> selectorId.equals(entry.getValue().getSelectorId())));
            return;
        }
        Map<String, RuleData> pathRuleDataCache = ruleDataMap.get(pluginName);
        if (Objects.isNull(pathRuleDataCache) || pathRuleDataCache.isEmpty()) {
            return;
        }
        pathRuleDataCache.entrySet().removeIf(entry -> selectorId.equals(entry.getValue().getSelectorId()));
    }

    /**
     * remove empty rule data.
     *
     * @param pluginName plugin name
     */
    public void removeEmptyRuleData(final String pluginName) {
        if (this == INSTANCE) {
            PUBLISHED.removeEmptyRuleData(pluginName);
        }
        if (StringUtils.isBlank(pluginName)) {
            return;
        }
        Map<String, RuleData> pathRuleDataCache = ruleDataMap.get(pluginName);
        if (Objects.isNull(pathRuleDataCache) || pathRuleDataCache.isEmpty()) {
            return;
        }
        pathRuleDataCache.entrySet().removeIf(entry -> Objects.isNull(entry.getValue().getId()));
    }
    
    /**
     * clear the cache.
     */
    public void cleanRuleDataData() {
        if (this == INSTANCE) {
            PUBLISHED.cleanRuleDataData();
        }
        ruleDataMap.clear();
    }
    
    /**
     * get rule data.
     *
     * @param pluginName pluginName
     * @param path path
     * @return ruleData
     */
    public RuleData obtainRuleData(final String pluginName, final String path) {
        final Map<String, RuleData> lruMap = ruleDataMap.get(pluginName);
        return Objects.isNull(lruMap) ? null : lruMap.get(path);
    }
    
    /**
     * get selector match cache.
     *
     * @return selector match cache
     */
    public ConcurrentMap<String, Map<String, SelectorData>> getSelectorMatchCache() {
        return selectorDataMap;
    }
    
    /**
     * get rule match cache.
     *
     * @return rule match cache
     */
    public ConcurrentMap<String, Map<String, RuleData>> getRuleMatchCache() {
        return ruleDataMap;
    }
    
    /**
     * Isolated match cache for already-frozen values. This API never freezes on the request path.
     */
    public static final class ReadOnly {

        private ReadOnly() {
        }

        /**
         * Get a frozen matched selector.
         * @param pluginName plugin name
         * @param path request path
         * @return frozen value or null
         */
        public SelectorData obtainSelectorData(final String pluginName, final String path) {
            return PUBLISHED.obtainSelectorData(pluginName, path);
        }

        /**
         * Get a frozen matched rule.
         * @param pluginName plugin name
         * @param path request path
         * @return frozen value or null
         */
        public RuleData obtainRuleData(final String pluginName, final String path) {
            return PUBLISHED.obtainRuleData(pluginName, path);
        }

        /**
         * Cache a previously published selector without copying it.
         * @param path request path
         * @param data frozen selector
         * @param initialCapacity initial capacity
         * @param maximumSize maximum size
         * @param ttl lifetime in milliseconds
         */
        public void cacheSelectorData(final String path, final SelectorData data, final int initialCapacity, final long maximumSize, final long ttl) {
            if (!data.frozen()) {
                throw new IllegalArgumentException("Read-only matching requires a frozen selector");
            }
            PUBLISHED.cacheSelectorData(path, data, initialCapacity, maximumSize, ttl);
        }

        /**
         * Cache a previously published rule without copying it.
         * @param path request path
         * @param data frozen rule
         * @param initialCapacity initial capacity
         * @param maximumSize maximum size
         * @param ttl lifetime in milliseconds
         */
        public void cacheRuleData(final String path, final RuleData data, final int initialCapacity, final long maximumSize, final long ttl) {
            if (!data.frozen()) {
                throw new IllegalArgumentException("Read-only matching requires a frozen rule");
            }
            PUBLISHED.cacheRuleData(path, data, initialCapacity, maximumSize, ttl);
        }
    }
}
