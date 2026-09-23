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
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ConcurrentMap;
import java.util.stream.Collectors;

/**
 * The type Base data cache.
 */
public final class BaseDataCache {

    private static final BaseDataCache INSTANCE = new BaseDataCache();

    /**
     * pluginName -> PluginData.
     */
    private static final ConcurrentMap<String, PluginData> PLUGIN_MAP = Maps.newConcurrentMap();

    /**
     * pluginName -> SelectorData.
     */
    private static final ConcurrentMap<String, List<SelectorData>> SELECTOR_MAP = Maps.newConcurrentMap();

    /**
     * selectorId -> RuleData.
     */
    private static final ConcurrentMap<String, List<RuleData>> RULE_MAP = Maps.newConcurrentMap();

    private BaseDataCache() {
    }
    
    /**
     * Gets instance.
     *
     * @return the instance
     */
    public static BaseDataCache getInstance() {
        return INSTANCE;
    }
    
    /**
     * Cache plugin data.
     *
     * @param pluginData the plugin data
     */
    public void cachePluginData(final PluginData pluginData) {
        Optional.ofNullable(pluginData).ifPresent(data -> PLUGIN_MAP.put(data.getName(), data));
    }
    
    /**
     * Remove plugin data.
     *
     * @param pluginData the plugin data
     */
    public void removePluginData(final PluginData pluginData) {
        Optional.ofNullable(pluginData).ifPresent(data -> PLUGIN_MAP.remove(data.getName()));
    }
    
    /**
     * Remove plugin data by plugin name.
     *
     * @param pluginName the plugin name
     */
    public void removePluginDataByPluginName(final String pluginName) {
        PLUGIN_MAP.remove(pluginName);
    }
    
    /**
     * Clean plugin data.
     */
    public void cleanPluginData() {
        PLUGIN_MAP.clear();
    }
    
    /**
     * Clean plugin data self.
     *
     * @param pluginDataList the plugin data list
     */
    public void cleanPluginDataSelf(final List<PluginData> pluginDataList) {
        pluginDataList.forEach(this::removePluginData);
    }
    
    /**
     * Obtain plugin data plugin data.
     *
     * @param pluginName the plugin name
     * @return the plugin data
     */
    public PluginData obtainPluginData(final String pluginName) {
        return PLUGIN_MAP.get(pluginName);
    }
    
    /**
     * Cache select data.
     *
     * @param selectorData the selector data
     */
    public void cacheSelectData(final SelectorData selectorData) {
        Optional.ofNullable(selectorData).ifPresent(this::selectorAccept);
    }
    
    /**
     * Remove select data.
     *
     * @param selectorData the selector data
     */
    public void removeSelectData(final SelectorData selectorData) {
        Optional.ofNullable(selectorData).ifPresent(data -> {
            SELECTOR_MAP.computeIfPresent(data.getPluginName(), (key, value) -> {
                final List<SelectorData> result = value.stream()
                        .filter(selector -> !Objects.equals(selector.getId(), data.getId()))
                        .collect(Collectors.toList());
                return result.isEmpty() ? null : List.copyOf(result);
            });
        });
    }
    
    /**
     * Remove select data by plugin name.
     *
     * @param pluginName the plugin name
     */
    public void removeSelectDataByPluginName(final String pluginName) {
        SELECTOR_MAP.remove(pluginName);
    }
    
    /**
     * Clean selector data.
     */
    public void cleanSelectorData() {
        SELECTOR_MAP.clear();
    }
    
    /**
     * Clean selector data self.
     *
     * @param selectorDataList the selector data list
     */
    public void cleanSelectorDataSelf(final List<SelectorData> selectorDataList) {
        selectorDataList.forEach(this::removeSelectData);
    }
    
    /**
     * Obtain selector data list list.
     *
     * @param pluginName the plugin name
     * @return the immutable snapshot, or {@code null} if no selector data exists
     */
    public List<SelectorData> obtainSelectorData(final String pluginName) {
        return SELECTOR_MAP.get(pluginName);
    }
    
    /**
     * Cache rule data.
     *
     * @param ruleData the rule data
     */
    public void cacheRuleData(final RuleData ruleData) {
        Optional.ofNullable(ruleData).ifPresent(this::ruleAccept);
    }
    
    /**
     * Remove rule data.
     *
     * @param ruleData the rule data
     */
    public void removeRuleData(final RuleData ruleData) {
        Optional.ofNullable(ruleData).ifPresent(data -> {
            RULE_MAP.computeIfPresent(data.getSelectorId(), (key, value) -> {
                final List<RuleData> result = value.stream()
                        .filter(rule -> !Objects.equals(rule.getId(), data.getId()))
                        .collect(Collectors.toList());
                return result.isEmpty() ? null : List.copyOf(result);
            });
        });
    }
    
    /**
     * Remove rule data by selector id.
     *
     * @param selectorId the selector id
     */
    public void removeRuleDataBySelectorId(final String selectorId) {
        RULE_MAP.remove(selectorId);
    }
    
    /**
     * Clean rule data.
     */
    public void cleanRuleData() {
        RULE_MAP.clear();
    }
    
    /**
     * Clean rule data self.
     *
     * @param ruleDataList the rule data list
     */
    public void cleanRuleDataSelf(final List<RuleData> ruleDataList) {
        ruleDataList.forEach(this::removeRuleData);
    }
    
    /**
     * Obtain rule data list list.
     *
     * @param selectorId the selector id
     * @return the immutable snapshot, or {@code null} if no rule data exists
     */
    public List<RuleData> obtainRuleData(final String selectorId) {
        return RULE_MAP.get(selectorId);
    }
    
    /**
     * Gets plugin map.
     *
     * @return the plugin map
     */
    public ConcurrentMap<String, PluginData> getPluginMap() {
        return PLUGIN_MAP;
    }
    
    /**
     * Gets selector map.
     *
     * @return the selector map
     */
    public ConcurrentMap<String, List<SelectorData>> getSelectorMap() {
        return SELECTOR_MAP;
    }
    
    /**
     * Gets rule map.
     *
     * @return the rule map
     */
    public ConcurrentMap<String, List<RuleData>> getRuleMap() {
        return RULE_MAP;
    }
    

    /**
     *  cache rule data.
     *
     * @param data the rule data
     */
    private void ruleAccept(final RuleData data) {
        String selectorId = data.getSelectorId();
        RULE_MAP.compute(selectorId, (key, value) -> {
            final List<RuleData> result = Objects.isNull(value) ? new ArrayList<>() : new ArrayList<>(value);
            result.removeIf(rule -> Objects.equals(rule.getId(), data.getId()));
            result.add(data);
            result.sort(Comparator.comparing(RuleData::getSort));
            return List.copyOf(result);
        });
    }

    /**
     * cache selector data.
     *
     * @param data the selector data
     */
    private void selectorAccept(final SelectorData data) {
        String key = data.getPluginName();
        SELECTOR_MAP.compute(key, (pluginName, value) -> {
            final List<SelectorData> result = Objects.isNull(value) ? new ArrayList<>() : new ArrayList<>(value);
            result.removeIf(selector -> Objects.equals(selector.getId(), data.getId()));
            result.add(data);
            result.sort(Comparator.comparing(SelectorData::getSort));
            return List.copyOf(result);
        });
    }
}
