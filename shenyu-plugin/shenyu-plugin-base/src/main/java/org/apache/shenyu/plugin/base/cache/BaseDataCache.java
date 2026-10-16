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
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentMap;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * The type Base data cache.
 */
public final class BaseDataCache {

    private static final BaseDataCache INSTANCE = new BaseDataCache();

    /**
     * pluginName -> PluginData.
     */
    private static volatile ConcurrentMap<String, PluginData> pluginMap = Maps.newConcurrentMap();

    /**
     * pluginName -> SelectorData.
     */
    private static volatile ConcurrentMap<String, List<SelectorData>> selectorMap = Maps.newConcurrentMap();

    /**
     * selectorId -> RuleData.
     */
    private static volatile ConcurrentMap<String, List<RuleData>> ruleMap = Maps.newConcurrentMap();

    private volatile long version;

    private BaseDataCache() {
    }

    /**
     * Get the configuration generation used to isolate derived match results.
     *
     * @return the current generation
     */
    public long getVersion() {
        return version;
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
    public synchronized void cachePluginData(final PluginData pluginData) {
        Optional.ofNullable(pluginData).ifPresent(data -> pluginMap.put(data.getName(), data));
        version++;
    }
    
    /**
     * Remove plugin data.
     *
     * @param pluginData the plugin data
     */
    public synchronized void removePluginData(final PluginData pluginData) {
        Optional.ofNullable(pluginData).ifPresent(data -> removePluginDataByPluginName(data.getName()));
        version++;
    }
    
    /**
     * Remove plugin data by plugin name.
     *
     * @param pluginName the plugin name
     */
    public synchronized void removePluginDataByPluginName(final String pluginName) {
        pluginMap.remove(pluginName);
        removeSelectDataByPluginName(pluginName);
        ruleMap.entrySet().removeIf(entry -> entry.getValue().stream().anyMatch(rule -> pluginName.equals(rule.getPluginName())));
        version++;
    }
    
    /**
     * Clean plugin data.
     */
    public synchronized void cleanPluginData() {
        pluginMap.clear();
        cleanSelectorData();
        version++;
    }
    
    /**
     * Clean plugin data self.
     *
     * @param pluginDataList the plugin data list
     */
    public synchronized void cleanPluginDataSelf(final List<PluginData> pluginDataList) {
        pluginDataList.forEach(this::removePluginData);
        version++;
    }
    
    /**
     * Obtain plugin data plugin data.
     *
     * @param pluginName the plugin name
     * @return the plugin data
     */
    public PluginData obtainPluginData(final String pluginName) {
        return pluginMap.get(pluginName);
    }
    
    /**
     * Cache select data.
     *
     * @param selectorData the selector data
     */
    public synchronized void cacheSelectData(final SelectorData selectorData) {
        Optional.ofNullable(selectorData).ifPresent(this::selectorAccept);
        version++;
    }
    
    /**
     * Remove select data.
     *
     * @param selectorData the selector data
     */
    public synchronized void removeSelectData(final SelectorData selectorData) {
        Optional.ofNullable(selectorData).ifPresent(data -> {
            if (StringUtils.isBlank(data.getPluginName())) {
                // a dangling selector carries no plugin name, so its entry may live under any plugin bucket
                selectorMap.keySet().forEach(pluginName -> removeSelectData(pluginName, data.getId()));
            } else {
                removeSelectData(data.getPluginName(), data.getId());
            }
        });
        version++;
    }

    private void removeSelectData(final String pluginName, final String selectorId) {
        ruleMap.remove(selectorId);
        selectorMap.computeIfPresent(pluginName, (key, value) -> {
            final List<SelectorData> result = value.stream()
                    .filter(selector -> !Objects.equals(selector.getId(), selectorId))
                    .collect(Collectors.toList());
            return result.isEmpty() ? null : List.copyOf(result);
        });
    }
    
    /**
     * Remove select data by plugin name.
     *
     * @param pluginName the plugin name
     */
    public synchronized void removeSelectDataByPluginName(final String pluginName) {
        List<SelectorData> removed = selectorMap.remove(pluginName);
        if (Objects.nonNull(removed)) {
            removed.forEach(selector -> ruleMap.remove(selector.getId()));
        }
        version++;
    }
    
    /**
     * Clean selector data.
     */
    public synchronized void cleanSelectorData() {
        selectorMap.clear();
        ruleMap.clear();
        version++;
    }
    
    /**
     * Clean selector data self.
     *
     * @param selectorDataList the selector data list
     */
    public synchronized void cleanSelectorDataSelf(final List<SelectorData> selectorDataList) {
        selectorDataList.forEach(this::removeSelectData);
        version++;
    }

    /**
     * Remove a batch of selectors with a single merge per affected plugin bucket.
     * Selectors without a plugin name are matched against every bucket, mirroring
     * {@link #removeSelectData(SelectorData)}.
     *
     * @param selectorDataList the selectors to remove
     */
    void removeSelectDataBatch(final List<SelectorData> selectorDataList) {
        if (selectorDataList.isEmpty()) {
            return;
        }
        final Map<String, Set<String>> idsByPlugin = new HashMap<>();
        final Set<String> danglingIds = new HashSet<>();
        for (SelectorData data : selectorDataList) {
            if (Objects.isNull(data) || Objects.isNull(data.getId())) {
                continue;
            }
            if (StringUtils.isBlank(data.getPluginName())) {
                danglingIds.add(data.getId());
            } else {
                idsByPlugin.computeIfAbsent(data.getPluginName(), key -> new HashSet<>()).add(data.getId());
            }
        }
        if (!danglingIds.isEmpty()) {
            selectorMap.keySet().forEach(pluginName -> removeSelectDataByIds(pluginName, danglingIds));
        }
        idsByPlugin.forEach(this::removeSelectDataByIds);
    }

    private void removeSelectDataByIds(final String pluginName, final Set<String> selectorIds) {
        selectorMap.computeIfPresent(pluginName, (key, value) -> {
            final List<SelectorData> result = value.stream()
                    .filter(selector -> !selectorIds.contains(selector.getId()))
                    .collect(Collectors.toList());
            return result.isEmpty() ? null : List.copyOf(result);
        });
    }
    
    /**
     * Obtain selector data list list.
     *
     * @param pluginName the plugin name
     * @return the immutable snapshot, or {@code null} if no selector data exists
     */
    public List<SelectorData> obtainSelectorData(final String pluginName) {
        return selectorMap.get(pluginName);
    }
    
    /**
     * Cache rule data.
     *
     * @param ruleData the rule data
     */
    public synchronized void cacheRuleData(final RuleData ruleData) {
        Optional.ofNullable(ruleData).ifPresent(this::ruleAccept);
        version++;
    }
    
    /**
     * Remove rule data.
     *
     * @param ruleData the rule data
     */
    public synchronized void removeRuleData(final RuleData ruleData) {
        Optional.ofNullable(ruleData).ifPresent(data -> {
            if (StringUtils.isBlank(data.getSelectorId())) {
                ruleMap.keySet().forEach(selectorId -> removeRuleData(selectorId, data.getId()));
            } else {
                removeRuleData(data.getSelectorId(), data.getId());
            }
        });
        version++;
    }
    
    private void removeRuleData(final String selectorId, final String ruleId) {
        ruleMap.computeIfPresent(selectorId, (key, value) -> {
            List<RuleData> retained = value.stream().filter(rule -> !Objects.equals(rule.getId(), ruleId)).collect(Collectors.toList());
            return retained.isEmpty() ? null : List.copyOf(retained);
        });
    }

    /**
     * Remove rule data by selector id.
     *
     * @param selectorId the selector id
     */
    public synchronized void removeRuleDataBySelectorId(final String selectorId) {
        ruleMap.remove(selectorId);
        version++;
    }
    
    /**
     * Clean rule data.
     */
    public synchronized void cleanRuleData() {
        ruleMap.clear();
        version++;
    }
    
    /**
     * Clean rule data self.
     *
     * @param ruleDataList the rule data list
     */
    public synchronized void cleanRuleDataSelf(final List<RuleData> ruleDataList) {
        ruleDataList.forEach(this::removeRuleData);
        version++;
    }

    /**
     * Remove a batch of rules with a single merge per affected selector bucket.
     *
     * @param ruleDataList the rules to remove
     */
    void removeRuleDataBatch(final List<RuleData> ruleDataList) {
        if (ruleDataList.isEmpty()) {
            return;
        }
        final Map<String, Set<String>> idsBySelector = new HashMap<>();
        for (RuleData data : ruleDataList) {
            if (Objects.isNull(data) || Objects.isNull(data.getId()) || Objects.isNull(data.getSelectorId())) {
                continue;
            }
            idsBySelector.computeIfAbsent(data.getSelectorId(), key -> new HashSet<>()).add(data.getId());
        }
        idsBySelector.forEach((selectorId, ruleIds) -> ruleMap.computeIfPresent(selectorId, (key, value) -> {
            final List<RuleData> result = value.stream()
                    .filter(rule -> !ruleIds.contains(rule.getId()))
                    .collect(Collectors.toList());
            return result.isEmpty() ? null : List.copyOf(result);
        }));
    }
    
    /**
     * Obtain rule data list list.
     *
     * @param selectorId the selector id
     * @return the immutable snapshot, or {@code null} if no rule data exists
     */
    public List<RuleData> obtainRuleData(final String selectorId) {
        return ruleMap.get(selectorId);
    }
    
    /**
     * Gets plugin map.
     *
     * @return the plugin map
     */
    public ConcurrentMap<String, PluginData> getPluginMap() {
        return pluginMap;
    }
    
    /**
     * Gets selector map.
     *
     * @return the selector map
     */
    public ConcurrentMap<String, List<SelectorData>> getSelectorMap() {
        return selectorMap;
    }
    
    /**
     * Gets rule map.
     *
     * @return the rule map
     */
    public ConcurrentMap<String, List<RuleData>> getRuleMap() {
        return ruleMap;
    }
    

    /**
     *  cache rule data.
     *
     * @param data the rule data
     */
    private void ruleAccept(final RuleData data) {
        ruleMap.compute(data.getSelectorId(), (key, value) ->
                upsertSorted(value, List.of(data), RuleData::getId, RuleData::getSort));
        removeReassigned(ruleMap, List.of(data), RuleData::getId, RuleData::getSelectorId);
    }

    /**
     * cache selector data.
     *
     * @param data the selector data
     */
    private void selectorAccept(final SelectorData data) {
        selectorMap.compute(data.getPluginName(), (key, value) ->
                upsertSorted(value, List.of(data), SelectorData::getId, SelectorData::getSort));
        removeReassigned(selectorMap, List.of(data), SelectorData::getId, SelectorData::getPluginName);
    }

    private <T> void removeReassigned(final Map<String, List<T>> cache, final List<T> incoming,
                                      final Function<T, String> idOf, final Function<T, String> groupOf) {
        Map<String, String> owners = incoming.stream().filter(Objects::nonNull)
                .collect(Collectors.toMap(idOf, groupOf, (first, last) -> last));
        cache.replaceAll((group, entries) -> {
            boolean moved = entries.stream().anyMatch(entry -> owners.containsKey(idOf.apply(entry)) && !group.equals(owners.get(idOf.apply(entry))));
            if (!moved) {
                return entries;
            }
            return entries.stream().filter(entry -> !owners.containsKey(idOf.apply(entry)) || group.equals(owners.get(idOf.apply(entry))))
                    .collect(Collectors.toUnmodifiableList());
        });
        cache.entrySet().removeIf(entry -> entry.getValue().isEmpty());
    }

    /**
     * Merge the batch into the current list as an upsert (same-id entries are replaced)
     * and return a new immutable snapshot sorted by the sort key. The list is sorted once
     * per merge, so a batch refresh of arbitrary size costs a single sort per key instead
     * of one sort per element.
     *
     * @param current the currently cached list, may be {@code null}
     * @param batch the incoming entries, must not contain {@code null} elements
     * @param idOf the id extractor used to replace entries
     * @param sortOf the sort key extractor
     * @param <T> the entry type
     * @return a new immutable list sorted by the sort key
     */
    private <T> List<T> upsertSorted(final List<T> current, final List<T> batch,
                                     final Function<T, String> idOf, final Function<T, Integer> sortOf) {
        final List<T> result = Objects.isNull(current) ? new ArrayList<>() : new ArrayList<>(current);
        final Set<String> replacedIds = batch.stream().map(idOf).collect(Collectors.toSet());
        result.removeIf(item -> replacedIds.contains(idOf.apply(item)));
        result.addAll(batch);
        result.sort(Comparator.comparing(sortOf));
        return List.copyOf(result);
    }

    /**
     * Merge a batch without exposing partially refreshed data to readers.
     * Missing entries are retained because refresh messages may cover only one plugin.
     *
     * @param dataList the received data
     */
    synchronized void refreshPluginData(final List<PluginData> dataList) {
        if (dataList.isEmpty()) {
            return;
        }
        ConcurrentMap<String, PluginData> next = Maps.newConcurrentMap();
        next.putAll(pluginMap);
        dataList.forEach(data -> next.put(data.getName(), data));
        pluginMap = next;
        version++;
    }

    /**
     * Merge a batch without exposing partially refreshed data to readers.
     * Missing entries are retained because refresh messages may cover only one plugin.
     * The batch is grouped per plugin, so each plugin's list is merged and sorted once.
     *
     * @param dataList the received data
     */
    synchronized void refreshSelectorData(final List<SelectorData> dataList) {
        if (dataList.isEmpty()) {
            return;
        }
        ConcurrentMap<String, List<SelectorData>> next = Maps.newConcurrentMap();
        next.putAll(selectorMap);
        Map<String, List<SelectorData>> grouped = dataList.stream()
                .filter(Objects::nonNull)
                .collect(Collectors.groupingBy(SelectorData::getPluginName));
        grouped.forEach((pluginName, batch) -> next.compute(pluginName, (key, value) ->
                upsertSorted(value, batch, SelectorData::getId, SelectorData::getSort)));
        removeReassigned(next, dataList, SelectorData::getId, SelectorData::getPluginName);
        selectorMap = next;
        version++;
    }

    /**
     * Merge a batch without exposing partially refreshed data to readers.
     * Missing entries are retained because refresh messages may cover only one selector.
     * The batch is grouped per selector, so each selector's list is merged and sorted once.
     *
     * @param dataList the received data
     */
    synchronized void refreshRuleData(final List<RuleData> dataList) {
        if (dataList.isEmpty()) {
            return;
        }
        ConcurrentMap<String, List<RuleData>> next = Maps.newConcurrentMap();
        next.putAll(ruleMap);
        Map<String, List<RuleData>> grouped = dataList.stream()
                .filter(Objects::nonNull)
                .collect(Collectors.groupingBy(RuleData::getSelectorId));
        grouped.forEach((selectorId, batch) -> next.compute(selectorId, (key, value) ->
                upsertSorted(value, batch, RuleData::getId, RuleData::getSort)));
        removeReassigned(next, dataList, RuleData::getId, RuleData::getSelectorId);
        ruleMap = next;
        version++;
    }
}
