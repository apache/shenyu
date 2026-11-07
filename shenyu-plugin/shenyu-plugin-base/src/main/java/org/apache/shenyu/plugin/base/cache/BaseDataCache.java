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
import org.apache.shenyu.common.dto.DtoSnapshots;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Collections;
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

    private static final BaseDataCache INSTANCE = new BaseDataCache(false);

    private static final BaseDataCache PUBLISHED = new BaseDataCache(true);

    private static final ReadOnly READ_ONLY = new ReadOnly();

    private final boolean published;

    /**
     * pluginName -> PluginData.
     */
    private volatile ConcurrentMap<String, PluginData> pluginMap = Maps.newConcurrentMap();

    /**
     * pluginName -> SelectorData.
     */
    private volatile ConcurrentMap<String, List<SelectorData>> selectorMap = Maps.newConcurrentMap();

    /**
     * selectorId -> RuleData.
     */
    private volatile ConcurrentMap<String, List<RuleData>> ruleMap = Maps.newConcurrentMap();

    private volatile Map<String, PluginData> pluginView = Collections.unmodifiableMap(pluginMap);

    private volatile Map<String, List<SelectorData>> selectorView = Collections.unmodifiableMap(selectorMap);

    private volatile Map<String, List<RuleData>> ruleView = Collections.unmodifiableMap(ruleMap);

    private BaseDataCache(final boolean published) {
        this.published = published;
    }

    /**
     * Explicit publication-only read contract. Legacy map writes do not reach this view.
     * @return shared read-only access, without copying on reads
     */
    public ReadOnly readOnly() {
        return READ_ONLY;
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
        if (!published) {
            PUBLISHED.cachePluginData(pluginData);
        }
        Optional.ofNullable(pluginData).ifPresent(data -> pluginMap.put(data.getName(), published ? DtoSnapshots.freeze(data) : data));
    }
    
    /**
     * Remove plugin data.
     *
     * @param pluginData the plugin data
     */
    public synchronized void removePluginData(final PluginData pluginData) {
        Optional.ofNullable(pluginData).ifPresent(data -> pluginMap.remove(data.getName()));
        if (!published) {
            PUBLISHED.removePluginData(pluginData);
        }
    }
    
    /**
     * Remove plugin data by plugin name.
     *
     * @param pluginName the plugin name
     */
    public synchronized void removePluginDataByPluginName(final String pluginName) {
        pluginMap.remove(pluginName);
        if (!published) {
            PUBLISHED.removePluginDataByPluginName(pluginName);
        }
    }
    
    /**
     * Clean plugin data.
     */
    public synchronized void cleanPluginData() {
        pluginMap.clear();
        if (!published) {
            PUBLISHED.cleanPluginData();
        }
    }
    
    /**
     * Clean plugin data self.
     *
     * @param pluginDataList the plugin data list
     */
    public synchronized void cleanPluginDataSelf(final List<PluginData> pluginDataList) {
        pluginDataList.forEach(this::removePluginData);
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
        if (!published) {
            PUBLISHED.cacheSelectData(selectorData);
        }
        Optional.ofNullable(selectorData).ifPresent(data -> selectorAccept(published ? DtoSnapshots.freeze(data) : data));
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
        if (!published) {
            PUBLISHED.removeSelectData(selectorData);
        }
    }

    private void removeSelectData(final String pluginName, final String selectorId) {
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
        selectorMap.remove(pluginName);
        if (!published) {
            PUBLISHED.removeSelectDataByPluginName(pluginName);
        }
    }
    
    /**
     * Clean selector data.
     */
    public synchronized void cleanSelectorData() {
        selectorMap.clear();
        if (!published) {
            PUBLISHED.cleanSelectorData();
        }
    }
    
    /**
     * Clean selector data self.
     *
     * @param selectorDataList the selector data list
     */
    public synchronized void cleanSelectorDataSelf(final List<SelectorData> selectorDataList) {
        selectorDataList.forEach(this::removeSelectData);
    }

    /**
     * Remove a batch of selectors with a single merge per affected plugin bucket.
     * Selectors without a plugin name are matched against every bucket, mirroring
     * {@link #removeSelectData(SelectorData)}.
     *
     * @param selectorDataList the selectors to remove
     */
    synchronized void removeSelectDataBatch(final List<SelectorData> selectorDataList) {
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
        if (!published) {
            PUBLISHED.removeSelectDataBatch(selectorDataList);
        }
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
        if (!published) {
            PUBLISHED.cacheRuleData(ruleData);
        }
        Optional.ofNullable(ruleData).ifPresent(data -> ruleAccept(published ? DtoSnapshots.freeze(data) : data));
    }
    
    /**
     * Remove rule data.
     *
     * @param ruleData the rule data
     */
    public synchronized void removeRuleData(final RuleData ruleData) {
        Optional.ofNullable(ruleData).ifPresent(data -> {
            ruleMap.computeIfPresent(data.getSelectorId(), (key, value) -> {
                final List<RuleData> result = value.stream()
                        .filter(rule -> !Objects.equals(rule.getId(), data.getId()))
                        .collect(Collectors.toList());
                return result.isEmpty() ? null : List.copyOf(result);
            });
        });
        if (!published) {
            PUBLISHED.removeRuleData(ruleData);
        }
    }
    
    /**
     * Remove rule data by selector id.
     *
     * @param selectorId the selector id
     */
    public synchronized void removeRuleDataBySelectorId(final String selectorId) {
        ruleMap.remove(selectorId);
        if (!published) {
            PUBLISHED.removeRuleDataBySelectorId(selectorId);
        }
    }
    
    /**
     * Clean rule data.
     */
    public synchronized void cleanRuleData() {
        ruleMap.clear();
        if (!published) {
            PUBLISHED.cleanRuleData();
        }
    }
    
    /**
     * Clean rule data self.
     *
     * @param ruleDataList the rule data list
     */
    public synchronized void cleanRuleDataSelf(final List<RuleData> ruleDataList) {
        ruleDataList.forEach(this::removeRuleData);
    }

    /**
     * Remove a batch of rules with a single merge per affected selector bucket.
     *
     * @param ruleDataList the rules to remove
     */
    synchronized void removeRuleDataBatch(final List<RuleData> ruleDataList) {
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
        if (!published) {
            PUBLISHED.removeRuleDataBatch(ruleDataList);
        }
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
    }

    /**
     * cache selector data.
     *
     * @param data the selector data
     */
    private void selectorAccept(final SelectorData data) {
        selectorMap.compute(data.getPluginName(), (key, value) ->
                upsertSorted(value, List.of(data), SelectorData::getId, SelectorData::getSort));
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
        dataList.forEach(data -> next.put(data.getName(), published ? DtoSnapshots.freeze(data) : data));
        if (!published) {
            PUBLISHED.refreshPluginData(dataList);
        }
        pluginMap = next;
        pluginView = Collections.unmodifiableMap(next);
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
                .map(data -> published ? DtoSnapshots.freeze(data) : data)
                .collect(Collectors.groupingBy(SelectorData::getPluginName));
        grouped.forEach((pluginName, batch) -> next.compute(pluginName, (key, value) ->
                upsertSorted(value, batch, SelectorData::getId, SelectorData::getSort)));
        if (!published) {
            PUBLISHED.refreshSelectorData(dataList);
        }
        selectorMap = next;
        selectorView = Collections.unmodifiableMap(next);
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
                .map(data -> published ? DtoSnapshots.freeze(data) : data)
                .collect(Collectors.groupingBy(RuleData::getSelectorId));
        grouped.forEach((selectorId, batch) -> next.compute(selectorId, (key, value) ->
                upsertSorted(value, batch, RuleData::getId, RuleData::getSort)));
        if (!published) {
            PUBLISHED.refreshRuleData(dataList);
        }
        ruleMap = next;
        ruleView = Collections.unmodifiableMap(next);
    }

    /**
     * Frozen configuration access. Updates must use the owning cache's publication methods.
     * Individual buckets are immutable snapshots; this is not a multi-object transaction.
     */
    public static final class ReadOnly {

        private ReadOnly() {
        }

        /**
         * Get published plugin data.
         * @param pluginName plugin name
         * @return frozen data or null
         */
        public PluginData obtainPluginData(final String pluginName) {
            return PUBLISHED.obtainPluginData(pluginName);
        }

        /**
         * Get a published selector bucket.
         * @param pluginName plugin name
         * @return frozen list and elements, or null
         */
        public List<SelectorData> obtainSelectorData(final String pluginName) {
            return PUBLISHED.obtainSelectorData(pluginName);
        }

        /**
         * Get a published rule bucket.
         * @param selectorId selector id
         * @return frozen list and elements, or null
         */
        public List<RuleData> obtainRuleData(final String selectorId) {
            return PUBLISHED.obtainRuleData(selectorId);
        }

        /**
         * Read published plugins without exposing a writable backing map.
         * @return unmodifiable live view of the current map generation
         */
        public Map<String, PluginData> getPluginMap() {
            return PUBLISHED.pluginView;
        }

        /**
         * Read published selector buckets.
         * @return unmodifiable live view of the current map generation
         */
        public Map<String, List<SelectorData>> getSelectorMap() {
            return PUBLISHED.selectorView;
        }

        /**
         * Read published rule buckets.
         * @return unmodifiable live view of the current map generation
         */
        public Map<String, List<RuleData>> getRuleMap() {
            return PUBLISHED.ruleView;
        }
    }
}
