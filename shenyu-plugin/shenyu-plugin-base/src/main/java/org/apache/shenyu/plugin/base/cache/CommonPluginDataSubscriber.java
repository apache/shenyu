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

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.config.ShenyuConfig.RuleMatchCache;
import org.apache.shenyu.common.config.ShenyuConfig.SelectorMatchCache;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.enums.PluginHandlerEventEnum;
import org.apache.shenyu.common.utils.InitialSyncApplication;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.lang.NonNull;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * The type Common plugin data subscriber.
 */
public class CommonPluginDataSubscriber implements PluginDataSubscriber {
    
    private static final Logger LOG = LoggerFactory.getLogger(CommonPluginDataSubscriber.class);
    
    private final Map<String, PluginDataHandler> handlerMap;

    private final Set<String> builtInHandlerNames;

    private ApplicationEventPublisher eventPublisher;
    
    private final SelectorMatchCache selectorMatchConfig;
    
    private final RuleMatchCache ruleMatchCacheConfig;
    
    /**
     * Instantiates a new Common plugin data subscriber.
     *
     * @param pluginDataHandlerList the plugin data handler list
     * @param selectorMatchConfig   shenyu selector cache config
     * @param ruleMatchCacheConfig  shenyu rule cache config
     */
    public CommonPluginDataSubscriber(final List<PluginDataHandler> pluginDataHandlerList,
                                      final SelectorMatchCache selectorMatchConfig,
                                      final RuleMatchCache ruleMatchCacheConfig) {
        this.handlerMap = pluginDataHandlerList.stream().collect(Collectors.toConcurrentMap(PluginDataHandler::pluginNamed, e -> e));
        this.builtInHandlerNames = Set.copyOf(handlerMap.keySet());
        this.selectorMatchConfig = selectorMatchConfig;
        this.ruleMatchCacheConfig = ruleMatchCacheConfig;
    }
    
    /**
     * Instantiates a new Common plugin data subscriber.
     *
     * @param pluginDataHandlerList the plugin data handler list
     * @param eventPublisher        eventPublisher is used to publish sort plugin event
     * @param selectorMatchConfig   shenyu trie config
     * @param ruleMatchCacheConfig  shenyu trie config
     */
    public CommonPluginDataSubscriber(final List<PluginDataHandler> pluginDataHandlerList,
                                      final ApplicationEventPublisher eventPublisher,
                                      final SelectorMatchCache selectorMatchConfig,
                                      final RuleMatchCache ruleMatchCacheConfig) {
        this.handlerMap = pluginDataHandlerList.stream().collect(Collectors.toConcurrentMap(PluginDataHandler::pluginNamed, e -> e));
        this.builtInHandlerNames = Set.copyOf(handlerMap.keySet());
        this.eventPublisher = eventPublisher;
        this.selectorMatchConfig = selectorMatchConfig;
        this.ruleMatchCacheConfig = ruleMatchCacheConfig;
    }
    
    /**
     * Add or replace extension handlers without overriding built-in handlers.
     *
     * @param handlers the handlers
     */
    public void putExtendPluginDataHandler(final List<PluginDataHandler> handlers) {
        if (CollectionUtils.isEmpty(handlers)) {
            return;
        }
        for (PluginDataHandler handler : handlers) {
            String pluginNamed = handler.pluginNamed();
            if (!builtInHandlerNames.contains(pluginNamed)) {
                handlerMap.put(pluginNamed, handler);
                LOG.info("shenyu auto add or replace extends plugin data handler name is :{}", pluginNamed);
            }
        }
    }

    /**
     * Remove handlers belonging to unloaded extension plugins.
     *
     * @param pluginNames extension plugin names
     */
    public void removeExtendPluginDataHandlers(final Set<String> pluginNames) {
        for (String pluginName : pluginNames) {
            if (!builtInHandlerNames.contains(pluginName)) {
                handlerMap.remove(pluginName);
            }
        }
    }
    
    /**
     * Remove extension handlers owned by a displaced loader without removing their replacements.
     *
     * @param classLoader owner class loader
     */
    public void removeExtendPluginDataHandlers(final ClassLoader classLoader) {
        handlerMap.entrySet().removeIf(entry -> !builtInHandlerNames.contains(entry.getKey())
                && entry.getValue().getClass().getClassLoader() == classLoader);
    }

    @Override
    public void onSubscribe(final PluginData pluginData) {
        LOG.info("subscribe plugin data for plugin: [id: {}, name: {}, config: {}]", pluginData.getId(), pluginData.getName(), pluginData.getConfig());
        subscribeDataHandler(pluginData, DataEventTypeEnum.UPDATE);
    }
    
    @Override
    public void unSubscribe(final PluginData pluginData) {
        LOG.info("unSubscribe plugin data for plugin: [id: {}, name: {}]", pluginData.getId(), pluginData.getName());
        subscribeDataHandler(pluginData, DataEventTypeEnum.DELETE);
    }
    
    @Override
    public void refreshPluginDataNamespace(final String namespaceId) {
        List<PluginData> stale = BaseDataCache.getInstance().getPluginMap().values().stream()
                .filter(data -> namespaceId.equals(data.getNamespaceId()))
                .collect(Collectors.toList());
        stale.forEach(this::unSubscribe);
    }

    @Override
    public void refreshSelectorDataNamespace(final String namespaceId) {
        List<SelectorData> stale = BaseDataCache.getInstance().getSelectorMap().values().stream()
                .flatMap(List::stream)
                .filter(data -> namespaceId.equals(data.getNamespaceId()))
                .collect(Collectors.toList());
        stale.forEach(this::unSelectorSubscribe);
    }

    @Override
    public void refreshRuleDataNamespace(final String namespaceId) {
        List<RuleData> stale = BaseDataCache.getInstance().getRuleMap().values().stream()
                .flatMap(List::stream)
                .filter(data -> namespaceId.equals(data.getNamespaceId()))
                .collect(Collectors.toList());
        stale.forEach(this::unRuleSubscribe);
    }

    @Override
    public void applyPluginDataSnapshot(final String namespaceId, final List<PluginData> dataList) {
        Set<String> pluginNames = dataList.stream().map(PluginData::getName).collect(Collectors.toSet());
        List<PluginData> stale = BaseDataCache.getInstance().getPluginMap().values().stream()
                .filter(data -> namespaceId.equals(data.getNamespaceId()))
                .filter(data -> !pluginNames.contains(data.getName()))
                .collect(Collectors.toList());
        dataList.forEach(this::onSubscribe);
        stale.forEach(this::unSubscribe);
    }

    @Override
    public void applySelectorDataSnapshot(final String namespaceId, final List<SelectorData> dataList) {
        final BaseDataCache cache = BaseDataCache.getInstance();
        // A row without a plugin name yields a null grouping key in refreshSelectorData, whose compute on a
        // concurrent map throws and would abort the whole snapshot (and, over a websocket, loop resyncs).
        // The previous per-row subscribe path isolated such a row; skip it up front so valid rows still converge.
        final List<SelectorData> incoming = new ArrayList<>(dataList.size());
        for (SelectorData data : dataList) {
            if (Objects.nonNull(data) && Objects.nonNull(data.getPluginName())) {
                incoming.add(data);
            } else {
                LOG.warn("skip malformed selector row without plugin name in snapshot for namespace [{}]", namespaceId);
            }
        }
        final Set<String> selectorIds = incoming.stream().map(SelectorData::getId).collect(Collectors.toSet());
        final List<SelectorData> stale = cache.getSelectorMap().values().stream()
                .flatMap(List::stream)
                .filter(data -> namespaceId.equals(data.getNamespaceId()))
                .filter(data -> !selectorIds.contains(data.getId()))
                .collect(Collectors.toList());
        // Merge and sort once per affected plugin bucket instead of per selector row.
        if (!incoming.isEmpty()) {
            cache.refreshSelectorData(incoming);
        }
        if (!stale.isEmpty()) {
            cache.removeSelectDataBatch(stale);
        }
        try {
            // Preserve per-item plugin handler callbacks and the initial-sync failure semantics
            // that the previous per-row subscribe path provided.
            incoming.forEach(data -> runGuarded(data, () -> {
                if (StringUtils.isNotBlank(data.getPluginName())) {
                    Optional.ofNullable(handlerMap.get(data.getPluginName()))
                            .ifPresent(handler -> handler.handlerSelector(data));
                }
            }));
            stale.forEach(data -> runGuarded(data, () -> {
                // the concurrent handler map rejects null keys, and a dangling selector carries no plugin name
                if (StringUtils.isNotBlank(data.getPluginName())) {
                    Optional.ofNullable(handlerMap.get(data.getPluginName()))
                            .ifPresent(handler -> handler.removeSelector(data));
                }
            }));
        } finally {
            // Invalidate even if a handler aborted during initial sync: the stale rows are already gone from
            // BaseDataCache above, so a retry cannot rediscover them and their match cache would stay stale.
            invalidateSelectorMatchCacheByPlugins(collectAffectedPlugins(incoming, stale, SelectorData::getPluginName));
            invalidateDanglingSelectorMatchCache(incoming, stale);
        }
    }

    @Override
    public void applyRuleDataSnapshot(final String namespaceId, final List<RuleData> dataList) {
        final BaseDataCache cache = BaseDataCache.getInstance();
        // A row without a selector id yields a null grouping key in refreshRuleData, whose compute on a
        // concurrent map throws and would abort the whole snapshot; skip it up front so valid rows still converge.
        final List<RuleData> incoming = new ArrayList<>(dataList.size());
        for (RuleData data : dataList) {
            if (Objects.nonNull(data) && Objects.nonNull(data.getSelectorId())) {
                incoming.add(data);
            } else {
                LOG.warn("skip malformed rule row without selector id in snapshot for namespace [{}]", namespaceId);
            }
        }
        final Set<String> ruleIds = incoming.stream().map(RuleData::getId).collect(Collectors.toSet());
        final List<RuleData> stale = cache.getRuleMap().values().stream()
                .flatMap(List::stream)
                .filter(data -> namespaceId.equals(data.getNamespaceId()))
                .filter(data -> !ruleIds.contains(data.getId()))
                .collect(Collectors.toList());
        // Merge and sort once per affected selector bucket instead of per rule row.
        if (!incoming.isEmpty()) {
            cache.refreshRuleData(incoming);
        }
        if (!stale.isEmpty()) {
            cache.removeRuleDataBatch(stale);
        }
        try {
            incoming.forEach(data -> runGuarded(data, () -> {
                if (StringUtils.isNotBlank(data.getPluginName())) {
                    Optional.ofNullable(handlerMap.get(data.getPluginName()))
                            .ifPresent(handler -> handler.handlerRule(data));
                }
            }));
            stale.forEach(data -> runGuarded(data, () -> {
                if (StringUtils.isNotBlank(data.getPluginName())) {
                    Optional.ofNullable(handlerMap.get(data.getPluginName()))
                            .ifPresent(handler -> handler.removeRule(data));
                }
            }));
        } finally {
            // Invalidate even if a handler aborted during initial sync, so a retry is not left with stale
            // match-cache entries for rows already removed from BaseDataCache above.
            invalidateRuleMatchCacheByPlugins(collectAffectedPlugins(incoming, stale, RuleData::getPluginName));
            invalidateDanglingRuleMatchCache(incoming, stale);
        }
    }

    @Override
    public void refreshPluginDataAll() {
        BaseDataCache.getInstance().cleanPluginData();
    }
    
    @Override
    public void refreshPluginDataSelf(final List<PluginData> pluginDataList) {
        LOG.info("start refresh plugin data self");
        if (CollectionUtils.isEmpty(pluginDataList)) {
            return;
        }
        BaseDataCache.getInstance().cleanPluginDataSelf(pluginDataList);
    }
    
    @Override
    public void onSelectorSubscribe(final SelectorData selectorData) {
        LOG.debug("subscribe select data for selector: [id: {}, pluginName: {}, name: {}]", selectorData.getId(), selectorData.getPluginName(), selectorData.getName());
        subscribeDataHandler(selectorData, DataEventTypeEnum.UPDATE);
    }
    
    @Override
    public void unSelectorSubscribe(final SelectorData selectorData) {
        LOG.debug("unSubscribe select data for selector: [id: {}, pluginName: {}, name: {}]", selectorData.getId(), selectorData.getPluginName(), selectorData.getName());
        subscribeDataHandler(selectorData, DataEventTypeEnum.DELETE);
    }
    
    @Override
    public void refreshSelectorDataAll() {
        LOG.debug("start refresh all selector data");
        BaseDataCache.getInstance().cleanSelectorData();
        MatchDataCache.getInstance().cleanSelectorData();
    }
    
    @Override
    public void refreshSelectorDataSelf(final List<SelectorData> selectorDataList) {
        if (CollectionUtils.isEmpty(selectorDataList)) {
            return;
        }
        BaseDataCache.getInstance().cleanSelectorDataSelf(selectorDataList);
    }
    
    @Override
    public void onRuleSubscribe(final RuleData ruleData) {
        LOG.debug("subscribe rule data for rule[id: {}, selectorId: {}, name: {}]", ruleData.getId(), ruleData.getSelectorId(), ruleData.getName());
        subscribeDataHandler(ruleData, DataEventTypeEnum.UPDATE);
    }
    
    @Override
    public void unRuleSubscribe(final RuleData ruleData) {
        LOG.debug("unSubscribe rule data for rule[id: {}, selectorId: {}, name: {}]", ruleData.getId(), ruleData.getSelectorId(), ruleData.getName());
        subscribeDataHandler(ruleData, DataEventTypeEnum.DELETE);
    }
    
    @Override
    public void refreshRuleDataAll() {
        LOG.debug("start refresh all rule data");
        BaseDataCache.getInstance().cleanRuleData();
        MatchDataCache.getInstance().cleanRuleDataData();
    }
    
    @Override
    public void refreshRuleDataSelf(final List<RuleData> ruleDataList) {
        if (CollectionUtils.isEmpty(ruleDataList)) {
            return;
        }
        BaseDataCache.getInstance().cleanRuleDataSelf(ruleDataList);
    }
    
    private <T> void subscribeDataHandler(final T classData, final DataEventTypeEnum dataType) {
        try {
            if (dataType == DataEventTypeEnum.UPDATE) {
                Optional.ofNullable(classData)
                        .ifPresent(data -> updateCacheData(classData));
            } else if (dataType == DataEventTypeEnum.DELETE) {
                Optional.ofNullable(classData)
                        .ifPresent(data -> removeCacheData(classData));
            }
        } catch (Exception e) {
            if (InitialSyncApplication.isActive()) {
                throw new IllegalStateException("Initial configuration application failed", e);
            }
            LOG.error("subscribe data handler error, classData: {}, dataType: {}", JsonUtils.toJson(classData), dataType, e);
        }
    }
    
    /**
     * update cache data.
     *
     * @param data data is plugin mate data, data is not null
     * @param <T>  data type, support is [{@link PluginData},{@link SelectorData},{@link RuleData}]
     */
    private <T> void updateCacheData(@NonNull final T data) {
        if (data instanceof PluginData) {
            PluginData pluginData = (PluginData) data;
            final PluginData oldPluginData = BaseDataCache.getInstance().obtainPluginData(pluginData.getName());
            Optional.ofNullable(handlerMap.get(pluginData.getName()))
                    .ifPresent(handler -> handler.handlerPlugin(pluginData));

            BaseDataCache.getInstance().cachePluginData(pluginData);
            notifyPluginData(oldPluginData, pluginData);
        } else if (data instanceof SelectorData) {
            SelectorData selectorData = (SelectorData) data;
            BaseDataCache.getInstance().cacheSelectData(selectorData);
            handleSelectorData(selectorData);
        } else if (data instanceof RuleData) {
            RuleData ruleData = (RuleData) data;
            BaseDataCache.getInstance().cacheRuleData(ruleData);
            handleRuleData(ruleData);
        }
    }

    /**
     * judge need update plugin order.
     *
     * @param oldPluginData old pluginData
     * @param pluginData    current pluginData
     */
    private void sortPluginIfOrderChange(final PluginData oldPluginData, final PluginData pluginData) {
        if (Objects.isNull(eventPublisher) || Objects.isNull(pluginData.getSort())) {
            return;
        }
        if (Objects.isNull(oldPluginData) || Objects.isNull(oldPluginData.getSort())
                || !Objects.equals(oldPluginData.getSort(), pluginData.getSort())) {
            eventPublisher.publishEvent(new PluginHandlerEvent(PluginHandlerEventEnum.SORTED, pluginData));
        }
    }

    /**
     * remove cache data.
     *
     * @param data data is plugin mate data, data is not null
     * @param <T>  data type, support is [{@link PluginData},{@link SelectorData},{@link RuleData}]
     */
    private <T> void removeCacheData(@NonNull final T data) {
        if (data instanceof PluginData) {
            PluginData pluginData = (PluginData) data;
            BaseDataCache.getInstance().removePluginData(pluginData);
            Optional.ofNullable(handlerMap.get(pluginData.getName()))
                    .ifPresent(handler -> handler.removePlugin(pluginData));
            eventPublisher.publishEvent(new PluginHandlerEvent(PluginHandlerEventEnum.DELETE, pluginData));
        } else if (data instanceof SelectorData) {
            SelectorData selectorData = (SelectorData) data;
            BaseDataCache.getInstance().removeSelectData(selectorData);
            // the concurrent handler map rejects null keys, and a dangling selector carries no plugin name
            if (StringUtils.isNotBlank(selectorData.getPluginName())) {
                Optional.ofNullable(handlerMap.get(selectorData.getPluginName()))
                        .ifPresent(handler -> handler.removeSelector(selectorData));
            }
            // remove match cache
            if (selectorMatchConfig.getCache().getEnabled()) {
                MatchDataCache.getInstance().removeSelectorData(selectorData.getPluginName(), selectorData.getId());
                MatchDataCache.getInstance().removeEmptySelectorData(selectorData.getPluginName());
            }
            if (ruleMatchCacheConfig.getCache().getEnabled()) {
                MatchDataCache.getInstance().removeRuleDataBySelector(selectorData.getPluginName(), selectorData.getId());
                MatchDataCache.getInstance().removeEmptyRuleData(selectorData.getPluginName());
            }
        } else if (data instanceof RuleData) {
            RuleData ruleData = (RuleData) data;
            BaseDataCache.getInstance().removeRuleData(ruleData);
            if (StringUtils.isNotBlank(ruleData.getPluginName())) {
                Optional.ofNullable(handlerMap.get(ruleData.getPluginName()))
                        .ifPresent(handler -> handler.removeRule(ruleData));
            }
            if (ruleMatchCacheConfig.getCache().getEnabled()) {
                MatchDataCache.getInstance().removeRuleData(ruleData.getPluginName(), ruleData.getId());
                MatchDataCache.getInstance().removeEmptyRuleData(ruleData.getPluginName());
            }
        }
    }

    private void notifyPluginData(final PluginData oldPluginData, final PluginData pluginData) {
        // update enabled plugins
        PluginHandlerEventEnum state = Boolean.TRUE.equals(pluginData.getEnabled())
                ? PluginHandlerEventEnum.ENABLED : PluginHandlerEventEnum.DISABLED;
        eventPublisher.publishEvent(new PluginHandlerEvent(state, pluginData));
        // sorted plugin
        sortPluginIfOrderChange(oldPluginData, pluginData);

        final String pluginName = pluginData.getName();
        // if update plugin, remove selector and rule match cache/trie cache
        if (selectorMatchConfig.getCache().getEnabled()) {
            MatchDataCache.getInstance().removeSelectorData(pluginName);
        }
        if (ruleMatchCacheConfig.getCache().getEnabled()) {
            MatchDataCache.getInstance().removeRuleData(pluginName);
        }
    }

    private void handleSelectorData(final SelectorData selectorData) {
        Optional.ofNullable(handlerMap.get(selectorData.getPluginName()))
                .ifPresent(handler -> handler.handlerSelector(selectorData));
        invalidateSelectorMatchCache(selectorData);
    }

    private void invalidateSelectorMatchCache(final SelectorData selectorData) {
        // remove match cache
        if (selectorMatchConfig.getCache().getEnabled()) {
            MatchDataCache.getInstance().removeSelectorData(selectorData.getPluginName(), selectorData.getId());
            MatchDataCache.getInstance().removeEmptySelectorData(selectorData.getPluginName());
        }
        if (ruleMatchCacheConfig.getCache().getEnabled()) {
            MatchDataCache.getInstance().removeRuleDataBySelector(selectorData.getPluginName(), selectorData.getId());
            MatchDataCache.getInstance().removeEmptyRuleData(selectorData.getPluginName());
        }
    }

    private void handleRuleData(final RuleData ruleData) {
        Optional.ofNullable(handlerMap.get(ruleData.getPluginName()))
                .ifPresent(handler -> handler.handlerRule(ruleData));
        invalidateRuleMatchCache(ruleData);
    }

    private void invalidateRuleMatchCache(final RuleData ruleData) {
        if (ruleMatchCacheConfig.getCache().getEnabled()) {
            MatchDataCache.getInstance().removeRuleData(ruleData.getPluginName(), ruleData.getId());
            MatchDataCache.getInstance().removeEmptyRuleData(ruleData.getPluginName());
        }
    }

    /**
     * Run a per-row callback for a snapshot apply, preserving the exception semantics of
     * {@link #subscribeDataHandler}: during initial sync failures abort the snapshot, otherwise
     * they are logged so subsequent rows still converge.
     */
    private <T> void runGuarded(final T data, final Runnable action) {
        try {
            action.run();
        } catch (Exception e) {
            if (InitialSyncApplication.isActive()) {
                throw new IllegalStateException("Initial configuration application failed", e);
            }
            LOG.error("snapshot handler error, classData: {}", JsonUtils.toJson(data), e);
        }
    }

    private <T> Set<String> collectAffectedPlugins(final List<T> incoming, final List<T> stale,
                                                   final Function<T, String> pluginNameOf) {
        final Set<String> plugins = new HashSet<>();
        incoming.forEach(data -> {
            if (Objects.isNull(data)) {
                return;
            }
            final String name = pluginNameOf.apply(data);
            if (StringUtils.isNotBlank(name)) {
                plugins.add(name);
            }
        });
        stale.forEach(data -> {
            if (Objects.isNull(data)) {
                return;
            }
            final String name = pluginNameOf.apply(data);
            if (StringUtils.isNotBlank(name)) {
                plugins.add(name);
            }
        });
        return plugins;
    }

    /**
     * Mirror master's id-based cross-bucket sweep for dangling selectors that carry no plugin name:
     * {@link #collectAffectedPlugins} drops blank names, so without this a deleted dangling selector would
     * leave its cached matches (which may live under any plugin bucket) behind. Delegates to the blank-name
     * branch of {@link MatchDataCache#removeSelectorData(String, String)} and
     * {@link MatchDataCache#removeRuleDataBySelector(String, String)}.
     */
    private void invalidateDanglingSelectorMatchCache(final List<SelectorData> incoming, final List<SelectorData> stale) {
        if (!selectorMatchConfig.getCache().getEnabled() && !ruleMatchCacheConfig.getCache().getEnabled()) {
            return;
        }
        final MatchDataCache matchDataCache = MatchDataCache.getInstance();
        Stream.concat(incoming.stream(), stale.stream())
                .filter(Objects::nonNull)
                .filter(data -> StringUtils.isBlank(data.getPluginName()))
                .map(SelectorData::getId)
                .filter(Objects::nonNull)
                .forEach(id -> {
                    if (selectorMatchConfig.getCache().getEnabled()) {
                        matchDataCache.removeSelectorData(null, id);
                    }
                    if (ruleMatchCacheConfig.getCache().getEnabled()) {
                        matchDataCache.removeRuleDataBySelector(null, id);
                    }
                });
    }

    /**
     * Rule counterpart of {@link #invalidateDanglingSelectorMatchCache}: a stale rule keyed by selectorId can
     * carry a blank plugin name, so its cached matches are swept across every bucket by rule id via the
     * blank-name branch of {@link MatchDataCache#removeRuleData(String, String)}.
     */
    private void invalidateDanglingRuleMatchCache(final List<RuleData> incoming, final List<RuleData> stale) {
        if (!ruleMatchCacheConfig.getCache().getEnabled()) {
            return;
        }
        final MatchDataCache matchDataCache = MatchDataCache.getInstance();
        Stream.concat(incoming.stream(), stale.stream())
                .filter(Objects::nonNull)
                .filter(data -> StringUtils.isBlank(data.getPluginName()))
                .map(RuleData::getId)
                .filter(Objects::nonNull)
                .forEach(id -> matchDataCache.removeRuleData(null, id));
    }

    private void invalidateSelectorMatchCacheByPlugins(final Set<String> pluginNames) {
        if (pluginNames.isEmpty()) {
            return;
        }
        final MatchDataCache matchDataCache = MatchDataCache.getInstance();
        for (String pluginName : pluginNames) {
            if (selectorMatchConfig.getCache().getEnabled()) {
                matchDataCache.removeSelectorData(pluginName);
            }
            if (ruleMatchCacheConfig.getCache().getEnabled()) {
                // selector membership changed, so any cached rule match under this plugin may be stale
                matchDataCache.removeRuleData(pluginName);
            }
        }
    }

    private void invalidateRuleMatchCacheByPlugins(final Set<String> pluginNames) {
        if (pluginNames.isEmpty() || !ruleMatchCacheConfig.getCache().getEnabled()) {
            return;
        }
        final MatchDataCache matchDataCache = MatchDataCache.getInstance();
        pluginNames.forEach(matchDataCache::removeRuleData);
    }

    @Override
    public void onPluginRefresh(final List<PluginData> dataList) {
        if (CollectionUtils.isEmpty(dataList)) {
            return;
        }
        dataList.forEach(data -> Optional.ofNullable(handlerMap.get(data.getName()))
                .ifPresent(handler -> handler.handlerPlugin(data)));
        BaseDataCache.getInstance().refreshPluginData(dataList);
        // Legacy refresh removed the old entries before subscription, so it always notified sorting.
        dataList.forEach(data -> notifyPluginData(null, data));
    }

    @Override
    public void onSelectorRefresh(final List<SelectorData> dataList) {
        if (CollectionUtils.isEmpty(dataList)) {
            return;
        }
        BaseDataCache.getInstance().refreshSelectorData(dataList);
        dataList.forEach(this::handleSelectorData);
    }

    @Override
    public void onRuleRefresh(final List<RuleData> dataList) {
        if (CollectionUtils.isEmpty(dataList)) {
            return;
        }
        BaseDataCache.getInstance().refreshRuleData(dataList);
        dataList.forEach(this::handleRuleData);
    }

}
