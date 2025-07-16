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
import org.apache.shenyu.common.config.ShenyuConfig.RuleMatchCache;
import org.apache.shenyu.common.config.ShenyuConfig.SelectorMatchCache;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.enums.PluginHandlerEventEnum;
import org.apache.shenyu.common.enums.TrieCacheTypeEnum;
import org.apache.shenyu.common.enums.TrieEventEnum;
import org.apache.shenyu.common.utils.MapUtils;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.event.TrieEvent;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.base.trie.ShenyuTrie;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.lang.NonNull;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * The type Common plugin data subscriber.
 */
public class CommonPluginDataSubscriber implements PluginDataSubscriber {
    
    private static final Logger LOG = LoggerFactory.getLogger(CommonPluginDataSubscriber.class);
    
    private final Map<String, PluginDataHandler> handlerMap;

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
        this.eventPublisher = eventPublisher;
        this.selectorMatchConfig = selectorMatchConfig;
        this.ruleMatchCacheConfig = ruleMatchCacheConfig;
    }
    
    /**
     * Put extend plugin data handler.
     *
     * @param handlers the handlers
     */
    public void putExtendPluginDataHandler(final List<PluginDataHandler> handlers) {
        if (CollectionUtils.isEmpty(handlers)) {
            return;
        }
        for (PluginDataHandler handler : handlers) {
            String pluginNamed = handler.pluginNamed();
            MapUtils.computeIfAbsent(handlerMap, pluginNamed, name -> {
                LOG.info("shenyu auto add extends plugin data handler name is :{}", pluginNamed);
                return handler;
            });
        }
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
        ShenyuTrie selectorTrie = SpringBeanUtils.getInstance().getBean(TrieCacheTypeEnum.SELECTOR.getTrieType());
        selectorTrie.clear();
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
        ShenyuTrie ruleTrie = SpringBeanUtils.getInstance().getBean(TrieCacheTypeEnum.RULE.getTrieType());
        ruleTrie.clear();
    }
    
    @Override
    public void refreshRuleDataSelf(final List<RuleData> ruleDataList) {
        if (CollectionUtils.isEmpty(ruleDataList)) {
            return;
        }
        BaseDataCache.getInstance().cleanRuleDataSelf(ruleDataList);
    }
    
    private <T> void subscribeDataHandler(final T classData, final DataEventTypeEnum dataType) {
        if (dataType == DataEventTypeEnum.UPDATE) {
            Optional.ofNullable(classData)
                    .ifPresent(data -> updateCacheData(classData));
        } else if (dataType == DataEventTypeEnum.DELETE) {
            Optional.ofNullable(classData)
                    .ifPresent(data -> removeCacheData(classData));
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
        } else if (data instanceof SelectorData) {
            SelectorData selectorData = (SelectorData) data;
            BaseDataCache.getInstance().cacheSelectData(selectorData);
            Optional.ofNullable(handlerMap.get(selectorData.getPluginName()))
                    .ifPresent(handler -> handler.handlerSelector(selectorData));
            // remove match cache
            if (selectorMatchConfig.getCache().getEnabled()) {
                MatchDataCache.getInstance().removeSelectorData(selectorData.getPluginName(), selectorData.getId());
                MatchDataCache.getInstance().removeEmptySelectorData(selectorData.getPluginName());
            }
            if (ruleMatchCacheConfig.getCache().getEnabled()) {
                MatchDataCache.getInstance().removeRuleDataBySelector(selectorData.getPluginName(), selectorData.getId());
                MatchDataCache.getInstance().removeEmptyRuleData(selectorData.getPluginName());
            }
            updateSelectorTrieCache(selectorData);
        } else if (data instanceof RuleData) {
            RuleData ruleData = (RuleData) data;
            BaseDataCache.getInstance().cacheRuleData(ruleData);
            Optional.ofNullable(handlerMap.get(ruleData.getPluginName()))
                    .ifPresent(handler -> handler.handlerRule(ruleData));
            if (ruleMatchCacheConfig.getCache().getEnabled()) {
                MatchDataCache.getInstance().removeRuleData(ruleData.getPluginName(), ruleData.getId());
                MatchDataCache.getInstance().removeEmptyRuleData(ruleData.getPluginName());
            }
            updateRuleTrieCache(ruleData);
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
            Optional.ofNullable(handlerMap.get(selectorData.getPluginName()))
                    .ifPresent(handler -> handler.removeSelector(selectorData));
            // remove selector match cache
            if (selectorMatchConfig.getCache().getEnabled()) {
                MatchDataCache.getInstance().removeSelectorData(selectorData.getPluginName(), selectorData.getId());
                MatchDataCache.getInstance().removeEmptySelectorData(selectorData.getPluginName());
            }
            // remove selector trie cache
            if (selectorMatchConfig.getTrie().getEnabled()) {
                eventPublisher.publishEvent(new TrieEvent(TrieEventEnum.REMOVE, TrieCacheTypeEnum.SELECTOR, selectorData));
            }
        } else if (data instanceof RuleData) {
            RuleData ruleData = (RuleData) data;
            BaseDataCache.getInstance().removeRuleData(ruleData);
            Optional.ofNullable(handlerMap.get(ruleData.getPluginName()))
                    .ifPresent(handler -> handler.removeRule(ruleData));
            if (ruleMatchCacheConfig.getCache().getEnabled()) {
                MatchDataCache.getInstance().removeRuleData(ruleData.getPluginName(), ruleData.getId());
                MatchDataCache.getInstance().removeEmptyRuleData(ruleData.getPluginName());
            }
            if (ruleMatchCacheConfig.getTrie().getEnabled()) {
                eventPublisher.publishEvent(new TrieEvent(TrieEventEnum.REMOVE, TrieCacheTypeEnum.RULE, ruleData));
            }
        }
    }
    
    private void updateSelectorTrieCache(final SelectorData selectorData) {
        if (!selectorMatchConfig.getTrie().getEnabled()) {
            return;
        }
        if (Boolean.TRUE.equals(selectorData.getEnabled())) {
            if (CollectionUtils.isEmpty(selectorData.getBeforeConditionList())) {
                eventPublisher.publishEvent(new TrieEvent(TrieEventEnum.INSERT, TrieCacheTypeEnum.SELECTOR, selectorData));
            } else {
                // if selector data has before condition, update trie
                eventPublisher.publishEvent(new TrieEvent(TrieEventEnum.UPDATE, TrieCacheTypeEnum.SELECTOR, selectorData));
            }
        } else {
            eventPublisher.publishEvent(new TrieEvent(TrieEventEnum.REMOVE, TrieCacheTypeEnum.SELECTOR, selectorData));
        }
    }
    
    private void updateRuleTrieCache(final RuleData ruleData) {
        if (!ruleMatchCacheConfig.getTrie().getEnabled()) {
            return;
        }
        if (Boolean.TRUE.equals(ruleData.getEnabled())) {
            if (CollectionUtils.isEmpty(ruleData.getBeforeConditionDataList())) {
                eventPublisher.publishEvent(new TrieEvent(TrieEventEnum.INSERT, TrieCacheTypeEnum.RULE, ruleData));
            } else {
                // if rule data has before condition, update trie
                eventPublisher.publishEvent(new TrieEvent(TrieEventEnum.UPDATE, TrieCacheTypeEnum.RULE, ruleData));
            }
        } else {
            eventPublisher.publishEvent(new TrieEvent(TrieEventEnum.REMOVE, TrieCacheTypeEnum.RULE, ruleData));
        }
    }
}
