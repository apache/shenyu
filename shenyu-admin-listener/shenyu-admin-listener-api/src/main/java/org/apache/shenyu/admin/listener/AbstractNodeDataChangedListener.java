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

package org.apache.shenyu.admin.listener;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.DefaultNodeConstants;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReentrantLock;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;

/**
 * AbstractNodeDataChangedListener.
 */
public abstract class AbstractNodeDataChangedListener implements DataChangedListener {

    private static final Logger LOG = LoggerFactory.getLogger(AbstractNodeDataChangedListener.class);

    private final ChangeData changeData;

    private final Map<String, ReentrantLock> listSaveLockMap = new ConcurrentHashMap<>();

    /**
     * AbstractListDataChangedListener.
     *
     * @param changeData changeData
     */
    protected AbstractNodeDataChangedListener(final ChangeData changeData) {
        this.changeData = changeData;
    }

    @Override
    public void onAppAuthChanged(final List<AppAuthData> changed, final DataEventTypeEnum eventType) {
        this.onAppAuthChanged(changed, eventType, null);
    }

    @Override
    public void onAppAuthChanged(final List<AppAuthData> changed, final DataEventTypeEnum eventType, final String namespaceId) {
        // an empty payload is only meaningful when the event names the namespace: it is then a full snapshot
        if (CollectionUtils.isEmpty(changed) && StringUtils.isBlank(namespaceId)) {
            return;
        }
        final String resolvedNamespaceId = resolveNamespace(namespaceId, changed, AppAuthData::getNamespaceId);
        final String configKeyPrefix = resolvedNamespaceId + DefaultNodeConstants.JOIN_POINT + changeData.getAuthDataId() + DefaultNodeConstants.JOIN_POINT;
        this.onCommonChanged(configKeyPrefix, changed, eventType, AppAuthData::getAppKey, AppAuthData.class);
        LOG.debug("[DataChangedListener] AppAuthChanged {}", configKeyPrefix);
    }

    @Override
    public void onPluginChanged(final List<PluginData> changed, final DataEventTypeEnum eventType) {
        this.onPluginChanged(changed, eventType, null);
    }

    @Override
    public void onPluginChanged(final List<PluginData> changed, final DataEventTypeEnum eventType, final String namespaceId) {
        // an empty payload is only meaningful when the event names the namespace: it is then a full snapshot
        if (CollectionUtils.isEmpty(changed) && StringUtils.isBlank(namespaceId)) {
            return;
        }
        final String resolvedNamespaceId = resolveNamespace(namespaceId, changed, PluginData::getNamespaceId);
        if (DataEventTypeEnum.REFRESH == eventType && StringUtils.isNotBlank(namespaceId)) {
            this.deleteStalePluginGroups(resolvedNamespaceId, changed);
        }
        final String configKeyPrefix = resolvedNamespaceId + DefaultNodeConstants.JOIN_POINT + changeData.getPluginDataId() + DefaultNodeConstants.JOIN_POINT;
        this.onCommonChanged(configKeyPrefix, changed, eventType, PluginData::getName, PluginData.class);
        LOG.debug("[DataChangedListener] PluginChanged {}", configKeyPrefix);
    }

    /**
     * Resolve the namespace of an event. An explicit namespace wins over the payload, so an empty
     * snapshot can still be reconciled; callers reject an empty payload without one.
     */
    private static <T> String resolveNamespace(final String namespaceId, final List<T> changed,
                                               final Function<? super T, ? extends String> namespaceGetter) {
        if (StringUtils.isNotBlank(namespaceId)) {
            return namespaceId;
        }
        return StringUtils.defaultString(namespaceGetter.apply(changed.get(0)), SYS_DEFAULT_NAMESPACE_ID);
    }

    @Override
    public void onMetaDataChanged(final List<MetaData> changed, final DataEventTypeEnum eventType) {
        if (CollectionUtils.isEmpty(changed)) {
            return;
        }
        final String namespaceId = changed.stream().map(value -> StringUtils.defaultString(value.getNamespaceId(), SYS_DEFAULT_NAMESPACE_ID)).findFirst().get();
        final String configKeyPrefix = namespaceId + DefaultNodeConstants.JOIN_POINT + changeData.getMetaDataId() + DefaultNodeConstants.JOIN_POINT;
        this.onCommonChanged(configKeyPrefix, changed, eventType, MetaData::getId, MetaData.class);
        LOG.debug("[DataChangedListener] MetaDataChanged {}", changeData.getMetaDataId());
    }

    /**
     * onCommonChanged.
     * save to configuration center common methods.
     * examples data:
     *  namespaceId.meta.list
     *   -> meta.id
     *   -> meta.id
     *   -> meta.id
     *
     * @param configKeyPrefix configKeyPrefix
     * @param changedList changedList
     * @param eventType eventType
     * @param mapperToKey mapperToKey
     * @param tClass tClass
     */
    private <T> void onCommonChanged(final String configKeyPrefix, final List<T> changedList,
                                     final DataEventTypeEnum eventType, final Function<? super T, ? extends String> mapperToKey,
                                     final Class<T> tClass) {
        // Avoiding concurrent operations on list nodes
        final ReentrantLock reentrantLock = listSaveLockMap.computeIfAbsent(configKeyPrefix, key -> new ReentrantLock());
        try {
            reentrantLock.lock();
            final List<String> changeNames = changedList.stream().map(mapperToKey).collect(Collectors.toList());
            switch (eventType) {
                case DELETE:
                    changedList.stream().map(mapperToKey).forEach(removeKey -> delConfig(configKeyPrefix + removeKey));
                    delChangedData(configKeyPrefix, changeNames);
                    break;
                case REFRESH:
                case MYSELF:
                    final List<String> configDataNames = this.getConfigDataNames(configKeyPrefix);
                    changedList.forEach(changedData -> publishConfig(configKeyPrefix + mapperToKey.apply(changedData), changedData));

                    if (Objects.nonNull(configDataNames)) {
                        configDataNames.removeAll(changeNames);
                        configDataNames.forEach(staleName -> delConfig(configKeyPrefix + staleName));
                    }

                    publishConfig(configKeyPrefix + DefaultNodeConstants.LIST_STR, changeNames);
                    break;
                default:
                    changedList.forEach(changedData -> publishConfig(configKeyPrefix + mapperToKey.apply(changedData), changedData));
                    putChangeData(configKeyPrefix, changeNames);
                    break;
            }
        } catch (Exception e) {
            LOG.error("AbstractNodeDataChangedListener onCommonMultiChanged error ", e);
        } finally {
            reentrantLock.unlock();
        }
    }

    private void putChangeData(final String configKeyPrefix, final List<String> changeNames) {
        final String oldNodeListStr = Optional.ofNullable(getConfig(configKeyPrefix + DefaultNodeConstants.LIST_STR))
                .orElse(DefaultNodeConstants.EMPTY_ARRAY_STR);

        final List<String> oldNodeList = GsonUtils.getInstance().fromList(oldNodeListStr, String.class);
        oldNodeList.addAll(changeNames.stream().filter(changeName -> !oldNodeList.contains(changeName)).collect(Collectors.toList()));
        publishConfig(configKeyPrefix + DefaultNodeConstants.LIST_STR, oldNodeList);
    }

    private void delChangedData(final String configKeyPrefix, final List<String> changeNames) {
        final String oldNodeListStr = Optional.ofNullable(getConfig(configKeyPrefix + DefaultNodeConstants.LIST_STR))
                .orElse(DefaultNodeConstants.EMPTY_ARRAY_STR);

        final List<String> oldNodeList = GsonUtils.getInstance().fromList(oldNodeListStr, String.class);
        oldNodeList.removeAll(changeNames);
        publishConfig(configKeyPrefix + DefaultNodeConstants.LIST_STR, oldNodeList);
    }

    @Override
    public void onSelectorChanged(final List<SelectorData> changed, final DataEventTypeEnum eventType) {
        this.onSelectorChanged(changed, eventType, null);
    }

    @Override
    public void onSelectorChanged(final List<SelectorData> changed, final DataEventTypeEnum eventType, final String namespaceId) {
        // an empty payload is only meaningful when the event names the namespace: it is then a full snapshot
        if (CollectionUtils.isEmpty(changed) && StringUtils.isBlank(namespaceId)) {
            return;
        }
        final String resolvedNamespaceId = resolveNamespace(namespaceId, changed, SelectorData::getNamespaceId);
        final String configKeyPrefix = resolvedNamespaceId + DefaultNodeConstants.JOIN_POINT + changeData.getSelectorDataId() + DefaultNodeConstants.JOIN_POINT;
        if (DataEventTypeEnum.REFRESH == eventType && StringUtils.isNotBlank(namespaceId)) {
            this.onSelectorRefreshed(resolvedNamespaceId, changed, configKeyPrefix);
        } else {
            this.onCommonMultiChanged(changed, eventType, configKeyPrefix, SelectorData::getPluginName, SelectorData::getId);
        }
        LOG.debug("[DataChangedListener] SelectorChanged {}", configKeyPrefix);
    }

    @Override
    public void onRuleChanged(final List<RuleData> changed, final DataEventTypeEnum eventType) {
        this.onRuleChanged(changed, eventType, null);
    }

    @Override
    public void onRuleChanged(final List<RuleData> changed, final DataEventTypeEnum eventType, final String namespaceId) {
        // an empty payload is only meaningful when the event names the namespace: it is then a full snapshot
        if (CollectionUtils.isEmpty(changed) && StringUtils.isBlank(namespaceId)) {
            return;
        }
        final String resolvedNamespaceId = resolveNamespace(namespaceId, changed, RuleData::getNamespaceId);
        final String configKeyPrefix = resolvedNamespaceId + DefaultNodeConstants.JOIN_POINT + changeData.getRuleDataId() + DefaultNodeConstants.JOIN_POINT;
        if (DataEventTypeEnum.REFRESH == eventType && StringUtils.isNotBlank(namespaceId)) {
            this.onRuleRefreshed(resolvedNamespaceId, changed, configKeyPrefix);
        } else {
            this.onCommonMultiChanged(changed, eventType, configKeyPrefix, AbstractNodeDataChangedListener::ruleGroupKey, RuleData::getId);
        }
        LOG.debug("[DataChangedListener] RuleChanged {}", changeData.getRuleDataId());
    }

    /**
     * onCommonMultiChanged.
     * save to configuration center common multi methods.
     * examples data:
     *  namespaceId.selector.key1
     *   -> selector.key1.value1
     *   -> selector.key1.value2
     *   -> selector.key1.value3
     *  namespaceId.selector.key2
     *   -> selector.key2.value4
     *   -> selector.key2.value5
     *   -> selector.key2.value6
     * @param changedList changedList
     * @param eventType eventType
     * @param configKeyPrefix configKeyPrefix
     * @param mappingKey mappingKey
     * @param mappingValue mappingValue
     */
    private <T> void onCommonMultiChanged(final List<T> changedList, final DataEventTypeEnum eventType,
                                          final String configKeyPrefix, final Function<? super T, ? extends String> mappingKey,
                                          final Function<? super T, ? extends String> mappingValue) {
        // Avoiding concurrent operations on list nodes
        final ReentrantLock reentrantLock = listSaveLockMap.computeIfAbsent(configKeyPrefix, key -> new ReentrantLock());
        try {
            reentrantLock.lock();
            final Map<String, List<String>> nameToIdMap = changedList.stream()
                    .collect(Collectors.groupingBy(mappingKey, Collectors.mapping(mappingValue, Collectors.toList())));
            switch (eventType) {
                case DELETE:
                    changedList.forEach(changedData -> delConfig(configKeyPrefix + mappingKey.apply(changedData) + DefaultNodeConstants.JOIN_POINT + mappingValue.apply(changedData)));
                    this.delChangedMapToList(nameToIdMap, configKeyPrefix);
                    break;
                case REFRESH:
                    changedList.forEach(changedData -> publishConfig(configKeyPrefix + mappingKey.apply(changedData) + DefaultNodeConstants.JOIN_POINT + mappingValue.apply(changedData), changedData));
                    this.refreshChangedMapToList(nameToIdMap, configKeyPrefix);
                    break;
                case MYSELF:
                default:
                    changedList.forEach(changedData -> publishConfig(configKeyPrefix + mappingKey.apply(changedData) + DefaultNodeConstants.JOIN_POINT + mappingValue.apply(changedData), changedData));
                    this.putChangedMapToList(nameToIdMap, configKeyPrefix);
                    break;
            }
        } catch (Exception e) {
            LOG.error("AbstractNodeDataChangedListener onCommonMultiChanged error ", e);
        } finally {
            reentrantLock.unlock();
        }
    }

    private void refreshChangedMapToList(final Map<String, List<String>> stringListMap, final String configKeyPrefix) {
        stringListMap.forEach((key, listIds) -> {
            final String listKey = configKeyPrefix + key + DefaultNodeConstants.POINT_LIST;
            final String oldNodeListStr = Optional.ofNullable(getConfig(listKey)).orElse(DefaultNodeConstants.EMPTY_ARRAY_STR);
            final List<String> oldNodeList = GsonUtils.getInstance().fromList(oldNodeListStr, String.class);
            oldNodeList.removeAll(listIds);
            oldNodeList.forEach(id -> delConfig(configKeyPrefix + key + DefaultNodeConstants.JOIN_POINT + id));
            publishConfig(listKey, listIds);
        });
    }

    /**
     * A namespace-scoped REFRESH is a full snapshot of the group. Upsert the payload first, then
     * reconcile every known group of the namespace so entries absent from the payload are removed.
     */
    private void onSelectorRefreshed(final String namespaceId, final List<SelectorData> changed, final String configKeyPrefix) {
        final ReentrantLock reentrantLock = listSaveLockMap.computeIfAbsent(configKeyPrefix, key -> new ReentrantLock());
        try {
            reentrantLock.lock();
            changed.forEach(data -> publishConfig(configKeyPrefix + data.getPluginName() + DefaultNodeConstants.JOIN_POINT + data.getId(), data));
            final Map<String, List<String>> idsByPlugin = changed.stream()
                    .collect(Collectors.groupingBy(SelectorData::getPluginName, Collectors.mapping(SelectorData::getId, Collectors.toList())));
            final Set<String> plugins = new LinkedHashSet<>(getPluginNames(namespaceId));
            plugins.addAll(idsByPlugin.keySet());
            this.refreshMapToList(idsByPlugin, plugins, configKeyPrefix,
                    (pluginName, selectorId) -> deleteRuleGroup(namespaceId, pluginName, selectorId));
        } catch (Exception e) {
            LOG.error("AbstractNodeDataChangedListener onSelectorRefreshed error ", e);
        } finally {
            reentrantLock.unlock();
        }
    }

    private void onRuleRefreshed(final String namespaceId, final List<RuleData> changed, final String configKeyPrefix) {
        final ReentrantLock reentrantLock = listSaveLockMap.computeIfAbsent(configKeyPrefix, key -> new ReentrantLock());
        try {
            reentrantLock.lock();
            changed.forEach(data -> publishConfig(configKeyPrefix + ruleGroupKey(data) + DefaultNodeConstants.JOIN_POINT + data.getId(), data));
            final Map<String, List<String>> idsByGroup = changed.stream()
                    .collect(Collectors.groupingBy(AbstractNodeDataChangedListener::ruleGroupKey, Collectors.mapping(RuleData::getId, Collectors.toList())));
            final Set<String> groupKeys = new LinkedHashSet<>();
            getPluginNames(namespaceId).forEach(pluginName -> {
                final List<String> selectorIds = getConfigList(selectorListKey(namespaceId, pluginName));
                if (Objects.nonNull(selectorIds)) {
                    selectorIds.forEach(selectorId -> groupKeys.add(pluginName + DefaultNodeConstants.JOIN_POINT + selectorId));
                }
            });
            groupKeys.addAll(idsByGroup.keySet());
            this.refreshMapToList(idsByGroup, groupKeys, configKeyPrefix, null);
        } catch (Exception e) {
            LOG.error("AbstractNodeDataChangedListener onRuleRefreshed error ", e);
        } finally {
            reentrantLock.unlock();
        }
    }

    private void refreshMapToList(final Map<String, List<String>> idsByGroup, final Set<String> groupKeys,
                                  final String configKeyPrefix, final BiConsumer<String, String> staleIdHandler) {
        groupKeys.forEach(group -> {
            final List<String> expectedIds = idsByGroup.getOrDefault(group, Collections.emptyList());
            final String listKey = configKeyPrefix + group + DefaultNodeConstants.POINT_LIST;
            final List<String> existingIds = getConfigList(listKey);
            if (Objects.nonNull(existingIds)) {
                existingIds.removeAll(expectedIds);
                existingIds.forEach(staleId -> {
                    delConfig(configKeyPrefix + group + DefaultNodeConstants.JOIN_POINT + staleId);
                    if (Objects.nonNull(staleIdHandler)) {
                        staleIdHandler.accept(group, staleId);
                    }
                });
            }
            // publish the snapshot even when it is empty, like every other REFRESH path in this class
            publishConfig(listKey, expectedIds);
        });
    }

    /**
     * A plugin absent from the namespace snapshot no longer exists, so its selectors and rules
     * must go too; otherwise nothing would discover them once the plugin index is overwritten.
     */
    private void deleteStalePluginGroups(final String namespaceId, final List<PluginData> changed) {
        final Set<String> currentPlugins = changed.stream().map(PluginData::getName).collect(Collectors.toSet());
        getPluginNames(namespaceId).stream()
                .filter(pluginName -> !currentPlugins.contains(pluginName))
                .forEach(pluginName -> deletePluginGroups(namespaceId, pluginName));
    }

    private void deletePluginGroups(final String namespaceId, final String pluginName) {
        final String selectorPrefix = namespaceId + DefaultNodeConstants.JOIN_POINT + changeData.getSelectorDataId()
                + DefaultNodeConstants.JOIN_POINT + pluginName;
        final List<String> selectorIds = getConfigList(selectorPrefix + DefaultNodeConstants.POINT_LIST);
        if (Objects.isNull(selectorIds)) {
            return;
        }
        selectorIds.forEach(selectorId -> {
            delConfig(selectorPrefix + DefaultNodeConstants.JOIN_POINT + selectorId);
            deleteRuleGroup(namespaceId, pluginName, selectorId);
        });
        delConfig(selectorPrefix + DefaultNodeConstants.POINT_LIST);
    }

    /**
     * Rules cannot outlive their selector: the following rule REFRESH can only discover rule groups
     * through the selector index, so drop them together with the stale selector.
     */
    private void deleteRuleGroup(final String namespaceId, final String pluginName, final String selectorId) {
        final String ruleKeyPrefix = namespaceId + DefaultNodeConstants.JOIN_POINT + changeData.getRuleDataId()
                + DefaultNodeConstants.JOIN_POINT + pluginName + DefaultNodeConstants.JOIN_POINT + selectorId;
        final List<String> ruleIds = getConfigList(ruleKeyPrefix + DefaultNodeConstants.POINT_LIST);
        if (Objects.isNull(ruleIds)) {
            return;
        }
        ruleIds.forEach(ruleId -> delConfig(ruleKeyPrefix + DefaultNodeConstants.JOIN_POINT + ruleId));
        delConfig(ruleKeyPrefix + DefaultNodeConstants.POINT_LIST);
    }

    private static String ruleGroupKey(final RuleData ruleData) {
        return String.join(DefaultNodeConstants.JOIN_POINT, ruleData.getPluginName(), ruleData.getSelectorId());
    }

    private String selectorListKey(final String namespaceId, final String pluginName) {
        return namespaceId + DefaultNodeConstants.JOIN_POINT + changeData.getSelectorDataId()
                + DefaultNodeConstants.JOIN_POINT + pluginName + DefaultNodeConstants.POINT_LIST;
    }

    /**
     * Plugin names of the namespace, stored in the plugin index. They identify the selector and rule
     * groups that may hold stale data even when the current payload no longer mentions them.
     */
    private List<String> getPluginNames(final String namespaceId) {
        return Optional.ofNullable(getConfigList(namespaceId + DefaultNodeConstants.JOIN_POINT
                        + changeData.getPluginDataId() + DefaultNodeConstants.POINT_LIST))
                .orElseGet(Collections::emptyList);
    }

    private List<String> getConfigList(final String dataId) {
        final String value = getConfig(dataId);
        if (Objects.isNull(value)) {
            return null;
        }
        final List<String> list = GsonUtils.getInstance().fromList(value, String.class);
        return Objects.isNull(list) ? new ArrayList<>() : list;
    }

    private void putChangedMapToList(final Map<String, List<String>> stringListMap, final String configKeyPrefix) {
        stringListMap.forEach((key, listIds) -> {
            final String oldNodeListStr = Optional.ofNullable(getConfig(configKeyPrefix + key + DefaultNodeConstants.POINT_LIST))
                    .orElse(DefaultNodeConstants.EMPTY_ARRAY_STR);

            final List<String> oldNodeList = GsonUtils.getInstance().fromList(oldNodeListStr, String.class);
            oldNodeList.addAll(listIds.stream().filter(selectorId -> !oldNodeList.contains(selectorId)).collect(Collectors.toList()));
            if (ObjectUtils.isEmpty(oldNodeList)) {
                delConfig(configKeyPrefix + key + DefaultNodeConstants.POINT_LIST);
            } else {
                publishConfig(configKeyPrefix + key + DefaultNodeConstants.POINT_LIST, oldNodeList);
            }
        });
    }

    private void delChangedMapToList(final Map<String, List<String>> stringListMap, final String configKeyPrefix) {
        stringListMap.forEach((key, listIds) -> {
            final String oldNodeListStr = Optional.ofNullable(getConfig(configKeyPrefix + key + DefaultNodeConstants.POINT_LIST))
                    .orElse(DefaultNodeConstants.EMPTY_ARRAY_STR);
            final List<String> oldNodeList = GsonUtils.getInstance().fromList(oldNodeListStr, String.class);
            oldNodeList.removeAll(listIds);
            if (ObjectUtils.isEmpty(oldNodeList)) {
                delConfig(configKeyPrefix + key + DefaultNodeConstants.POINT_LIST);
            } else {
                publishConfig(configKeyPrefix + key + DefaultNodeConstants.POINT_LIST, oldNodeList);
            }
        });
    }

    @Override
    public void onProxySelectorChanged(final List<ProxySelectorData> changed, final DataEventTypeEnum eventType) {
        if (CollectionUtils.isEmpty(changed)) {
            return;
        }
        final String namespaceId = changed.stream().map(value -> StringUtils.defaultString(value.getNamespaceId(), SYS_DEFAULT_NAMESPACE_ID)).findFirst().get();
        final String configKeyPrefix = namespaceId + DefaultNodeConstants.JOIN_POINT + changeData.getProxySelectorDataId() + DefaultNodeConstants.JOIN_POINT;
        this.onCommonMultiChanged(changed, eventType, configKeyPrefix, ProxySelectorData::getPluginName, ProxySelectorData::getId);
        LOG.debug("[DataChangedListener] ProxySelectorChanged {}", changeData.getProxySelectorDataId());
    }

    @Override
    public void onDiscoveryUpstreamChanged(final List<DiscoverySyncData> changed, final DataEventTypeEnum eventType) {
        if (CollectionUtils.isEmpty(changed)) {
            return;
        }
        final String namespaceId = changed.stream().map(value -> StringUtils.defaultString(value.getNamespaceId(), SYS_DEFAULT_NAMESPACE_ID)).findFirst().get();
        final String configKeyPrefix = namespaceId + DefaultNodeConstants.JOIN_POINT + changeData.getDiscoveryDataId() + DefaultNodeConstants.JOIN_POINT;
        this.onCommonMultiChanged(changed, eventType, configKeyPrefix, DiscoverySyncData::getPluginName, DiscoverySyncData::getSelectorId);
        LOG.debug("[DataChangedListener] DiscoveryUpstreamChanged {}", changeData.getDiscoveryDataId());
    }

    private void publishConfig(final String dataId, final Object data) {
        LOG.debug("[DataChangedListener] publishConfig {}", dataId);
        this.doPublishConfig(dataId, data);
    }

    /**
     * doPublishConfig.
     *
     * @param dataId dataId
     * @param data   data
     */
    public abstract void doPublishConfig(String dataId, Object data);

    private void delConfig(final String dataId) {
        LOG.debug("[DataChangedListener] delConfig {}", dataId);
        this.doDelConfig(dataId);
    }

    /**
     * doDelConfig.
     *
     * @param dataId dataId
     */
    protected abstract void doDelConfig(String dataId);

    /**
     * getConfig.
     *
     * @param dataId dataId
     * @return {@link String}
     */
    public abstract String getConfig(String dataId);

    /**
     * getConfigDataNames.
     *
     * @param configKeyPrefix configKeyPrefix
     * @return {@link List }
     */
    private <T> List<String> getConfigDataNames(final String configKeyPrefix) {
        return GsonUtils.getInstance().fromList(this.getConfig(configKeyPrefix + DefaultNodeConstants.LIST_STR), String.class);
    }

    public static class ChangeData {

        /**
         * plugin data id.
         */
        private final String pluginDataId;

        /**
         * selector data id.
         */
        private final String selectorDataId;

        /**
         * rule data id.
         */
        private final String ruleDataId;

        /**
         * auth data id.
         */
        private final String authDataId;

        /**
         * meta data id.
         */
        private final String metaDataId;

        /**
         * proxySelector data id.
         */
        private final String proxySelectorDataId;

        /**
         * discovery data id.
         */
        private final String discoveryDataId;

        /**
         * ChangeData.
         *
         * @param pluginDataId   pluginDataId
         * @param selectorDataId selectorDataId
         * @param ruleDataId     ruleDataId
         * @param authDataId     authDataId
         * @param metaDataId     metaDataId
         */
        public ChangeData(final String pluginDataId, final String selectorDataId,
                          final String ruleDataId, final String authDataId,
                          final String metaDataId, final String proxySelectorDataId, final String discoveryDataId) {
            this.pluginDataId = pluginDataId;
            this.selectorDataId = selectorDataId;
            this.ruleDataId = ruleDataId;
            this.authDataId = authDataId;
            this.metaDataId = metaDataId;
            this.proxySelectorDataId = proxySelectorDataId;
            this.discoveryDataId = discoveryDataId;
        }

        /**
         * pluginDataId.
         *
         * @return PluginDataId
         */
        public String getPluginDataId() {
            return pluginDataId;
        }

        /**
         * selectorDataId.
         *
         * @return SelectorDataId
         */
        public String getSelectorDataId() {
            return selectorDataId;
        }

        /**
         * ruleDataId.
         *
         * @return RuleDataId
         */
        public String getRuleDataId() {
            return ruleDataId;
        }

        /**
         * authDataId.
         *
         * @return AuthDataId
         */
        public String getAuthDataId() {
            return authDataId;
        }

        /**
         * metaDataId.
         *
         * @return MetaDataId
         */
        public String getMetaDataId() {
            return metaDataId;
        }

        /**
         * get proxySelectorDataId.
         *
         * @return proxySelectorDataId
         */
        public String getProxySelectorDataId() {
            return proxySelectorDataId;
        }

        /**
         * get discoveryDataId.
         *
         * @return discoveryDataId
         */
        public String getDiscoveryDataId() {
            return discoveryDataId;
        }

    }

}
