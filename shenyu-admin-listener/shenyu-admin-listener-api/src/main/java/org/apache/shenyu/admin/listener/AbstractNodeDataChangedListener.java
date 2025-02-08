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
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReentrantLock;
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
        if (CollectionUtils.isEmpty(changed)) {
            return;
        }
        final String namespaceId = changed.stream().map(value -> StringUtils.defaultString(value.getNamespaceId(), SYS_DEFAULT_NAMESPACE_ID)).findFirst().get();
        final String configKeyPrefix = namespaceId + DefaultNodeConstants.JOIN_POINT + changeData.getAuthDataId() + DefaultNodeConstants.JOIN_POINT;
        this.onCommonChanged(configKeyPrefix, changed, eventType, AppAuthData::getAppKey, AppAuthData.class);
        LOG.debug("[DataChangedListener] AppAuthChanged {}", configKeyPrefix);
    }

    @Override
    public void onPluginChanged(final List<PluginData> changed, final DataEventTypeEnum eventType) {
        if (CollectionUtils.isEmpty(changed)) {
            return;
        }
        final String namespaceId = changed.stream().map(value -> StringUtils.defaultString(value.getNamespaceId(), SYS_DEFAULT_NAMESPACE_ID)).findFirst().get();
        final String configKeyPrefix = namespaceId + DefaultNodeConstants.JOIN_POINT + changeData.getPluginDataId() + DefaultNodeConstants.JOIN_POINT;
        this.onCommonChanged(configKeyPrefix, changed, eventType, PluginData::getName, PluginData.class);
        LOG.debug("[DataChangedListener] PluginChanged {}", configKeyPrefix);
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

                    if (Objects.nonNull(configDataNames) && configDataNames.size() > changedList.size()) {
                        configDataNames.removeAll(changeNames);
                        configDataNames.forEach(this::delConfig);
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
        if (CollectionUtils.isEmpty(changed)) {
            return;
        }
        SelectorData selectorData = changed.stream().findFirst().orElseThrow(() -> new ShenyuException("selectorData is null"));
        final String configKeyPrefix = selectorData.getNamespaceId() + DefaultNodeConstants.JOIN_POINT + changeData.getSelectorDataId() + DefaultNodeConstants.JOIN_POINT;
        this.onCommonMultiChanged(changed, eventType, configKeyPrefix, SelectorData::getPluginName, SelectorData::getId);
        LOG.debug("[DataChangedListener] SelectorChanged {}", configKeyPrefix);
    }

    @Override
    public void onRuleChanged(final List<RuleData> changed, final DataEventTypeEnum eventType) {
        if (CollectionUtils.isEmpty(changed)) {
            return;
        }
        final String namespaceId = changed.stream().map(value -> StringUtils.defaultString(value.getNamespaceId(), SYS_DEFAULT_NAMESPACE_ID)).findFirst().get();
        final String configKeyPrefix = namespaceId + DefaultNodeConstants.JOIN_POINT + changeData.getRuleDataId() + DefaultNodeConstants.JOIN_POINT;
        this.onCommonMultiChanged(changed, eventType,
                configKeyPrefix,
                ruleData -> String.join(DefaultNodeConstants.JOIN_POINT, ruleData.getPluginName(), ruleData.getSelectorId()),
                RuleData::getId);
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
