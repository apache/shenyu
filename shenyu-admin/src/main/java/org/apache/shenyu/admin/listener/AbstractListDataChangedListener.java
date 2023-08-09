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
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * AbstractNodeDataChangedListener.
 */
public abstract class AbstractListDataChangedListener implements DataChangedListener {

    private static final Logger LOG = LoggerFactory.getLogger(AbstractListDataChangedListener.class);

    private final ChangeData changeData;

    /**
     * AbstractListDataChangedListener.
     *
     * @param changeData changeData
     */
    protected AbstractListDataChangedListener(final ChangeData changeData) {
        this.changeData = changeData;
    }

    @Override
    public void onAppAuthChanged(final List<AppAuthData> changed, final DataEventTypeEnum eventType) {
        final String configKeyPrefix = changeData.getAuthDataId() + ".";
        this.onCommonChanged(configKeyPrefix, changed, eventType, AppAuthData::getAppKey, AppAuthData.class);
        LOG.debug("[DataChangedListener] AppAuthChanged {}", configKeyPrefix);
    }

    @Override
    public void onPluginChanged(final List<PluginData> changed, final DataEventTypeEnum eventType) {
        final String configKeyPrefix = changeData.getPluginDataId() + ".";
        this.onCommonChanged(configKeyPrefix, changed, eventType, PluginData::getName, PluginData.class);
        LOG.debug("[DataChangedListener] PluginChanged {}", configKeyPrefix);
    }

    @Override
    public void onMetaDataChanged(final List<MetaData> changed, final DataEventTypeEnum eventType) {
        final String configKeyPrefix = changeData.getMetaDataId() + ".";
        this.onCommonChanged(configKeyPrefix, changed, eventType, MetaData::getId, MetaData.class);
        LOG.debug("[DataChangedListener] MetaDataChanged {}", changeData.getMetaDataId());
    }

    /**
     * onCommonChanged.
     * save to configuration center common methods.
     * examples data:
     *  meta.list
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
        final List<String> changeNames = changedList.stream().map(mapperToKey).collect(Collectors.toList());
        switch (eventType) {
            case DELETE:
                changedList.stream().map(mapperToKey).forEach(removeKey -> {
                    delConfig(configKeyPrefix + removeKey);
                });
                delChangedData(configKeyPrefix, changeNames);
                break;
            case REFRESH:
            case MYSELF:
                final List<String> configDataNames = this.getConfigDataNames(configKeyPrefix, changedList, mapperToKey, tClass);
                changedList.forEach(changedData -> {
                    if (configDataNames.contains(mapperToKey.apply(changedData))) {
                        delConfig(configKeyPrefix + mapperToKey.apply(changedData));
                    } else {
                        publishConfig(configKeyPrefix + mapperToKey.apply(changedData), changedData);
                    }
                });
                publishConfig(configKeyPrefix + "list", changeNames);
                break;
            default:
                changedList.forEach(changedData -> publishConfig(configKeyPrefix + mapperToKey.apply(changedData), changedData));
                putChangeData(configKeyPrefix, changeNames);
                break;
        }
    }

    private void putChangeData(final String configKeyPrefix, final List<String> changeNames) {
        final String dataList2 = Optional.ofNullable(getConfig(configKeyPrefix + "list")).orElse("[]");
        final List<String> orginList = GsonUtils.getInstance().fromList(dataList2, String.class);
        orginList.addAll(changeNames.stream().filter(changeName -> !orginList.contains(changeName)).collect(Collectors.toList()));
        publishConfig(configKeyPrefix + "list", orginList);
    }

    private void delChangedData(final String configKeyPrefix, final List<String> changeNames) {
        final String dataList = Optional.ofNullable(getConfig(configKeyPrefix + "list")).orElse("[]");
        final List<String> orginList = GsonUtils.getInstance().fromList(dataList, String.class);
        orginList.removeAll(changeNames);
        publishConfig(configKeyPrefix + "list", orginList);
    }

    @Override
    public void onSelectorChanged(final List<SelectorData> changed, final DataEventTypeEnum eventType) {
        final String configKeyPrefix = changeData.getSelectorDataId() + ".";
        this.onCommonMultiChanged(changed, eventType, configKeyPrefix, SelectorData::getPluginName, SelectorData::getId);
        LOG.debug("[DataChangedListener] SelectorChanged {}", configKeyPrefix);
    }

    @Override
    public void onRuleChanged(final List<RuleData> changed, final DataEventTypeEnum eventType) {
        final String configKeyPrefix = changeData.getRuleDataId() + ".";
        this.onCommonMultiChanged(changed, eventType, configKeyPrefix, RuleData::getSelectorId, RuleData::getId);
        LOG.debug("[DataChangedListener] RuleChanged {}", changeData.getRuleDataId());
    }

    /**
     * onCommonMultiChanged.
     * save to configuration center common multi methods.
     * examples data:
     *  selector.key1
     *   -> selector.key1.value1
     *   -> selector.key1.value2
     *   -> selector.key1.value3
     *  selector.key2
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
        final Map<String, List<String>> pluginNameToSelectorIdMap = changedList.stream()
                .collect(Collectors.groupingBy(mappingKey, Collectors.mapping(mappingValue, Collectors.toList())));
        switch (eventType) {
            case DELETE:
                changedList.forEach(changedData -> {
                    delConfig(configKeyPrefix + mappingKey.apply(changedData) + "." + mappingValue.apply(changedData));
                });
                this.delChangedMapToList(pluginNameToSelectorIdMap, configKeyPrefix);
                break;
            case REFRESH:
            case MYSELF:
            default:
                changedList.forEach(changedData -> {
                    publishConfig(configKeyPrefix + mappingKey.apply(changedData) + "." + mappingValue.apply(changedData), changedData);
                });
                this.putChangedMapToList(pluginNameToSelectorIdMap, configKeyPrefix);
                break;
        }
    }

    private void putChangedMapToList(final Map<String, List<String>> stringListMap, final String configKeyPrefix) {
        stringListMap.forEach((key, listIds) -> {
            final String dataList = Optional.ofNullable(getConfig(configKeyPrefix + key + ".list"))
                    .orElse("[]");
            final List<String> selectorIds = GsonUtils.getInstance().fromList(dataList, String.class);
            selectorIds.addAll(listIds.stream().filter(selectorId -> !selectorIds.contains(selectorId)).collect(Collectors.toList()));
            if (ObjectUtils.isEmpty(selectorIds)) {
                delConfig(configKeyPrefix + key + ".list");
            } else {
                publishConfig(configKeyPrefix + key + ".list", selectorIds);
            }
        });
    }

    private void delChangedMapToList(final Map<String, List<String>> stringListMap, final String configKeyPrefix) {
        stringListMap.forEach((key, listIds) -> {
            final String dataList = Optional.ofNullable(getConfig(configKeyPrefix + key + ".list"))
                    .orElse("[]");
            final List<String> selectorIds = GsonUtils.getInstance().fromList(dataList, String.class);
            selectorIds.removeAll(listIds);
            if (ObjectUtils.isEmpty(selectorIds)) {
                delConfig(configKeyPrefix + key + ".list");
            } else {
                publishConfig(configKeyPrefix + key + ".list", selectorIds);
            }
        });
    }

    @Override
    public void onProxySelectorChanged(final List<ProxySelectorData> changed, final DataEventTypeEnum eventType) {
        final String configKeyPrefix = changeData.getProxySelectorDataId() + ".";
        this.onCommonMultiChanged(changed, eventType, configKeyPrefix, ProxySelectorData::getPluginName, ProxySelectorData::getName);
        LOG.debug("[DataChangedListener] ProxySelectorChanged {}", changeData.getProxySelectorDataId());
    }

    @Override
    public void onDiscoveryUpstreamChanged(final List<DiscoverySyncData> changed, final DataEventTypeEnum eventType) {
        final String configKeyPrefix = changeData.getDiscoveryDataId() + ".";
        this.onCommonMultiChanged(changed, eventType, configKeyPrefix, DiscoverySyncData::getPluginName, DiscoverySyncData::getSelectorName);
        LOG.debug("[DataChangedListener] DiscoveryUpstreamChanged {}", changeData.getDiscoveryDataId());
    }

    /**
     * publishConfig.
     *
     * @param dataId dataId
     * @param data   data
     */
    public abstract void publishConfig(String dataId, Object data);

    /**
     * delConfig.
     *
     * @param dataId dataId
     */
    public void delConfig(final String dataId) {

    }

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
     * @param authDataId authDataId
     * @param changed changed
     * @param mapper mapper
     * @param tClass tClass
     * @return {@link List }
     */
    private <T> List<String> getConfigDataNames(final String authDataId, final List<T> changed,
                                                final Function<? super T, ? extends String> mapper, final Class<T> tClass) {
        final List<String> names = changed.stream().map(mapper).collect(Collectors.toList());
        return names.stream().map(pluginName -> this.getConfig(authDataId + ".info." + pluginName))
                .filter(StringUtils::hasText)
                .map(configStr -> GsonUtils.getInstance().fromJson(configStr, tClass))
                .map(mapper)
                .collect(Collectors.toList());
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
