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

import com.google.common.collect.Maps;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
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

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentMap;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * AbstractNodeDataChangedListener.
 */
public abstract class AbstractListDataChangedListener implements DataChangedListener {

    private static final Logger LOG = LoggerFactory.getLogger(AbstractListDataChangedListener.class);

    private static final ConcurrentMap<String, PluginData> PLUGIN_MAP = Maps.newConcurrentMap();

    private static final ConcurrentMap<String, List<SelectorData>> SELECTOR_MAP = Maps.newConcurrentMap();

    private static final ConcurrentMap<String, List<RuleData>> RULE_MAP = Maps.newConcurrentMap();

    private static final ConcurrentMap<String, AppAuthData> AUTH_MAP = Maps.newConcurrentMap();

    private static final ConcurrentMap<String, MetaData> META_DATA = Maps.newConcurrentMap();

    private static final ConcurrentMap<String, List<ProxySelectorData>> PROXY_SELECTOR_MAP = Maps.newConcurrentMap();

    private static final Comparator<SelectorData> SELECTOR_DATA_COMPARATOR = Comparator.comparing(SelectorData::getSort);

    private static final Comparator<RuleData> RULE_DATA_COMPARATOR = Comparator.comparing(RuleData::getSort);

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
        final String configKeyPrefix = changeData.getAuthDataId();
        final List<String> changeNames = changed.stream().map(AppAuthData::getAppKey).collect(Collectors.toList());
        switch (eventType) {
            case DELETE:
                changed.stream().map(AppAuthData::getAppKey).forEach(removeKey -> {
                    delConfig(configKeyPrefix + "." + removeKey);
                });
                delChangedData(configKeyPrefix, changeNames);
                break;
            case REFRESH:
            case MYSELF:
                final List<String> configDataNames = this.getConfigDataNames(configKeyPrefix, changed, AppAuthData::getAppKey, AppAuthData.class);
                changed.forEach(appAuthData -> {
                    if (configDataNames.contains(appAuthData.getAppKey())) {
                        delConfig(configKeyPrefix + "." + appAuthData.getAppKey());
                    } else {
                        publishConfig(configKeyPrefix + "." + appAuthData.getAppKey(), appAuthData);
                    }
                });
                publishConfig(configKeyPrefix + ".list", changeNames);
                break;
            default:
                changed.forEach(appAuthData -> publishConfig(configKeyPrefix + "." + appAuthData.getAppKey(), appAuthData));
                putChangeData(configKeyPrefix, changeNames);
                break;
        }

        publishConfig(configKeyPrefix, AUTH_MAP);
        LOG.debug("[DataChangedListener] AppAuthChanged {}", configKeyPrefix);
    }

    @Override
    public void onPluginChanged(final List<PluginData> changed, final DataEventTypeEnum eventType) {
        final String configKeyPrefix = changeData.getPluginDataId();
        final List<String> changeNames = changed.stream().map(PluginData::getName).collect(Collectors.toList());
        switch (eventType) {
            case DELETE:
                changed.stream().map(PluginData::getName).forEach(removeKey -> {
                    delConfig(configKeyPrefix + "." + removeKey);
                });
                delChangedData(configKeyPrefix, changeNames);
                break;
            case REFRESH:
            case MYSELF:
                final List<String> configDataNames = this.getConfigDataNames(configKeyPrefix, changed, PluginData::getName, PluginData.class);
                changed.forEach(plugin -> {
                    if (configDataNames.contains(plugin.getName())) {
                        delConfig(configKeyPrefix + "." + plugin.getName());
                    } else {
                        publishConfig(configKeyPrefix + "." + plugin.getName(), plugin);
                    }
                });
                publishConfig(configKeyPrefix + ".list", changeNames);
                break;
            default:
                changed.forEach(plugin -> publishConfig(configKeyPrefix + "." + plugin.getName(), plugin));
                putChangeData(configKeyPrefix, changeNames);
                break;
        }

        LOG.debug("[DataChangedListener] PluginChanged {}", configKeyPrefix);
    }

    private void putChangeData(final String configKeyPrefix, final List<String> changeNames) {
        final String dataList2 = Optional.ofNullable(getConfig(configKeyPrefix + ".list")).orElse("[]");
        final List<String> orginList = GsonUtils.getInstance().fromList(dataList2, String.class);
        orginList.addAll(changeNames.stream().filter(changeName -> !orginList.contains(changeName)).collect(Collectors.toList()));
        publishConfig(configKeyPrefix + ".list", orginList);
    }

    private void delChangedData(final String configKeyPrefix, final List<String> changeNames) {
        final String dataList = Optional.ofNullable(getConfig(configKeyPrefix + ".list")).orElse("[]");
        final List<String> orginList = GsonUtils.getInstance().fromList(dataList, String.class);
        orginList.removeAll(changeNames);
        publishConfig(configKeyPrefix + ".list", orginList);
    }

    @Override
    public void onMetaDataChanged(final List<MetaData> changed, final DataEventTypeEnum eventType) {
        final String configKeyPrefix = changeData.getMetaDataId();
        final List<String> changeNames = changed.stream().map(MetaData::getId).collect(Collectors.toList());
        switch (eventType) {
            case DELETE:
                changed.stream().map(MetaData::getId).forEach(removeKey -> {
                    delConfig(configKeyPrefix + "." + removeKey);
                });
                delChangedData(configKeyPrefix, changeNames);
                break;
            case REFRESH:
            case MYSELF:
                final List<String> configDataNames = this.getConfigDataNames(configKeyPrefix, changed, MetaData::getId, MetaData.class);
                changed.forEach(metaData -> {
                    if (configDataNames.contains(metaData.getId())) {
                        delConfig(configKeyPrefix + "." + metaData.getId());
                    } else {
                        publishConfig(configKeyPrefix + "." + metaData.getId(), metaData);
                    }
                });
                publishConfig(configKeyPrefix + ".list", changeNames);
                break;
            default:
                changed.forEach(metaData -> publishConfig(configKeyPrefix + "." + metaData.getId(), metaData));
                putChangeData(configKeyPrefix, changeNames);
                break;
        }

        LOG.debug("[DataChangedListener] MetaDataChanged {}", changeData.getMetaDataId());
    }

    @Override
    public void onSelectorChanged(final List<SelectorData> changed, final DataEventTypeEnum eventType) {
        final String configKeyPrefix = changeData.getSelectorDataId();
        final Map<String, List<String>> pluginNameToSelectorIdMap = changed.stream()
                .collect(Collectors.groupingBy(SelectorData::getPluginName, Collectors.mapping(SelectorData::getId, Collectors.toList())));
        switch (eventType) {
            case DELETE:
                changed.forEach(selector -> {
                    delConfig(configKeyPrefix + "." + selector.getPluginName() + "." + selector.getId());
                });
                this.delChangedMapToList(pluginNameToSelectorIdMap, configKeyPrefix);
                break;
            case REFRESH:
            case MYSELF:
            default:
                changed.forEach(selector -> {
                    publishConfig(configKeyPrefix + "." + selector.getPluginName() + "." + selector.getId(), selector);
                });
                this.putChangedMapToList(pluginNameToSelectorIdMap, configKeyPrefix);
                break;
        }

        LOG.debug("[DataChangedListener] SelectorChanged {}", configKeyPrefix);
    }

    @Override
    public void onRuleChanged(final List<RuleData> changed, final DataEventTypeEnum eventType) {
        final String configKeyPrefix = changeData.getRuleDataId();
        final Map<String, List<String>> pluginNameToSelectorIdMap = changed.stream()
                .collect(Collectors.groupingBy(RuleData::getSelectorId, Collectors.mapping(RuleData::getId, Collectors.toList())));
        switch (eventType) {
            case DELETE:
                changed.forEach(selector -> {
                    delConfig(configKeyPrefix + "." + selector.getSelectorId() + "." + selector.getId());
                });
                this.delChangedMapToList(pluginNameToSelectorIdMap, configKeyPrefix);
                break;
            case REFRESH:
            case MYSELF:
            default:
                changed.forEach(selector -> {
                    publishConfig(configKeyPrefix + "." + selector.getSelectorId() + "." + selector.getId(), selector);
                });
                this.putChangedMapToList(pluginNameToSelectorIdMap, configKeyPrefix);
                break;
        }
        LOG.debug("[DataChangedListener] RuleChanged {}", changeData.getRuleDataId());
    }

    private void putChangedMapToList(final Map<String, List<String>> stringListMap, final String configKeyPrefix) {
        stringListMap.forEach((key, listIds) -> {
            final String dataList = Optional.ofNullable(getConfig(configKeyPrefix + "." + key + ".list"))
                    .orElse("[]");
            final List<String> selectorIds = GsonUtils.getInstance().fromList(dataList, String.class);
            selectorIds.addAll(listIds.stream().filter(selectorId -> !selectorIds.contains(selectorId)).collect(Collectors.toList()));
            if (ObjectUtils.isEmpty(selectorIds)) {
                delConfig(configKeyPrefix + "." + key + ".list");
            } else {
                publishConfig(configKeyPrefix + "." + key + ".list", selectorIds);
            }
        });
    }

    private void delChangedMapToList(final Map<String, List<String>> stringListMap, final String configKeyPrefix) {
        stringListMap.forEach((key, listIds) -> {
            final String dataList = Optional.ofNullable(getConfig(configKeyPrefix + "." + key + ".list"))
                    .orElse("[]");
            final List<String> selectorIds = GsonUtils.getInstance().fromList(dataList, String.class);
            selectorIds.removeAll(listIds);
            if (ObjectUtils.isEmpty(selectorIds)) {
                delConfig(configKeyPrefix + "." + key + ".list");
            } else {
                publishConfig(configKeyPrefix + "." + key + ".list", selectorIds);
            }
        });
    }

    @Override
    public void onProxySelectorChanged(final List<ProxySelectorData> changed, final DataEventTypeEnum eventType) {
        updateRuleMap(Optional.ofNullable(getConfig(changeData.getProxySelectorDataId())).orElse("{}"));
        switch (eventType) {
            case DELETE:
                changed.forEach(proxySelectorData -> {
                    List<ProxySelectorData> ls = PROXY_SELECTOR_MAP
                            .getOrDefault(proxySelectorData.getId(), new ArrayList<>())
                            .stream()
                            .filter(s -> !s.getId().equals(proxySelectorData.getId()))
                            .collect(Collectors.toList/**/());
                    PROXY_SELECTOR_MAP.put(proxySelectorData.getId(), ls);
                });
                break;
            case REFRESH:
            case MYSELF:
                Set<String> selectIdSet = changed
                        .stream()
                        .map(proxySelectorData ->
                                proxySelectorData.getId()
                        )
                        .collect(Collectors.toSet());
                PROXY_SELECTOR_MAP.keySet().removeAll(selectIdSet);
                changed.forEach(proxySelectorData -> {
                    List<ProxySelectorData> ls = new ArrayList<>(PROXY_SELECTOR_MAP.getOrDefault(proxySelectorData.getId(),
                            new ArrayList<>()));
                    ls.add(proxySelectorData);
                    PROXY_SELECTOR_MAP.put(proxySelectorData.getId(), ls);
                });
                break;
            default:
                changed.forEach(proxySelectorData -> {
                    List<ProxySelectorData> ls = PROXY_SELECTOR_MAP
                            .getOrDefault(proxySelectorData.getId(), new ArrayList<>())
                            .stream()
                            .filter(s -> !s.getId().equals(proxySelectorData.getId()))
                            .collect(Collectors.toList());
                    ls.add(proxySelectorData);
                    PROXY_SELECTOR_MAP.put(proxySelectorData.getId(), ls);
                });
                break;
        }
        publishConfig(changeData.getProxySelectorDataId(), PROXY_SELECTOR_MAP);
        LOG.debug("[DataChangedListener] ProxySelectorChanged {}", changeData.getProxySelectorDataId());
    }

    @Override
    public void onDiscoveryUpstreamChanged(final List<DiscoverySyncData> changed, final DataEventTypeEnum eventType) {
        // need to impl
        DataChangedListener.super.onDiscoveryUpstreamChanged(changed, eventType);
    }

    private void updateAuthMap(final String configInfo) {
        JsonObject jo = GsonUtils.getInstance().fromJson(configInfo, JsonObject.class);
        Set<String> set = new HashSet<>(AUTH_MAP.keySet());
        for (Map.Entry<String, JsonElement> e : jo.entrySet()) {
            set.remove(e.getKey());
            AUTH_MAP.put(e.getKey(), GsonUtils.getInstance().fromJson(e.getValue(), AppAuthData.class));
        }
        AUTH_MAP.keySet().removeAll(set);
    }

    private void updatePluginMap(final String configInfo) {
        JsonObject jo = GsonUtils.getInstance().fromJson(configInfo, JsonObject.class);
        Set<String> set = new HashSet<>(PLUGIN_MAP.keySet());
        for (Map.Entry<String, JsonElement> e : jo.entrySet()) {
            set.remove(e.getKey());
            PLUGIN_MAP.put(e.getKey(), GsonUtils.getInstance().fromJson(e.getValue(), PluginData.class));
        }
        PLUGIN_MAP.keySet().removeAll(set);
    }

    private void updateSelectorMap(final String configInfo) {
        JsonObject jo = GsonUtils.getInstance().fromJson(configInfo, JsonObject.class);
        Set<String> set = new HashSet<>(SELECTOR_MAP.keySet());
        for (Map.Entry<String, JsonElement> e : jo.entrySet()) {
            set.remove(e.getKey());
            List<SelectorData> ls = new ArrayList<>();
            e.getValue().getAsJsonArray().forEach(je -> ls.add(GsonUtils.getInstance().fromJson(je, SelectorData.class)));
            SELECTOR_MAP.put(e.getKey(), ls);
        }
        SELECTOR_MAP.keySet().removeAll(set);
    }

    private void updateMetaDataMap(final String configInfo) {
        JsonObject jo = GsonUtils.getInstance().fromJson(configInfo, JsonObject.class);
        Set<String> set = new HashSet<>(META_DATA.keySet());
        for (Map.Entry<String, JsonElement> e : jo.entrySet()) {
            set.remove(e.getKey());
            META_DATA.put(e.getKey(), GsonUtils.getInstance().fromJson(e.getValue(), MetaData.class));
        }
        META_DATA.keySet().removeAll(set);
    }

    private void updateRuleMap(final String configInfo) {
        JsonObject jo = GsonUtils.getInstance().fromJson(configInfo, JsonObject.class);
        Set<String> set = new HashSet<>(RULE_MAP.keySet());
        for (Map.Entry<String, JsonElement> e : jo.entrySet()) {
            set.remove(e.getKey());
            List<RuleData> ls = new ArrayList<>();
            e.getValue().getAsJsonArray().forEach(je -> ls.add(GsonUtils.getInstance().fromJson(je, RuleData.class)));
            RULE_MAP.put(e.getKey(), ls);
        }
        RULE_MAP.keySet().removeAll(set);
    }

    private void updateProxySelectorMap(final String configInfo) {
        JsonObject jo = GsonUtils.getInstance().fromJson(configInfo, JsonObject.class);
        Set<String> set = new HashSet<>(PROXY_SELECTOR_MAP.keySet());
        for (Map.Entry<String, JsonElement> e : jo.entrySet()) {
            set.remove(e.getKey());
            List<ProxySelectorData> ls = new ArrayList<>();
            e.getValue().getAsJsonArray().forEach(je -> ls.add(GsonUtils.getInstance().fromJson(je, ProxySelectorData.class)));
            PROXY_SELECTOR_MAP.put(e.getKey(), ls);
        }
        PROXY_SELECTOR_MAP.keySet().removeAll(set);
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
