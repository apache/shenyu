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
import org.apache.commons.collections.CollectionUtils;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ConcurrentMap;
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
        updateAuthMap(getConfig(changeData.getAuthDataId()));
        switch (eventType) {
            case DELETE:
                changed.forEach(appAuth -> AUTH_MAP.remove(appAuth.getAppKey()));
                break;
            case REFRESH:
            case MYSELF:
                Set<String> set = new HashSet<>(AUTH_MAP.keySet());
                changed.forEach(appAuth -> {
                    set.remove(appAuth.getAppKey());
                    AUTH_MAP.put(appAuth.getAppKey(), appAuth);
                });
                AUTH_MAP.keySet().removeAll(set);
                break;
            default:
                changed.forEach(appAuth -> AUTH_MAP.put(appAuth.getAppKey(), appAuth));
                break;
        }
        publishConfig(changeData.getAuthDataId(), AUTH_MAP);
        LOG.debug("[DataChangedListener] AppAuthChanged {}", changeData.getAuthDataId());
    }

    @Override
    public void onPluginChanged(final List<PluginData> changed, final DataEventTypeEnum eventType) {
        updatePluginMap(getConfig(changeData.getPluginDataId()));
        switch (eventType) {
            case DELETE:
                changed.forEach(plugin -> PLUGIN_MAP.remove(plugin.getName()));
                break;
            case REFRESH:
            case MYSELF:
                Set<String> set = new HashSet<>(PLUGIN_MAP.keySet());
                changed.forEach(plugin -> {
                    set.remove(plugin.getName());
                    PLUGIN_MAP.put(plugin.getName(), plugin);
                });
                PLUGIN_MAP.keySet().removeAll(set);
                break;
            default:
                changed.forEach(plugin -> PLUGIN_MAP.put(plugin.getName(), plugin));
                break;
        }
        publishConfig(changeData.getPluginDataId(), PLUGIN_MAP);
        LOG.debug("[DataChangedListener] PluginChanged {}", changeData.getPluginDataId());
    }

    @Override
    public void onSelectorChanged(final List<SelectorData> changed, final DataEventTypeEnum eventType) {
        updateSelectorMap(getConfig(changeData.getSelectorDataId()));
        switch (eventType) {
            case DELETE:
                changed.forEach(selector -> {
                    List<SelectorData> ls = SELECTOR_MAP
                            .getOrDefault(selector.getPluginName(), new ArrayList<>())
                            .stream()
                            .filter(s -> !s.getId().equals(selector.getId()))
                            .sorted(SELECTOR_DATA_COMPARATOR)
                            .collect(Collectors.toList());
                    SELECTOR_MAP.put(selector.getPluginName(), ls);
                });
                break;
            case REFRESH:
            case MYSELF:
                if (CollectionUtils.isNotEmpty(changed)) {
                    SELECTOR_MAP.remove(changed.get(0).getPluginName());
                }
                changed.forEach(selector -> {
                    List<SelectorData> ls = new ArrayList<>(SELECTOR_MAP.getOrDefault(selector.getPluginName(),
                            new ArrayList<>()));
                    ls.add(selector);
                    ls.sort(SELECTOR_DATA_COMPARATOR);
                    SELECTOR_MAP.put(selector.getPluginName(), ls);
                });
                break;
            default:
                changed.forEach(selector -> {
                    List<SelectorData> ls = SELECTOR_MAP
                            .getOrDefault(selector.getPluginName(), new ArrayList<>())
                            .stream()
                            .filter(s -> !s.getId().equals(selector.getId()))
                            .collect(Collectors.toList());
                    ls.add(selector);
                    ls.sort(SELECTOR_DATA_COMPARATOR);
                    SELECTOR_MAP.put(selector.getPluginName(), ls);
                });
                break;
        }
        publishConfig(changeData.getSelectorDataId(), SELECTOR_MAP);
        LOG.debug("[DataChangedListener] SelectorChanged {}", changeData.getSelectorDataId());
    }

    @Override
    public void onMetaDataChanged(final List<MetaData> changed, final DataEventTypeEnum eventType) {
        updateMetaDataMap(getConfig(changeData.getMetaDataId()));
        switch (eventType) {
            case DELETE:
                changed.forEach(meta -> META_DATA.remove(meta.getPath()));
                break;
            case REFRESH:
            case MYSELF:
                Set<String> set = new HashSet<>(META_DATA.keySet());
                changed.forEach(meta -> {
                    set.remove(meta.getPath());
                    META_DATA.put(meta.getPath(), meta);
                });
                META_DATA.keySet().removeAll(set);
                break;
            default:
                changed.forEach(meta -> {
                    META_DATA
                            .values()
                            .stream()
                            .filter(md -> Objects.equals(md.getId(), meta.getId()))
                            .forEach(md -> META_DATA.remove(md.getPath()));

                    META_DATA.put(meta.getPath(), meta);
                });
                break;
        }
        publishConfig(changeData.getMetaDataId(), META_DATA);
        LOG.debug("[DataChangedListener] MetaDataChanged {}", changeData.getMetaDataId());
    }

    @Override
    public void onRuleChanged(final List<RuleData> changed, final DataEventTypeEnum eventType) {
        updateRuleMap(getConfig(changeData.getRuleDataId()));
        switch (eventType) {
            case DELETE:
                changed.forEach(rule -> {
                    List<RuleData> ls = RULE_MAP
                            .getOrDefault(rule.getSelectorId(), new ArrayList<>())
                            .stream()
                            .filter(s -> !s.getId().equals(rule.getId()))
                            .sorted(RULE_DATA_COMPARATOR)
                            .collect(Collectors.toList());
                    RULE_MAP.put(rule.getSelectorId(), ls);
                });
                break;
            case REFRESH:
            case MYSELF:
                Set<String> selectIdSet = changed
                        .stream()
                        .map(RuleData::getSelectorId)
                        .collect(Collectors.toSet());
                RULE_MAP.keySet().removeAll(selectIdSet);
                changed.forEach(rule -> {
                    List<RuleData> ls = new ArrayList<>(RULE_MAP.getOrDefault(rule.getSelectorId(),
                            new ArrayList<>()));
                    ls.add(rule);
                    ls.sort(RULE_DATA_COMPARATOR);
                    RULE_MAP.put(rule.getSelectorId(), ls);
                });
                break;
            default:
                changed.forEach(rule -> {
                    List<RuleData> ls = RULE_MAP
                            .getOrDefault(rule.getSelectorId(), new ArrayList<>())
                            .stream()
                            .filter(s -> !s.getId().equals(rule.getId()))
                            .collect(Collectors.toList());
                    ls.add(rule);
                    ls.sort(RULE_DATA_COMPARATOR);
                    RULE_MAP.put(rule.getSelectorId(), ls);
                });
                break;
        }
        publishConfig(changeData.getRuleDataId(), RULE_MAP);
        LOG.debug("[DataChangedListener] RuleChanged {}", changeData.getRuleDataId());
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

    /**
     * publishConfig.
     *
     * @param dataId dataId
     * @param data data
     */
    public abstract void publishConfig(String dataId, Object data);

    /**
     * getConfig.
     *
     * @param dataId dataId
     * @return {@link String}
     */
    public abstract String getConfig(String dataId);

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
         * ChangeData.
         *
         * @param pluginDataId pluginDataId
         * @param selectorDataId selectorDataId
         * @param ruleDataId ruleDataId
         * @param authDataId authDataId
         * @param metaDataId metaDataId
         */
        public ChangeData(final String pluginDataId, final String selectorDataId,
                          final String ruleDataId, final String authDataId, final String metaDataId) {
            this.pluginDataId = pluginDataId;
            this.selectorDataId = selectorDataId;
            this.ruleDataId = ruleDataId;
            this.authDataId = authDataId;
            this.metaDataId = metaDataId;
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
    }

}
