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

package org.apache.shenyu.admin.listener.consul;

import com.ecwid.consul.v1.ConsulClient;
import com.ecwid.consul.v1.Response;
import com.ecwid.consul.v1.kv.model.GetValue;
import com.google.common.collect.Maps;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.apache.shenyu.admin.listener.DataChangedListener;
import org.apache.shenyu.common.constant.ConsulConstants;
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
 *  Use Consul to push data changes.
 */
public class ConsulDataChangedListener implements DataChangedListener {

    private static final Logger LOG = LoggerFactory.getLogger(ConsulDataChangedListener.class);

    private static final ConcurrentMap<String, PluginData> PLUGIN_MAP = Maps.newConcurrentMap();

    private static final ConcurrentMap<String, List<SelectorData>> SELECTOR_MAP = Maps.newConcurrentMap();

    private static final ConcurrentMap<String, List<RuleData>> RULE_MAP = Maps.newConcurrentMap();

    private static final ConcurrentMap<String, AppAuthData> AUTH_MAP = Maps.newConcurrentMap();

    private static final ConcurrentMap<String, MetaData> META_DATA = Maps.newConcurrentMap();

    private static final Comparator<SelectorData> SELECTOR_DATA_COMPARATOR = Comparator.comparing(SelectorData::getSort);

    private static final Comparator<RuleData> RULE_DATA_COMPARATOR = Comparator.comparing(RuleData::getSort);

    private final ConsulClient consulClient;

    public ConsulDataChangedListener(final ConsulClient consulClient) {
        this.consulClient = consulClient;
    }

    @Override
    public void onAppAuthChanged(final List<AppAuthData> changed, final DataEventTypeEnum eventType) {
        updateAuthMap(getConfig(ConsulConstants.AUTH_DATA));
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
        publishConfig(ConsulConstants.AUTH_DATA, AUTH_MAP);
    }

    @Override
    public void onPluginChanged(final List<PluginData> changed, final DataEventTypeEnum eventType) {
        updatePluginMap(getConfig(ConsulConstants.PLUGIN_DATA));
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
        publishConfig(ConsulConstants.PLUGIN_DATA, PLUGIN_MAP);
    }

    @Override
    public void onSelectorChanged(final List<SelectorData> changed, final DataEventTypeEnum eventType) {
        updateSelectorMap(getConfig(ConsulConstants.SELECTOR_DATA));
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
                SELECTOR_MAP.keySet().removeAll(SELECTOR_MAP.keySet());
                changed.forEach(selector -> {
                    List<SelectorData> ls = SELECTOR_MAP
                            .getOrDefault(selector.getPluginName(), new ArrayList<>())
                            .stream()
                            .sorted(SELECTOR_DATA_COMPARATOR)
                            .collect(Collectors.toList());
                    ls.add(selector);
                    SELECTOR_MAP.put(selector.getPluginName(), ls);
                });
                break;
            default:
                changed.forEach(selector -> {
                    List<SelectorData> ls = SELECTOR_MAP
                            .getOrDefault(selector.getPluginName(), new ArrayList<>())
                            .stream()
                            .filter(s -> !s.getId().equals(selector.getId()))
                            .sorted(SELECTOR_DATA_COMPARATOR)
                            .collect(Collectors.toList());
                    ls.add(selector);
                    SELECTOR_MAP.put(selector.getPluginName(), ls);
                });
                break;
        }
        publishConfig(ConsulConstants.SELECTOR_DATA, SELECTOR_MAP);
    }

    @Override
    public void onMetaDataChanged(final List<MetaData> changed, final DataEventTypeEnum eventType) {
        updateMetaDataMap(getConfig(ConsulConstants.META_DATA));
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
        publishConfig(ConsulConstants.META_DATA, META_DATA);
    }

    @Override
    public void onRuleChanged(final List<RuleData> changed, final DataEventTypeEnum eventType) {
        updateRuleMap(getConfig(ConsulConstants.RULE_DATA));
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
                RULE_MAP.keySet().removeAll(RULE_MAP.keySet());
                changed.forEach(rule -> {
                    List<RuleData> ls = RULE_MAP
                            .getOrDefault(rule.getSelectorId(), new ArrayList<>())
                            .stream()
                            .sorted(RULE_DATA_COMPARATOR)
                            .collect(Collectors.toList());
                    ls.add(rule);
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

        publishConfig(ConsulConstants.RULE_DATA, RULE_MAP);
    }

    private void publishConfig(final String dataKey, final Object data) {
        consulClient.setKVValue(dataKey, GsonUtils.getInstance().toJson(data));
    }

    private String getConfig(final String dataKey) {
        Response<GetValue> kvValue = consulClient.getKVValue(dataKey);
        return Objects.nonNull(kvValue.getValue()) ? kvValue.getValue().getDecodedValue() : ConsulConstants.EMPTY_CONFIG_DEFAULT_VALUE;
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
}
