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

package org.dromara.soul.admin.listener.nacos;

import com.alibaba.nacos.api.config.ConfigService;
import com.google.common.collect.Maps;
import lombok.SneakyThrows;
import org.dromara.soul.admin.listener.DataChangedListener;
import org.dromara.soul.common.dto.AppAuthData;
import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.enums.DataEventTypeEnum;
import org.dromara.soul.common.utils.GsonUtils;

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
 * Use nacos to push data changes.
 *
 * @author chenxj
 */
public class NacosDataChangedListener implements DataChangedListener {

    private static final ConcurrentMap<String, PluginData> PLUGIN_MAP = Maps.newConcurrentMap();

    private static final ConcurrentMap<String, List<SelectorData>> SELECTOR_MAP = Maps.newConcurrentMap();

    private static final ConcurrentMap<String, List<RuleData>> RULE_MAP = Maps.newConcurrentMap();

    private static final ConcurrentMap<String, AppAuthData> AUTH_MAP = Maps.newConcurrentMap();

    private static final ConcurrentMap<String, MetaData> META_DATA = Maps.newConcurrentMap();

    private static final Comparator<SelectorData> SELECTOR_DATA_COMPARATOR = Comparator.comparing(SelectorData::getSort);

    private static final Comparator<RuleData> RULE_DATA_COMPARATOR = Comparator.comparing(RuleData::getSort);

    private static final String GROUP = "DEFAULT_GROUP";

    private static final String PLUGIN_DATA_ID = "soul.plugin.json";

    private static final String SELECTOR_DATA_ID = "soul.selector.json";

    private static final String RULE_DATA_ID = "soul.rule.json";

    private static final String AUTH_DATA_ID = "soul.auth.json";

    private static final String META_DATA_ID = "soul.meta.json";

    private final ConfigService configService;

    public NacosDataChangedListener(final ConfigService configService) {
        this.configService = configService;
    }

    @SneakyThrows
    private void publishConfig(final String dataId, final Object data) {
        configService.publishConfig(dataId, GROUP, GsonUtils.getInstance().toJson(data));
    }

    @Override
    @SneakyThrows
    public void onAppAuthChanged(final List<AppAuthData> changed, final DataEventTypeEnum eventType) {
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
        publishConfig(AUTH_DATA_ID, AUTH_MAP);
    }

    @Override
    public void onPluginChanged(final List<PluginData> changed, final DataEventTypeEnum eventType) {
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
        publishConfig(PLUGIN_DATA_ID, PLUGIN_MAP);
    }

    @Override
    public void onSelectorChanged(final List<SelectorData> changed, final DataEventTypeEnum eventType) {
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
                Set<String> set = new HashSet<>(SELECTOR_MAP.keySet());
                changed.forEach(selector -> {
                    set.remove(selector.getPluginName());
                    List<SelectorData> ls = SELECTOR_MAP
                            .getOrDefault(selector.getPluginName(), new ArrayList<>())
                            .stream()
                            .sorted(SELECTOR_DATA_COMPARATOR)
                            .collect(Collectors.toList());
                    SELECTOR_MAP.put(selector.getPluginName(), ls);
                });
                SELECTOR_MAP.keySet().removeAll(set);
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
        publishConfig(SELECTOR_DATA_ID, SELECTOR_MAP);
    }

    @Override
    public void onMetaDataChanged(final List<MetaData> changed, final DataEventTypeEnum eventType) {
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
        publishConfig(META_DATA_ID, META_DATA);
    }

    @Override
    public void onRuleChanged(final List<RuleData> changed, final DataEventTypeEnum eventType) {
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
                Set<String> set = new HashSet<>(RULE_MAP.keySet());
                changed.forEach(rule -> {
                    set.remove(rule.getSelectorId());
                    List<RuleData> ls = RULE_MAP
                            .getOrDefault(rule.getSelectorId(), new ArrayList<>())
                            .stream()
                            .sorted(RULE_DATA_COMPARATOR)
                            .collect(Collectors.toList());
                    RULE_MAP.put(rule.getSelectorId(), ls);
                });
                RULE_MAP.keySet().removeAll(set);
                break;
            default:
                changed.forEach(rule -> {
                    Map<String, RuleData> currentData = RULE_MAP
                            .getOrDefault(rule.getSelectorId(), new ArrayList<>())
                            .parallelStream()
                            .collect(Collectors.toMap(RuleData::getId, v -> v));
                    currentData.put(rule.getId(), rule);
                    RULE_MAP.put(rule.getSelectorId(),
                            currentData
                                    .values()
                                    .parallelStream()
                                    .sorted(RULE_DATA_COMPARATOR)
                                    .collect(Collectors.toList()));
                });
                break;
        }

        publishConfig(RULE_DATA_ID, RULE_MAP);
    }
}
