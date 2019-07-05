/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 */

package org.dromara.soul.web.cache;

import org.apache.commons.collections4.CollectionUtils;
import org.dromara.soul.common.dto.AppAuthData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.enums.DataEventTypeEnum;
import org.dromara.soul.common.enums.PluginEnum;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * The type Websocket cache handler.
 *
 * @author xiaoyu(Myth)
 */
class WebsocketCacheHandler extends AbstractLocalCacheManager {

    /**
     * Handle plugin.
     *
     * @param pluginDataList the plugin data list
     * @param eventType      the event type
     */
    void handlePlugin(final List<PluginData> pluginDataList, final String eventType) {
        if (CollectionUtils.isNotEmpty(pluginDataList)) {
            if (eventType.equals(DataEventTypeEnum.REFRESH.name())) {
                PLUGIN_MAP.clear();
                pluginDataList.forEach(e -> PLUGIN_MAP.put(e.getName(), e));
            } else if (eventType.equals(DataEventTypeEnum.DELETE.name())) {
                for (PluginData pluginData : pluginDataList) {
                    PLUGIN_MAP.remove(pluginData.getName());
                }
            } else {
                pluginDataList.forEach(e -> PLUGIN_MAP.put(e.getName(), e));
            }
        }
    }

    /**
     * Handle selector.
     *
     * @param selectorDataList the selector data list
     * @param eventType        the event type
     */
    void handleSelector(final List<SelectorData> selectorDataList, final String eventType) {
        if (CollectionUtils.isNotEmpty(selectorDataList)) {
            DataEventTypeEnum eventTypeEnum = DataEventTypeEnum.acquireByName(eventType);
            switch (eventTypeEnum) {
                case REFRESH:
                    SELECTOR_MAP.clear();
                    Map<String, List<SelectorData>> allMap = selectorDataList.stream()
                            .filter(Objects::nonNull)
                            .collect(Collectors.groupingBy(SelectorData::getPluginName,
                                    Collectors.toCollection(ArrayList::new)));
                    SELECTOR_MAP.putAll(allMap);
                    UpstreamCacheManager.clear();
                    for (SelectorData selectorData : selectorDataList) {
                        if (selectorData.getPluginName().equals(PluginEnum.DIVIDE.getName())) {
                            UpstreamCacheManager.submit(selectorData);
                        }
                    }
                    break;
                case DELETE:
                    for (SelectorData selectorData : selectorDataList) {
                        if (selectorData.getPluginName().equals(PluginEnum.DIVIDE.getName())) {
                            UpstreamCacheManager.removeByKey(selectorData.getId());
                        }
                        List<SelectorData> existList = SELECTOR_MAP.get(selectorData.getPluginName());
                        existList.removeIf(e -> e.getId().equals(selectorData.getId()));
                    }
                    break;
                case CREATE:
                    for (SelectorData selectorData : selectorDataList) {
                        if (selectorData.getPluginName().equals(PluginEnum.DIVIDE.getName())) {
                            UpstreamCacheManager.submit(selectorData);
                        }
                        String key = selectorData.getPluginName();
                        if (SELECTOR_MAP.containsKey(key)) {
                            List<SelectorData> existList = SELECTOR_MAP.get(key);
                            existList.add(selectorData);
                            final List<SelectorData> resultList = existList.stream()
                                    .sorted(Comparator.comparing(SelectorData::getSort))
                                    .collect(Collectors.toList());
                            SELECTOR_MAP.put(key, resultList);
                        }
                    }
                    break;
                case UPDATE:
                    for (SelectorData selectorData : selectorDataList) {
                        if (selectorData.getPluginName().equals(PluginEnum.DIVIDE.getName())) {
                            UpstreamCacheManager.submit(selectorData);
                        }
                        String key = selectorData.getPluginName();
                        List<SelectorData> existList = SELECTOR_MAP.get(key);
                        final List<SelectorData> resultList = existList.stream()
                                .filter(r -> !r.getId()
                                        .equals(selectorData.getId()))
                                .collect(Collectors.toList());
                        resultList.add(selectorData);
                        final List<SelectorData> collect = existList.stream()
                                .sorted(Comparator.comparing(SelectorData::getSort))
                                .collect(Collectors.toList());
                        SELECTOR_MAP.put(key, collect);
                    }
                    break;
                default:
                    break;
            }
        }
    }

    /**
     * Handle rule.
     *
     * @param ruleDataList the rule data list
     * @param eventType    the event type
     */
    void handleRule(final List<RuleData> ruleDataList, final String eventType) {
        if (CollectionUtils.isNotEmpty(ruleDataList)) {
            DataEventTypeEnum eventTypeEnum = DataEventTypeEnum.acquireByName(eventType);
            switch (eventTypeEnum) {
                case REFRESH:
                    RULE_MAP.clear();
                    Map<String, List<RuleData>> allMap = ruleDataList.stream()
                            .filter(Objects::nonNull)
                            .collect(Collectors.groupingBy(RuleData::getSelectorId,
                                    Collectors.toCollection(ArrayList::new)));
                    RULE_MAP.putAll(allMap);
                    break;
                case DELETE:
                    for (RuleData ruleData : ruleDataList) {
                        List<RuleData> existList = RULE_MAP.get(ruleData.getSelectorId());
                        existList.removeIf(e -> e.getId().equals(ruleData.getId()));
                    }
                    break;
                case CREATE:
                    for (RuleData ruleData : ruleDataList) {
                        String key = ruleData.getSelectorId();
                        if (RULE_MAP.containsKey(key)) {
                            List<RuleData> existList = RULE_MAP.get(key);
                            existList.add(ruleData);
                            final List<RuleData> resultList = existList.stream()
                                    .sorted(Comparator.comparing(RuleData::getSort))
                                    .collect(Collectors.toList());
                            RULE_MAP.put(key, resultList);
                        }
                    }
                    break;
                case UPDATE:
                    for (RuleData ruleData : ruleDataList) {
                        String key = ruleData.getSelectorId();
                        List<RuleData> existList = RULE_MAP.get(key);
                        final List<RuleData> resultList = existList.stream()
                                .filter(r -> !r.getId()
                                        .equals(ruleData.getId()))
                                .collect(Collectors.toList());
                        resultList.add(ruleData);
                        final List<RuleData> collect = existList.stream()
                                .sorted(Comparator.comparing(RuleData::getSort))
                                .collect(Collectors.toList());
                        RULE_MAP.put(key, collect);
                    }
                    break;
                default:
                    break;
            }
        }
    }

    /**
     * Handle app auth.
     *
     * @param appAuthDataList the app auth data list
     * @param eventType       the event type
     */
    void handleAppAuth(final List<AppAuthData> appAuthDataList, final String eventType) {
        if (CollectionUtils.isNotEmpty(appAuthDataList)) {
            if (eventType.equals(DataEventTypeEnum.REFRESH.name())) {
                AUTH_MAP.clear();
                appAuthDataList.forEach(e -> AUTH_MAP.put(e.getAppKey(), e));
            } else if (eventType.equals(DataEventTypeEnum.DELETE.name())) {
                for (AppAuthData appAuthData : appAuthDataList) {
                    AUTH_MAP.remove(appAuthData.getAppKey());
                }
            } else {
                appAuthDataList.forEach(e -> AUTH_MAP.put(e.getAppKey(), e));
            }
        }
    }

}
