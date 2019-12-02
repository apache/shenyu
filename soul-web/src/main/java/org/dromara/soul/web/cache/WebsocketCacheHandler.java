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
import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.enums.DataEventTypeEnum;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.web.plugin.dubbo.ApplicationConfigCache;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * The type Websocket cache handler.
 *
 * @author xiaoyu(Myth)
 */
@SuppressWarnings("all")
class WebsocketCacheHandler extends CommonCacheHandler {

    /**
     * Handle plugin.
     *
     * @param pluginDataList the plugin data list
     * @param eventType      the event type
     */
    void handlePlugin(final List<PluginData> pluginDataList, final String eventType) {
        if (CollectionUtils.isNotEmpty(pluginDataList)) {
            DataEventTypeEnum eventTypeEnum = DataEventTypeEnum.acquireByName(eventType);
            switch (eventTypeEnum) {
                case REFRESH:
                case MYSELF:
                    PLUGIN_MAP.clear();
                    configPlugin(pluginDataList);
                    pluginDataList.forEach(e -> PLUGIN_MAP.put(e.getName(), e));
                    break;
                case DELETE:
                    pluginDataList.forEach(e -> PLUGIN_MAP.remove(e.getName()));
                case UPDATE:
                case CREATE:
                    configPlugin(pluginDataList);
                    pluginDataList.forEach(e -> PLUGIN_MAP.put(e.getName(), e));
                    break;
                default:
                    break;
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
                case MYSELF:
                    SELECTOR_MAP.clear();
                    Map<String, List<SelectorData>> allMap =
                            selectorDataList
                                    .stream()
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
                    selectorDataList.forEach(this::deleteSelectorData);
                    break;
                case CREATE:
                case UPDATE:
                    selectorDataList.forEach(this::cacheSelectorData);
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
                case MYSELF:
                    RULE_MAP.clear();
                    Map<String, List<RuleData>> allMap =
                            ruleDataList
                                    .stream()
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
                case UPDATE:
                    ruleDataList.forEach(this::cacheRuleData);
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
            DataEventTypeEnum eventTypeEnum = DataEventTypeEnum.acquireByName(eventType);
            switch (eventTypeEnum) {
                case REFRESH:
                case MYSELF:
                    AUTH_MAP.clear();
                    appAuthDataList.forEach(e -> AUTH_MAP.put(e.getAppKey(), e));
                    break;
                case DELETE:
                    appAuthDataList.forEach(e -> AUTH_MAP.remove(e.getAppKey()));
                case UPDATE:
                case CREATE:
                    appAuthDataList.forEach(e -> AUTH_MAP.put(e.getAppKey(), e));
                    break;
                default:
                    break;
            }
        }
    }

    void handleMetaData(final List<MetaData> metaDataList, final String eventType) {
        if (CollectionUtils.isNotEmpty(metaDataList)) {
            DataEventTypeEnum eventTypeEnum = DataEventTypeEnum.acquireByName(eventType);
            switch (eventTypeEnum) {
                case REFRESH:
                case MYSELF:
                    initDubboRef(metaDataList);
                    metaDataList.forEach(e -> META_DATA.put(e.getPath(), e));
                    break;
                case DELETE:
                    metaDataList.forEach(e -> {
                        ApplicationConfigCache.getInstance().invalidate(e.getServiceName());
                        META_DATA.remove(e.getPath());
                    });
                    break;
                case UPDATE:
                case CREATE:
                    initDubboRef(metaDataList);
                    metaDataList.forEach(e -> META_DATA.put(e.getPath(), e));
                    break;
                default:
                    break;
            }
        }
    }

}
