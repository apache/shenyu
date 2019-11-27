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
import org.dromara.soul.common.enums.PluginEnum;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * The type Http cache handler.
 *
 * @author xiaoyu(Myth)
 */
class HttpCacheHandler extends AbstractLocalCacheManager {

    private static final Logger LOGGER = LoggerFactory.getLogger(HttpCacheHandler.class);

    /**
     * Flush all plugin. If the collection is empty, the cache will be emptied.
     *
     * @param pluginDataList the plugin config
     */
    void flushAllPlugin(final List<PluginData> pluginDataList) {
        if (CollectionUtils.isEmpty(pluginDataList)) {
            LOGGER.info("clear all plugin cache, old cache:{}", PLUGIN_MAP);
            PLUGIN_MAP.clear();
        } else {
            PLUGIN_MAP.clear();
            configPlugin(pluginDataList);
            pluginDataList.forEach(pluginData -> PLUGIN_MAP.put(pluginData.getName(), pluginData));
        }
    }

    /**
     * Flush all selector.
     *
     * @param selectorDataList the selector config
     */
    void flushAllSelector(final List<SelectorData> selectorDataList) {
        if (CollectionUtils.isEmpty(selectorDataList)) {
            LOGGER.info("clear all selector cache, old cache:{}", SELECTOR_MAP);
            SELECTOR_MAP.keySet().forEach(UpstreamCacheManager::removeByKey);
            SELECTOR_MAP.clear();
        } else {
            // update cache for UpstreamCacheManager
            SELECTOR_MAP.values().forEach(selectors -> selectors.forEach(selector -> {
                if (PluginEnum.DIVIDE.getName().equals(selector.getPluginName())) {
                    UpstreamCacheManager.removeByKey(selector.getId());
                }
            }));
            // 是否需要刷新UpstreamCacheManager？
            UpstreamCacheManager.clear();
            selectorDataList.forEach(selector -> {
                if (PluginEnum.DIVIDE.getName().equals(selector.getPluginName())) {
                    UpstreamCacheManager.submit(selector);
                }
            });

            // group by pluginName, then sort by sort value
            Map<String, List<SelectorData>> pluginNameToSelectors = selectorDataList.stream()
                    .filter(Objects::nonNull)
                    .collect(Collectors.groupingBy(SelectorData::getPluginName,
                            Collectors.toCollection(ArrayList::new)));

            pluginNameToSelectors.keySet().forEach(pluginName -> {
                List<SelectorData> sorted = pluginNameToSelectors.get(pluginName).stream()
                        .sorted(Comparator.comparing(SelectorData::getSort)).collect(Collectors.toList());
                pluginNameToSelectors.put(pluginName, sorted);
            });
            SELECTOR_MAP.clear();
            SELECTOR_MAP.putAll(pluginNameToSelectors);
        }
    }

    /**
     * Flush all rule.
     *
     * @param ruleDataList the rule config
     */
    void flushAllRule(final List<RuleData> ruleDataList) {
        if (CollectionUtils.isEmpty(ruleDataList)) {
            LOGGER.info("clear all rule cache, old cache:{}", RULE_MAP);
            RULE_MAP.clear();
        } else {
            // group by selectorId, then sort by sort value
            Map<String, List<RuleData>> selectorToRules = ruleDataList.stream()
                    .collect(Collectors.groupingBy(RuleData::getSelectorId));
            selectorToRules.keySet().forEach(selectorId -> {
                List<RuleData> sorted = selectorToRules.get(selectorId).stream()
                        .sorted(Comparator.comparing(RuleData::getSort)).collect(Collectors.toList());
                selectorToRules.put(selectorId, sorted);
            });
            RULE_MAP.clear();
            RULE_MAP.putAll(selectorToRules);
        }
    }

    /**
     * Flush all app auth.
     *
     * @param appAuthDataList the app auth config
     */
    void flushAllAppAuth(final List<AppAuthData> appAuthDataList) {
        if (CollectionUtils.isEmpty(appAuthDataList)) {
            LOGGER.info("clear all appAuth cache, old cache:{}", AUTH_MAP);
            AUTH_MAP.clear();
        } else {
            AUTH_MAP.clear();
            appAuthDataList.forEach(appAuth -> AUTH_MAP.put(appAuth.getAppKey(), appAuth));
        }
    }

    void flushMetaData(final List<MetaData> metaDataList) {
        if (CollectionUtils.isEmpty(metaDataList)) {
            LOGGER.info("clear all metaDataList cache, old cache:{}", AUTH_MAP);
            META_DATA.clear();
        } else {
            initDubboRef(metaDataList);
            metaDataList.forEach(metaData -> META_DATA.put(metaData.getPath(), metaData));
        }
    }
}
