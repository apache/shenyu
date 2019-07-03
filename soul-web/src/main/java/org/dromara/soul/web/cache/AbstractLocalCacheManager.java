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

import com.google.common.collect.Maps;
import org.apache.commons.collections4.CollectionUtils;
import org.dromara.soul.common.dto.AppAuthData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.enums.PluginEnum;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentMap;
import java.util.stream.Collectors;

/**
 * Implements the main method of LookupCacheManager, providing an API for updating cache operations.
 *
 * @author huangxiaofeng
 */
public abstract class AbstractLocalCacheManager implements LocalCacheManager {

    /**
     * pluginName -> PluginData.
     */
    static final ConcurrentMap<String, PluginData> PLUGIN_MAP = Maps.newConcurrentMap();

    /**
     * pluginName -> SelectorData.
     */
    static final ConcurrentMap<String, List<SelectorData>> SELECTOR_MAP = Maps.newConcurrentMap();

    /**
     * selectorId -> RuleData.
     */
    static final ConcurrentMap<String, List<RuleData>> RULE_MAP = Maps.newConcurrentMap();

    /**
     * appKey -> AppAuthData.
     */
    static final ConcurrentMap<String, AppAuthData> AUTH_MAP = Maps.newConcurrentMap();

    private static final Logger LOGGER = LoggerFactory.getLogger(AbstractLocalCacheManager.class);

    /**
     * acquire AppAuthData by appKey with AUTH_MAP container.
     *
     * @param appKey this is appKey.
     * @return AppAuthData {@linkplain AppAuthData}
     */
    @Override
    public AppAuthData findAuthDataByAppKey(final String appKey) {
        return AUTH_MAP.get(appKey);
    }

    /**
     * acquire PluginData by pluginName with PLUGIN_MAP container.
     *
     * @param pluginName this is plugin name.
     * @return PluginData {@linkplain  PluginData}
     */
    @Override
    public PluginData findPluginByName(final String pluginName) {
        return PLUGIN_MAP.get(pluginName);
    }

    /**
     * acquire SelectorData list  by pluginName with  SELECTOR_MAP HashMap container.
     *
     * @param pluginName this is plugin name.
     * @return SelectorData list {@linkplain  SelectorData}
     */
    @Override
    public List<SelectorData> findSelectorByPluginName(final String pluginName) {
        return SELECTOR_MAP.get(pluginName);
    }

    /**
     * acquire RuleData list by selectorId with  RULE_MAP HashMap container.
     *
     * @param selectorId this is selectorId.
     * @return RuleData list {@linkplain  RuleData}
     */
    @Override
    public List<RuleData> findRuleBySelectorId(final String selectorId) {
        return RULE_MAP.get(selectorId);
    }


    /**
     * Flush all plugin. If the collection is empty, the cache will be emptied.
     *
     * @param pluginConfig the plugin config
     */
    void flushAllPlugin(final List<PluginData> pluginConfig) {
        if (CollectionUtils.isEmpty(pluginConfig)) {
            LOGGER.info("clear all plugin cache, old cache:{}", PLUGIN_MAP);
            PLUGIN_MAP.clear();
        } else {
            PLUGIN_MAP.clear();
            pluginConfig.forEach(pluginData -> PLUGIN_MAP.put(pluginData.getName(), pluginData));
        }
    }

    /**
     * Flush all app auth.
     *
     * @param appAuthConfig the app auth config
     */
    void flushAllAppAuth(final List<AppAuthData> appAuthConfig) {
        if (CollectionUtils.isEmpty(appAuthConfig)) {
            LOGGER.info("clear all appAuth cache, old cache:{}", AUTH_MAP);
            AUTH_MAP.clear();
        } else {
            AUTH_MAP.clear();
            appAuthConfig.forEach(appAuth -> AUTH_MAP.put(appAuth.getAppKey(), appAuth));
        }
    }

    /**
     * Flush all rule.
     *
     * @param ruleConfig the rule config
     */
    void flushAllRule(final List<RuleData> ruleConfig) {
        if (CollectionUtils.isEmpty(ruleConfig)) {
            LOGGER.info("clear all rule cache, old cache:{}", RULE_MAP);
            RULE_MAP.clear();
        } else {
            // group by selectorId, then sort by sort value
            Map<String, List<RuleData>> selectorToRules = ruleConfig.stream().collect(Collectors.groupingBy(RuleData::getSelectorId));
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
     * Flush all selector.
     *
     * @param selectorConfig the selector config
     */
    void flushAllSelector(final List<SelectorData> selectorConfig) {
        if (CollectionUtils.isEmpty(selectorConfig)) {
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
            selectorConfig.forEach(selector -> {
                if (PluginEnum.DIVIDE.getName().equals(selector.getPluginName())) {
                    UpstreamCacheManager.submit(selector);
                }
            });

            // group by pluginName, then sort by sort value
            Map<String, List<SelectorData>> pluginNameToSelectors = selectorConfig.stream().collect(Collectors.groupingBy(SelectorData::getPluginName));
            pluginNameToSelectors.keySet().forEach(pluginName -> {
                List<SelectorData> sorted = pluginNameToSelectors.get(pluginName).stream()
                        .sorted(Comparator.comparing(SelectorData::getSort)).collect(Collectors.toList());
                pluginNameToSelectors.put(pluginName, sorted);
            });
            SELECTOR_MAP.clear();
            SELECTOR_MAP.putAll(pluginNameToSelectors);
        }
    }

}
