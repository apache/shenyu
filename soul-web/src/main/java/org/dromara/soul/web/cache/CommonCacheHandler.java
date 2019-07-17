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

import com.google.common.collect.Lists;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.enums.PluginEnum;

import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

/**
 * The type Common cache handler.
 *
 * @author xiaoyu(Myth)
 */
class CommonCacheHandler extends AbstractLocalCacheManager {


    /**
     * Delete selector data.
     *
     * @param selectorData the selector data
     */
    void deleteSelectorData(final SelectorData selectorData) {
        if (selectorData.getPluginName().equals(PluginEnum.DIVIDE.getName())) {
            UpstreamCacheManager.removeByKey(selectorData.getId());
        }
        List<SelectorData> existList = SELECTOR_MAP.get(selectorData.getPluginName());
        existList.removeIf(e -> e.getId().equals(selectorData.getId()));
    }

    /**
     * Handler selector data.
     *
     * @param selectorData the selector data
     */
    void cacheSelectorData(final SelectorData selectorData) {
        if (selectorData.getPluginName().equals(PluginEnum.DIVIDE.getName())) {
            UpstreamCacheManager.submit(selectorData);
        }
        String key = selectorData.getPluginName();
        if (SELECTOR_MAP.containsKey(key)) {
            List<SelectorData> existList = SELECTOR_MAP.get(key);
            final List<SelectorData> resultList =
                    existList.stream()
                            .filter(r -> !r.getId().equals(selectorData.getId()))
                            .collect(Collectors.toList());
            resultList.add(selectorData);
            final List<SelectorData> collect = resultList.stream()
                    .sorted(Comparator.comparing(SelectorData::getSort))
                    .collect(Collectors.toList());
            SELECTOR_MAP.put(key, collect);
        } else {
            SELECTOR_MAP.put(key, Lists.newArrayList(selectorData));
        }
    }

    /**
     * Cache rule data.
     *
     * @param ruleData the rule data
     */
    void cacheRuleData(final RuleData ruleData) {
        String key = ruleData.getSelectorId();
        if (RULE_MAP.containsKey(key)) {
            List<RuleData> existList = RULE_MAP.get(key);
            final List<RuleData> resultList =
                    existList.stream()
                            .filter(r -> !r.getId().equals(ruleData.getId()))
                            .collect(Collectors.toList());
            resultList.add(ruleData);
            final List<RuleData> collect = resultList.stream()
                    .sorted(Comparator.comparing(RuleData::getSort))
                    .collect(Collectors.toList());
            RULE_MAP.put(key, collect);
        } else {
            RULE_MAP.put(key, Lists.newArrayList(ruleData));
        }
    }
}
