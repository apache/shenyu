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

package org.apache.shenyu.admin.scale.monitor.subject.cache;

import org.apache.shenyu.admin.model.entity.ScaleRuleDO;
import org.springframework.stereotype.Component;

import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

@Component
public class ScaleRuleCache {

    private final ConcurrentHashMap<String, ScaleRuleDO> ruleCache = new ConcurrentHashMap<>();

    /**
     * initializeCache.
     *
     * @param rules rules
     */
    public void initialize(final List<ScaleRuleDO> rules) {
        ruleCache.clear();
        rules.forEach(rule -> ruleCache.put(rule.getId(), rule));
    }

    /**
     * addOrUpdateRuleToCache.
     *
     * @param rule rule
     */
    public void addOrUpdateRuleToCache(final ScaleRuleDO rule) {
        ruleCache.put(rule.getId(), rule);
    }

    /**
     * removeRulesFromCache.
     *
     * @param metricNames metricNames
     */
    public void removeRulesFromCache(final List<String> metricNames) {
        ruleCache.forEach((id, rule) -> {
            if (metricNames.contains(rule.getMetricName())) {
                ruleCache.remove(id, rule);
            }
        });
    }

    /**
     * Removes rules from cache by their primary keys.
     *
     * @param ids rule primary keys
     */
    public void removeRulesByIdsFromCache(final List<String> ids) {
        ids.forEach(ruleCache::remove);
    }

    /**
     * getAllRules.
     *
     * @return List
     */
    public List<ScaleRuleDO> getAllRules() {
        return ruleCache.values().stream().toList();
    }

    /**
     * Gets the highest-priority rule for a metric, breaking equal priorities by rule ID.
     *
     * @param metricName metricName
     * @return ScaleRuleDO
     */
    public ScaleRuleDO getRuleByMetricName(final String metricName) {
        return ruleCache.values().stream()
                .filter(rule -> Objects.equals(metricName, rule.getMetricName()))
                .min(Comparator.comparing(ScaleRuleDO::getSort, Comparator.nullsLast(Integer::compareTo))
                        .thenComparing(ScaleRuleDO::getId))
                .orElse(null);
    }
}
