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

import java.util.List;
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
        rules.forEach(rule -> ruleCache.put(rule.getMetricName(), rule));
    }

    /**
     * addOrUpdateRuleToCache.
     *
     * @param rule rule
     */
    public void addOrUpdateRuleToCache(final ScaleRuleDO rule) {
        ruleCache.put(rule.getMetricName(), rule);
    }

    /**
     * removeRulesFromCache.
     *
     * @param metricNames metricNames
     */
    public void removeRulesFromCache(final List<String> metricNames) {
        metricNames.forEach(ruleCache::remove);
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
     * getRuleByMetricName.
     *
     * @param metricName metricName
     * @return ScaleRuleDO
     */
    public ScaleRuleDO getRuleByMetricName(final String metricName) {
        return ruleCache.get(metricName);
    }
}