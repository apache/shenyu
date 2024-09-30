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

package org.apache.shenyu.admin.scale.monitor.helper;

import org.apache.shenyu.admin.mapper.ScaleRuleMapper;
import org.apache.shenyu.admin.scale.collector.provider.MetricData;
import org.apache.shenyu.admin.scale.scaler.ScaleAction;

public class ScaleRuleEvaluator {

    private final ScaleRuleMapper scaleRuleMapper;

    public ScaleRuleEvaluator(ScaleRuleMapper scaleRuleMapper) {
        this.scaleRuleMapper = scaleRuleMapper;
    }

    public ScaleAction evaluate(MetricData metricData) {
        // 获取当前所有的规则
        List<ScaleRule> rules = scaleRuleMapper.getAllRules();

        // 逐一评估是否有匹配的规则
        for (ScaleRule rule : rules) {
            if (matchesRule(rule, metricData)) {
                // 返回对应的扩缩容动作
                return new ScaleAction(rule.getAction(), rule.getReplicaCount());
            }
        }
        return null; // 如果没有匹配的规则，则返回 null
    }

    private boolean matchesRule(ScaleRule rule, MetricData metricData) {
        // 判断 Metrics 数据是否符合规则条件
        // 例如，判断 QPS 是否超过某个阈值
        if (rule.getMetricName().equals(metricData.getMetricName()) && metricData.getValue() > rule.getThreshold()) {
            return true;
        }
        return false;
    }
}
