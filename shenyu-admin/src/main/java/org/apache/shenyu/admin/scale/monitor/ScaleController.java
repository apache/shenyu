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

package org.apache.shenyu.admin.scale.monitor;

import org.apache.shenyu.admin.scale.collector.provider.MetricData;
import org.apache.shenyu.admin.scale.monitor.helper.ScaleRuleEvaluator;
import org.apache.shenyu.admin.scale.scaler.K8sScaler;
import org.apache.shenyu.admin.scale.scaler.ScaleAction;

public class ScaleController implements Observer{
    private final K8sScaler k8sScaler;
    private final ScaleRuleEvaluator ruleEvaluator;

    public ScaleController(K8sScaler k8sScaler, ScaleRuleEvaluator ruleEvaluator) {
        this.k8sScaler = k8sScaler;
        this.ruleEvaluator = ruleEvaluator;
    }

    @Override
    public void update(MetricData metricData) {
        try {
            // 根据最新的 Metrics 数据评估规则并执行扩缩容
            ScaleAction action = ruleEvaluator.evaluate(metricData);

            if (action != null) {
                k8sScaler.scale(action);
            }
        } catch (Exception e) {
            // 处理异常
            e.printStackTrace();
        }
    }
}
