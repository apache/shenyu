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

package org.apache.shenyu.admin.scale.monitor.observer;

import org.apache.shenyu.admin.model.entity.ScaleRuleDO;
import org.apache.shenyu.admin.scale.collector.provider.MetricData;
import org.apache.shenyu.admin.scale.scaler.dynamic.ScaleRuleEvaluator;
import org.apache.shenyu.admin.scale.scaler.KubernetesScaler;
import org.apache.shenyu.admin.scale.scaler.dynamic.ScaleAction;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ScaleObserver implements Observer {

    private static final Logger LOG = LoggerFactory.getLogger(ScaleObserver.class);

    private final KubernetesScaler kubernetesScaler;

    private final ScaleRuleEvaluator ruleEvaluator;

    public ScaleObserver(final KubernetesScaler kubernetesScaler, final ScaleRuleEvaluator ruleEvaluator) {
        this.kubernetesScaler = kubernetesScaler;
        this.ruleEvaluator = ruleEvaluator;
    }

    @Override
    public void update(final MetricData metricData, final ScaleRuleDO rule) {
        try {
            ScaleAction action = ruleEvaluator.evaluate(metricData, rule);
            if (action != null) {
                kubernetesScaler.scaleByAction(action);
            }
        } catch (Exception e) {
            LOG.error("update observer error. cause: {} ", e.getMessage());
        }
    }
}
