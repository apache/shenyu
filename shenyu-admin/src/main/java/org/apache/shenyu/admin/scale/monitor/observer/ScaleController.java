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

import org.apache.shenyu.admin.scale.collector.provider.MetricData;
import org.apache.shenyu.admin.scale.scaler.ScaleRuleEvaluator;
import org.apache.shenyu.admin.scale.scaler.K8sScaler;
import org.apache.shenyu.admin.scale.scaler.ScaleAction;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ScaleController implements Observer {

    private static final Logger LOG = LoggerFactory.getLogger(ScaleController.class);

    private final K8sScaler k8sScaler;

    private final ScaleRuleEvaluator ruleEvaluator;

    public ScaleController(final K8sScaler k8sScaler, final ScaleRuleEvaluator ruleEvaluator) {
        this.k8sScaler = k8sScaler;
        this.ruleEvaluator = ruleEvaluator;
    }

    @Override
    public void update(final MetricData metricData) {
        try {
            ScaleAction action = ruleEvaluator.evaluate(metricData);

            if (action != null) {
                k8sScaler.scale(action);
            }
        } catch (Exception e) {
            LOG.error("update observer error. cause: {} ", e.getMessage());
        }
    }
}
