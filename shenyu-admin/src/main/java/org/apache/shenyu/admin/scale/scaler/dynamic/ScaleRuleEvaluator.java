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

package org.apache.shenyu.admin.scale.scaler.dynamic;

import org.apache.shenyu.admin.model.entity.ScaleRuleDO;
import org.apache.shenyu.admin.scale.collector.provider.MetricData;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Component
public class ScaleRuleEvaluator {

    private static final Logger LOG = LoggerFactory.getLogger(ScaleRuleEvaluator.class);

    /**
     * matches rule.
     *
     * @param rule rule
     * @param metricData metricData
     * @return ScaleAction
     */
    public ScaleAction evaluate(final MetricData metricData, final ScaleRuleDO rule) {
        double value = metricData.value();
        Double max = parseToDouble(rule.getMaximum());
        Double min = parseToDouble(rule.getMinimum());
        if (max != null && value > max) {
            return new ScaleAction(ScaleAction.ActionType.SCALE_UP);
        }
        if (min != null && value < min) {
            return new ScaleAction(ScaleAction.ActionType.SCALE_DOWN);
        }
        return null;
    }

    /**
     * parse string to double.
     *
     * @param str string
     * @return java.lang.Double
     */
    private Double parseToDouble(final String str) {
        if (str == null || str.isEmpty()) {
            return null;
        }
        try {
            return Double.parseDouble(str);
        } catch (NumberFormatException e) {
            LOG.error("Failed to parse value. cause: {} ", e.getMessage());
            return null;
        }
    }
}
