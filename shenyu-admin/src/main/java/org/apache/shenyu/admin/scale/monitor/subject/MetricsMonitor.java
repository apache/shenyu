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

package org.apache.shenyu.admin.scale.monitor.subject;

import org.apache.shenyu.admin.model.entity.ScaleRuleDO;
import org.apache.shenyu.admin.scale.collector.PrometheusMetricsProvider;
import org.apache.shenyu.admin.scale.collector.provider.MetricData;
import org.apache.shenyu.admin.scale.monitor.observer.Observer;
import org.apache.shenyu.admin.scale.monitor.subject.cache.ScaleRuleCache;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

@Component
public class MetricsMonitor implements Subject {

    private final List<Observer> observers = new ArrayList<>();

    private final PrometheusMetricsProvider metricsProvider;

    private final ScaleRuleCache scaleRuleCache;

    public MetricsMonitor(final PrometheusMetricsProvider metricsProvider,
                          final ScaleRuleCache scaleRuleCache) {
        this.metricsProvider = metricsProvider;
        this.scaleRuleCache = scaleRuleCache;
    }

    @Override
    public void addObserver(final Observer observer) {
        observers.add(observer);
    }

    @Override
    public void removeObserver(final Observer observer) {
        observers.remove(observer);
    }

    @Override
    public void notifyObservers(final MetricData metricData, final ScaleRuleDO rule) {
        for (Observer observer : observers) {
            observer.update(metricData, rule);
        }
    }

    /**
     * monitor metrics.
     */
    public void monitorMetrics() {
        List<ScaleRuleDO> sortedRules = scaleRuleCache.getAllRules().stream()
                .filter(rule -> rule.getStatus() == 1)
                .sorted(Comparator.comparingInt(ScaleRuleDO::getSort))
                .toList();

        for (ScaleRuleDO rule : sortedRules) {
            MetricData metricData = metricsProvider.getMetricData(rule.getMetricName());

            if (metricData != null) {
                notifyObservers(metricData, rule);
            }
        }
    }
}
