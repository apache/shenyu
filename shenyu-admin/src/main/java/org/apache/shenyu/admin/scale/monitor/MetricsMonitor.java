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

import org.apache.shenyu.admin.scale.collector.K8sMetricsProvider;
import org.apache.shenyu.admin.scale.collector.ShenYuMetricsProvider;
import org.apache.shenyu.admin.scale.collector.provider.MetricData;

import java.util.ArrayList;
import java.util.List;

public class MetricsMonitor implements Subject{

    private final List<Observer> observers = new ArrayList<>();
    private final ShenYuMetricsProvider shenyuMetricsProvider;
    private final K8sMetricsProvider k8sMetricsProvider;

    public MetricsMonitor(ShenYuMetricsProvider shenyuMetricsProvider, K8sMetricsProvider k8sMetricsProvider) {
        this.shenyuMetricsProvider = shenyuMetricsProvider;
        this.k8sMetricsProvider = k8sMetricsProvider;
    }

    @Override
    public void addObserver(Observer observer) {
        observers.add(observer);
    }

    @Override
    public void removeObserver(Observer observer) {
        observers.remove(observer);
    }

    @Override
    public void notifyObservers(MetricData metricData) {
        for (Observer observer : observers) {
            observer.update(metricData);
        }
    }

    // 定期监控 Metrics 数据
    public void monitorMetrics() throws Exception {
        // 获取 ShenYu Metrics
        List<MetricData> shenyuMetrics = shenyuMetricsProvider.getMetrics();
        // 获取 K8s Metrics
        List<MetricData> k8sMetrics = k8sMetricsProvider.getMetrics();

        // 通知所有观察者 ShenYu Metrics
        for (MetricData metricData : shenyuMetrics) {
            notifyObservers(metricData);
        }

        // 通知所有观察者 K8s Metrics
        for (MetricData metricData : k8sMetrics) {
            notifyObservers(metricData);
        }
    }
}
