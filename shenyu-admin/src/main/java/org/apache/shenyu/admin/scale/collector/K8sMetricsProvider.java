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

package org.apache.shenyu.admin.scale.collector;

import org.apache.shenyu.admin.scale.collector.provider.MetricData;
import org.apache.shenyu.admin.scale.collector.provider.MetricsProvider;

import java.util.ArrayList;
import java.util.List;

public class K8sMetricsProvider implements MetricsProvider {

    private final PrometheusMetricsCollector metricsCollector;

    public K8sMetricsProvider(final PrometheusMetricsCollector metricsCollector) {
        this.metricsCollector = metricsCollector;
    }

    @Override
    public List<MetricData> getMetrics() throws Exception {

        String cpuUsageQuery = "sum(rate(container_cpu_usage_seconds_total{pod=~\"shenyu-gateway.*\"}[1m])) by (pod)";
        String memoryUsageQuery = "sum(container_memory_usage_bytes{pod=~\"shenyu-gateway.*\"}) by (pod)";

        List<MetricData> cpuMetrics = metricsCollector.queryMetrics(cpuUsageQuery);
        List<MetricData> memoryMetrics = metricsCollector.queryMetrics(memoryUsageQuery);

        List<MetricData> allMetrics = new ArrayList<>();
        allMetrics.addAll(cpuMetrics);
        allMetrics.addAll(memoryMetrics);

        return allMetrics;
    }
}
