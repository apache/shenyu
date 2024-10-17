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

public class ShenYuMetricsProvider implements MetricsProvider {

    private final PrometheusMetricsCollector metricsCollector;

    public ShenYuMetricsProvider(final PrometheusMetricsCollector metricsCollector) {
        this.metricsCollector = metricsCollector;
    }

    @Override
    public List<MetricData> getMetrics() throws Exception {

        String qpsQuery = "sum(rate(shenyu_requests_total[1m]))";
        String latencyQuery = "histogram_quantile(0.95, sum(rate(shenyu_request_duration_seconds_bucket[1m])) by (le))";

        List<MetricData> qpsMetrics = metricsCollector.queryMetrics(qpsQuery);
        List<MetricData> latencyMetrics = metricsCollector.queryMetrics(latencyQuery);

        List<MetricData> allMetrics = new ArrayList<>();
        allMetrics.addAll(qpsMetrics);
        allMetrics.addAll(latencyMetrics);

        return allMetrics;
    }
}