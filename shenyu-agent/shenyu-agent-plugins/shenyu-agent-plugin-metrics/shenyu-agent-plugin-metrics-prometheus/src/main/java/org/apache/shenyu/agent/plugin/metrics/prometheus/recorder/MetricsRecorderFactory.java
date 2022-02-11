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

package org.apache.shenyu.agent.plugin.metrics.prometheus.recorder;

import org.apache.shenyu.agent.plugin.metrics.api.MetricsRecorder;
import org.apache.shenyu.agent.plugin.metrics.api.config.Metrics.Metric;
import org.apache.shenyu.agent.plugin.metrics.api.enums.MetricsType;

import java.util.Optional;

/**
 * The type Collector registry factory.
 */
public final class MetricsRecorderFactory {
    
    /**
     * New instance collector.
     *
     * @param metric the metric
     * @return the collector
     */
    public static Optional<MetricsRecorder> newInstance(final Metric metric) {
        MetricsType metricsType = MetricsType.acquireByName(metric.getType());
        switch (metricsType) {
            case COUNTER:
                return Optional.of(new CounterRecorder(metric));
            case GAUGE:
                return Optional.of(new GaugeRecorder(metric));
            case HISTOGRAM:
                return Optional.of(new HistogramRecorder(metric));
            case SUMMARY:
                return Optional.of(new SummaryRecorder(metric));
            default:
                return Optional.empty();
        }
    }
}
