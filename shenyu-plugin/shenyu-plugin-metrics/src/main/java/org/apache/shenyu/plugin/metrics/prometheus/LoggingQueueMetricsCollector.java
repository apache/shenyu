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

package org.apache.shenyu.plugin.metrics.prometheus;

import io.prometheus.client.Collector;
import io.prometheus.client.CounterMetricFamily;
import io.prometheus.client.GaugeMetricFamily;
import org.apache.shenyu.common.metrics.LoggingQueueMetrics;
import org.apache.shenyu.plugin.metrics.constant.LabelNames;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Locale;

/**
 * Scrape logging admission metrics without depending on a logging plugin or polling thread.
 */
public final class LoggingQueueMetricsCollector extends Collector implements Collector.Describable {

    private static final List<String> LABELS = List.of("collector", "mode");

    @Override
    public List<MetricFamilySamples> collect() {
        return samples(LoggingQueueMetrics.registered());
    }

    @Override
    public List<MetricFamilySamples> describe() {
        return samples(Collections.emptyList());
    }

    private List<MetricFamilySamples> samples(final Collection<LoggingQueueMetrics> sources) {
        final CounterMetricFamily accepted = new CounterMetricFamily(LabelNames.LOGGING_QUEUE_ACCEPTED_TOTAL,
                "Records admitted to logging queues; does not count successful backend deliveries", LABELS);
        final CounterMetricFamily dropped = new CounterMetricFamily(LabelNames.LOGGING_QUEUE_DROPPED_TOTAL,
                "Records rejected by logging queue capacity; excludes sampling, disabled collectors and backend send failures", LABELS);
        final GaugeMetricFamily size = new GaugeMetricFamily(LabelNames.LOGGING_QUEUE_SIZE,
                "Records currently queued, excluding batches already drained for sending; aggregated across selector queues", LABELS);
        final GaugeMetricFamily capacity = new GaugeMetricFamily(LabelNames.LOGGING_QUEUE_CAPACITY,
                "Effective logging queue admission capacity in records; summed across selector queues; zero for inactive modes", LABELS);
        for (LoggingQueueMetrics source : sources) {
            List<String> labels = List.of(source.getCollector(), source.getMode().name().toLowerCase(Locale.ROOT));
            accepted.addMetric(labels, source.getAccepted());
            dropped.addMetric(labels, source.getDropped());
            size.addMetric(labels, source.getQueuedRecords());
            capacity.addMetric(labels, source.getCapacity());
        }
        return List.of(accepted, dropped, size, capacity);
    }
}
