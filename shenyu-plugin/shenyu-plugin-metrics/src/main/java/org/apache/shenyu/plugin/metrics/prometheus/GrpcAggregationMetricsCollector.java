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
import org.apache.shenyu.common.metrics.GrpcAggregationMetrics;
import org.apache.shenyu.common.metrics.GrpcAggregationMetrics.Outcome;
import org.apache.shenyu.common.metrics.GrpcAggregationMetrics.Totals;

import java.util.List;
import java.util.Locale;

/**
 * Scrape gRPC aggregation counters without depending on the gRPC plugin.
 */
final class GrpcAggregationMetricsCollector extends Collector implements Collector.Describable {

    private static final List<String> LABELS = List.of("outcome");

    @Override
    public List<MetricFamilySamples> collect() {
        return samples(true);
    }

    @Override
    public List<MetricFamilySamples> describe() {
        return samples(false);
    }

    private List<MetricFamilySamples> samples(final boolean includeValues) {
        CounterMetricFamily calls = new CounterMetricFamily("shenyu_grpc_aggregate_calls_total",
                "Terminal gRPC bridge calls", LABELS);
        CounterMetricFamily items = new CounterMetricFamily("shenyu_grpc_aggregate_items_total",
                "Response items admitted into terminal gRPC bridge aggregates", LABELS);
        CounterMetricFamily payloadBytes = new CounterMetricFamily("shenyu_grpc_aggregate_payload_bytes_total",
                "UTF-8 payload bytes admitted into terminal gRPC bridge aggregates", LABELS);
        if (includeValues) {
            for (Outcome outcome : Outcome.values()) {
                Totals totals = GrpcAggregationMetrics.totals(outcome);
                List<String> labels = List.of(outcome.name().toLowerCase(Locale.ROOT));
                calls.addMetric(labels, totals.calls());
                items.addMetric(labels, totals.items());
                payloadBytes.addMetric(labels, totals.payloadBytes());
            }
        }
        return List.of(calls, items, payloadBytes);
    }
}
