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

import io.prometheus.client.CollectorRegistry;
import org.apache.shenyu.common.metrics.GrpcAggregationMetrics;
import org.apache.shenyu.common.metrics.GrpcAggregationMetrics.Outcome;
import org.apache.shenyu.common.metrics.GrpcAggregationMetrics.Totals;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Verifies the exported gRPC aggregation metrics using a Prometheus registry.
 */
class GrpcAggregationMetricsCollectorTest {

    @Test
    void exportsOnlyFixedOutcomeLabels() {
        CollectorRegistry registry = new CollectorRegistry();
        new GrpcAggregationMetricsCollector().register(registry);
        Totals before = GrpcAggregationMetrics.totals(Outcome.OVERFLOW);
        GrpcAggregationMetrics.record(Outcome.OVERFLOW, 3L, 128L);

        assertEquals(before.calls() + 1D, sample(registry, "shenyu_grpc_aggregate_calls_total"));
        assertEquals(before.items() + 3D, sample(registry, "shenyu_grpc_aggregate_items_total"));
        assertEquals(before.payloadBytes() + 128D, sample(registry, "shenyu_grpc_aggregate_payload_bytes_total"));
        registry.metricFamilySamples().asIterator().forEachRemaining(family -> {
            assertEquals(Outcome.values().length, family.samples.size());
            family.samples.forEach(sample -> assertEquals(List.of("outcome"), sample.labelNames));
        });
    }

    private Double sample(final CollectorRegistry registry, final String name) {
        return registry.getSampleValue(name, new String[]{"outcome"}, new String[]{"overflow"});
    }
}
