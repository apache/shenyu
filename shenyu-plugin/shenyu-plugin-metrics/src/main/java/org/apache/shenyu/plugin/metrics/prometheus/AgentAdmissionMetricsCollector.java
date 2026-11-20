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
import io.prometheus.client.GaugeMetricFamily;
import org.apache.shenyu.common.metrics.AgentAdmissionMetrics;

import java.util.List;
import java.util.stream.Collectors;

/** On-scrape admission state using bounded aggregate labels. */
public final class AgentAdmissionMetricsCollector extends Collector implements Collector.Describable {

    @Override
    public List<MetricFamilySamples> collect() {
        return samples(AgentAdmissionMetrics.snapshots());
    }

    @Override
    public List<MetricFamilySamples> describe() {
        return samples(List.of());
    }

    private List<MetricFamilySamples> samples(final List<AgentAdmissionMetrics.Snapshot> states) {
        GaugeMetricFamily inFlight = new GaugeMetricFamily("shenyu_agent_admission_inflight",
                "Admitted work by fixed scope", List.of("scope"));
        GaugeMetricFamily queued = new GaugeMetricFamily("shenyu_agent_admission_queued",
                "Queued work by fixed scope", List.of("scope"));
        GaugeMetricFamily inFlightLimit = new GaugeMetricFamily("shenyu_agent_admission_inflight_limit",
                "Configured in-flight limit by fixed scope", List.of("scope"));
        GaugeMetricFamily queueLimit = new GaugeMetricFamily("shenyu_agent_admission_queue_limit",
                "Configured queue limit by fixed scope", List.of("scope"));
        GaugeMetricFamily rejected = new GaugeMetricFamily("shenyu_agent_admission_rejections_total",
                "Explicit admission rejections by fixed scope", List.of("scope"));
        for (AgentAdmissionMetrics.Scope scope : AgentAdmissionMetrics.Scope.values()) {
            List<AgentAdmissionMetrics.Snapshot> scoped = states.stream()
                    .filter(state -> state.scope() == scope).collect(Collectors.toList());
            inFlight.addMetric(label(scope), sum(scoped, AgentAdmissionMetrics.Snapshot::inFlight));
            queued.addMetric(label(scope), sum(scoped, AgentAdmissionMetrics.Snapshot::queued));
            inFlightLimit.addMetric(label(scope), sum(scoped, AgentAdmissionMetrics.Snapshot::inFlightLimit));
            queueLimit.addMetric(label(scope), sum(scoped, AgentAdmissionMetrics.Snapshot::queueLimit));
            rejected.addMetric(label(scope), states.stream().filter(state -> state.scope() == scope)
                    .mapToLong(AgentAdmissionMetrics.Snapshot::rejected).sum());
        }
        return List.of(inFlight, queued, inFlightLimit, queueLimit, rejected);
    }

    private long sum(final List<AgentAdmissionMetrics.Snapshot> states,
                     final java.util.function.ToIntFunction<AgentAdmissionMetrics.Snapshot> value) {
        return states.stream().mapToLong(value::applyAsInt).sum();
    }

    private List<String> label(final AgentAdmissionMetrics.Scope scope) {
        return List.of(scope.name().toLowerCase(java.util.Locale.ROOT));
    }
}
