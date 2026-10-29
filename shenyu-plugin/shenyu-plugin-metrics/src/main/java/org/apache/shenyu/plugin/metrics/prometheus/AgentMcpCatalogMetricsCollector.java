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
import org.apache.shenyu.common.metrics.AgentMcpCatalogMetrics;
import org.apache.shenyu.common.metrics.AgentMcpCatalogMetrics.Snapshot;
import org.apache.shenyu.plugin.metrics.constant.LabelNames;

import java.util.List;

/** Scrape bounded numeric catalog state without target, tool, revision or credential labels. */
public final class AgentMcpCatalogMetricsCollector extends Collector implements Collector.Describable {

    @Override
    public List<MetricFamilySamples> collect() {
        return samples(AgentMcpCatalogMetrics.snapshots());
    }

    @Override
    public List<MetricFamilySamples> describe() {
        return samples(List.of());
    }

    private List<MetricFamilySamples> samples(final List<Snapshot> sources) {
        boolean present = !sources.isEmpty();
        long ageMillis = sources.stream().mapToLong(Snapshot::lastSuccessAgeMillis).max().orElse(-1L);
        return List.of(
                counter(LabelNames.AGENT_MCP_CATALOG_REFRESH_TOTAL, "Remote discovery attempts over active catalog instance lifetimes",
                        sources.stream().mapToLong(Snapshot::attempts).sum(), present),
                counter(LabelNames.AGENT_MCP_CATALOG_FAILURES_TOTAL, "Failed current remote discovery attempts; excludes invalid and superseded configuration",
                        sources.stream().mapToLong(Snapshot::failures).sum(), present),
                counter(LabelNames.AGENT_MCP_CATALOG_RECOVERIES_TOTAL, "Successful discovery after failures in the same configuration",
                        sources.stream().mapToLong(Snapshot::recoveries).sum(), present),
                gauge(LabelNames.AGENT_MCP_CATALOG_CONFIGURED, "Active catalogs configured with remote targets",
                        sources.stream().filter(Snapshot::configured).count(), present),
                gauge(LabelNames.AGENT_MCP_CATALOG_AVAILABLE, "Active catalogs currently publishing remote directories",
                        sources.stream().filter(Snapshot::available).count(), present),
                gauge(LabelNames.AGENT_MCP_CATALOG_FAILURE_ACTIVE, "Active catalogs with a current validation or discovery failure",
                        sources.stream().filter(Snapshot::failureActive).count(), present),
                gauge(LabelNames.AGENT_MCP_CATALOG_CONSECUTIVE_FAILURES, "Largest bounded failure streak among active catalogs",
                        sources.stream().mapToInt(Snapshot::consecutiveFailures).max().orElse(0), present),
                gauge(LabelNames.AGENT_MCP_CATALOG_LAST_SUCCESS_AGE_SECONDS, "Largest live age of last successful publication for current configuration; -1 before success",
                        ageMillis < 0 ? -1.0 : ageMillis / 1000.0, present));
    }

    private CounterMetricFamily counter(final String name, final String help, final long value, final boolean present) {
        CounterMetricFamily family = new CounterMetricFamily(name, help, List.of());
        if (present) {
            family.addMetric(List.of(), value);
        }
        return family;
    }

    private GaugeMetricFamily gauge(final String name, final String help, final double value, final boolean present) {
        GaugeMetricFamily family = new GaugeMetricFamily(name, help, List.of());
        if (present) {
            family.addMetric(List.of(), value);
        }
        return family;
    }
}
