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
import org.apache.shenyu.common.metrics.AgentCatalogMetrics;

import java.util.List;

/** On-scrape catalog state using the existing exporter, without polling or another SDK. */
public final class AgentCatalogMetricsCollector extends Collector implements Collector.Describable {

    @Override
    public List<MetricFamilySamples> collect() {
        return samples(AgentCatalogMetrics.snapshots());
    }

    @Override
    public List<MetricFamilySamples> describe() {
        return samples(List.of());
    }

    private List<MetricFamilySamples> samples(final List<AgentCatalogMetrics.Snapshot> states) {
        GaugeMetricFamily catalogs = new GaugeMetricFamily("shenyu_agent_catalog_instances", "Owned catalogs grouped by current availability", List.of("state"));
        catalogs.addMetric(List.of("available"), states.stream().filter(AgentCatalogMetrics.Snapshot::available).count());
        catalogs.addMetric(List.of("unavailable"), states.stream().filter(state -> !state.available()).count());
        GaugeMetricFamily updating = new GaugeMetricFamily("shenyu_agent_catalog_updating", "Owned catalogs currently refreshing",
                states.stream().filter(AgentCatalogMetrics.Snapshot::updating).count());
        GaugeMetricFamily failed = new GaugeMetricFamily("shenyu_agent_catalog_failed", "Owned catalogs with configuration or cleanup failure",
                states.stream().filter(AgentCatalogMetrics.Snapshot::failed).count());
        GaugeMetricFamily clients = new GaugeMetricFamily("shenyu_agent_catalog_clients", "Owned endpoint clients, not physical connections",
                states.stream().mapToLong(AgentCatalogMetrics.Snapshot::clients).sum());
        GaugeMetricFamily leases = new GaugeMetricFamily("shenyu_agent_catalog_leases", "Active generation leases, not MCP sessions",
                states.stream().mapToLong(AgentCatalogMetrics.Snapshot::references).sum());
        GaugeMetricFamily pending = new GaugeMetricFamily("shenyu_agent_catalog_pending_requests", "Owned endpoint HTTP requests, not logical MCP calls",
                states.stream().mapToLong(AgentCatalogMetrics.Snapshot::pending).sum());
        GaugeMetricFamily generations = new GaugeMetricFamily("shenyu_agent_catalog_generations", "Retained directory generations",
                states.stream().mapToLong(AgentCatalogMetrics.Snapshot::generations).sum());
        return List.of(catalogs, updating, failed, clients, leases, pending, generations);
    }
}
