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
import org.apache.shenyu.common.metrics.AgentCatalogMetrics;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AgentCatalogMetricsCollectorTest {

    @Test
    void scrapeTracksUpdatesAndIndependentRegistrationLifetimes() {
        CollectorRegistry registry = new CollectorRegistry();
        new AgentCatalogMetricsCollector().register(registry);
        AtomicReference<AgentCatalogMetrics.Snapshot> state = new AtomicReference<>(new AgentCatalogMetrics.Snapshot(true, false, 1, 2, 3, 5, false));
        try (AgentCatalogMetrics first = AgentCatalogMetrics.register(state::get);
                AgentCatalogMetrics second = AgentCatalogMetrics.register(() -> new AgentCatalogMetrics.Snapshot(false, true, 2, 4, 1, 7, true))) {
            assertEquals(6.0, registry.getSampleValue("shenyu_agent_catalog_clients"));
            assertEquals(4.0, registry.getSampleValue("shenyu_agent_catalog_leases"));
            assertEquals(12.0, registry.getSampleValue("shenyu_agent_catalog_pending_requests"));
            assertEquals(1.0, registry.getSampleValue("shenyu_agent_catalog_failed"));
            assertEquals(1.0, registry.getSampleValue("shenyu_agent_catalog_instances", new String[]{"state"}, new String[]{"available"}));
            state.set(new AgentCatalogMetrics.Snapshot(false, false, 0, 0, 0, 0, false));
            assertEquals(0.0, registry.getSampleValue("shenyu_agent_catalog_instances", new String[]{"state"}, new String[]{"available"}));
            first.close();
            first.close();
            assertEquals(4.0, registry.getSampleValue("shenyu_agent_catalog_clients"));
            registry.metricFamilySamples().asIterator().forEachRemaining(family -> family.samples.forEach(sample ->
                    assertTrue(sample.labelNames.isEmpty() || sample.labelNames.equals(List.of("state")))));
        }
        assertEquals(0.0, registry.getSampleValue("shenyu_agent_catalog_clients"));
        assertEquals(0.0, registry.getSampleValue("shenyu_agent_catalog_leases"));
    }

    @Test
    void brokenDiagnosticSupplierReportsUnavailableWithoutFailingScrape() {
        CollectorRegistry registry = new CollectorRegistry();
        new AgentCatalogMetricsCollector().register(registry);
        try (AgentCatalogMetrics ignored = AgentCatalogMetrics.register(() -> {
            throw new IllegalStateException("private credential");
        })) {
            assertEquals(1.0, registry.getSampleValue("shenyu_agent_catalog_instances", new String[]{"state"}, new String[]{"unavailable"}));
            assertEquals(1.0, registry.getSampleValue("shenyu_agent_catalog_failed"));
        }
    }
}
