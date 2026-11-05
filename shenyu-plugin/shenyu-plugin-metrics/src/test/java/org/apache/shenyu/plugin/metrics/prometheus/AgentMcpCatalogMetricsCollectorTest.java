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
import org.apache.shenyu.common.metrics.AgentMcpCatalogMetrics;
import org.apache.shenyu.common.metrics.AgentMcpCatalogMetrics.Snapshot;
import org.apache.shenyu.plugin.metrics.constant.LabelNames;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AgentMcpCatalogMetricsCollectorTest {

    private final CollectorRegistry registry = new CollectorRegistry();

    private final AtomicReference<Snapshot> state = new AtomicReference<>(new Snapshot(0, 0, 0, 0, -1, false, false, false));

    private final AgentMcpCatalogMetrics metrics = new AgentMcpCatalogMetrics(state::get);

    @AfterEach
    void tearDown() {
        metrics.unregister();
        registry.clear();
    }

    @Test
    void exportsLiveAgeFailureAndRecoveryWithoutAnAdditionalRefresh() {
        metrics.register();
        new AgentMcpCatalogMetricsCollector().register(registry);
        assertEquals(-1.0, sample(LabelNames.AGENT_MCP_CATALOG_LAST_SUCCESS_AGE_SECONDS));
        state.set(new Snapshot(4, 4, 0, 4, -1, true, false, true));
        assertEquals(4.0, sample(LabelNames.AGENT_MCP_CATALOG_REFRESH_TOTAL));
        assertEquals(4.0, sample(LabelNames.AGENT_MCP_CATALOG_FAILURES_TOTAL));
        assertEquals(0.0, sample(LabelNames.AGENT_MCP_CATALOG_RECOVERIES_TOTAL));
        assertEquals(1.0, sample(LabelNames.AGENT_MCP_CATALOG_CONFIGURED));
        assertEquals(0.0, sample(LabelNames.AGENT_MCP_CATALOG_AVAILABLE));
        assertEquals(1.0, sample(LabelNames.AGENT_MCP_CATALOG_FAILURE_ACTIVE));
        assertEquals(4.0, sample(LabelNames.AGENT_MCP_CATALOG_CONSECUTIVE_FAILURES));
        state.set(new Snapshot(5, 4, 1, 0, 12500, true, true, false));
        assertEquals(1.0, sample(LabelNames.AGENT_MCP_CATALOG_RECOVERIES_TOTAL));
        assertEquals(1.0, sample(LabelNames.AGENT_MCP_CATALOG_AVAILABLE));
        assertEquals(0.0, sample(LabelNames.AGENT_MCP_CATALOG_FAILURE_ACTIVE));
        assertEquals(0.0, sample(LabelNames.AGENT_MCP_CATALOG_CONSECUTIVE_FAILURES));
        assertEquals(12.5, sample(LabelNames.AGENT_MCP_CATALOG_LAST_SUCCESS_AGE_SECONDS));
    }

    @Test
    void discoversLateSourcesAndRemovesStoppedCatalogs() {
        new AgentMcpCatalogMetricsCollector().register(registry);
        assertNull(sample(LabelNames.AGENT_MCP_CATALOG_AVAILABLE));
        metrics.register();
        assertEquals(0.0, sample(LabelNames.AGENT_MCP_CATALOG_AVAILABLE));
        metrics.unregister();
        assertNull(sample(LabelNames.AGENT_MCP_CATALOG_AVAILABLE));
    }

    @Test
    void aggregatesSourcesIntoFixedSeriesWithoutLabels() {
        AgentMcpCatalogMetrics second = new AgentMcpCatalogMetrics(() -> new Snapshot(2, 1, 1, 0, 30000, true, true, false));
        try {
            metrics.register();
            second.register();
            state.set(new Snapshot(4, 4, 0, 4, -1, true, false, true));
            new AgentMcpCatalogMetricsCollector().register(registry);
            assertEquals(6.0, sample(LabelNames.AGENT_MCP_CATALOG_REFRESH_TOTAL));
            assertEquals(5.0, sample(LabelNames.AGENT_MCP_CATALOG_FAILURES_TOTAL));
            assertEquals(2.0, sample(LabelNames.AGENT_MCP_CATALOG_CONFIGURED));
            assertEquals(1.0, sample(LabelNames.AGENT_MCP_CATALOG_AVAILABLE));
            assertEquals(30.0, sample(LabelNames.AGENT_MCP_CATALOG_LAST_SUCCESS_AGE_SECONDS));
            registry.metricFamilySamples().asIterator().forEachRemaining(family ->
                    family.samples.forEach(sample -> assertTrue(sample.labelNames.isEmpty())));
        } finally {
            second.unregister();
        }
    }

    private Double sample(final String name) {
        return registry.getSampleValue(name);
    }
}
