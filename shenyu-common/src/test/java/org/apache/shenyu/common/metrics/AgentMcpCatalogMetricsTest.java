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

package org.apache.shenyu.common.metrics;

import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AgentMcpCatalogMetricsTest {

    @Test
    void snapshotsReadLiveValuesAndUnregisterOnlyTheirOwnSource() {
        AtomicLong age = new AtomicLong();
        AgentMcpCatalogMetrics first = new AgentMcpCatalogMetrics(() -> snapshot(age.get()));
        AgentMcpCatalogMetrics second = new AgentMcpCatalogMetrics(() -> snapshot(7));
        try {
            assertTrue(first.register());
            assertTrue(first.register());
            assertTrue(second.register());
            List<AgentMcpCatalogMetrics.Snapshot> before = AgentMcpCatalogMetrics.snapshots();
            assertEquals(2, before.size());
            age.set(5000);
            assertTrue(AgentMcpCatalogMetrics.snapshots().contains(snapshot(5000)));
            assertTrue(before.contains(snapshot(0)));
            first.unregister();
            assertEquals(List.of(snapshot(7)), AgentMcpCatalogMetrics.snapshots());
        } finally {
            first.unregister();
            second.unregister();
        }
        assertTrue(AgentMcpCatalogMetrics.snapshots().isEmpty());
    }

    @Test
    void sourceBudgetIsBoundedAndReleasedOnClose() {
        List<AgentMcpCatalogMetrics> sources = new ArrayList<>();
        int registered = 0;
        try {
            for (int index = 0; index < 32; index++) {
                AgentMcpCatalogMetrics source = new AgentMcpCatalogMetrics(() -> snapshot(0));
                sources.add(source);
                if (source.register()) {
                    registered++;
                }
            }
            assertEquals(16, registered);
            assertEquals(16, AgentMcpCatalogMetrics.snapshots().size());
            sources.get(0).unregister();
            assertTrue(sources.get(31).register());
            assertEquals(16, AgentMcpCatalogMetrics.snapshots().size());
        } finally {
            sources.forEach(AgentMcpCatalogMetrics::unregister);
        }
        assertTrue(AgentMcpCatalogMetrics.snapshots().isEmpty());
    }

    private static AgentMcpCatalogMetrics.Snapshot snapshot(final long age) {
        return new AgentMcpCatalogMetrics.Snapshot(1, 0, 0, 0, age, true, true, false);
    }
}
