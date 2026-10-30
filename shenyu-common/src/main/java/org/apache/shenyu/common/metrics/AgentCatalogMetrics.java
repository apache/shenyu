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

import java.util.List;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Supplier;

/** Live, payload-free catalog snapshots, aggregated without endpoint or generation labels. */
public final class AgentCatalogMetrics implements AutoCloseable {

    private static final Set<AgentCatalogMetrics> SOURCES = ConcurrentHashMap.newKeySet();

    private final Supplier<Snapshot> source;

    private AgentCatalogMetrics(final Supplier<Snapshot> source) {
        this.source = source;
    }

    /** Register after the managed catalog starts; callers own the returned registration.
     * @param source in-memory snapshot supplier, never network I/O
     * @return registration removed only by its owner
     */
    public static AgentCatalogMetrics register(final Supplier<Snapshot> source) {
        AgentCatalogMetrics registration = new AgentCatalogMetrics(java.util.Objects.requireNonNull(source));
        SOURCES.add(registration);
        return registration;
    }

    /** Capture active catalog states; a failed supplier is explicitly unavailable.
     * @return immutable snapshots, with no identity labels
     */
    public static List<Snapshot> snapshots() {
        return SOURCES.stream().map(registration -> {
            try {
                return java.util.Objects.requireNonNull(registration.source.get());
            } catch (RuntimeException ignored) {
                return new Snapshot(false, false, 0, 0, 0, 0, true);
            }
        }).toList();
    }

    @Override
    public void close() {
        SOURCES.remove(this);
    }

    /** Catalog state, not session or physical connection counts.
     * @param available a current usable directory exists
     * @param updating a generation is being built
     * @param generations retained generations
     * @param clients owned endpoint clients, not socket count
     * @param references active generation leases, not sessions
     * @param pending owned endpoint HTTP requests, not logical MCP calls
     * @param failed configuration or cleanup diagnostics contain failure
     */
    public record Snapshot(boolean available, boolean updating, int generations, int clients, int references, int pending, boolean failed) {
    }
}
