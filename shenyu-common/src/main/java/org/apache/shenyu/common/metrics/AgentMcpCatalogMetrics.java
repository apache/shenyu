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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.function.Supplier;

/** Bounded process-local catalog sources, independent of the metrics backend. */
public final class AgentMcpCatalogMetrics {

    private static final int MAX_SOURCES = 16;

    private static final Set<AgentMcpCatalogMetrics> REGISTERED = new HashSet<>();

    private final Supplier<Snapshot> source;

    /**
     * Create a source containing numeric measurements only.
     * @param source nonblocking immutable snapshot supplier
     */
    public AgentMcpCatalogMetrics(final Supplier<Snapshot> source) {
        this.source = Objects.requireNonNull(source, "source");
    }

    /**
     * Register at most a fixed number of active catalog instances.
     * @return whether this source is registered
     */
    public boolean register() {
        synchronized (REGISTERED) {
            if (REGISTERED.contains(this)) {
                return true;
            }
            return REGISTERED.size() < MAX_SOURCES && REGISTERED.add(this);
        }
    }

    /** Remove this source when the catalog closes. */
    public void unregister() {
        synchronized (REGISTERED) {
            REGISTERED.remove(this);
        }
    }

    /**
     * Read live ages outside the registry lock to avoid lifecycle lock inversion.
     * @return immutable snapshots of active sources
     */
    public static List<Snapshot> snapshots() {
        List<AgentMcpCatalogMetrics> sources;
        synchronized (REGISTERED) {
            sources = List.copyOf(REGISTERED);
        }
        List<Snapshot> result = new ArrayList<>();
        for (AgentMcpCatalogMetrics metrics : sources) {
            result.add(metrics.source.get());
        }
        return List.copyOf(result);
    }

    /**
     * Numeric measurements for one owned catalog; age is -1 until this configuration first publishes.
     * Counters span the instance lifetime; failures and recoveries exclude superseded work and invalid configuration.
     * @param attempts remote discovery attempts started
     * @param failures failed current remote discovery attempts
     * @param recoveries successful current attempts following failures in the same configuration
     * @param consecutiveFailures bounded current failure streak
     * @param lastSuccessAgeMillis elapsed monotonic time since this configuration last published
     * @param configured whether the current configuration contains remote targets
     * @param available whether a remote directory is currently published
     * @param failureActive whether current configuration or discovery has a recorded failure
     */
    public record Snapshot(long attempts, long failures, long recoveries, int consecutiveFailures, long lastSuccessAgeMillis,
                           boolean configured, boolean available, boolean failureActive) {
    }
}
