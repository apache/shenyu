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

/** Live payload-free admission snapshots, without tool, subject, tenant, or endpoint labels. */
public final class AgentAdmissionMetrics implements AutoCloseable {

    private static final Set<AgentAdmissionMetrics> SOURCES = ConcurrentHashMap.newKeySet();

    private final Supplier<List<Snapshot>> source;

    private AgentAdmissionMetrics(final Supplier<List<Snapshot>> source) {
        this.source = source;
    }

    /**
     * Register one controller after startup; the controller owns this registration.
     * @param source in-memory snapshot supplier
     * @return registration removed by close
     */
    public static AgentAdmissionMetrics register(final Supplier<List<Snapshot>> source) {
        AgentAdmissionMetrics value = new AgentAdmissionMetrics(source);
        SOURCES.add(value);
        return value;
    }

    /** Aggregate all live controller states.
     * @return immutable snapshots
     */
    public static List<Snapshot> snapshots() {
        return SOURCES.stream().flatMap(registration -> {
            try {
                return registration.source.get().stream();
            } catch (RuntimeException ignored) {
                return java.util.stream.Stream.empty();
            }
        }).toList();
    }

    @Override
    public void close() {
        SOURCES.remove(this);
    }

    /** One bounded scope; names never carry request or target identity. */
    public record Snapshot(Scope scope, int inFlight, int queued, int inFlightLimit, int queueLimit, long rejected) {
    }

    /** Fixed admission scopes, safe as metric labels. */
    public enum Scope {
        GLOBAL, LOCAL, REMOTE
    }
}
