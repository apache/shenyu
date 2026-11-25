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

import java.util.EnumMap;
import java.util.Map;
import java.util.concurrent.atomic.LongAdder;

/**
 * Process-wide counters for gRPC bridge calls, independent of the metrics backend.
 * Each outcome has fixed storage and never retains request metadata or payloads.
 */
public final class GrpcAggregationMetrics {

    private static final Map<Outcome, Counters> COUNTERS = new EnumMap<>(Outcome.class);

    static {
        for (Outcome outcome : Outcome.values()) {
            COUNTERS.put(outcome, new Counters());
        }
    }

    private GrpcAggregationMetrics() {
    }

    /**
     * Record a terminal call and the aggregate admitted before that outcome.
     *
     * @param outcome terminal outcome
     * @param items admitted response items
     * @param payloadBytes admitted UTF-8 payload bytes
     */
    public static void record(final Outcome outcome, final long items, final long payloadBytes) {
        Counters counters = COUNTERS.get(outcome);
        counters.items.add(items);
        counters.payloadBytes.add(payloadBytes);
        counters.calls.increment();
    }

    /**
     * Read cumulative counters for an outcome.
     *
     * @param outcome terminal outcome
     * @return current counter values
     */
    public static Totals totals(final Outcome outcome) {
        Counters counters = COUNTERS.get(outcome);
        return new Totals(counters.calls.sum(), counters.items.sum(), counters.payloadBytes.sum());
    }

    /**
     * Terminal outcomes at the upstream RPC boundary.
     */
    public enum Outcome {
        /** RPC completed and produced an aggregate. */
        SUCCESS,
        /** A response exceeded the aggregate budget. */
        OVERFLOW,
        /** The upstream RPC deadline expired. */
        DEADLINE,
        /** The downstream subscription cancelled the pending RPC. */
        CANCELLED,
        /** Another upstream failure terminated the RPC. */
        ERROR
    }

    /**
     * Cumulative measurements for one outcome.
     *
     * @param calls terminal calls
     * @param items admitted response items
     * @param payloadBytes admitted UTF-8 payload bytes
     */
    public record Totals(long calls, long items, long payloadBytes) {
    }

    private static final class Counters {

        private final LongAdder calls = new LongAdder();

        private final LongAdder items = new LongAdder();

        private final LongAdder payloadBytes = new LongAdder();
    }
}
