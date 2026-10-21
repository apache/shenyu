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

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.LongAdder;
import java.util.function.LongSupplier;

/**
 * Admission counters and live queue measurements, independent of the metrics backend.
 * Only collector implementation classes and the two queue modes identify a series.
 * Suppliers are evaluated when metrics are scraped, never when a record is admitted.
 */
public final class LoggingQueueMetrics {

    private static final Map<String, LoggingQueueMetrics> REGISTERED = new ConcurrentHashMap<>();

    private final String collector;

    private final Mode mode;

    private final LongSupplier queuedRecords;

    private final LongSupplier capacity;

    private final LongAdder accepted = new LongAdder();

    private final LongAdder dropped = new LongAdder();

    /**
     * Create measurements for one collector and queue mode.
     *
     * @param collectorType implementation class, never a request or selector identifier
     * @param mode queue mode
     * @param queuedRecords current number of queued records
     * @param capacity current admission capacity, aggregated across selector queues
     */
    public LoggingQueueMetrics(final Class<?> collectorType, final Mode mode,
                               final LongSupplier queuedRecords, final LongSupplier capacity) {
        this.collector = collectorType.getName();
        this.mode = mode;
        this.queuedRecords = queuedRecords;
        this.capacity = capacity;
    }

    /**
     * Publish measurements after the collector starts.
     */
    public void register() {
        REGISTERED.put(collector + mode.name(), this);
    }

    /**
     * Remove only this collector's measurements when it stops.
     */
    public void unregister() {
        REGISTERED.remove(collector + mode.name(), this);
    }

    /**
     * Get the currently running collectors, including collectors started before metrics.
     *
     * @return snapshot of registered measurements
     */
    public static Collection<LoggingQueueMetrics> registered() {
        return List.copyOf(REGISTERED.values());
    }

    /**
     * Record an attempted admission. Call only after lifecycle and client checks.
     *
     * @param admitted whether the record was queued; false means an admission rejection
     */
    public void recordAdmission(final boolean admitted) {
        if (admitted) {
            accepted.increment();
        } else {
            dropped.increment();
        }
    }

    /**
     * Get the collector implementation name.
     *
     * @return bounded collector label
     */
    public String getCollector() {
        return collector;
    }

    /**
     * Get the queue mode.
     *
     * @return queue mode
     */
    public Mode getMode() {
        return mode;
    }

    /**
     * Get admissions over this collector instance's lifetime, including restarts.
     *
     * @return admitted records, not successful backend sends
     */
    public long getAccepted() {
        return accepted.sum();
    }

    /**
     * Get queue admission rejections, excluding sampling and backend send failures.
     *
     * @return rejected records
     */
    public long getDropped() {
        return dropped.sum();
    }

    /**
     * Get the current backlog, excluding batches already drained for sending.
     *
     * @return queued records
     */
    public long getQueuedRecords() {
        return queuedRecords.getAsLong();
    }

    /**
     * Get effective admission capacity; a backlog retained after shrinking may exceed it.
     *
     * @return capacity in records
     */
    public long getCapacity() {
        return capacity.getAsLong();
    }

    /**
     * Fixed queue modes, independent of request and selector data.
     */
    public enum Mode {
        /** Shared queue for a single client. */
        SINGLE,
        /** Aggregated queues for selector-specific clients. */
        MULTI
    }
}
