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

package org.apache.shenyu.k8s.observability;

import com.google.common.collect.Maps;

import java.util.Collections;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.LongAdder;

/**
 * Bounded Kubernetes controller metrics tracking reconcile latency, errors, work queue pressure,
 * watch/relist recovery, cache readiness, and rejected runtime configuration.
 *
 * <p>To protect metric cardinality in Prometheus and other telemetry backends, all metric labels
 * are strictly bounded to finite enum-like identifiers (controller name, result, bounded reason).
 * Dynamic paths, URLs, user IDs, and session IDs are strictly forbidden as label values.</p>
 */
public final class K8sControllerMetrics {

    private static final K8sControllerMetrics INSTANCE = new K8sControllerMetrics();

    /** controller:result -> count. */
    private final Map<String, LongAdder> reconcileCounters = Maps.newConcurrentMap();

    /** controller -> total reconcile duration in nanos. */
    private final Map<String, LongAdder> reconcileDurationNanos = Maps.newConcurrentMap();

    /** controller:errorType -> count. */
    private final Map<String, LongAdder> reconcileErrorCounters = Maps.newConcurrentMap();

    /** kind:reason -> count. */
    private final Map<String, LongAdder> rejectedRouteCounters = Maps.newConcurrentMap();

    /** queueName -> current queue depth. */
    private final Map<String, AtomicInteger> workQueueDepths = Maps.newConcurrentMap();

    /** resourceType -> watch relist / recovery count. */
    private final Map<String, LongAdder> watchRecoveryCounters = Maps.newConcurrentMap();

    private final AtomicBoolean cacheReady = new AtomicBoolean(false);

    private final AtomicLong pendingInformers = new AtomicLong(0);

    private final AtomicLong pendingWorkItems = new AtomicLong(0);

    private K8sControllerMetrics() {
    }

    /**
     * Gets singleton metrics instance.
     *
     * @return metrics instance
     */
    public static K8sControllerMetrics getInstance() {
        return INSTANCE;
    }

    /**
     * Records a completed controller reconcile cycle.
     *
     * @param controller the controller name (e.g. "httproute", "ingress", "gateway")
     * @param result the reconcile outcome (e.g. "success", "rejected", "deleted")
     * @param durationNanos execution time in nanoseconds
     */
    public void recordReconcile(final String controller, final String result, final long durationNanos) {
        String safeController = normalizeLabel(controller);
        String safeResult = normalizeLabel(result);
        reconcileCounters.computeIfAbsent(safeController + ":" + safeResult, k -> new LongAdder()).increment();
        reconcileDurationNanos.computeIfAbsent(safeController, k -> new LongAdder()).add(durationNanos);
    }

    /**
     * Records an unexpected reconciliation error.
     *
     * @param controller the controller name
     * @param errorType the class or bounded category of error
     */
    public void recordReconcileError(final String controller, final String errorType) {
        String safeController = normalizeLabel(controller);
        String safeError = normalizeLabel(errorType);
        reconcileErrorCounters.computeIfAbsent(safeController + ":" + safeError, k -> new LongAdder()).increment();
    }

    /**
     * Records a rejected route configuration with its spec-defined failure reason.
     *
     * @param kind the resource kind (e.g. "HTTPRoute", "Ingress")
     * @param reason the bounded rejection reason (e.g. "NoMatchingParent", "RefNotPermitted")
     */
    public void recordRejectedRoute(final String kind, final String reason) {
        String safeKind = normalizeLabel(kind);
        String safeReason = normalizeLabel(reason);
        rejectedRouteCounters.computeIfAbsent(safeKind + ":" + safeReason, k -> new LongAdder()).increment();
    }

    /**
     * Updates work queue depth gauge.
     *
     * @param queueName the queue name
     * @param depth current depth
     */
    public void updateWorkQueueDepth(final String queueName, final int depth) {
        String safeQueue = normalizeLabel(queueName);
        workQueueDepths.computeIfAbsent(safeQueue, k -> new AtomicInteger()).set(depth);
    }

    /**
     * Updates informer cache readiness state.
     *
     * @param ready whether informers have synced and queues are drained
     * @param pendingInformersCount number of unsynced informers
     * @param pendingItemsCount number of items waiting in queues
     */
    public void updateCacheReadiness(final boolean ready, final long pendingInformersCount, final long pendingItemsCount) {
        this.cacheReady.set(ready);
        this.pendingInformers.set(pendingInformersCount);
        this.pendingWorkItems.set(pendingItemsCount);
    }

    /**
     * Records a watch relist or recovery event.
     *
     * @param resourceType the Kubernetes resource type
     */
    public void recordWatchRecovery(final String resourceType) {
        String safeType = normalizeLabel(resourceType);
        watchRecoveryCounters.computeIfAbsent(safeType, k -> new LongAdder()).increment();
    }

    /**
     * Gets reconcile count for a controller and result.
     *
     * @param controller controller name
     * @param result result
     * @return count
     */
    public long getReconcileCount(final String controller, final String result) {
        LongAdder counter = reconcileCounters.get(normalizeLabel(controller) + ":" + normalizeLabel(result));
        return Objects.isNull(counter) ? 0L : counter.sum();
    }

    /**
     * Gets total reconcile duration in nanoseconds for a controller.
     *
     * @param controller controller name
     * @return total duration in nanos
     */
    public long getReconcileDurationNanos(final String controller) {
        LongAdder duration = reconcileDurationNanos.get(normalizeLabel(controller));
        return Objects.isNull(duration) ? 0L : duration.sum();
    }

    /**
     * Gets error count for a controller and error type.
     *
     * @param controller controller name
     * @param errorType error type
     * @return count
     */
    public long getReconcileErrorCount(final String controller, final String errorType) {
        LongAdder counter = reconcileErrorCounters.get(normalizeLabel(controller) + ":" + normalizeLabel(errorType));
        return Objects.isNull(counter) ? 0L : counter.sum();
    }

    /**
     * Gets rejected route count by kind and reason.
     *
     * @param kind resource kind
     * @param reason rejection reason
     * @return count
     */
    public long getRejectedRouteCount(final String kind, final String reason) {
        LongAdder counter = rejectedRouteCounters.get(normalizeLabel(kind) + ":" + normalizeLabel(reason));
        return Objects.isNull(counter) ? 0L : counter.sum();
    }

    /**
     * Gets queue depth by name.
     *
     * @param queueName queue name
     * @return depth
     */
    public int getWorkQueueDepth(final String queueName) {
        AtomicInteger gauge = workQueueDepths.get(normalizeLabel(queueName));
        return Objects.isNull(gauge) ? 0 : gauge.get();
    }

    /**
     * Gets watch recovery count for a resource type.
     *
     * @param resourceType resource type
     * @return count
     */
    public long getWatchRecoveryCount(final String resourceType) {
        LongAdder counter = watchRecoveryCounters.get(normalizeLabel(resourceType));
        return Objects.isNull(counter) ? 0L : counter.sum();
    }

    /**
     * Whether informer cache is ready.
     *
     * @return true if ready
     */
    public boolean isCacheReady() {
        return cacheReady.get();
    }

    /**
     * Gets pending informers count.
     *
     * @return pending informers count
     */
    public long getPendingInformers() {
        return pendingInformers.get();
    }

    /**
     * Gets pending work items count.
     *
     * @return pending work items count
     */
    public long getPendingWorkItems() {
        return pendingWorkItems.get();
    }

    /**
     * Snapshot of reconcile counts.
     *
     * @return unmodifiable map of controller:result to count
     */
    public Map<String, Long> getReconcileCountersSnapshot() {
        Map<String, Long> snapshot = Maps.newLinkedHashMap();
        reconcileCounters.forEach((k, v) -> snapshot.put(k, v.sum()));
        return Collections.unmodifiableMap(snapshot);
    }

    /**
     * Snapshot of rejected route counts.
     *
     * @return unmodifiable map of kind:reason to count
     */
    public Map<String, Long> getRejectedRouteCountersSnapshot() {
        Map<String, Long> snapshot = Maps.newLinkedHashMap();
        rejectedRouteCounters.forEach((k, v) -> snapshot.put(k, v.sum()));
        return Collections.unmodifiableMap(snapshot);
    }

    /**
     * Snapshot of work queue depths.
     *
     * @return unmodifiable map of queue to depth
     */
    public Map<String, Integer> getWorkQueueDepthsSnapshot() {
        Map<String, Integer> snapshot = Maps.newLinkedHashMap();
        workQueueDepths.forEach((k, v) -> snapshot.put(k, v.get()));
        return Collections.unmodifiableMap(snapshot);
    }

    /**
     * Resets all metrics for testing purposes.
     */
    public void resetForTesting() {
        reconcileCounters.clear();
        reconcileDurationNanos.clear();
        reconcileErrorCounters.clear();
        rejectedRouteCounters.clear();
        workQueueDepths.clear();
        watchRecoveryCounters.clear();
        cacheReady.set(false);
        pendingInformers.set(0);
        pendingWorkItems.set(0);
    }

    private static String normalizeLabel(final String value) {
        if (Objects.isNull(value) || value.isBlank()) {
            return "unknown";
        }
        return value.trim().toLowerCase(Locale.ROOT);
    }
}
