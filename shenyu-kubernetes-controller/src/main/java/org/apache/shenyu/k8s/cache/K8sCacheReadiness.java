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

package org.apache.shenyu.k8s.cache;

import io.kubernetes.client.extended.workqueue.WorkQueue;
import io.kubernetes.client.informer.SharedIndexInformer;

import java.util.Collection;
import java.util.List;
import java.util.Objects;

/**
 * Aggregates the initial-sync state of all informers and the controller work queues into a
 * single readiness signal, used to gate Kubernetes readiness so a cold pod (empty
 * {@code BaseDataCache}) receives no traffic until its local cache holds the full cluster
 * state.
 *
 * <p>{@code hasSynced()} only means each informer finished its initial LIST; the controller
 * work queues may still hold those objects waiting to be reconciled into
 * {@code BaseDataCache}. On a large cluster that gap can span minutes, so readiness
 * additionally requires the work queues to drain. {@link WorkQueue#length()} does not count
 * an item a worker already popped but has not finished reconciling, so the queue must be
 * observed empty on {@link #REQUIRED_DRAINED_OBSERVATIONS} consecutive polls before the
 * drained state is trusted; polls are seconds apart (kubelet probe period), far longer than
 * that race window.
 *
 * <p>Readiness latches: once every informer completed its initial LIST and the initial
 * reconciliation backlog drained, it reports ready forever. During a transient API server
 * outage the local cache still serves the last known state, so flapping back to not-ready
 * would only cause needless endpoint churn.
 */
public final class K8sCacheReadiness {

    /** Consecutive polls observing every queue empty before the drain is trusted. */
    private static final int REQUIRED_DRAINED_OBSERVATIONS = 2;

    private final List<SharedIndexInformer<?>> informers;

    private final List<WorkQueue<?>> workQueues;

    private int drainedStreak;

    private volatile boolean ready;

    public K8sCacheReadiness(final Collection<SharedIndexInformer<?>> informers,
                             final Collection<WorkQueue<?>> workQueues) {
        if (Objects.isNull(informers) || informers.isEmpty()) {
            throw new IllegalArgumentException("At least one informer is required");
        }
        this.informers = List.copyOf(informers);
        this.workQueues = Objects.isNull(workQueues) ? List.of() : List.copyOf(workQueues);
    }

    public boolean isReady() {
        if (ready) {
            return true;
        }
        if (informersSynced() && workQueues.stream().allMatch(queue -> queue.length() == 0)) {
            drainedStreak++;
            ready = drainedStreak >= REQUIRED_DRAINED_OBSERVATIONS;
        } else {
            drainedStreak = 0;
        }
        return ready;
    }

    public long pendingInformers() {
        return informers.stream().filter(informer -> !informer.hasSynced()).count();
    }

    /**
     * Pending items across the controller work queues: the initial reconciliation backlog
     * each not-yet-ready pod must still process before serving traffic.
     *
     * @return total number of queued items
     */
    public long pendingWorkItems() {
        return workQueues.stream().mapToInt(WorkQueue::length).sum();
    }

    private boolean informersSynced() {
        return informers.stream().allMatch(SharedIndexInformer::hasSynced);
    }
}
