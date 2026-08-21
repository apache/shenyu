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

import io.kubernetes.client.informer.SharedIndexInformer;

import java.util.Collection;
import java.util.List;
import java.util.Objects;

/**
 * Aggregates the initial-sync state of all informers into a single readiness signal, used
 * to gate Kubernetes readiness so a cold pod (empty {@code BaseDataCache}) receives no
 * traffic until its local cache holds the full cluster state.
 *
 * <p>Readiness latches: once every informer completed its initial LIST, it reports ready
 * forever. During a transient API server outage the local cache still serves the last
 * known state, so flapping back to not-ready would only cause needless endpoint churn.
 */
public final class K8sCacheReadiness {

    private final List<SharedIndexInformer<?>> informers;

    private volatile boolean ready;

    public K8sCacheReadiness(final Collection<SharedIndexInformer<?>> informers) {
        if (Objects.isNull(informers) || informers.isEmpty()) {
            throw new IllegalArgumentException("At least one informer is required");
        }
        this.informers = List.copyOf(informers);
    }

    public boolean isReady() {
        if (ready) {
            return true;
        }
        ready = informers.stream().allMatch(SharedIndexInformer::hasSynced);
        return ready;
    }

    public long pendingInformers() {
        return informers.stream().filter(informer -> !informer.hasSynced()).count();
    }
}
