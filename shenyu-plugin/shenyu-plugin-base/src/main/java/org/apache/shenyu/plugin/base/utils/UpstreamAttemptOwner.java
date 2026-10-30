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

package org.apache.shenyu.plugin.base.utils;

import org.apache.shenyu.loadbalancer.entity.Upstream;

import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;

/**
 * Tracks which upstream attempt currently owns this request's inflight reservation.
 *
 * <p>An attempt owns a reservation exactly while it is the serving target. A failed
 * attempt releases only itself (identity CAS), a successful attempt hands ownership to
 * the final response lifetime, and a failover transfers ownership to the next target.
 * This keeps initial and retry attempts independently accountable (#8158).</p>
 */
public final class UpstreamAttemptOwner {

    /**
     * Exchange attribute containing the request's attempt owner.
     */
    public static final String ATTRIBUTE = UpstreamAttemptOwner.class.getName();

    private final AtomicReference<Upstream> current = new AtomicReference<>();

    private final Consumer<Upstream> releaser;

    /**
     * Create an owner for one request.
     *
     * @param releaser releases one attempt's reservation
     */
    public UpstreamAttemptOwner(final Consumer<Upstream> releaser) {
        this.releaser = Objects.requireNonNull(releaser);
    }

    /**
     * Get the upstream that currently owns the reservation.
     *
     * @return the current attempt, or null when no attempt is reserved
     */
    public Upstream current() {
        return current.get();
    }

    /**
     * Transfer ownership to a new attempt. A defensive previous value is released first.
     *
     * @param upstream the new attempt
     */
    public void move(final Upstream upstream) {
        if (Objects.isNull(upstream)) {
            return;
        }
        final Upstream previous = current.getAndSet(upstream);
        if (Objects.nonNull(previous) && previous != upstream) {
            releaser.accept(previous);
        }
    }

    /**
     * Release the current attempt if one is reserved.
     */
    public void releaseCurrent() {
        final Upstream serving = current.getAndSet(null);
        if (Objects.nonNull(serving)) {
            releaser.accept(serving);
        }
    }

    /**
     * Release a specific attempt only while it still owns the reservation.
     *
     * @param attempt the attempt that failed or was cancelled
     * @return true when this call released the attempt
     */
    public boolean release(final Upstream attempt) {
        if (Objects.nonNull(attempt) && current.compareAndSet(attempt, null)) {
            releaser.accept(attempt);
            return true;
        }
        return false;
    }
}
