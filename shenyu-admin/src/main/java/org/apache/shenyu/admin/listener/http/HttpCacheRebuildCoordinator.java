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

package org.apache.shenyu.admin.listener.http;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

/**
 * Bounded snapshot work, with one scheduled or running rebuild per namespace/group.
 * The first event sets the deadline; later events never postpone it.
 */
final class HttpCacheRebuildCoordinator implements AutoCloseable {

    private static final Logger LOG = LoggerFactory.getLogger(HttpCacheRebuildCoordinator.class);

    private static final long FAILURE_RETRY_MILLIS = 1000;

    private final Map<String, PendingRebuild> pending = new HashMap<>();

    private final ScheduledExecutorService executor;

    private final long windowMillis;

    private final int maxPendingKeys;

    private boolean closed;

    HttpCacheRebuildCoordinator(final ScheduledExecutorService executor, final long windowMillis, final int maxPendingKeys) {
        this.executor = executor;
        this.windowMillis = windowMillis;
        this.maxPendingKeys = maxPendingKeys;
    }

    /**
     * Mark a committed key dirty without retaining the event payload.
     *
     * @param key namespace/group key
     * @param rebuild reload, publish and notify action
     * @return false when the caller must rebuild synchronously (capacity or executor rejection)
     */
    boolean request(final String key, final Runnable rebuild) {
        synchronized (pending) {
            if (closed) {
                throw new IllegalStateException("HTTP cache rebuild coordinator is closed");
            }
            PendingRebuild existing = pending.get(key);
            if (Objects.nonNull(existing)) {
                existing.generation++;
                return true;
            }
            if (pending.size() >= maxPendingKeys) {
                return false;
            }
            PendingRebuild next = new PendingRebuild(rebuild);
            pending.put(key, next);
            return schedule(key, next, windowMillis);
        }
    }

    private boolean schedule(final String key, final PendingRebuild state, final long delayMillis) {
        try {
            executor.schedule(() -> drain(key, state), delayMillis, TimeUnit.MILLISECONDS);
            return true;
        } catch (RejectedExecutionException ex) {
            pending.remove(key, state);
            LOG.warn("HTTP cache rebuild scheduling rejected for {}", key, ex);
            return false;
        }
    }

    private void drain(final String key, final PendingRebuild state) {
        long generation;
        synchronized (pending) {
            if (closed) {
                return;
            }
            generation = state.generation;
        }
        boolean successful = false;
        try {
            state.rebuild.run();
            successful = true;
        } catch (Exception ex) {
            LOG.error("HTTP cache rebuild failed for {}, will retry", key, ex);
        } finally {
            synchronized (pending) {
                if (!closed) {
                    if (successful && generation == state.generation) {
                        pending.remove(key, state);
                    } else {
                        // Keep ownership until a successful rebuild covers all arrivals. Yield between
                        // generations so a hot or failing key cannot monopolize the worker.
                        schedule(key, state, successful ? windowMillis : Math.max(windowMillis, FAILURE_RETRY_MILLIS));
                    }
                }
            }
        }
    }

    @Override
    public void close() {
        synchronized (pending) {
            closed = true;
            pending.clear();
            executor.shutdownNow();
        }
    }

    private static final class PendingRebuild {

        private final Runnable rebuild;

        private long generation;

        private PendingRebuild(final Runnable rebuild) {
            this.rebuild = rebuild;
        }
    }
}
