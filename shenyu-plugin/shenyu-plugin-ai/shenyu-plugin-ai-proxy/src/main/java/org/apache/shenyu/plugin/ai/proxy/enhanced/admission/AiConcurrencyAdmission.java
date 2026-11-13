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

package org.apache.shenyu.plugin.ai.proxy.enhanced.admission;

import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;

import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Per-gateway concurrency admission for AI proxy traffic (issue #8254).
 *
 * <p>Two pool dimensions gate an invocation. The <em>logical</em> permit lives in
 * the trusted-tenant pool: one per invocation, held across retries, fallbacks and
 * repairs until the terminal signal. The <em>target</em> permit lives in the model
 * pool: one per actual attempt, released when that attempt ends — for a streaming
 * response that is body completion, error or cancellation, not the response
 * headers — so backoff between attempts holds no target capacity.</p>
 *
 * <p>Each pool is a fair {@link Semaphore} (deterministic FIFO queue discipline)
 * with a finite waiting bound; when the bound is reached new arrivals are
 * rejected immediately instead of queueing. Waiting consumes the shared request
 * deadline: the effective wait is the smaller of the configured maximum and the
 * deadline remainder, so time already queued counts. Waiting happens off the
 * event loop on the bounded-elastic scheduler, bounded by the waiting limits.
 * The pool map is hard-capped, so neither tenants nor models can grow the state
 * without bound.</p>
 */
public final class AiConcurrencyAdmission {

    private static final String TENANT_PREFIX = "tenant:";

    private static final String MODEL_PREFIX = "model:";

    private static final String ANONYMOUS = "anonymous";

    private final AiConcurrencyAdmissionProperties properties;

    private final ConcurrentMap<String, Pool> pools = new ConcurrentHashMap<>();

    public AiConcurrencyAdmission(final AiConcurrencyAdmissionProperties properties) {
        properties.validate();
        this.properties = properties;
    }

    /**
     * Acquire the logical permit of the trusted-tenant pool. A null tenant is
     * anonymous traffic and shares one pool; the identity never comes from wire
     * metadata.
     *
     * @param tenant trusted tenant from the authenticated context, may be {@code null}
     * @param deadlineNanos absolute shared request deadline
     * @return the admission result
     */
    public Mono<AdmissionResult> acquireTenant(final String tenant, final long deadlineNanos) {
        return acquire(TENANT_PREFIX + (Objects.isNull(tenant) || tenant.isBlank() ? ANONYMOUS : tenant),
                properties.getTenantMaxInflight(), properties.getTenantMaxWaiting(), deadlineNanos);
    }

    /**
     * Acquire the target permit of the model pool for one attempt.
     *
     * @param model the effective target model after overrides, may be {@code null}
     * @param deadlineNanos absolute shared request deadline
     * @return the admission result
     */
    public Mono<AdmissionResult> acquireTarget(final String model, final long deadlineNanos) {
        return acquire(MODEL_PREFIX + (Objects.isNull(model) || model.isBlank() ? ANONYMOUS : model),
                properties.getModelMaxInflight(), properties.getModelMaxWaiting(), deadlineNanos);
    }

    /**
     * Release a granted permit exactly once; releasing an unknown pool is a no-op
     * so a late release after a pool map eviction can never corrupt another pool.
     *
     * @param ticket the permit to release
     */
    public void release(final AdmissionTicket ticket) {
        if (Objects.isNull(ticket)) {
            return;
        }
        Pool pool = pools.get(ticket.poolKey());
        if (Objects.nonNull(pool)) {
            pool.permits.release();
        }
    }

    /**
     * Current in-flight count of one pool; bounded observability, never exported
     * as a raw tenant or model metric label.
     *
     * @param poolKey the pool key
     * @return the in-flight count, or {@code -1} for an unknown pool
     */
    public int inflightOf(final String poolKey) {
        Pool pool = pools.get(poolKey);
        return Objects.isNull(pool) ? -1 : pool.cap - pool.permits.availablePermits();
    }

    /**
     * Current waiting count of one pool.
     *
     * @param poolKey the pool key
     * @return the waiting count, or {@code -1} for an unknown pool
     */
    public int waitingOf(final String poolKey) {
        Pool pool = pools.get(poolKey);
        return Objects.isNull(pool) ? -1 : pool.waiting.get();
    }

    private Mono<AdmissionResult> acquire(final String poolKey, final int maxInflight, final int maxWaiting,
                                          final long deadlineNanos) {
        long remainingMillis = (deadlineNanos - System.nanoTime()) / 1_000_000L;
        if (remainingMillis <= 0L) {
            return Mono.just(new AdmissionResult(AdmissionOutcome.OVERLOAD, null));
        }
        Pool pool = poolFor(poolKey, maxInflight);
        if (Objects.isNull(pool)) {
            return Mono.just(new AdmissionResult(AdmissionOutcome.OVERLOAD, null));
        }
        long waitMillis = Math.min(properties.getMaxWaitMillis(), remainingMillis);
        // every permit decision runs at subscription time, never at assembly: a
        // retry attempt assembles while the failed attempt may still hold its
        // permit, and only the post-delay subscription can see it released
        return Mono.fromCallable(() -> {
            // fast path: a permit is free right now; the fair semaphore never barges
            // ahead of queued waiters, so this cannot jump the queue
            if (pool.permits.tryAcquire()) {
                return new AdmissionResult(AdmissionOutcome.ACQUIRED, new AdmissionTicket(poolKey));
            }
            if (pool.waiting.incrementAndGet() > maxWaiting) {
                pool.waiting.decrementAndGet();
                return new AdmissionResult(AdmissionOutcome.OVERLOAD, null);
            }
            boolean acquired = false;
            try {
                acquired = pool.permits.tryAcquire(waitMillis, TimeUnit.MILLISECONDS);
            } finally {
                pool.waiting.decrementAndGet();
            }
            if (acquired) {
                return new AdmissionResult(AdmissionOutcome.ACQUIRED, new AdmissionTicket(poolKey));
            }
            return new AdmissionResult(AdmissionOutcome.WAIT_DEADLINE_EXCEEDED, null);
        }).subscribeOn(Schedulers.boundedElastic());
    }

    private Pool poolFor(final String poolKey, final int cap) {
        Pool existing = pools.get(poolKey);
        if (Objects.nonNull(existing)) {
            return existing;
        }
        if (pools.size() >= properties.getMaxPools()) {
            return null;
        }
        return pools.computeIfAbsent(poolKey, key -> new Pool(cap));
    }

    private static final class Pool {

        private final Semaphore permits;

        private final int cap;

        private final AtomicInteger waiting = new AtomicInteger();

        private Pool(final int cap) {
            this.cap = cap;
            this.permits = new Semaphore(cap, true);
        }
    }

    /**
     * Queue discipline of one acquisition.
     */
    public enum AdmissionOutcome {
        /**
         * A permit was granted; the ticket must be released exactly once.
         */
        ACQUIRED,

        /**
         * The waiting bound, the pool map cap or a non-positive remaining
         * deadline rejected the arrival before any work started.
         */
        OVERLOAD,

        /**
         * The bounded wait expired without a permit.
         */
        WAIT_DEADLINE_EXCEEDED
    }

    /**
     * A granted permit; the holder must release it once on the pool it names.
     */
    public record AdmissionTicket(String poolKey) {
    }

    /**
     * One acquisition result.
     *
     * @param outcome queue outcome
     * @param ticket the permit when {@code ACQUIRED}, otherwise {@code null}
     */
    public record AdmissionResult(AdmissionOutcome outcome, AdmissionTicket ticket) {

        /**
         * The shared no-op result for a disabled pipeline.
         */
        public static final AdmissionResult ADMITTED =
                new AdmissionResult(AdmissionOutcome.ACQUIRED, null);
    }
}
