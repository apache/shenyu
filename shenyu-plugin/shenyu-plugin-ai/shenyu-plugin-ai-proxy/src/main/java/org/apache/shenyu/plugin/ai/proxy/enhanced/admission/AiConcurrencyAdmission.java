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
import reactor.core.publisher.MonoSink;

import java.time.Duration;
import java.util.Iterator;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedDeque;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.Semaphore;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

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
 * <p>Waiting is a non-blocking FIFO waiter queue, not a blocking semaphore wait:
 * a waiter parks on a {@code Mono.create} sink (no scheduler thread is occupied
 * while waiting), a released permit is handed to the head of the queue, and the
 * waiter's subscription cancellation removes it from the queue — or, if the permit
 * was already handed over in the meantime, returns it to the pool, so a cancelled
 * wait can never leak a permit. The queue deadline is enforced by a reactor timeout
 * on the waiter, and expiry is reported as its own outcome, distinct from overload.
 * The fast path only barges when no waiter is queued, so the FIFO claim holds.
 * The pool map is hard-capped; when the cap is reached, fully idle pools (no
 * waiters, every permit free) are evicted before an arrival is rejected, so
 * caller-controlled keys cannot permanently consume the cap.</p>
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
     * Release a granted permit exactly once; a second release is ignored so it can
     * never inflate the semaphore. Releasing an unknown pool is a no-op so a late
     * release after a pool map eviction can never corrupt another pool. The release
     * hands the freed permit to the head of the wait queue.
     *
     * @param ticket the permit to release
     */
    public void release(final AdmissionTicket ticket) {
        if (Objects.isNull(ticket) || !ticket.released.compareAndSet(false, true)) {
            return;
        }
        Pool pool = pools.get(ticket.poolKey());
        if (Objects.nonNull(pool)) {
            pool.permits.release();
            dispatch(pool, ticket.poolKey());
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
        // every permit decision runs at subscription time, never at assembly: a
        // retry attempt assembles while the failed attempt may still hold its
        // permit, and only the post-delay subscription can see it released
        return Mono.defer(() -> {
            if (System.nanoTime() >= deadlineNanos) {
                // the shared deadline already ran out: its own outcome, distinct from overload
                return Mono.just(new AdmissionResult(AdmissionOutcome.WAIT_DEADLINE_EXCEEDED, null));
            }
            Pool pool = poolFor(poolKey, maxInflight);
            if (Objects.isNull(pool)) {
                return Mono.just(new AdmissionResult(AdmissionOutcome.OVERLOAD, null));
            }
            return Mono.<AdmissionResult>create(sink -> {
                // fast path: a free permit and no queued waiter; if a waiter raced in
                // between the two checks, the permit is handed back and dispatched to
                // the queue head instead of barging ahead of it
                if (pool.permits.tryAcquire()) {
                    if (pool.waiting.get() == 0) {
                        sink.success(new AdmissionResult(AdmissionOutcome.ACQUIRED, new AdmissionTicket(poolKey)));
                        return;
                    }
                    pool.permits.release();
                    dispatch(pool, poolKey);
                }
                Waiter waiter = new Waiter(sink, deadlineNanos, poolKey);
                if (pool.waiting.incrementAndGet() > Math.max(maxWaiting, 0)) {
                    pool.waiting.decrementAndGet();
                    sink.success(new AdmissionResult(AdmissionOutcome.OVERLOAD, null));
                    return;
                }
                sink.onDispose(() -> {
                    // a completed subscription also fires onDispose: only an unfinished
                    // hand-over needs cleanup. A waiter still queued is simply removed;
                    // one already dequeued by dispatch is marked cancelled so the
                    // hand-over below returns the permit instead of leaking it to a
                    // cancelled subscriber (#8254 review)
                    if (waiter.isDelivered()) {
                        return;
                    }
                    if (waiter.markCancelled()) {
                        if (pool.queue.remove(waiter)) {
                            pool.waiting.decrementAndGet();
                        }
                    } else if (waiter.isGranted()) {
                        // dispatch already decremented waiting when it dequeued; the hand
                        // back returns only the permit, never the counter again (#8254 re-review)
                        pool.permits.release();
                        dispatch(pool, poolKey);
                    }
                });
                pool.queue.addLast(waiter);
                dispatch(pool, poolKey);
            }).timeout(Duration.ofMillis(waitMillis(deadlineNanos)),
                    Mono.just(new AdmissionResult(AdmissionOutcome.WAIT_DEADLINE_EXCEEDED, null)));
        });
    }

    private long waitMillis(final long deadlineNanos) {
        long remainingMillis = (deadlineNanos - System.nanoTime()) / 1_000_000L;
        return Math.max(Math.min(Math.max(properties.getMaxWaitMillis(), 0L), remainingMillis), 0L);
    }

    /**
     * Hand free permits to the head of the wait queue. Waiters past their deadline
     * settle as their own outcome without consuming a permit.
     */
    private void dispatch(final Pool pool, final String poolKey) {
        while (pool.permits.availablePermits() > 0) {
            Waiter waiter = pool.queue.peekFirst();
            if (Objects.isNull(waiter)) {
                return;
            }
            if (waiter.expired() && waiter.markExpired()) {
                if (pool.queue.remove(waiter)) {
                    pool.waiting.decrementAndGet();
                }
                waiter.settleExpired();
                continue;
            }
            if (!pool.permits.tryAcquire()) {
                return;
            }
            // dispatch runs concurrently (release, fast-path hand-back, enqueue): another
            // caller may have taken this head. Removing by identity instead of pollFirst
            // never drops a different waiter whose turn has not come (#8254 re-review)
            if (!pool.queue.remove(waiter)) {
                pool.permits.release();
                continue;
            }
            pool.waiting.decrementAndGet();
            if (!waiter.grantAndSettle(new AdmissionTicket(poolKey))) {
                // the subscriber cancelled while the hand-over was in flight: return
                // the permit instead of granting it to nobody (#8254 review)
                pool.permits.release();
            }
        }
    }

    private Pool poolFor(final String poolKey, final int cap) {
        Pool existing = pools.get(poolKey);
        if (Objects.nonNull(existing)) {
            return existing;
        }
        if (pools.size() >= Math.max(properties.getMaxPools(), 0)) {
            evictIdlePools();
            if (pools.size() >= properties.getMaxPools()) {
                return null;
            }
        }
        return pools.computeIfAbsent(poolKey, key -> new Pool(cap));
    }

    /**
     * Evict fully idle pools (every permit free, nobody waiting) so caller-controlled
     * keys cannot permanently consume the map cap and starve later arrivals.
     */
    private void evictIdlePools() {
        for (Iterator<ConcurrentMap.Entry<String, Pool>> it = pools.entrySet().iterator(); it.hasNext();) {
            Pool pool = it.next().getValue();
            if (pool.waiting.get() == 0 && pool.permits.availablePermits() == pool.cap) {
                it.remove();
            }
        }
    }

    private static final class Pool {

        private final Semaphore permits;

        private final int cap;

        private final AtomicInteger waiting = new AtomicInteger();

        private final ConcurrentLinkedDeque<Waiter> queue = new ConcurrentLinkedDeque<>();

        private Pool(final int cap) {
            this.cap = cap;
            this.permits = new Semaphore(cap, true);
        }
    }

    private enum WaiterState {
        WAITING, GRANTED, CANCELLED, EXPIRED
    }

    private static final class Waiter {

        private final MonoSink<AdmissionResult> sink;

        private final long deadlineNanos;

        private final String poolKey;

        private final AtomicReference<WaiterState> state = new AtomicReference<>(WaiterState.WAITING);

        private volatile boolean delivered;

        Waiter(final MonoSink<AdmissionResult> sink, final long deadlineNanos, final String poolKey) {
            this.sink = sink;
            this.deadlineNanos = deadlineNanos;
            this.poolKey = poolKey;
        }

        boolean expired() {
            return System.nanoTime() >= deadlineNanos;
        }

        /**
         * Claim the waiter for cancellation; fails when dispatch already owns it.
         */
        boolean markCancelled() {
            return state.compareAndSet(WaiterState.WAITING, WaiterState.CANCELLED);
        }

        /**
         * Claim the waiter for expiry; fails when cancellation or a grant won it.
         */
        boolean markExpired() {
            return state.compareAndSet(WaiterState.WAITING, WaiterState.EXPIRED);
        }

        boolean isGranted() {
            return state.get() == WaiterState.GRANTED;
        }

        boolean isDelivered() {
            return delivered;
        }

        /**
         * Hand the granted ticket to the subscriber; reports whether the subscriber
         * was still there to receive it (a cancellation that won the race means the
         * permit must be returned by the caller).
         */
        boolean grantAndSettle(final AdmissionTicket ticket) {
            if (!state.compareAndSet(WaiterState.WAITING, WaiterState.GRANTED)) {
                return false;
            }
            delivered = true;
            sink.success(new AdmissionResult(AdmissionOutcome.ACQUIRED, ticket));
            return true;
        }

        void settleExpired() {
            sink.success(new AdmissionResult(AdmissionOutcome.WAIT_DEADLINE_EXCEEDED, null));
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
         * The waiting bound, the pool map cap or an unstartable arrival was
         * rejected before any work started.
         */
        OVERLOAD,

        /**
         * The bounded wait expired without a permit.
         */
        WAIT_DEADLINE_EXCEEDED
    }

    /**
     * A granted permit; the holder must release it once on the pool it names.
     * Release is idempotent: a second call is ignored.
     */
    public static final class AdmissionTicket {

        private final String poolKey;

        private final AtomicBoolean released = new AtomicBoolean();

        AdmissionTicket(final String poolKey) {
            this.poolKey = poolKey;
        }

        /**
         * The pool key this permit belongs to.
         *
         * @return the pool key
         */
        public String poolKey() {
            return poolKey;
        }
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
