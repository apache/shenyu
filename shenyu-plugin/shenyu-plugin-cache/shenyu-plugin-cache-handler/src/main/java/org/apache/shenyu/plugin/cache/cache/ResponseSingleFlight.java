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

package org.apache.shenyu.plugin.cache.cache;

import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;

import java.time.Duration;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Coalesces concurrent cache misses for one key into a single upstream fill on this gateway instance.
 *
 * <p>The first request for a key leads the fill and executes the plugin chain; later requests for the
 * same key follow it and replay the captured response bytes once the leader publishes them. A follower
 * never subscribes to the leader's response publisher and never shares a {@code DataBuffer}: it decodes
 * its own copy of the bytes into its own response. When the leader cannot produce a reusable response,
 * or the follower waits too long, the follower fills the key itself, which is the behaviour without
 * coalescing.</p>
 *
 * <p>Coordination is per gateway instance. Replicas may still fill the same key independently, and no
 * distributed lock is taken. Keys are exactly the response cache keys, so coalescing never widens cache
 * eligibility or merges responses that the cache would have kept apart.</p>
 */
public final class ResponseSingleFlight {

    private static final ResponseSingleFlight DISABLED = new ResponseSingleFlight(0, 0, 0L, 0L, 0L);

    private final int maxKeys;

    private final int maxWaiters;

    private final long maxResponseBytes;

    private final Duration waitTimeout;

    private final long fillTimeoutMillis;

    private final ConcurrentMap<String, Fill> fills = new ConcurrentHashMap<>();

    /**
     * Instantiates a coordinator.
     *
     * @param maxKeys the maximum number of keys filling at once, zero or less disables coalescing
     * @param maxWaiters the maximum number of requests waiting on one key
     * @param waitTimeoutMillis how long a request waits for another request's fill
     * @param fillTimeoutMillis how long a key stays owned when its leader never completes
     * @param maxResponseBytes the largest response shared with waiting requests, zero or less shares none
     */
    public ResponseSingleFlight(final int maxKeys, final int maxWaiters, final long waitTimeoutMillis,
                                final long fillTimeoutMillis, final long maxResponseBytes) {
        this.maxKeys = maxKeys;
        this.maxWaiters = maxWaiters;
        this.waitTimeout = Duration.ofMillis(waitTimeoutMillis);
        this.fillTimeoutMillis = fillTimeoutMillis;
        this.maxResponseBytes = maxResponseBytes;
    }

    /**
     * A coordinator that never coalesces, used when the feature is switched off.
     *
     * @return the disabled coordinator
     */
    public static ResponseSingleFlight disabled() {
        return DISABLED;
    }

    /**
     * Try to lead the fill of a key.
     *
     * @param key the response cache key
     * @return the owned fill, or empty when the key is already filling or this instance is at capacity
     */
    public Optional<Fill> lead(final String key) {
        if (maxKeys <= 0) {
            return Optional.empty();
        }
        // The size check and putIfAbsent are not atomic, so concurrent leaders can exceed maxKeys by the
        // number of requests racing here. This bounds capacity; it is not an exact count.
        if (fills.size() >= maxKeys) {
            expireStale();
            if (fills.size() >= maxKeys) {
                return Optional.empty();
            }
        }
        final Fill candidate = new Fill(this, key, System.nanoTime());
        final Fill existing = fills.putIfAbsent(key, candidate);
        return Objects.isNull(existing) ? Optional.of(candidate) : Optional.empty();
    }

    /**
     * Follow the fill another request leads for a key.
     *
     * @param key the response cache key
     * @return a signal resolving to the reusable response packet, or empty when there is nothing to follow
     */
    public Optional<Mono<Optional<byte[]>>> follow(final String key) {
        // Sweep here as well as in lead(), so a fill whose leader vanished on an otherwise idle key is
        // released by time rather than waiting for the next request to reach the key limit.
        expireStale();
        // A fill that has already published stays visible until it is removed, so a request arriving in
        // that window still replays the packet instead of filling the key again.
        final Fill fill = fills.get(key);
        if (Objects.isNull(fill)) {
            return Optional.empty();
        }
        if (fill.waiters.incrementAndGet() > maxWaiters) {
            fill.waiters.decrementAndGet();
            return Optional.empty();
        }
        return Optional.of(fill.signal.asMono()
                .defaultIfEmpty(Optional.empty())
                .timeout(waitTimeout)
                .onErrorReturn(Optional.empty())
                .doFinally(ignored -> fill.waiters.decrementAndGet()));
    }

    /**
     * Publish a reusable response packet and release the key.
     *
     * <p>Only the first call for a fill takes effect, so a leader that already published a packet is not
     * affected by a later cancellation.</p>
     *
     * @param fill the owned fill
     * @param packet the encoded response, empty when the response cannot be reused
     */
    public void complete(final Fill fill, final Optional<byte[]> packet) {
        if (!fill.finished.compareAndSet(false, true)) {
            return;
        }
        final Optional<byte[]> shared = maxResponseBytes <= 0
                ? Optional.empty()
                : packet.filter(bytes -> bytes.length <= maxResponseBytes);
        // Publish before releasing the key so a request arriving in between still replays this fill.
        fill.signal.tryEmitValue(shared);
        fills.remove(fill.key, fill);
    }

    /**
     * Release the key without a reusable response, for example when the fill failed or was cancelled.
     *
     * @param fill the owned fill
     */
    public void release(final Fill fill) {
        complete(fill, Optional.empty());
    }

    /**
     * Release keys whose leader never completed, so one stuck request cannot hold a key forever.
     */
    private void expireStale() {
        final long now = System.nanoTime();
        final long timeoutNanos = Duration.ofMillis(fillTimeoutMillis).toNanos();
        fills.values().stream()
                .filter(fill -> now - fill.createdAtNanos > timeoutNanos)
                .forEach(this::release);
    }

    /**
     * One upstream fill owned by a single request.
     */
    public static final class Fill {

        private final ResponseSingleFlight owner;

        private final String key;

        private final long createdAtNanos;

        private final Sinks.One<Optional<byte[]>> signal = Sinks.one();

        private final AtomicInteger waiters = new AtomicInteger();

        private final AtomicBoolean finished = new AtomicBoolean();

        private Fill(final ResponseSingleFlight owner, final String key, final long createdAtNanos) {
            this.owner = owner;
            this.key = key;
            this.createdAtNanos = createdAtNanos;
        }

        /**
         * Publish a reusable response packet and release the key.
         *
         * @param packet the encoded response, empty when the response cannot be reused
         */
        public void complete(final Optional<byte[]> packet) {
            owner.complete(this, packet);
        }

        /**
         * Release the key without a reusable response.
         */
        public void release() {
            owner.release(this);
        }
    }
}
