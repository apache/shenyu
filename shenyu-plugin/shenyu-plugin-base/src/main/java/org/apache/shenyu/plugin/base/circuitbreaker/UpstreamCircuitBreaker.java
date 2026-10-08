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

package org.apache.shenyu.plugin.base.circuitbreaker;

import org.apache.shenyu.loadbalancer.entity.Upstream;
import reactor.core.publisher.Mono;
import reactor.core.publisher.SignalType;

import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

/**
 * A lightweight built-in circuit breaker for divide/http proxy upstreams.
 *
 * <p>State is tracked per selector-upstream pair. When an upstream accumulates
 * {@code failureThreshold} consecutive failures, its breaker opens and further
 * requests fail fast without sending any traffic to the upstream. After
 * {@code halfOpenWaitMillis} the breaker allows a single half-open probe
 * request; if the probe succeeds the breaker closes again, otherwise it
 * opens for another wait window.
 *
 * <p>This breaker is enabled by default and is fully independent from the
 * optional resilience4j plugin: when no failure occurs it only performs a few
 * map lookups, so the overhead on the happy path is negligible.
 */
public final class UpstreamCircuitBreaker {

    /**
     * The default number of consecutive failures before the breaker opens.
     */
    public static final int DEFAULT_FAILURE_THRESHOLD = 3;

    /**
     * The default wait time in milliseconds before a half-open probe is allowed.
     */
    public static final long DEFAULT_HALF_OPEN_WAIT_MILLIS = 10000L;

    private static final UpstreamCircuitBreaker INSTANCE =
            new UpstreamCircuitBreaker(DEFAULT_FAILURE_THRESHOLD, DEFAULT_HALF_OPEN_WAIT_MILLIS);

    private final int failureThreshold;

    private final long halfOpenWaitMillis;

    private final ConcurrentMap<String, CircuitBreakerState> breakers;

    private UpstreamCircuitBreaker(final int failureThreshold, final long halfOpenWaitMillis) {
        this.failureThreshold = failureThreshold;
        this.halfOpenWaitMillis = halfOpenWaitMillis;
        this.breakers = new ConcurrentHashMap<>(16);
    }

    /**
     * Get the singleton instance used by the gateway plugins.
     *
     * @return the shared {@link UpstreamCircuitBreaker} instance
     */
    public static UpstreamCircuitBreaker getInstance() {
        return INSTANCE;
    }

    /**
     * Create a standalone breaker with custom thresholds, mainly for testing.
     *
     * @param failureThreshold consecutive failures required to open the breaker
     * @param halfOpenWaitMillis wait time in milliseconds before a half-open probe is allowed
     * @return a new {@link UpstreamCircuitBreaker} instance
     */
    public static UpstreamCircuitBreaker create(final int failureThreshold, final long halfOpenWaitMillis) {
        return new UpstreamCircuitBreaker(failureThreshold, halfOpenWaitMillis);
    }

    /**
     * Build the breaker key for the given selector and upstream.
     *
     * @param selectorId the selector id the upstream belongs to
     * @param upstream the upstream
     * @return the breaker key
     */
    public static String buildKey(final String selectorId, final Upstream upstream) {
        return selectorId + ":" + upstream.buildDomain();
    }

    /**
     * Whether a request to the given upstream is currently allowed.
     *
     * <p>Requests are allowed when the breaker is closed, half-open, or open
     * but already past its half-open wait window (i.e. eligible to be probed).
     *
     * @param key the breaker key
     * @return true if a request may be sent to the upstream
     */
    public boolean isRequestAllowed(final String key) {
        CircuitBreakerState state = this.breakers.get(key);
        if (Objects.isNull(state)) {
            return true;
        }
        return state.isRequestAllowed();
    }

    /**
     * Atomically move an open breaker past its wait window into the half-open
     * state and grant a single probe request.
     *
     * @param key the breaker key
     * @return true if the caller is granted the probe request
     */
    public boolean tryAcquireHalfOpenProbe(final String key) {
        CircuitBreakerState state = this.breakers.get(key);
        if (Objects.isNull(state)) {
            return false;
        }
        return state.tryAcquireHalfOpen();
    }

    /**
     * Record a successful request, closing the breaker and clearing failures.
     *
     * @param key the breaker key
     */
    public void recordSuccess(final String key) {
        CircuitBreakerState state = this.breakers.get(key);
        if (Objects.nonNull(state)) {
            state.recordSuccess();
        }
    }

    /**
     * Record a failed request; the breaker opens once the consecutive failure
     * threshold is reached or a half-open probe fails.
     *
     * @param key the breaker key
     */
    public void recordFailure(final String key) {
        this.breakers.computeIfAbsent(key, k -> new CircuitBreakerState(this.failureThreshold, this.halfOpenWaitMillis))
                .recordFailure();
    }

    /**
     * Whether the breaker currently blocks requests for the given key.
     *
     * @param key the breaker key
     * @return true if the breaker is open and still within its wait window
     */
    public boolean isBlocking(final String key) {
        CircuitBreakerState state = this.breakers.get(key);
        if (Objects.isNull(state)) {
            return false;
        }
        return state.isBlocking();
    }

    /**
     * Remove the state for the given key, mainly for testing.
     *
     * @param key the breaker key
     */
    public void reset(final String key) {
        this.breakers.remove(key);
    }

    /**
     * Wrap a forwarded {@link Mono} so that its terminal signal is recorded as
     * a success or a failure of the upstream identified by the given key.
     *
     * @param <T> the element type of the source mono
     * @param source the mono executing the upstream call
     * @param key the breaker key
     * @return the wrapped mono with outcome recording attached
     */
    public static <T> Mono<T> recordOutcome(final Mono<T> source, final String key) {
        return source.doFinally(signal -> {
            if (signal == SignalType.ON_COMPLETE) {
                INSTANCE.recordSuccess(key);
            } else if (signal == SignalType.ON_ERROR) {
                INSTANCE.recordFailure(key);
            }
        });
    }

    /**
     * Mutable state of a single breaker.
     */
    private static final class CircuitBreakerState {

        private enum State {
            /**
             * Requests flow normally.
             */
            CLOSED,
            /**
             * Requests fail fast until the wait window elapses.
             */
            OPEN,
            /**
             * A single probe request is in flight.
             */
            HALF_OPEN
        }

        private final int failureThreshold;

        private final long halfOpenWaitMillis;

        private State state;

        private int consecutiveFailures;

        private long openedAtMillis;

        CircuitBreakerState(final int failureThreshold, final long halfOpenWaitMillis) {
            this.failureThreshold = failureThreshold;
            this.halfOpenWaitMillis = halfOpenWaitMillis;
            this.state = State.CLOSED;
        }

        synchronized boolean isRequestAllowed() {
            if (this.state == State.OPEN) {
                return System.currentTimeMillis() - this.openedAtMillis >= this.halfOpenWaitMillis;
            }
            return true;
        }

        synchronized boolean isBlocking() {
            return !isRequestAllowed();
        }

        synchronized boolean tryAcquireHalfOpen() {
            if (this.state == State.OPEN
                    && System.currentTimeMillis() - this.openedAtMillis >= this.halfOpenWaitMillis) {
                this.state = State.HALF_OPEN;
                return true;
            }
            return false;
        }

        synchronized void recordSuccess() {
            this.state = State.CLOSED;
            this.consecutiveFailures = 0;
        }

        synchronized void recordFailure() {
            if (this.state == State.HALF_OPEN) {
                open();
                return;
            }
            this.consecutiveFailures++;
            if (this.consecutiveFailures >= this.failureThreshold) {
                open();
            }
        }

        private void open() {
            this.state = State.OPEN;
            this.openedAtMillis = System.currentTimeMillis();
        }
    }
}
