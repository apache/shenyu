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
import org.springframework.web.server.ResponseStatusException;
import reactor.core.publisher.Mono;
import reactor.core.publisher.SignalType;

import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.atomic.AtomicBoolean;

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
     * Exchange attribute marking an exclusive, single-attempt recovery probe.
     */
    public static final String HALF_OPEN_PROBE = "shenyu.upstreamCircuitBreaker.halfOpenProbe";

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
        return upstream.buildSelectorKey(selectorId);
    }

    /**
     * Whether a request to the given upstream is currently allowed.
     *
     * <p>Only CLOSED breakers allow ordinary requests. Recovery traffic must
     * acquire a probe and record its outcome through that probe.
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
     * Acquire an exclusive recovery probe tied to this state incarnation.
     *
     * @param key the breaker key
     * @return the acquired probe, or null when unavailable
     */
    public HalfOpenProbe acquireHalfOpenProbe(final String key) {
        CircuitBreakerState state = breakers.get(key);
        if (Objects.isNull(state)) {
            return null;
        }
        synchronized (state) {
            return state.tryAcquireHalfOpen() ? new HalfOpenProbe(key, state, state.generation) : null;
        }
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
     * Re-open a breaker when its acquired half-open probe is cancelled.
     *
     * @param key the breaker key
     */
    public void recordProbeCancellation(final String key) {
        CircuitBreakerState state = this.breakers.get(key);
        if (Objects.nonNull(state)) {
            state.recordProbeCancellation();
        }
    }

    /**
     * Whether the breaker currently blocks requests for the given key.
     *
     * @param key the breaker key
     * @return true if ordinary requests are blocked
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

    private static boolean isClientError(final Throwable error) {
        return error instanceof ResponseStatusException
                && ((ResponseStatusException) error).getStatusCode().is4xxClientError();
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
        return Mono.defer(() -> {
            CircuitBreakerState initial = INSTANCE.breakers.get(key);
            long generation = Objects.isNull(initial) ? 0 : initial.getGeneration();
            // a client-caused 4xx (e.g. the gateway's own request-body limit) is not an
            // upstream failure; counting it would open the breaker for a healthy backend,
            // so classify the terminal error before the outcome reaches the state machine
            final AtomicBoolean clientError = new AtomicBoolean(false);
            return source.doOnError(error -> clientError.set(isClientError(error)))
                    .doFinally(signal -> {
                        if (signal != SignalType.ON_COMPLETE && signal != SignalType.ON_ERROR) {
                            return;
                        }
                        if (clientError.get()) {
                            return;
                        }
                        CircuitBreakerState current = INSTANCE.breakers.get(key);
                        if (Objects.isNull(initial)) {
                            if (Objects.isNull(current) && signal == SignalType.ON_ERROR) {
                                current = INSTANCE.breakers.computeIfAbsent(key,
                                        ignored -> new CircuitBreakerState(INSTANCE.failureThreshold, INSTANCE.halfOpenWaitMillis));
                            }
                        } else if (current != initial) {
                            return;
                        }
                        if (Objects.nonNull(current)) {
                            current.recordOrdinaryOutcome(generation, signal);
                        }
                    });
        });
    }

    /**
     * Record a request's outcome, including cancellation of an already granted probe.
     * The granted probe is bound to its current state and generation before execution.
     * @param <T> the element type of the source mono
     * @param source the mono executing the upstream call
     * @param key the breaker key
     * @param probeGranted whether this request acquired the half-open probe
     * @return the wrapped mono with outcome recording attached
     */
    public static <T> Mono<T> recordOutcome(final Mono<T> source, final String key, final boolean probeGranted) {
        if (!probeGranted) {
            return recordOutcome(source, key);
        }
        CircuitBreakerState state = INSTANCE.breakers.get(key);
        if (Objects.isNull(state)) {
            return source;
        }
        synchronized (state) {
            return INSTANCE.new HalfOpenProbe(key, state, state.generation).recordOutcome(source);
        }
    }

    /**
     * A recovery request's exclusive permission and terminal-signal recorder.
     */
    public final class HalfOpenProbe {

        private final String key;

        private final CircuitBreakerState state;

        private final long generation;

        private HalfOpenProbe(final String key, final CircuitBreakerState state, final long generation) {
            this.key = key;
            this.state = state;
            this.generation = generation;
        }

        /**
         * Record completion, error or cancellation only for this probe.
         *
         * @param <T> source element type
         * @param source the exclusively selected upstream request
         * @return the request with probe outcome recording
         */
        public <T> Mono<T> recordOutcome(final Mono<T> source) {
            return source.doFinally(signal -> {
                if (breakers.get(key) == state) {
                    state.recordProbeOutcome(generation, signal);
                }
            });
        }
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

        private long generation;

        CircuitBreakerState(final int failureThreshold, final long halfOpenWaitMillis) {
            this.failureThreshold = failureThreshold;
            this.halfOpenWaitMillis = halfOpenWaitMillis;
            this.state = State.CLOSED;
        }

        synchronized boolean isRequestAllowed() {
            return this.state == State.CLOSED;
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
            if (this.state != State.CLOSED) {
                generation++;
            }
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

        synchronized void recordProbeCancellation() {
            if (this.state == State.HALF_OPEN) {
                open();
            }
        }

        private void open() {
            generation++;
            this.state = State.OPEN;
            this.openedAtMillis = System.currentTimeMillis();
        }

        synchronized long getGeneration() {
            return generation;
        }

        synchronized void recordOrdinaryOutcome(final long expectedGeneration, final SignalType signal) {
            if (state == State.CLOSED && generation == expectedGeneration) {
                if (signal == SignalType.ON_COMPLETE) {
                    recordSuccess();
                } else {
                    recordFailure();
                }
            }
        }

        synchronized void recordProbeOutcome(final long expectedGeneration, final SignalType signal) {
            if (state == State.HALF_OPEN && generation == expectedGeneration) {
                if (signal == SignalType.ON_COMPLETE) {
                    recordSuccess();
                } else if (signal == SignalType.ON_ERROR || signal == SignalType.CANCEL) {
                    open();
                }
            }
        }
    }
}
