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

package org.apache.shenyu.plugin.ai.token.limiter.budget;

import org.apache.shenyu.common.metrics.AiTokenBudgetObserver;
import org.apache.shenyu.common.metrics.AiTokenBudgetObserver.Event;
import org.apache.shenyu.plugin.ai.token.limiter.budget.RedisTokenBudgetStore.CompletionResult;
import org.apache.shenyu.plugin.ai.token.limiter.budget.RedisTokenBudgetStore.ReservationResult;
import org.apache.shenyu.plugin.ai.token.limiter.budget.RedisTokenBudgetStore.SettlementResult;
import org.apache.shenyu.plugin.ai.token.limiter.budget.RedisTokenBudgetStore.UsageOutcome;
import reactor.core.publisher.Mono;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.Duration;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

/**
 * One subscription's budget identity and conservative forwarding/cleanup state.
 */
public final class TokenBudgetRequest {

    private static final Logger LOG = LoggerFactory.getLogger(TokenBudgetRequest.class);

    private static final int NOT_FORWARDED = 0;

    private static final int FORWARDED = 1;

    private static final int CLOSED = 2;

    private final RedisTokenBudgetStore store;

    private final AiTokenBudgetObserver observer;

    private final String requestId = UUID.randomUUID().toString();

    private final AtomicInteger state = new AtomicInteger(NOT_FORWARDED);

    private final AtomicReference<Long> usage = new AtomicReference<>();

    private volatile TokenBudgetWindow window;

    private volatile boolean reservationAttempted;

    public TokenBudgetRequest(final RedisTokenBudgetStore store) {
        this(store, null);
    }

    public TokenBudgetRequest(final RedisTokenBudgetStore store, final AiTokenBudgetObserver observer) {
        this.store = Objects.requireNonNull(store, "store");
        this.observer = observer;
    }

    /**
     * Attempt admission, reselecting once if the fixed window changed before Redis admission.
     *
     * @param scope rule/key budget scope
     * @param windowSize configured fixed window size
     * @param limit configured token budget
     * @param amount per-request estimate
     * @return reservation outcome; a repeatedly stale window is an error
     */
    public Mono<ReservationResult> reserve(final String scope, final Duration windowSize, final long limit, final long amount) {
        return reserve(scope, windowSize, limit, amount, 1).doOnNext(result -> {
            if (result == ReservationResult.RESERVED) {
                observe(Event.ADMITTED, amount);
            } else if (result == ReservationResult.REJECTED) {
                observe(Event.REJECTED, 0L);
            }
        }).doOnError(ignored -> observe(Event.ADMISSION_ERROR, 0L));
    }

    private Mono<ReservationResult> reserve(final String scope, final Duration windowSize, final long limit, final long amount, final int retries) {
        return store.currentWindow(scope, windowSize).flatMap(selected -> Mono.defer(() -> {
            if (state.get() == CLOSED) {
                return Mono.empty();
            }
            window = selected;
            reservationAttempted = true;
            return store.reserve(selected, requestId, limit, amount).flatMap(result -> {
                if (result != ReservationResult.RESERVED) {
                    reservationAttempted = false;
                }
                if (result == ReservationResult.WINDOW_NOT_CURRENT) {
                    if (retries > 0) {
                        return reserve(scope, windowSize, limit, amount, retries - 1);
                    }
                    return Mono.error(new IllegalStateException("AI token budget window changed repeatedly during admission"));
                }
                return Mono.just(result);
            });
        }));
    }

    /**
     * Mark possible forwarding before invoking the downstream plugin chain.
     * Cleanup and forwarding race atomically, so a released request cannot subsequently start forwarding.
     *
     * @return whether forwarding may start
     */
    public boolean beginForwarding() {
        return state.compareAndSet(NOT_FORWARDED, FORWARDED);
    }

    /**
     * Retain reliable reported usage until request cleanup, including a reported zero.
     *
     * @param tokens reported completion tokens
     */
    public void recordUsage(final long tokens) {
        if (tokens >= 0L && tokens <= TokenBudgetWindow.MAX_EXACT_INTEGER) {
            usage.set(tokens);
        }
    }

    /**
     * Reconcile the original window or conservatively complete without usage.
     * Cancellation during Redis admission can leave an unacknowledged reservation; its window TTL remains the bound.
     *
     * @return asynchronous cleanup completion
     */
    public Mono<Void> finish() {
        return Mono.defer(() -> {
            final int previous = state.getAndSet(CLOSED);
            final TokenBudgetWindow original = window;
            if (previous == CLOSED || !reservationAttempted || Objects.isNull(original)) {
                return Mono.empty();
            }
            final Long reported = usage.get();
            if (Objects.nonNull(reported)) {
                observe(Event.USAGE_REPORTED, reported);
                return store.settle(original.getKey(), requestId, reported).doOnNext(result -> {
                    if (result == SettlementResult.SETTLED) {
                        observe(Event.SETTLED, 0L);
                    } else if (result == SettlementResult.WINDOW_NOT_FOUND) {
                        observe(Event.WINDOW_EXPIRED, 0L);
                    } else if (result == SettlementResult.RESERVATION_NOT_FOUND) {
                        observe(Event.RESERVATION_MISSING, 0L);
                    }
                }).then();
            }
            final UsageOutcome outcome = previous == FORWARDED ? UsageOutcome.USAGE_UNKNOWN : UsageOutcome.NOT_FORWARDED;
            return store.completeWithoutUsage(original.getKey(), requestId, outcome).doOnNext(result -> {
                if (result == CompletionResult.RELEASED) {
                    observe(Event.RELEASED, 0L);
                } else if (result == CompletionResult.USAGE_HELD) {
                    observe(Event.USAGE_HELD, 0L);
                } else if (result == CompletionResult.WINDOW_NOT_FOUND) {
                    observe(Event.WINDOW_EXPIRED, 0L);
                } else if (result == CompletionResult.RESERVATION_NOT_FOUND) {
                    observe(Event.RESERVATION_MISSING, 0L);
                }
            }).then();
        });
    }

    /** Record failed or timed-out completion accounting without changing request behavior. */
    public void accountingFailed() {
        observe(Event.ACCOUNTING_ERROR, 0L);
    }

    private void observe(final Event event, final long tokens) {
        if (Objects.nonNull(observer)) {
            try {
                observer.record(event, tokens);
            } catch (RuntimeException failure) {
                LOG.warn("Failed to observe AI token budget event", failure);
            }
        }
    }
}
