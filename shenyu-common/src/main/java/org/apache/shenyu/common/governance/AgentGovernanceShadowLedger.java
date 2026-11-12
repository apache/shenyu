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

package org.apache.shenyu.common.governance;

import java.util.Collections;
import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Bounded, in-memory shadow-state example used by the shared contract tests.
 * It never mutates enforce quota or budget state, and a generation is immutable
 * after replacement so in-flight work can settle in the ledger that admitted it.
 */
public final class AgentGovernanceShadowLedger {

    private final String generation;

    private final long capacity;

    private final Map<String, ExpirableReservation> active;

    private final Map<AgentGovernanceDecision.Reason, AtomicLong> denied;

    private final Clock clock;

    private final AtomicLong accepted;

    private final AtomicLong observedDenials;

    private final AtomicLong overflowed;

    private final AtomicLong settled;

    private final AtomicLong cancelled;

    private final AtomicLong expired;

    private boolean closed;

    public AgentGovernanceShadowLedger(final String generation, final long capacity) {
        this(generation, capacity, Clock.systemUTC());
    }

    AgentGovernanceShadowLedger(final String generation, final long capacity, final Clock clock) {
        if (Objects.isNull(generation) || generation.isBlank() || generation.length() > 128) {
            throw new IllegalArgumentException("generation must be bounded nonblank text");
        }
        if (capacity < 1) {
            throw new IllegalArgumentException("capacity must be positive");
        }
        this.generation = generation;
        this.capacity = capacity;
        this.active = new HashMap<>();
        this.clock = Objects.requireNonNull(clock, "clock");
        this.denied = new EnumMap<>(AgentGovernanceDecision.Reason.class);
        this.accepted = new AtomicLong();
        this.observedDenials = new AtomicLong();
        this.overflowed = new AtomicLong();
        this.settled = new AtomicLong();
        this.cancelled = new AtomicLong();
        this.expired = new AtomicLong();
    }

    public synchronized Reservation begin(final AgentGovernanceDecision decision, final long amount) {
        return begin(decision, amount, Duration.ofMinutes(5));
    }

    public synchronized Reservation begin(final AgentGovernanceDecision decision, final long amount,
                                          final Duration reservationTtl) {
        Objects.requireNonNull(decision, "decision");
        if (amount < 1) {
            throw new IllegalArgumentException("amount must be positive");
        }
        if (Objects.isNull(reservationTtl) || reservationTtl.isZero() || reservationTtl.isNegative()) {
            throw new IllegalArgumentException("reservationTtl must be positive");
        }
        if (decision.getMode() != AgentGovernanceDecision.Mode.SHADOW
                || decision.getResult() != AgentGovernanceDecision.Result.ALLOW
                || !decision.getLedgerGeneration().equals(generation)) {
            throw new IllegalArgumentException("decision does not belong to this shadow ledger");
        }
        evictExpired();
        if (closed) {
            return null;
        }
        if (active.containsKey(decision.getCorrelationId())
                || capacity - sumActive() < amount) {
            overflowed.incrementAndGet();
            return null;
        }
        Reservation reservation = new Reservation(generation, decision.getCorrelationId(), amount);
        active.put(decision.getCorrelationId(), new ExpirableReservation(reservation,
                clock.instant().plus(reservationTtl)));
        accepted.incrementAndGet();
        return reservation;
    }

    public synchronized boolean observe(final AgentGovernanceDecision decision) {
        Objects.requireNonNull(decision, "decision");
        if (decision.getMode() != AgentGovernanceDecision.Mode.SHADOW
                || decision.getResult() != AgentGovernanceDecision.Result.WOULD_DENY
                || !decision.getLedgerGeneration().equals(generation)) {
            throw new IllegalArgumentException("decision does not belong to this shadow ledger");
        }
        if (closed) {
            return false;
        }
        denied.computeIfAbsent(decision.getReason(), ignored -> new AtomicLong()).incrementAndGet();
        observedDenials.incrementAndGet();
        return true;
    }

    public synchronized boolean settle(final Reservation reservation) {
        return complete(reservation, false);
    }

    public synchronized boolean cancel(final Reservation reservation) {
        return complete(reservation, true);
    }

    private boolean complete(final Reservation reservation, final boolean cancelledOperation) {
        Objects.requireNonNull(reservation, "reservation");
        evictExpired();
        ExpirableReservation current = active.get(reservation.correlationId);
        if (!reservation.generation.equals(generation) || Objects.isNull(current)
                || current.reservation() != reservation) {
            return false;
        }
        active.remove(reservation.correlationId);
        if (cancelledOperation) {
            cancelled.incrementAndGet();
        } else {
            settled.incrementAndGet();
        }
        return true;
    }

    public synchronized AgentGovernanceShadowSnapshot snapshot() {
        Map<AgentGovernanceDecision.Reason, Long> counts = new EnumMap<>(AgentGovernanceDecision.Reason.class);
        denied.forEach((reason, count) -> counts.put(reason, count.get()));
        evictExpired();
        return new AgentGovernanceShadowSnapshot(generation, capacity, sumActive(),
                Collections.unmodifiableMap(counts), accepted.get(), observedDenials.get(),
                overflowed.get(), settled.get(), cancelled.get(), expired.get());
    }

    public synchronized void close() {
        closed = true;
    }

    private long sumActive() {
        return active.values().stream().mapToLong(value -> value.reservation().amount).sum();
    }

    private void evictExpired() {
        Instant now = clock.instant();
        active.values().removeIf(value -> {
            boolean expiredReservation = !value.deadline().isAfter(now);
            if (expiredReservation) {
                expired.incrementAndGet();
            }
            return expiredReservation;
        });
    }

    public static final class Reservation {

        private final String generation;

        private final String correlationId;

        private final long amount;

        private Reservation(final String generation, final String correlationId, final long amount) {
            this.generation = generation;
            this.correlationId = correlationId;
            this.amount = amount;
        }

        public String getGeneration() {
            return generation;
        }

        public String getCorrelationId() {
            return correlationId;
        }
    }

    private record ExpirableReservation(Reservation reservation, Instant deadline) {
    }

    public record AgentGovernanceShadowSnapshot(
            String generation, long capacity, long active,
            Map<AgentGovernanceDecision.Reason, Long> deniedByReason,
            long accepted, long observedDenials, long overflowed, long settled,
            long cancelled, long expired) {
    }
}
