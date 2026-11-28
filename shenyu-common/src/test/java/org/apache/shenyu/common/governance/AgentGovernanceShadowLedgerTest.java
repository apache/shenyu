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

import org.junit.jupiter.api.Test;

import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.Objects;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

final class AgentGovernanceShadowLedgerTest {

    private final Instant effectiveTime = Instant.parse("2026-10-26T10:00:00Z");

    @Test
    void shadowAllowConsumesAndReleasesIsolatedCapacity() {
        AgentGovernanceShadowLedger ledger = new AgentGovernanceShadowLedger("generation-1", 2);
        AgentGovernanceShadowLedger.Reservation reservation = ledger.begin(allow("request-1"), 2);
        assertNotNull(reservation);
        assertEquals(2, ledger.snapshot().active());
        assertTrue(ledger.settle(reservation));
        assertEquals(0, ledger.snapshot().active());
        assertEquals(1, ledger.snapshot().settled());
        assertFalse(ledger.settle(reservation));
    }

    @Test
    void generationDoesNotMoveOrDoubleSettleReservations() {
        AgentGovernanceShadowLedger oldLedger = new AgentGovernanceShadowLedger("generation-1", 2);
        AgentGovernanceShadowLedger newLedger = new AgentGovernanceShadowLedger("generation-2", 2);
        AgentGovernanceShadowLedger.Reservation reservation = oldLedger.begin(allow("request-1"), 1);
        assertFalse(newLedger.settle(reservation));
        assertTrue(oldLedger.settle(reservation));
        assertFalse(oldLedger.settle(reservation));
    }

    @Test
    void oldGenerationCanStillSettleAfterReplacement() {
        AgentGovernanceShadowLedger oldLedger = new AgentGovernanceShadowLedger("generation-1", 2);
        AgentGovernanceShadowLedger.Reservation reservation = oldLedger.begin(allow("request-1"), 1);
        oldLedger.close();
        assertTrue(oldLedger.settle(reservation));
        assertEquals(1, oldLedger.snapshot().settled());
    }

    @Test
    void capacityOverflowAndRepeatedWouldDenyAreObserved() {
        AgentGovernanceShadowLedger ledger = new AgentGovernanceShadowLedger("generation-1", 1);
        assertNotNull(ledger.begin(allow("request-1"), 1));
        assertNull(ledger.begin(allow("request-2"), 1));
        assertTrue(ledger.observe(wouldDeny("request-3")));
        AgentGovernanceShadowLedger.AgentGovernanceShadowSnapshot snapshot = ledger.snapshot();
        assertEquals(1, snapshot.active());
        assertEquals(1, snapshot.accepted());
        assertEquals(1, snapshot.observedDenials());
        assertEquals(1, snapshot.overflowed());
        assertEquals(0, snapshot.settled());
    }

    @Test
    void rejectsForeignAndClosedState() {
        AgentGovernanceShadowLedger ledger = new AgentGovernanceShadowLedger("generation-1", 1);
        assertThrows(IllegalArgumentException.class, () -> ledger.begin(wouldDeny("request-1"), 1));
        assertThrows(IllegalArgumentException.class, () -> ledger.begin(enforceDeny("request-1"), 1));
        AgentGovernanceShadowLedger.Reservation reservation = ledger.begin(allow("request-1"), 1);
        ledger.close();
        assertNull(ledger.begin(allow("request-2"), 1));
        assertFalse(ledger.observe(wouldDeny("request-3")));
    }

    @Test
    void cancellationAndExpiryReleaseCapacityExactlyOnce() {
        ZonedDateTime time = ZonedDateTime.parse("2026-10-26T10:00:00Z");
        MutableClock clock = new MutableClock(time);
        AgentGovernanceShadowLedger ledger = new AgentGovernanceShadowLedger("generation-1", 2, clock);
        final AgentGovernanceShadowLedger.Reservation cancelledReservation = ledger.begin(allow("request-1"), 1,
                Duration.ofSeconds(30));
        final AgentGovernanceShadowLedger.Reservation expiredReservation = ledger.begin(allow("request-2"), 1,
                Duration.ofSeconds(30));
        assertTrue(ledger.cancel(cancelledReservation));
        assertFalse(ledger.cancel(cancelledReservation));
        assertFalse(ledger.settle(cancelledReservation));
        clock.advanceSeconds(31);
        assertFalse(ledger.settle(expiredReservation));
        assertEquals(1, ledger.snapshot().cancelled());
        assertEquals(1, ledger.snapshot().expired());
        assertEquals(0, ledger.snapshot().active());
        assertNotNull(ledger.begin(allow("request-3"), 2));
    }

    @Test
    void concurrentAllowReservationsAreBounded() throws Exception {
        int tasks = 24;
        AgentGovernanceShadowLedger ledger = new AgentGovernanceShadowLedger("generation-1", 8);
        CountDownLatch ready = new CountDownLatch(tasks);
        CountDownLatch start = new CountDownLatch(1);
        ExecutorService executor = Executors.newFixedThreadPool(tasks);
        try {
            Future<Integer>[] results = new Future[tasks];
            for (int index = 0; index < tasks; index++) {
                final String correlationId = "request-" + index;
                results[index] = executor.submit(() -> {
                    ready.countDown();
                    assertTrue(start.await(5, TimeUnit.SECONDS));
                    return Objects.isNull(ledger.begin(allow(correlationId), 1)) ? 0 : 1;
                });
            }
            assertTrue(ready.await(5, TimeUnit.SECONDS));
            start.countDown();
            int accepted = 0;
            for (Future<Integer> result : results) {
                accepted += result.get(5, TimeUnit.SECONDS);
            }
            assertEquals(8, accepted);
        } finally {
            executor.shutdown();
            assertTrue(executor.awaitTermination(5, TimeUnit.SECONDS));
        }
        AgentGovernanceShadowLedger.AgentGovernanceShadowSnapshot snapshot = ledger.snapshot();
        assertEquals(8, snapshot.active());
        assertEquals(8, snapshot.accepted());
        assertEquals(tasks - 8, snapshot.overflowed());
    }

    @Test
    void shadowWouldDenyDoesNotConsumeAllowCapacity() {
        AgentGovernanceShadowLedger ledger = new AgentGovernanceShadowLedger("generation-1", 1);
        assertTrue(ledger.observe(wouldDeny("request-1")));
        assertNotNull(ledger.begin(allow("request-2"), 1));
        assertEquals(1, ledger.snapshot().observedDenials());
        assertEquals(1, ledger.snapshot().accepted());
    }

    private AgentGovernanceDecision wouldDeny(final String correlationId) {
        return AgentGovernanceDecision.builder(AgentGovernanceDecision.Consumer.MCP_QUOTA, AgentGovernanceDecision.Mode.SHADOW)
                .policyVersion("mcp-quota-v1").configurationVersion("configuration-1")
                .correlationId(correlationId).ledgerGeneration("generation-1").effectiveTime(effectiveTime)
                .result(AgentGovernanceDecision.Result.WOULD_DENY).reason(AgentGovernanceDecision.Reason.MCP_CONCURRENCY_LIMIT).build();
    }

    private AgentGovernanceDecision allow(final String correlationId) {
        return AgentGovernanceDecision.builder(AgentGovernanceDecision.Consumer.MCP_QUOTA, AgentGovernanceDecision.Mode.SHADOW)
                .policyVersion("mcp-quota-v1").configurationVersion("configuration-1")
                .correlationId(correlationId).ledgerGeneration("generation-1").effectiveTime(effectiveTime)
                .result(AgentGovernanceDecision.Result.ALLOW).reason(AgentGovernanceDecision.Reason.ALLOWED).build();
    }

    private AgentGovernanceDecision enforceDeny(final String correlationId) {
        return AgentGovernanceDecision.builder(AgentGovernanceDecision.Consumer.MCP_QUOTA, AgentGovernanceDecision.Mode.ENFORCE)
                .policyVersion("mcp-quota-v1").configurationVersion("configuration-1")
                .correlationId(correlationId).ledgerGeneration("generation-1").effectiveTime(effectiveTime)
                .result(AgentGovernanceDecision.Result.DENY).reason(AgentGovernanceDecision.Reason.MCP_CONCURRENCY_LIMIT).build();
    }

    private static final class MutableClock extends Clock {
        private ZonedDateTime value;

        private MutableClock(final ZonedDateTime value) {
            this.value = value;
        }

        void advanceSeconds(final long seconds) {
            value = value.plusSeconds(seconds);
        }

        @Override
        public ZoneId getZone() {
            return ZoneOffset.UTC;
        }

        @Override
        public Clock withZone(final ZoneId zone) {
            return this;
        }

        @Override
        public Instant instant() {
            return value.toInstant();
        }
    }
}
