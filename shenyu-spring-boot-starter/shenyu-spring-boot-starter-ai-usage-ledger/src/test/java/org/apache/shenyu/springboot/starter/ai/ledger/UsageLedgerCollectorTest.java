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

package org.apache.shenyu.springboot.starter.ai.ledger;

import org.apache.shenyu.common.ledger.LedgerRecord;

import org.apache.shenyu.common.metrics.AiAttemptEvent;
import org.apache.shenyu.common.metrics.AiUsage;
import org.apache.shenyu.common.metrics.LoggingQueueMetrics;
import org.junit.jupiter.api.Test;

import java.time.Duration;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class UsageLedgerCollectorTest {

    @Test
    void recorderConsumesAttemptsOnceAndNeverBillsTerminalUsageOrCacheHits() throws Exception {
        InMemoryUsageLedger store = new InMemoryUsageLedger(30, Duration.ofMinutes(1));
        UsageLedgerCollector collector = new UsageLedgerCollector(30, store);
        collector.start();
        try {
            AiUsageLedgerRecorder recorder = new AiUsageLedgerRecorder(LedgerTestFixtures.prices(), collector,
                    LedgerAttributionResolver.fromContext("namespace"), LedgerUsageResolver.currentContract(), LedgerTestFixtures.CLOCK);
            AiAttemptEvent event = LedgerTestFixtures.observed("request", "a", 1, AiAttemptEvent.Kind.PRIMARY,
                    new AiUsage(3L, 12L, 15L, AiUsage.Status.REPORTED));
            recorder.recordAttempt(event);
            recorder.recordAttempt(event);
            recorder.record(LedgerTestFixtures.completed("request", "a", 1, event.usage()));
            recorder.record(LedgerTestFixtures.completed("cached", "a", 0, event.usage()));
            await().atMost(Duration.ofSeconds(5)).until(() -> store.diagnostics().accepted() == 3);
            UsageLedgerService.Summary summary = new UsageLedgerService(store, LedgerTestFixtures.prices(), LedgerTestFixtures.CLOCK)
                    .export(LedgerTestFixtures.scope("a"), 100).summary();
            assertEquals(1, summary.actualAttempts());
            assertEquals(1, summary.noOutboundRequests());
            assertEquals(1, store.diagnostics().duplicates());
            assertEquals(0, recorder.invalidRecords());
            assertTrue(store.query(LedgerTestFixtures.scope("b"), 100).isEmpty());
            assertTrue(store.query(LedgerTestFixtures.scope("a"), 100).stream().allMatch(row -> java.util.Objects.isNull(row.facts().apiKeyId())));
        } finally {
            collector.close();
        }
    }

    @Test
    void sinkFailuresDoNotEscapeTheProducerAndRemainObservable() throws Exception {
        UsageLedgerStore broken = new DelegatingStore(new InMemoryUsageLedger(10, Duration.ofMinutes(1))) {
            @Override
            public AppendResult append(final LedgerRecord row) {
                throw new IllegalStateException("private backend credential must not reach a request");
            }
        };
        UsageLedgerCollector collector = new UsageLedgerCollector(10, broken);
        collector.start();
        try {
            collector.submit(LedgerTestFixtures.terminal("request", "a", 0));
            await().atMost(Duration.ofSeconds(5)).until(() -> collector.failedRecords() == 1);
            assertEquals(0, broken.diagnostics().accepted());
        } finally {
            collector.close();
        }
    }

    @Test
    void existingQueueBoundsRejectOverflowWithoutBlockingTheCaller() throws Exception {
        CountDownLatch entered = new CountDownLatch(1);
        CountDownLatch release = new CountDownLatch(1);
        InMemoryUsageLedger stored = new InMemoryUsageLedger(100, Duration.ofMinutes(1));
        UsageLedgerStore blocked = new DelegatingStore(stored) {
            @Override
            public AppendResult append(final LedgerRecord row) {
                entered.countDown();
                try {
                    assertTrue(release.await(5, TimeUnit.SECONDS));
                } catch (InterruptedException error) {
                    Thread.currentThread().interrupt();
                    throw new IllegalStateException("fixture interrupted");
                }
                return super.append(row);
            }
        };
        UsageLedgerCollector collector = new UsageLedgerCollector(2, blocked);
        collector.start();
        try {
            collector.submit(LedgerTestFixtures.terminal("first", "a", 0));
            assertTrue(entered.await(5, TimeUnit.SECONDS));
            for (int i = 0; i < 30; i++) {
                collector.submit(LedgerTestFixtures.terminal("extra-" + i, "a", 0));
            }
            LoggingQueueMetrics metrics = LoggingQueueMetrics.registered().stream().filter(value -> value.getCollector().equals(UsageLedgerCollector.class.getName())
                    && value.getMode() == LoggingQueueMetrics.Mode.SINGLE).findFirst().orElseThrow();
            assertEquals(2, metrics.getCapacity());
            assertTrue(metrics.getQueuedRecords() <= 2);
            assertTrue(metrics.getDropped() >= 28);
            release.countDown();
        } finally {
            release.countDown();
            collector.close();
        }
        assertFalse(LoggingQueueMetrics.registered().stream().anyMatch(value -> value.getCollector().equals(UsageLedgerCollector.class.getName())));
    }

    @Test
    void explicitCompositeDeduplicatesDelegatesAndContainsFailures() throws Exception {
        InMemoryUsageLedger store = new InMemoryUsageLedger(30, Duration.ofMinutes(1));
        UsageLedgerCollector collector = new UsageLedgerCollector(30, store);
        AtomicInteger delivered = new AtomicInteger();
        org.apache.shenyu.common.metrics.AiCallObserver existing = event -> delivered.incrementAndGet();
        org.apache.shenyu.common.metrics.AiCallObserver failing = event -> {
            throw new IllegalStateException("must-not-be-logged");
        };
        collector.start();
        try {
            LedgerObserver observer = new LedgerObserver(new AiUsageLedgerRecorder(LedgerTestFixtures.prices(), collector,
                    LedgerAttributionResolver.fromContext("namespace"), LedgerUsageResolver.currentContract(), LedgerTestFixtures.CLOCK),
                    List.of(existing, existing, failing));
            observer.record(LedgerTestFixtures.completed("request", "a", 0, AiUsage.UNKNOWN));
            await().atMost(Duration.ofSeconds(5)).until(() -> store.diagnostics().accepted() == 1);
            assertEquals(1, delivered.get());
            assertEquals(1, observer.failedDelegateCallbacks());
            assertEquals(observer, org.apache.shenyu.common.metrics.AiCallObserver.select(List.of(existing, observer)));
        } finally {
            collector.close();
        }
    }

    static class DelegatingStore implements UsageLedgerStore {

        private final UsageLedgerStore delegate;

        DelegatingStore(final UsageLedgerStore delegate) {
            this.delegate = delegate;
        }

        @Override
        public AppendResult append(final LedgerRecord row) {
            return delegate.append(row);
        }

        @Override
        public List<LedgerRecord> query(final Scope scope, final int limit) {
            return delegate.query(scope, limit);
        }

        @Override
        public LedgerRecord find(final Scope scope, final String id) {
            return delegate.find(scope, id);
        }

        @Override
        public int delete(final Scope scope) {
            return delegate.delete(scope);
        }

        @Override
        public Diagnostics diagnostics() {
            return delegate.diagnostics();
        }
    }
}
