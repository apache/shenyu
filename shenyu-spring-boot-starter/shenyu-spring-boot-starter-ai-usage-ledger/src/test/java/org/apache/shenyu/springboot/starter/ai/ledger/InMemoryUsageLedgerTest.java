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

import org.junit.jupiter.api.Test;

import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class InMemoryUsageLedgerTest {

    @Test
    void queryRejectsLimitsOutsideTheStoreContract() {
        InMemoryUsageLedger store = new InMemoryUsageLedger(2000, Duration.ofMinutes(1));
        assertThrows(IllegalArgumentException.class, () -> store.query(LedgerTestFixtures.scope("a"), 0));
        assertThrows(IllegalArgumentException.class, () -> store.query(LedgerTestFixtures.scope("a"), 1001));
        assertTrue(store.query(LedgerTestFixtures.scope("a"), 1000).isEmpty());
    }

    @Test
    void concurrentRedeliveryIsAtomicAndDoesNotDuplicateAmounts() throws Exception {
        InMemoryUsageLedger store = new InMemoryUsageLedger(100, Duration.ofMinutes(1));
        LedgerRecord row = LedgerTestFixtures.attempt("same", 1, "PRIMARY", "tenant", "main", LedgerTestFixtures.usage(3, 12));
        ExecutorService workers = Executors.newFixedThreadPool(8);
        CountDownLatch start = new CountDownLatch(1);
        try {
            List<Future<UsageLedgerStore.AppendResult>> results = new ArrayList<>();
            for (int i = 0; i < 64; i++) {
                results.add(workers.submit(() -> {
                    assertTrue(start.await(5, TimeUnit.SECONDS));
                    return store.append(row);
                }));
            }
            start.countDown();
            int accepted = 0;
            for (Future<UsageLedgerStore.AppendResult> result : results) {
                if (result.get(5, TimeUnit.SECONDS) == UsageLedgerStore.AppendResult.APPENDED) {
                    accepted++;
                }
            }
            assertEquals(1, accepted);
            assertEquals(63, store.diagnostics().duplicates());
            assertEquals(1, store.query(LedgerTestFixtures.scope("tenant"), 100).size());
        } finally {
            workers.shutdownNow();
        }
    }

    @Test
    void retentionAndCapacityBoundTheIdempotencyWindowWithoutEvictingLiveKeys() {
        AtomicLong time = new AtomicLong();
        InMemoryUsageLedger store = new InMemoryUsageLedger(1, Duration.ofMinutes(1), time::get);
        LedgerRecord row = LedgerTestFixtures.attempt("first", 1, "PRIMARY", "tenant", "main", LedgerTestFixtures.usage(1, 1));
        assertEquals(UsageLedgerStore.AppendResult.APPENDED, store.append(row));
        assertEquals(UsageLedgerStore.AppendResult.FULL, store.append(LedgerTestFixtures.terminal("another", "tenant", 0)));
        assertEquals(UsageLedgerStore.AppendResult.DUPLICATE, store.append(row));
        time.set(Duration.ofMinutes(1).toNanos());
        assertEquals(UsageLedgerStore.AppendResult.APPENDED, store.append(row));
        assertEquals(1, store.diagnostics().expired());
        assertEquals(1, store.diagnostics().rejected());
    }

    @Test
    void conflictingFactsDoNotRewriteHistoryAndScopesCannotReadOrDeleteOtherTenants() {
        InMemoryUsageLedger store = new InMemoryUsageLedger(10, Duration.ofMinutes(1));
        LedgerRecord first = LedgerTestFixtures.attempt("request", 1, "PRIMARY", "a", "main", LedgerTestFixtures.usage(1, 1));
        store.append(first);
        assertEquals(UsageLedgerStore.AppendResult.CONFLICT,
                store.append(LedgerTestFixtures.attempt("request", 1, "PRIMARY", "a", "main", LedgerTestFixtures.usage(20, 20))));
        store.append(LedgerTestFixtures.attempt("request", 1, "PRIMARY", "b", "main", LedgerTestFixtures.usage(2, 2)));
        assertNull(store.find(LedgerTestFixtures.scope("b"), first.id()));
        assertEquals(1, store.delete(LedgerTestFixtures.scope("b")));
        assertEquals(List.of(first), store.query(LedgerTestFixtures.scope("a"), 100));
        assertTrue(store.query(new UsageLedgerStore.Scope("other-namespace", "a", null), 100).isEmpty());
        assertTrue(store.query(new UsageLedgerStore.Scope("namespace", "a", "another-app"), 100).isEmpty());
    }
}
