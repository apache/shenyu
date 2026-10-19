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

package org.apache.shenyu.admin.listener.http;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayDeque;
import java.util.Queue;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.FutureTask;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Deterministic tests of deadlines, generations, retries and bounded admission.
 */
class HttpCacheRebuildCoordinatorTest {

    private ScheduledExecutorService executor;

    private Queue<Runnable> tasks;

    private HttpCacheRebuildCoordinator coordinator;

    @BeforeEach
    void setUp() {
        executor = mock(ScheduledExecutorService.class);
        tasks = new ArrayDeque<>();
        doAnswer(invocation -> {
            tasks.add(invocation.getArgument(0));
            return null;
        }).when(executor).schedule(any(Runnable.class), anyLong(), eq(TimeUnit.MILLISECONDS));
        coordinator = new HttpCacheRebuildCoordinator(executor, 10, 2);
    }

    @Test
    void burstKeepsFirstDeadlineAndReleasesIdleKey() {
        AtomicInteger rebuilds = new AtomicInteger();
        for (int index = 0; index < 1000; index++) {
            assertTrue(coordinator.request("a_RULE", rebuilds::incrementAndGet));
        }
        verify(executor, times(1)).schedule(any(Runnable.class), eq(10L), eq(TimeUnit.MILLISECONDS));
        tasks.remove().run();
        assertEquals(1, rebuilds.get());
        assertTrue(tasks.isEmpty());
        assertTrue(coordinator.request("a_RULE", rebuilds::incrementAndGet));
        tasks.remove().run();
        assertEquals(2, rebuilds.get());
    }

    @Test
    void concurrentArrivalDuringRebuildIsNotLost() throws Exception {
        CountDownLatch reading = new CountDownLatch(1);
        CountDownLatch release = new CountDownLatch(1);
        AtomicInteger database = new AtomicInteger(1);
        AtomicInteger published = new AtomicInteger();
        AtomicInteger calls = new AtomicInteger();
        Runnable rebuild = () -> {
            int snapshot = database.get();
            if (calls.incrementAndGet() == 1) {
                reading.countDown();
                try {
                    assertTrue(release.await(5, TimeUnit.SECONDS));
                } catch (InterruptedException ex) {
                    Thread.currentThread().interrupt();
                    throw new IllegalStateException(ex);
                }
            }
            published.set(snapshot);
        };
        coordinator.request("a_RULE", rebuild);
        FutureTask<Void> first = new FutureTask<>(tasks.remove(), null);
        Thread worker = new Thread(first);
        worker.start();
        try {
            assertTrue(reading.await(5, TimeUnit.SECONDS));
            database.set(2);
            coordinator.request("a_RULE", rebuild);
        } finally {
            release.countDown();
        }
        first.get(5, TimeUnit.SECONDS);
        assertEquals(1, published.get());
        assertEquals(1, tasks.size());
        tasks.remove().run();
        assertEquals(2, published.get());
        assertEquals(2, calls.get());
        assertTrue(tasks.isEmpty());
    }

    @Test
    void separatesKeysAndBoundsPendingWork() {
        AtomicInteger rebuilds = new AtomicInteger();
        assertTrue(coordinator.request("a_RULE", rebuilds::incrementAndGet));
        assertTrue(coordinator.request("b_RULE", rebuilds::incrementAndGet));
        assertFalse(coordinator.request("a_SELECTOR", rebuilds::incrementAndGet));
        assertTrue(coordinator.request("a_RULE", rebuilds::incrementAndGet));
        assertEquals(2, tasks.size());
        tasks.remove().run();
        assertTrue(coordinator.request("a_SELECTOR", rebuilds::incrementAndGet));
        tasks.remove().run();
        tasks.remove().run();
        assertEquals(3, rebuilds.get());
        assertTrue(tasks.isEmpty());
    }

    @Test
    void failureRetriesWithoutAnotherEventAndYieldsToOtherKeys() {
        AtomicInteger calls = new AtomicInteger();
        AtomicInteger other = new AtomicInteger();
        coordinator.request("a_RULE", () -> {
            if (calls.incrementAndGet() == 1) {
                throw new IllegalStateException("temporary database failure");
            }
        });
        coordinator.request("b_RULE", other::incrementAndGet);
        tasks.remove().run();
        assertEquals(2, tasks.size());
        verify(executor).schedule(any(Runnable.class), eq(1000L), eq(TimeUnit.MILLISECONDS));
        tasks.remove().run();
        assertEquals(1, other.get());
        tasks.remove().run();
        assertEquals(2, calls.get());
        assertTrue(tasks.isEmpty());
    }

    @Test
    void rejectedSubmissionDoesNotLeavePendingMarker() {
        when(executor.schedule(any(Runnable.class), eq(10L), eq(TimeUnit.MILLISECONDS)))
                .thenThrow(new RejectedExecutionException());
        assertFalse(coordinator.request("a_RULE", () -> { }));
        assertFalse(coordinator.request("a_RULE", () -> { }));
        verify(executor, times(2)).schedule(any(Runnable.class), eq(10L), eq(TimeUnit.MILLISECONDS));
    }

    @Test
    void closeCancelsPendingRebuilds() {
        AtomicInteger rebuilds = new AtomicInteger();
        coordinator.request("a_RULE", rebuilds::incrementAndGet);
        coordinator.close();
        tasks.remove().run();
        assertEquals(0, rebuilds.get());
        verify(executor).shutdownNow();
        assertThrows(IllegalStateException.class, () -> coordinator.request("a_RULE", rebuilds::incrementAndGet));
    }
}
