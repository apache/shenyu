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
import java.util.ArrayList;
import java.util.List;
import java.util.Queue;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.CyclicBarrier;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.FutureTask;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
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

    private List<Long> delays;

    private HttpCacheRebuildCoordinator coordinator;

    @BeforeEach
    void setUp() {
        executor = mock(ScheduledExecutorService.class);
        tasks = new ArrayDeque<>();
        delays = new ArrayList<>();
        doAnswer(invocation -> {
            tasks.add(invocation.getArgument(0));
            delays.add(invocation.getArgument(1));
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
        // A commit racing shutdown is told to rebuild synchronously instead of failing its hook.
        assertFalse(coordinator.request("a_RULE", rebuilds::incrementAndGet));
        assertTrue(tasks.isEmpty());
    }

    @Test
    void consecutiveFailuresBackOffExponentiallyUpToTheCapAndResetAfterSuccess() {
        AtomicInteger calls = new AtomicInteger();
        AtomicInteger failuresLeft = new AtomicInteger(7);
        Runnable rebuild = () -> {
            calls.incrementAndGet();
            if (failuresLeft.getAndDecrement() > 0) {
                throw new IllegalStateException("database unavailable");
            }
        };
        coordinator.request("a_RULE", rebuild);
        while (!tasks.isEmpty()) {
            tasks.remove().run();
        }
        assertEquals(8, calls.get());
        assertEquals(List.of(10L, 1000L, 2000L, 4000L, 8000L, 16000L, 30000L, 30000L), delays);

        delays.clear();
        failuresLeft.set(1);
        coordinator.request("a_RULE", rebuild);
        while (!tasks.isEmpty()) {
            tasks.remove().run();
        }
        // The previous success reset the failure count, so backoff starts again at one second.
        assertEquals(List.of(10L, 1000L), delays);
    }

    @Test
    void retryDelayNeverOverflowsAndNeverUndercutsTheWindow() {
        assertEquals(HttpCacheRebuildCoordinator.FAILURE_RETRY_MILLIS, coordinator.retryDelayMillis(1));
        assertEquals(HttpCacheRebuildCoordinator.MAX_FAILURE_RETRY_MILLIS, coordinator.retryDelayMillis(Integer.MAX_VALUE));
        HttpCacheRebuildCoordinator slowWindow = new HttpCacheRebuildCoordinator(executor, 5000, 2);
        assertEquals(5000, slowWindow.retryDelayMillis(1));
        assertEquals(HttpCacheRebuildCoordinator.MAX_FAILURE_RETRY_MILLIS, slowWindow.retryDelayMillis(10));
    }

    @Test
    void multipleWorkersRebuildDistinctKeysInParallel() throws Exception {
        ScheduledThreadPoolExecutor workers = new ScheduledThreadPoolExecutor(2);
        HttpCacheRebuildCoordinator parallel = new HttpCacheRebuildCoordinator(workers, 1, 16);
        try {
            // Each rebuild waits for the other key: this only completes when both run at the same time.
            CyclicBarrier bothRunning = new CyclicBarrier(2);
            CountDownLatch done = new CountDownLatch(2);
            Runnable rebuild = () -> {
                try {
                    bothRunning.await(5, TimeUnit.SECONDS);
                    done.countDown();
                } catch (Exception ex) {
                    throw new IllegalStateException(ex);
                }
            };
            assertTrue(parallel.request("a_RULE", rebuild));
            assertTrue(parallel.request("b_RULE", rebuild));
            assertTrue(done.await(5, TimeUnit.SECONDS));
        } finally {
            parallel.close();
        }
    }

    @Test
    void multipleWorkersNeverRebuildTheSameKeyConcurrently() throws Exception {
        ScheduledThreadPoolExecutor workers = new ScheduledThreadPoolExecutor(4);
        HttpCacheRebuildCoordinator parallel = new HttpCacheRebuildCoordinator(workers, 1, 16);
        AtomicInteger running = new AtomicInteger();
        AtomicInteger maxRunning = new AtomicInteger();
        AtomicInteger database = new AtomicInteger();
        AtomicInteger published = new AtomicInteger(-1);
        Runnable rebuild = () -> {
            maxRunning.accumulateAndGet(running.incrementAndGet(), Math::max);
            try {
                int snapshot = database.get();
                Thread.sleep(1);
                published.set(snapshot);
            } catch (InterruptedException ex) {
                Thread.currentThread().interrupt();
            } finally {
                running.decrementAndGet();
            }
        };
        ExecutorService writers = Executors.newFixedThreadPool(4);
        try {
            List<Future<?>> commits = new ArrayList<>();
            for (int index = 1; index <= 200; index++) {
                int value = index;
                commits.add(writers.submit(() -> {
                    database.accumulateAndGet(value, Math::max);
                    if (!parallel.request("a_RULE", rebuild)) {
                        rebuild.run();
                    }
                }));
            }
            for (Future<?> commit : commits) {
                commit.get(5, TimeUnit.SECONDS);
            }
            long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5);
            while (published.get() != 200 && System.nanoTime() < deadline) {
                Thread.sleep(5);
            }
            assertEquals(200, published.get());
            assertEquals(1, maxRunning.get());
        } finally {
            writers.shutdownNow();
            parallel.close();
        }
    }
}
