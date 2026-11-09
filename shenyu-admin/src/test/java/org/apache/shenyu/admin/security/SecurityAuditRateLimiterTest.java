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

package org.apache.shenyu.admin.security;

import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.BrokenBarrierException;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.CyclicBarrier;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicLong;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

/**
 * Test case for {@link SecurityAuditRateLimiter}.
 */
public class SecurityAuditRateLimiterTest {

    @Test
    public void testAllowsUpToQuotaWithinWindow() {
        SecurityAuditRateLimiter limiter = new SecurityAuditRateLimiter(60_000L, 3);
        long now = System.currentTimeMillis();
        assertThat(limiter.tryAcquire(SecurityAuditEventType.LOGIN, now).isAllowed(), is(true));
        assertThat(limiter.tryAcquire(SecurityAuditEventType.LOGIN, now + 1).isAllowed(), is(true));
        assertThat(limiter.tryAcquire(SecurityAuditEventType.LOGIN, now + 2).isAllowed(), is(true));
        assertThat(limiter.tryAcquire(SecurityAuditEventType.LOGIN, now + 3).isAllowed(), is(false));
    }

    @Test
    public void testSuppressedCountFlushesExactlyOnceOnWindowRollover() {
        SecurityAuditRateLimiter limiter = new SecurityAuditRateLimiter(1_000L, 1);
        long start = System.currentTimeMillis();
        assertThat(limiter.tryAcquire(SecurityAuditEventType.LOGIN, start).isAllowed(), is(true));
        assertThat(limiter.tryAcquire(SecurityAuditEventType.LOGIN, start + 1).isAllowed(), is(false));
        assertThat(limiter.tryAcquire(SecurityAuditEventType.LOGIN, start + 2).isAllowed(), is(false));
        SecurityAuditRateLimiter.Acquisition rollover = limiter.tryAcquire(SecurityAuditEventType.LOGIN, start + 1_500L);
        assertThat(rollover.isAllowed(), is(true));
        assertThat(rollover.getFlushedSuppressed(), is(2L));
        SecurityAuditRateLimiter.Acquisition after = limiter.tryAcquire(SecurityAuditEventType.LOGIN, start + 1_501L);
        assertThat(after.isAllowed(), is(false));
        assertThat(after.getFlushedSuppressed(), is(0L));
    }

    @Test
    public void testSlotsAreIndependentPerEventType() {
        SecurityAuditRateLimiter limiter = new SecurityAuditRateLimiter(60_000L, 1);
        long now = System.currentTimeMillis();
        assertThat(limiter.tryAcquire(SecurityAuditEventType.LOGIN, now).isAllowed(), is(true));
        assertThat(limiter.tryAcquire(SecurityAuditEventType.LOGIN, now).isAllowed(), is(false));
        assertThat(limiter.tryAcquire(SecurityAuditEventType.PERMISSION_CHECK, now).isAllowed(), is(true));
    }

    @Test
    public void testSuppressionSummarySlotHasItsOwnQuota() {
        SecurityAuditRateLimiter limiter = new SecurityAuditRateLimiter(60_000L, 2);
        long now = System.currentTimeMillis();
        assertThat(limiter.tryAcquire(SecurityAuditEventType.AUDIT_SUPPRESSED, now).isAllowed(), is(true));
        assertThat(limiter.tryAcquire(SecurityAuditEventType.AUDIT_SUPPRESSED, now).isAllowed(), is(true));
        assertThat(limiter.tryAcquire(SecurityAuditEventType.AUDIT_SUPPRESSED, now).isAllowed(), is(false));
    }

    @Test
    public void testConcurrentStressConservesTotals() throws Exception {
        final int maxPerWindow = 1_000;
        final int threads = 8;
        final int perThread = 5_000;
        SecurityAuditRateLimiter limiter = new SecurityAuditRateLimiter(60_000L, maxPerWindow);
        long now = System.currentTimeMillis();
        ExecutorService pool = Executors.newFixedThreadPool(threads);
        CountDownLatch ready = new CountDownLatch(threads);
        CountDownLatch go = new CountDownLatch(1);
        AtomicLong allowed = new AtomicLong();
        AtomicLong suppressed = new AtomicLong();
        List<Future<?>> futures = new ArrayList<>();
        for (int t = 0; t < threads; t++) {
            futures.add(pool.submit(() -> {
                ready.countDown();
                try {
                    go.await(5, TimeUnit.SECONDS);
                    for (int i = 0; i < perThread; i++) {
                        SecurityAuditRateLimiter.Acquisition acquisition = limiter.tryAcquire(SecurityAuditEventType.LOGIN, now);
                        if (acquisition.isAllowed()) {
                            allowed.incrementAndGet();
                        } else {
                            suppressed.incrementAndGet();
                        }
                    }
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            }));
        }
        ready.await(5, TimeUnit.SECONDS);
        go.countDown();
        for (Future<?> future : futures) {
            future.get(30, TimeUnit.SECONDS);
        }
        pool.shutdown();
        assertThat(allowed.get(), is((long) maxPerWindow));
        assertThat(suppressed.get(), is((long) threads * perThread - maxPerWindow));
    }

    @Test
    public void testConcurrentRolloverFlushesSuppressedExactlyOnce() throws Exception {
        final int threads = 32;
        SecurityAuditRateLimiter limiter = new SecurityAuditRateLimiter(1_000L, 1);
        long start = System.currentTimeMillis();
        // first window: 1 emitted, 2 suppressed
        limiter.tryAcquire(SecurityAuditEventType.LOGIN, start);
        limiter.tryAcquire(SecurityAuditEventType.LOGIN, start + 1);
        limiter.tryAcquire(SecurityAuditEventType.LOGIN, start + 2);
        long rolloverTime = start + 1_500L;
        ExecutorService pool = Executors.newFixedThreadPool(threads);
        CountDownLatch ready = new CountDownLatch(threads);
        CountDownLatch go = new CountDownLatch(1);
        AtomicLong allowedCount = new AtomicLong();
        AtomicLong flushedTotal = new AtomicLong();
        AtomicLong flushReports = new AtomicLong();
        List<Future<?>> futures = new ArrayList<>();
        for (int t = 0; t < threads; t++) {
            futures.add(pool.submit(() -> {
                ready.countDown();
                try {
                    go.await(5, TimeUnit.SECONDS);
                    SecurityAuditRateLimiter.Acquisition acquisition = limiter.tryAcquire(SecurityAuditEventType.LOGIN, rolloverTime);
                    if (acquisition.isAllowed()) {
                        allowedCount.incrementAndGet();
                    }
                    if (acquisition.getFlushedSuppressed() > 0L) {
                        flushReports.incrementAndGet();
                        flushedTotal.addAndGet(acquisition.getFlushedSuppressed());
                    }
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            }));
        }
        ready.await(5, TimeUnit.SECONDS);
        go.countDown();
        for (Future<?> future : futures) {
            future.get(30, TimeUnit.SECONDS);
        }
        pool.shutdown();
        assertThat(allowedCount.get(), is(1L));
        assertThat(flushReports.get(), is(1L));
        // exact per-window attribution, not just conservation: the flip must report exactly
        // the 2 suppressions of the closed window, and every racer denied by the fresh quota
        // of the new window must be accounted to the new window and flushed by a second
        // rollover, so the two windows never mix their counts.
        assertThat(flushedTotal.get(), is(2L));
        long remainder = limiter.tryAcquire(SecurityAuditEventType.LOGIN, rolloverTime + 1_500L).getFlushedSuppressed();
        assertThat(remainder, is(threads - 1L));
    }

    @Test
    public void testConcurrentRolloverAttributesSuppressionsToTheirOwnWindow() throws Exception {
        final int threads = 8;
        final long windowMillis = 10_000L;
        SecurityAuditRateLimiter limiter = new SecurityAuditRateLimiter(windowMillis, 1);
        long start = System.currentTimeMillis();
        ExecutorService pool = Executors.newFixedThreadPool(threads);
        try {
            // window 0: 1 admitted, threads - 1 suppressed; nothing flushed (first window)
            runConcurrentPhase(pool, threads, limiter, start, 1L, 0L);
            // window 1: the concurrent flip must flush exactly the threads - 1 suppressions
            // of window 0, none of the suppressions the racers are about to add to window 1
            runConcurrentPhase(pool, threads, limiter, start + windowMillis, 1L, threads - 1L);
            // window 2: same again, proving attribution holds across consecutive flips
            runConcurrentPhase(pool, threads, limiter, start + 2 * windowMillis, 1L, threads - 1L);
        } finally {
            pool.shutdown();
        }
    }

    /**
     * release {@code threads} callers on one barrier so they race through the same window, then
     * assert the exact per-window outcome: {@code expectedAllowed} callers admitted and a single
     * rollover summary reporting exactly {@code expectedFlushed} suppressions of the window just
     * closed. The assertions are deterministic because every caller uses the same timestamp and
     * the limiter updates its window state atomically, so the aggregate outcome does not depend
     * on which caller wins the race.
     *
     * @param pool executor to run the callers on
     * @param threads number of racing callers
     * @param limiter limiter under test
     * @param phaseTimeMillis timestamp every caller uses
     * @param expectedAllowed number of callers that must be admitted
     * @param expectedFlushed suppressed total the flip must report, 0 when no flip is expected
     * @throws Exception when a caller fails
     */
    private void runConcurrentPhase(final ExecutorService pool, final int threads, final SecurityAuditRateLimiter limiter,
                                    final long phaseTimeMillis, final long expectedAllowed, final long expectedFlushed) throws Exception {
        CyclicBarrier barrier = new CyclicBarrier(threads);
        AtomicLong allowedCount = new AtomicLong();
        AtomicLong flushReports = new AtomicLong();
        AtomicLong flushedTotal = new AtomicLong();
        List<Future<?>> futures = new ArrayList<>();
        for (int t = 0; t < threads; t++) {
            futures.add(pool.submit(() -> {
                try {
                    barrier.await(5, TimeUnit.SECONDS);
                    SecurityAuditRateLimiter.Acquisition acquisition = limiter.tryAcquire(SecurityAuditEventType.LOGIN, phaseTimeMillis);
                    if (acquisition.isAllowed()) {
                        allowedCount.incrementAndGet();
                    }
                    if (acquisition.getFlushedSuppressed() > 0L) {
                        flushReports.incrementAndGet();
                        flushedTotal.addAndGet(acquisition.getFlushedSuppressed());
                    }
                } catch (InterruptedException | BrokenBarrierException | TimeoutException e) {
                    throw new IllegalStateException(e);
                }
            }));
        }
        for (Future<?> future : futures) {
            future.get(30, TimeUnit.SECONDS);
        }
        assertThat(allowedCount.get(), is(expectedAllowed));
        assertThat(flushReports.get(), is(expectedFlushed > 0L ? 1L : 0L));
        assertThat(flushedTotal.get(), is(expectedFlushed));
    }
}
