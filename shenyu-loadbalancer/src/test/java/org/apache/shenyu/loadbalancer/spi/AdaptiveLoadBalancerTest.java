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

package org.apache.shenyu.loadbalancer.spi;

import org.apache.shenyu.loadbalancer.entity.LoadBalanceData;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.loadbalancer.entity.UpstreamIdentity;
import org.apache.shenyu.loadbalancer.factory.LoadBalancerFactory;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Deterministic tests for the adaptive scoring selector: cold start compares with the
 * p2c load formula, warm candidates compare with the smoothed per-attempt latency, a
 * fast failing node is penalized through the synthetic failure floor, a 4xx is
 * neutral, and idle nodes keep the p2c forced exploration. No test sleeps.
 */
class AdaptiveLoadBalancerTest {

    private AdaptiveLoadBalancer balancer;

    @BeforeEach
    void setUp() {
        balancer = new AdaptiveLoadBalancer();
    }

    @Test
    void coldStartComparesWithTheP2cLoadFormula() {
        Upstream fast = upstream("fast:8080", 50);
        fast.setLag(0L);
        fast.getInflight().set(100L);
        Upstream slow = upstream("slow:8080", 50);
        slow.setLag(3_999_600_000L);
        slow.getInflight().set(1L);

        assertAlwaysPicked(fast, List.of(fast, slow), 10);
    }

    @Test
    void warmNodesPreferTheLowerSmoothedLatency() {
        Upstream slow = upstream("slow:8080", 50);
        Upstream quick = upstream("quick:8080", 50);
        feed(balancer, slow, AdaptiveOutcome.SUCCESS, 100L, 20);
        feed(balancer, quick, AdaptiveOutcome.SUCCESS, 1L, 20);

        assertTrue(balancer.stats().bothAdaptive(slow.identity(), quick.identity()));
        assertAlwaysPicked(quick, List.of(slow, quick), 10);
    }

    @Test
    void inFlightWorkTipsTheBalanceBetweenEqualLatencies() {
        Upstream busy = upstream("busy:8080", 50);
        Upstream idle = upstream("idle:8080", 50);
        feed(balancer, busy, AdaptiveOutcome.SUCCESS, 50L, 20);
        feed(balancer, idle, AdaptiveOutcome.SUCCESS, 50L, 20);

        List<Upstream> candidates = List.of(busy, idle);
        for (int i = 0; i < 10; i++) {
            // the selection reserves an attempt, so the asymmetry is re-applied per round
            busy.getInflight().set(100L);
            idle.getInflight().set(1L);
            long now = System.currentTimeMillis();
            busy.setLastPicked(now);
            idle.setLastPicked(now);
            Upstream picked = balancer.select(candidates, new LoadBalanceData());
            assertEquals(idle, picked, "round " + i + " must prefer the less loaded candidate");
        }
    }

    @Test
    void fastServerErrorsArePenalizedByTheFailureFloor() {
        Upstream fastFailing = upstream("failing:8080", 50);
        Upstream steady = upstream("steady:8080", 50);
        feed(balancer, fastFailing, AdaptiveOutcome.SERVER_ERROR, 1L, 20);
        feed(balancer, steady, AdaptiveOutcome.SUCCESS, 50L, 20);

        AdaptiveUpstreamStats.Stats failing = balancer.stats().statsOf(fastFailing.identity());
        assertEquals(AdaptiveUpstreamStats.FAILURE_PENALTY_MILLIS, failing.ewmaLatencyMillis(),
                "a millisecond 5xx must be smoothed in with the failure floor");

        assertAlwaysPicked(steady, List.of(fastFailing, steady), 10);
    }

    @Test
    void clientErrorObservationsAreNeutral() {
        Upstream node = upstream("node:8080", 50);
        feed(balancer, node, AdaptiveOutcome.SUCCESS, 100L, 20);
        AdaptiveUpstreamStats.Stats before = balancer.stats().statsOf(node.identity());

        feed(balancer, node, AdaptiveOutcome.CLIENT_ERROR, 1L, 5);

        AdaptiveUpstreamStats.Stats after = balancer.stats().statsOf(node.identity());
        assertEquals(before.ewmaLatencyMillis(), after.ewmaLatencyMillis(), "a 4xx must not move the latency");
        assertEquals(before.samples(), after.samples(), "a 4xx must not count as a latency sample");
        assertTrue(after.lastFeedbackMillis() >= before.lastFeedbackMillis());
    }

    @Test
    void clientErrorDoesNotReshapeTheNextLatencyUpdate() {
        Upstream node = upstream("node:8080", 50);
        AdaptiveUpstreamStats normal = new AdaptiveUpstreamStats();
        AdaptiveUpstreamStats with4xx = new AdaptiveUpstreamStats();
        normal.record(node.identity(), 1000L, AdaptiveOutcome.SERVER_ERROR, 0L);
        with4xx.record(node.identity(), 1000L, AdaptiveOutcome.SERVER_ERROR, 0L);
        with4xx.record(node.identity(), 1L, AdaptiveOutcome.CLIENT_ERROR, 999L);
        normal.record(node.identity(), 50L, AdaptiveOutcome.SUCCESS, 1000L);
        with4xx.record(node.identity(), 50L, AdaptiveOutcome.SUCCESS, 1000L);

        assertEquals(normal.statsOf(node.identity()).ewmaLatencyMillis(),
                with4xx.statsOf(node.identity()).ewmaLatencyMillis(),
                "an unrelated 4xx must not change the decay of the next latency-bearing sample");
        assertEquals(normal.statsOf(node.identity()).samples(), with4xx.statsOf(node.identity()).samples());
    }

    @Test
    void manyClientErrorsLeaveSubsequentUpdatesEquivalent() {
        Upstream node = upstream("node:8080", 50);
        AdaptiveUpstreamStats normal = new AdaptiveUpstreamStats();
        AdaptiveUpstreamStats with4xx = new AdaptiveUpstreamStats();
        for (int time = 0; time <= 5_000; time += 100) {
            normal.record(node.identity(), 50L, AdaptiveOutcome.SUCCESS, time);
            with4xx.record(node.identity(), 50L, AdaptiveOutcome.SUCCESS, time);
            for (int i = 1; i <= 5; i++) {
                with4xx.record(node.identity(), 1L, AdaptiveOutcome.CLIENT_ERROR, time + i);
            }
        }

        assertEquals(normal.statsOf(node.identity()).ewmaLatencyMillis(),
                with4xx.statsOf(node.identity()).ewmaLatencyMillis(),
                "any number of interleaved 4xx observations must leave the latency path equivalent");
        assertEquals(normal.statsOf(node.identity()).samples(), with4xx.statsOf(node.identity()).samples());
    }

    @Test
    void clientErrorAloneNeverWarmsAColdTarget() {
        Upstream node = upstream("cold:8080", 50);
        Upstream warm = upstream("warm:8080", 50);
        feed(balancer, warm, AdaptiveOutcome.SUCCESS, 10L, 20);
        for (int i = 0; i < 20; i++) {
            balancer.onFeedback(node, 1L, AdaptiveOutcome.CLIENT_ERROR);
        }

        AdaptiveUpstreamStats.Stats stats = balancer.stats().statsOf(node.identity());
        assertEquals(0L, stats.samples(), "a 4xx must never count as a latency sample");
        assertTrue(balancer.stats().score(node.identity(), 1L, 50).isEmpty(),
                "a target seen only through 4xx must stay cold");
        assertFalse(balancer.stats().bothAdaptive(node.identity(), warm.identity()));
    }

    @Test
    void identityFirstSeenThroughClientErrorStartsFromTheFirstRealSample() {
        AdaptiveUpstreamStats stats = new AdaptiveUpstreamStats();
        UpstreamIdentity identity = upstream("node:8080", 50).identity();
        stats.record(identity, 1L, AdaptiveOutcome.CLIENT_ERROR, 0L);
        stats.record(identity, 50L, AdaptiveOutcome.SUCCESS, 1000L);

        AdaptiveUpstreamStats.Stats record = stats.statsOf(identity);
        assertEquals(1L, record.samples());
        assertEquals(50L, record.ewmaLatencyMillis(),
                "the first real sample must be taken as-is, not decayed from the 4xx timestamp");
    }

    @Test
    void clientErrorActivityStillCountsForRetention() {
        AdaptiveUpstreamStats stats = new AdaptiveUpstreamStats();
        UpstreamIdentity retained = upstream("retained:8080", 50).identity();
        stats.record(retained, 10L, AdaptiveOutcome.SUCCESS, 0L);
        stats.record(retained, 1L, AdaptiveOutcome.CLIENT_ERROR, AdaptiveUpstreamStats.IDLE_MILLIS);
        for (int i = 0; i < AdaptiveUpstreamStats.PRUNE_THRESHOLD + 10; i++) {
            stats.record(upstream("filler-" + i + ":8080", 50).identity(), 10L, AdaptiveOutcome.SUCCESS, 0L);
        }

        stats.pruneIfNeeded(AdaptiveUpstreamStats.IDLE_MILLIS + 1L);
        assertNotNull(stats.statsOf(retained),
                "a recent 4xx keeps the identity alive for retention even without a new latency sample");
    }

    @Test
    void concurrentFeedbackKeepsSampleAccountingConsistent() throws Exception {
        AdaptiveUpstreamStats stats = new AdaptiveUpstreamStats();
        UpstreamIdentity identity = upstream("node:8080", 50).identity();
        int workers = 4;
        int perWorker = 250;
        ExecutorService pool = Executors.newFixedThreadPool(workers);
        try {
            for (int w = 0; w < workers; w++) {
                final int worker = w;
                pool.submit(() -> {
                    for (int i = 0; i < perWorker; i++) {
                        long now = (long) worker * perWorker + i;
                        stats.record(identity, 50L, AdaptiveOutcome.SUCCESS, now);
                        stats.record(identity, 1L, AdaptiveOutcome.CLIENT_ERROR, now);
                    }
                });
            }
            pool.shutdown();
            assertTrue(pool.awaitTermination(30, TimeUnit.SECONDS));
        } finally {
            pool.shutdownNow();
        }

        AdaptiveUpstreamStats.Stats record = stats.statsOf(identity);
        assertEquals((long) workers * perWorker, record.samples(),
                "only latency-bearing observations may count, regardless of interleaving");
        assertTrue(Double.isFinite(record.ewmaLatency()) && record.ewmaLatency() >= 0.0);
    }

    @Test
    void transportFailureEntersWithTheFailureFloorWithoutARealMeasurement() {
        Upstream node = upstream("node:8080", 50);

        balancer.onFeedback(node, 5L, AdaptiveOutcome.TRANSPORT_ERROR);

        AdaptiveUpstreamStats.Stats stats = balancer.stats().statsOf(node.identity());
        assertEquals(AdaptiveUpstreamStats.FAILURE_PENALTY_MILLIS, stats.ewmaLatencyMillis());
        assertEquals(1L, stats.samples());
    }

    @Test
    void cleanSamplesPullTheLatencyBackBelowTheFailureFloor() {
        AdaptiveUpstreamStats stats = new AdaptiveUpstreamStats();
        UpstreamIdentity identity = upstream("node:8080", 50).identity();
        long now = 0L;
        for (int i = 0; i < 20; i++) {
            stats.record(identity, 1L, AdaptiveOutcome.SERVER_ERROR, now);
            now += 1_000L;
        }
        long penalized = stats.statsOf(identity).ewmaLatencyMillis();
        assertTrue(penalized >= AdaptiveUpstreamStats.FAILURE_PENALTY_MILLIS);

        for (int i = 0; i < 6; i++) {
            stats.record(identity, 50L, AdaptiveOutcome.SUCCESS, now);
            now += 1_000L;
        }
        long recovered = stats.statsOf(identity).ewmaLatencyMillis();
        assertTrue(recovered < penalized, "successful samples must pull the smoothing down");
        assertTrue(recovered < AdaptiveUpstreamStats.FAILURE_PENALTY_MILLIS,
                "with a 1s gap the p=0.19 weighting recovers within a few samples");
    }

    @Test
    void sustainedSlowerSamplesConvergeDespiteShortFeedbackIntervals() {
        AdaptiveUpstreamStats stats = new AdaptiveUpstreamStats();
        UpstreamIdentity identity = upstream("latency:8080", 50).identity();
        stats.record(identity, 1L, AdaptiveOutcome.SUCCESS, 0L);
        for (int time = 1; time <= 10_000; time++) {
            stats.record(identity, 100L, AdaptiveOutcome.SUCCESS, time);
        }

        long smoothed = stats.statsOf(identity).ewmaLatencyMillis();
        assertTrue(smoothed >= 99L && smoothed <= 100L,
                "10,000 sustained 100 ms samples spaced 1 ms apart must converge toward 100 ms, got " + smoothed);
    }

    @Test
    void sameTimestampObservationsStillMoveTheEstimate() {
        AdaptiveUpstreamStats stats = new AdaptiveUpstreamStats();
        UpstreamIdentity identity = upstream("node:8080", 50).identity();
        stats.record(identity, 1L, AdaptiveOutcome.SUCCESS, 0L);
        for (int i = 0; i < 2_000; i++) {
            stats.record(identity, 100L, AdaptiveOutcome.SUCCESS, 0L);
        }

        assertTrue(stats.statsOf(identity).ewmaLatencyMillis() > 1L,
                "same-timestamp completions must blend with the minimum decay interval, not be discarded");
    }

    @Test
    void decreasingLatencyPullsTheEstimateDownAtShortIntervals() {
        AdaptiveUpstreamStats stats = new AdaptiveUpstreamStats();
        UpstreamIdentity identity = upstream("node:8080", 50).identity();
        stats.record(identity, 100L, AdaptiveOutcome.SUCCESS, 0L);
        for (int time = 1; time <= 10_000; time++) {
            stats.record(identity, 1L, AdaptiveOutcome.SUCCESS, time);
        }

        assertTrue(stats.statsOf(identity).ewmaLatencyMillis() <= 2L,
                "sustained faster samples must pull the smoothing down even at 1 ms feedback intervals");
    }

    @Test
    void failurePenaltySurvivesShortIntervalSmoothing() {
        AdaptiveUpstreamStats stats = new AdaptiveUpstreamStats();
        UpstreamIdentity identity = upstream("node:8080", 50).identity();
        stats.record(identity, 1L, AdaptiveOutcome.SERVER_ERROR, 0L);
        for (int time = 1; time <= 20; time++) {
            stats.record(identity, 1L, AdaptiveOutcome.SERVER_ERROR, time);
        }

        assertEquals(AdaptiveUpstreamStats.FAILURE_PENALTY_MILLIS, stats.statsOf(identity).ewmaLatencyMillis(),
                "the synthetic failure floor must hold while every sample is a fast failure");
    }

    @Test
    void idleNodeKeepsTheForcedExploration() {
        Upstream preferred = upstream("preferred:8080", 50);
        Upstream starved = upstream("starved:8080", 50);
        feed(balancer, preferred, AdaptiveOutcome.SUCCESS, 10L, 20);
        feed(balancer, starved, AdaptiveOutcome.SUCCESS, 500L, 20);
        starved.setLastPicked(System.currentTimeMillis() - 4_000L);

        List<Upstream> candidates = List.of(preferred, starved);
        for (int i = 0; i < 10; i++) {
            resetInflight(candidates);
            starved.setLastPicked(System.currentTimeMillis() - 4_000L);
            Upstream picked = balancer.select(candidates, new LoadBalanceData());
            assertEquals(starved, picked, "a node idle beyond the force gap must be probed once");
        }
    }

    @Test
    void singletonCandidateListPairsTheInflightReservation() {
        Upstream single = upstream("single:8080", 50);
        long before = single.getInflight().get();

        Upstream picked = balancer.select(List.of(single), new LoadBalanceData());

        assertEquals(single, picked);
        assertEquals(before + 1L, single.getInflight().get(), "the reservation must pair the completion decrement");
    }

    @Test
    void factoryResolvesTheAdaptiveSelector() {
        Upstream first = upstream("a:8080", 50);
        Upstream second = upstream("b:8080", 50);
        List<Upstream> candidates = List.of(first, second);

        Upstream picked = LoadBalancerFactory.selector(candidates, "adaptive", new LoadBalanceData());

        assertNotNull(picked);
        assertTrue(picked == first || picked == second);
    }

    @Test
    void staticFeedbackFeedsTheRegisteredSelector() {
        Upstream node = upstream("node:8080", 50);
        List<Upstream> candidates = List.of(node, upstream("other:8080", 50));

        AdaptiveLoadBalancer.feedback(node, 20L, AdaptiveOutcome.SUCCESS);
        Upstream picked = LoadBalancerFactory.selector(candidates, "adaptive", new LoadBalanceData());

        assertNotNull(picked);
    }

    @Test
    void pruneDropsIdleIdentitiesOnceTheMapOutgrowsItsThreshold() {
        AdaptiveUpstreamStats stats = new AdaptiveUpstreamStats();
        for (int i = 0; i < AdaptiveUpstreamStats.PRUNE_THRESHOLD + 10; i++) {
            Upstream node = upstream("node-" + i + ":8080", 50);
            stats.record(node.identity(), 10L, AdaptiveOutcome.SUCCESS, 0L);
        }
        assertEquals(AdaptiveUpstreamStats.PRUNE_THRESHOLD + 10, stats.size());

        assertEquals(0, stats.pruneIfNeeded(AdaptiveUpstreamStats.IDLE_MILLIS / 2),
                "below the idle bound nothing may be pruned");
        assertEquals(AdaptiveUpstreamStats.PRUNE_THRESHOLD + 10, stats.size());

        // maintenance is budgeted: each eligible run inspects at most the scan budget,
        // so draining the expired set takes several windows instead of one full sweep
        long now = AdaptiveUpstreamStats.IDLE_MILLIS + 1L;
        int pruned = 0;
        while (stats.size() > 0) {
            pruned += stats.pruneIfNeeded(now);
            assertTrue(stats.lastScanCount() <= AdaptiveUpstreamStats.MAINTENANCE_SCAN_BUDGET,
                    "one maintenance run must never inspect more than the scan budget");
            now += AdaptiveUpstreamStats.MAINTENANCE_INTERVAL_MILLIS;
        }
        assertEquals(AdaptiveUpstreamStats.PRUNE_THRESHOLD + 10, pruned);
        assertEquals(0, stats.size());
    }

    @Test
    void singletonTrafficStillDrivesCleanup() {
        for (int i = 0; i < 1000; i++) {
            Upstream old = upstream("old-" + i + ":8080", 50);
            balancer.stats().record(old.identity(), 1L, AdaptiveOutcome.SUCCESS, 0L);
        }
        Upstream live = upstream("live:8080", 50);
        final long before = live.getInflight().get();

        // the singleton fast path bypasses doSelect, so the selection itself must run
        // maintenance; the epoch-zero feedback is already expired against the real clock
        Upstream picked = balancer.select(List.of(live), new LoadBalanceData());
        assertEquals(live, picked);
        assertTrue(balancer.stats().lastScanCount() > 0,
                "a singleton selection must drive maintenance, not skip it");
        assertTrue(balancer.stats().size() < 1000, "the singleton selection must prune expired identities");
        assertEquals(before + 1L, live.getInflight().get(),
                "cleanup must not disturb the singleton inflight reservation pairing");

        // deterministic drain across maintenance windows, as repeated discovery
        // replacement would see under sustained singleton traffic
        long now = AdaptiveUpstreamStats.IDLE_MILLIS + 1L;
        while (balancer.stats().size() > 0) {
            balancer.stats().pruneIfNeeded(now);
            now += AdaptiveUpstreamStats.MAINTENANCE_INTERVAL_MILLIS;
        }
        assertEquals(0, balancer.stats().size(), "removed identities must eventually be reclaimed");
    }

    @Test
    void hotSetAboveThresholdDoesNotSweepPerSelection() {
        AdaptiveUpstreamStats stats = new AdaptiveUpstreamStats();
        long now = 1_000_000L;
        for (int i = 0; i < AdaptiveUpstreamStats.PRUNE_THRESHOLD + 200; i++) {
            stats.record(upstream("hot-" + i + ":8080", 50).identity(), 10L, AdaptiveOutcome.SUCCESS, now);
        }

        assertEquals(0, stats.pruneIfNeeded(now), "the first run finds nothing expired");
        for (int i = 1; i < 100; i++) {
            assertEquals(0, stats.pruneIfNeeded(now + i),
                    "selections inside the maintenance interval must pay no scan at all");
            assertEquals(0, stats.lastScanCount());
        }
        assertEquals(AdaptiveUpstreamStats.PRUNE_THRESHOLD + 200, stats.size(),
                "active history must be preserved while nothing is expired");

        assertEquals(0, stats.pruneIfNeeded(now + AdaptiveUpstreamStats.MAINTENANCE_INTERVAL_MILLIS));
        assertTrue(stats.lastScanCount() <= AdaptiveUpstreamStats.MAINTENANCE_SCAN_BUDGET,
                "even an eligible run stays within the per-call work budget");
    }

    @Test
    void hardCapacityEvictsOldestActivityWhenNothingIsIdle() {
        AdaptiveUpstreamStats stats = new AdaptiveUpstreamStats();
        int total = AdaptiveUpstreamStats.MAX_TRACKED_IDENTITIES + 100;
        for (int i = 0; i < total; i++) {
            stats.record(upstream("churn-" + i + ":8080", 50).identity(), 10L, AdaptiveOutcome.SUCCESS, i);
        }

        // nothing is idle at this timestamp, so only the capacity eviction may act
        stats.pruneIfNeeded(total + 1000L);

        assertEquals(AdaptiveUpstreamStats.MAX_TRACKED_IDENTITIES, stats.size(),
                "retained state must stay bounded even when every identity is active");
        assertNotNull(stats.statsOf(upstream("churn-" + (total - 1) + ":8080", 50).identity()),
                "the most recently active identity must survive the capacity eviction");
        assertNull(stats.statsOf(upstream("churn-0:8080", 50).identity()),
                "the oldest activity must be evicted first");
    }

    @Test
    void concurrentFeedbackSurvivesMaintenance() throws Exception {
        AdaptiveUpstreamStats stats = new AdaptiveUpstreamStats();
        UpstreamIdentity target = upstream("target:8080", 50).identity();
        stats.record(target, 10L, AdaptiveOutcome.SUCCESS, 0L);
        for (int i = 0; i < AdaptiveUpstreamStats.PRUNE_THRESHOLD + 10; i++) {
            stats.record(upstream("filler-" + i + ":8080", 50).identity(), 10L, AdaptiveOutcome.SUCCESS, 0L);
        }

        ExecutorService pool = Executors.newFixedThreadPool(2);
        try {
            pool.submit(() -> {
                for (int i = 0; i < 2_000; i++) {
                    stats.record(target, 10L, AdaptiveOutcome.SUCCESS, AdaptiveUpstreamStats.IDLE_MILLIS + 1L + i);
                }
            });
            pool.submit(() -> {
                long now = AdaptiveUpstreamStats.IDLE_MILLIS + 1L;
                for (int i = 0; i < 50; i++) {
                    stats.pruneIfNeeded(now);
                    now += AdaptiveUpstreamStats.MAINTENANCE_INTERVAL_MILLIS;
                }
            });
            pool.shutdown();
            assertTrue(pool.awaitTermination(30, TimeUnit.SECONDS));
        } finally {
            pool.shutdownNow();
        }

        assertNotNull(stats.statsOf(target),
                "a concurrently refreshed record must not be dropped by maintenance");
        assertTrue(stats.statsOf(target).lastFeedbackMillis() >= AdaptiveUpstreamStats.IDLE_MILLIS + 1L);
    }

    @Test
    void rejectedCandidatesWithoutIdentityFailSoftly() {
        balancer.onFeedback(null, 10L, AdaptiveOutcome.SUCCESS);
        assertNull(balancer.select(List.of(), new LoadBalanceData()));
    }

    private void assertAlwaysPicked(final Upstream expected, final List<Upstream> candidates, final int rounds) {
        for (int i = 0; i < rounds; i++) {
            resetInflight(candidates);
            Upstream picked = balancer.select(candidates, new LoadBalanceData());
            assertEquals(expected, picked, "round " + i + " must pick the lower scored candidate");
        }
    }

    private void resetInflight(final List<Upstream> candidates) {
        long now = System.currentTimeMillis();
        for (Upstream upstream : candidates) {
            upstream.getInflight().set(1L);
            upstream.setLastPicked(now);
        }
    }

    private void feed(final AdaptiveLoadBalancer target, final Upstream upstream, final AdaptiveOutcome outcome,
                      final long elapsedMillis, final int times) {
        for (int i = 0; i < times; i++) {
            target.onFeedback(upstream, elapsedMillis, outcome);
        }
    }

    private Upstream upstream(final String url, final int weight) {
        // Disable warm-up so a millisecond boundary cannot change effective weights during a comparison.
        Upstream upstream = Upstream.builder()
                .protocol("http://")
                .url(url)
                .weight(weight)
                .status(true)
                .timestamp(System.currentTimeMillis())
                .warmup(0)
                .build();
        upstream.setLastPicked(System.currentTimeMillis());
        return upstream;
    }
}
