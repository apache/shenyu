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

import static org.junit.jupiter.api.Assertions.assertEquals;
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

        int pruned = stats.pruneIfNeeded(AdaptiveUpstreamStats.IDLE_MILLIS + 1L);
        assertEquals(AdaptiveUpstreamStats.PRUNE_THRESHOLD + 10, pruned);
        assertEquals(0, stats.size());
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
