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

import org.apache.shenyu.common.enums.LoadBalanceEnum;
import org.apache.shenyu.loadbalancer.entity.LoadBalanceData;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.loadbalancer.entity.UpstreamIdentity;
import org.apache.shenyu.spi.ExtensionLoader;
import org.apache.shenyu.spi.Join;

import java.util.List;
import java.util.Objects;
import java.util.OptionalLong;
import java.util.Random;

/**
 * Adaptive scoring load balance impl (#8300).
 *
 * <p>Selection keeps the p2c pairing (two random candidates compared in O(1), no
 * full-list sort) but compares per-upstream adaptive scores instead of the raw
 * {@code (sqrt(lag)+1) * inflight} product: a smoothed per-attempt latency (with a
 * synthetic floor for failed attempts, so fast failures stay unattractive), the
 * in-flight count, and the warm-up adjusted configured weight. While a candidate is
 * still cold (fewer than {@value AdaptiveUpstreamStats#MIN_SAMPLES} latency samples)
 * the pair is compared with the p2c load formula, so cold start, restarts and
 * discovery churn behave exactly like today. Idle nodes keep the p2c forced
 * exploration so a slow node is never starved permanently.</p>
 *
 * <p>The feedback is a single hook: the divide plugin measures each attempt with its
 * own start time and calls {@link #feedback(Upstream, long, AdaptiveOutcome)}, so
 * overlapping attempts never share a timestamp, and the classified outcome keeps
 * client-caused 4xx results neutral. Health, canary partitioning, breaker admission,
 * sticky affinity and draining/ready gating all happen before this selector sees the
 * candidate list, so the adaptive comparison only ranks already eligible targets.</p>
 */
@Join
public class AdaptiveLoadBalancer extends AbstractLoadBalancer {

    /**
     * maximum tolerance of idle time, mirroring the p2c exploration.
     */
    private static final int FORCE_GAP = 3 * 1000;

    /**
     * pick times for the healthy-candidate redraw.
     */
    private static final int PICK_TIMES = 3;

    private final Random random = new Random();

    /**
     * p2c load formula reused as the cold-start comparison, so the fallback scale is
     * the same for both candidates of a pair.
     */
    private final P2cLoadBalancer coldStartComparison = new P2cLoadBalancer();

    private final AdaptiveUpstreamStats stats = new AdaptiveUpstreamStats();

    /**
     * The feedback statistics of this selector; visible for tests.
     *
     * @return the stats registry
     */
    AdaptiveUpstreamStats stats() {
        return stats;
    }

    @Override
    public Upstream select(final List<Upstream> upstreamList, final LoadBalanceData data) {
        Upstream picked = super.select(upstreamList, data);
        // AbstractLoadBalancer short-circuits a singleton candidate list before doSelect
        // runs, so the in-flight reservation made here pairs the completion decrement in
        // the divide plugin (same accounting fix as the p2c selector, #7974).
        if (Objects.nonNull(picked) && upstreamList.size() == 1) {
            picked.setLastPicked(System.currentTimeMillis());
            picked.getInflight().incrementAndGet();
        }
        return picked;
    }

    @Override
    protected Upstream doSelect(final List<Upstream> upstreamList, final LoadBalanceData data) {
        long now = System.currentTimeMillis();
        stats.pruneIfNeeded(now);
        Upstream[] upstreams = pickTwoUpstreams(upstreamList);
        Upstream picked;
        Upstream unpicked;
        if (compare(upstreams[0], upstreams[1]) > 0) {
            picked = upstreams[1];
            unpicked = upstreams[0];
        } else {
            picked = upstreams[0];
            unpicked = upstreams[1];
        }
        // If the loser is not selected once in the forceGap period, it is forced to be
        // selected once, mirroring the p2c exploration.
        if ((now - unpicked.getLastPicked()) > FORCE_GAP) {
            unpicked.setLastPicked(now);
            picked = unpicked;
        }

        if (picked != unpicked) {
            picked.setLastPicked(now);
        }
        picked.getInflight().incrementAndGet();
        return picked;
    }

    /**
     * Compare two candidates: adaptive scores when both are warm, the p2c load formula
     * for a cold pair (the scales must never be mixed inside one comparison).
     *
     * @param first the first candidate
     * @param second the second candidate
     * @return negative/zero/positive following the first candidate's advantage
     */
    private long compare(final Upstream first, final Upstream second) {
        if (stats.bothAdaptive(first.identity(), second.identity())) {
            return Long.compare(score(first), score(second));
        }
        return Long.compare(coldStartComparison.load(first), coldStartComparison.load(second));
    }

    private long score(final Upstream upstream) {
        OptionalLong adaptive = stats.score(upstream.identity(), upstream.getInflight().get(), getWeight(upstream));
        // bothAdaptive already guaranteed warm statistics; the fallback keeps the
        // comparison total if a concurrent prune removed the entry between the checks
        return adaptive.orElseGet(() -> coldStartComparison.load(upstream));
    }

    /**
     * Feed back one classified attempt. Called by the divide plugin's completion
     * trigger with the attempt's own start time, so the measurement is per attempt.
     * Resolves the registered adaptive selector, so the feedback always lands on the
     * same instance that performs the selections.
     *
     * @param upstream the upstream that served the attempt
     * @param elapsedMillis the measured attempt latency
     * @param outcome the classified outcome
     */
    public static void feedback(final Upstream upstream, final long elapsedMillis, final AdaptiveOutcome outcome) {
        LoadBalancer join = ExtensionLoader.getExtensionLoader(LoadBalancer.class)
                .getJoin(LoadBalanceEnum.ADAPTIVE.getName());
        if (join instanceof AdaptiveLoadBalancer) {
            ((AdaptiveLoadBalancer) join).onFeedback(upstream, elapsedMillis, outcome);
        }
    }

    /**
     * Feed back one classified attempt into this instance's statistics.
     *
     * @param upstream the upstream that served the attempt
     * @param elapsedMillis the measured attempt latency
     * @param outcome the classified outcome
     */
    void onFeedback(final Upstream upstream, final long elapsedMillis, final AdaptiveOutcome outcome) {
        if (Objects.isNull(upstream) || Objects.isNull(outcome)) {
            return;
        }
        UpstreamIdentity identity = upstream.identity();
        stats.record(identity, elapsedMillis, outcome, System.currentTimeMillis());
    }

    private Upstream[] pickTwoUpstreams(final List<Upstream> upstreamList) {
        Upstream[] upstreams = new Upstream[2];
        for (int i = 0; i < PICK_TIMES; i++) {
            int a = random.nextInt(upstreamList.size());
            int b = random.nextInt(upstreamList.size() - 1);
            // prevent random nodes from being the same.
            if (b >= a) {
                b += 1;
            }
            upstreams[0] = upstreamList.get(a);
            upstreams[1] = upstreamList.get(b);
            if (upstreams[0].isHealthy() && upstreams[1].isHealthy()) {
                break;
            }
        }
        return upstreams;
    }
}
