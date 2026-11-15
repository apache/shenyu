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
import org.junit.jupiter.api.Test;

import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * The type random balance test.
 */
public class RandomLoadBalancerTest {

    @Test
    public void randomLoadBalancesWeightEqualTest() {
        final Upstream upstreamOrdered = new RandomLoadBalancer().select(Stream.of(10, 10, 10)
                .map(weight -> Upstream.builder()
                        .url("upstream-" + weight)
                        .weight(weight)
                        .build())
                .collect(Collectors.toList()), new LoadBalanceData());
        assertNotNull(upstreamOrdered);
    }

    @Test
    public void randomLoadBalancesWeightZeroTest() {
        final Upstream upstreamOrdered = new RandomLoadBalancer().select(Stream.of(0, 0, 0)
                .map(weight -> Upstream.builder()
                        .url("upstream-" + weight)
                        .weight(weight)
                        .build())
                .collect(Collectors.toList()), new LoadBalanceData());
        assertNotNull(upstreamOrdered);
    }

    /**
     * random load balance test.
     */
    @Test
    public void randomLoadBalanceOrderedWeightTest() {
        final Upstream upstreamOrdered = new RandomLoadBalancer().select(Stream.of(10, 40, 50)
                .map(weight -> Upstream.builder()
                        .url("upstream-" + weight)
                        .weight(weight)
                        .build())
                .collect(Collectors.toList()), new LoadBalanceData());
        assertNotNull(upstreamOrdered);
    }

    @Test
    public void randomLoadBalanceDisOrderedWeightTest() {
        final Upstream upstreamDisordered = new RandomLoadBalancer().select(Stream.of(10, 50, 40)
                .map(weight -> Upstream.builder()
                        .url("upstream-" + weight)
                        .weight(weight)
                        .build())
                .collect(Collectors.toList()), new LoadBalanceData());
        assertNotNull(upstreamDisordered);
    }

    @Test
    public void randomLoadBalanceReversedWeightTest() {
        final Upstream upstreamReversed = new RandomLoadBalancer().select(Stream.of(50, 40, 10)
                .map(weight -> Upstream.builder()
                        .url("upstream-" + weight)
                        .weight(weight)
                        .build())
                .collect(Collectors.toList()), new LoadBalanceData());
        assertNotNull(upstreamReversed);
    }

    /**
     * Behavior pin for the deferred weight-array allocation (#8424): the uniform
     * path and the weighted path both select from the candidate list, and the
     * weighted path — whose array is only built when a differing weight appears —
     * follows the configured weights.
     */
    @Test
    public void uniformAndWeightedPathsSelectFromCandidates() {
        RandomLoadBalancer loadBalancer = new RandomLoadBalancer();
        java.util.List<Upstream> equalWeights = java.util.stream.IntStream.range(0, 3)
                .mapToObj(index -> Upstream.builder().url("equal-" + index).weight(10).status(true).build())
                .collect(Collectors.toList());
        java.util.Map<String, Integer> equalCounts = new java.util.HashMap<>();
        for (int i = 0; i < 1000; i++) {
            Upstream picked = loadBalancer.select(equalWeights, new LoadBalanceData());
            assertNotNull(picked);
            equalCounts.merge(picked.getUrl(), 1, Integer::sum);
        }
        assertEquals(3, equalCounts.size());
        equalCounts.values().forEach(count -> org.junit.jupiter.api.Assertions.assertTrue(count > 200));

        java.util.List<Upstream> mixedWeights = java.util.Arrays.asList(
                Upstream.builder().url("mixed-a").weight(90).status(true).build(),
                Upstream.builder().url("mixed-b").weight(5).status(true).build(),
                Upstream.builder().url("mixed-c").weight(5).status(true).build());
        java.util.Map<String, Integer> mixedCounts = new java.util.HashMap<>();
        for (int i = 0; i < 1000; i++) {
            Upstream picked = loadBalancer.select(mixedWeights, new LoadBalanceData());
            assertNotNull(picked);
            mixedCounts.merge(picked.getUrl(), 1, Integer::sum);
        }
        // first upstream weight 90 of total 100: ~900 picks, backfilled array keeps it
        org.junit.jupiter.api.Assertions.assertTrue(mixedCounts.get("mixed-a") > 800);
        org.junit.jupiter.api.Assertions.assertTrue(mixedCounts.get("mixed-b") + mixedCounts.get("mixed-c") > 20);
    }
}
