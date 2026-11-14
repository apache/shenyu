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
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * The type Load balance test.
 */
public final class RoundRobinLoadBalanceTest {

    private static final int SELECTION_ITERATIONS = 30;

    @ParameterizedTest
    @CsvSource({
        "1000000000, 1000000000, 15, 15",
        "1073741824, 1073741824, 15, 15",
        "1500000000, 1000000000, 18, 12",
        "1000000000, 2000000000, 10, 20",
        "2147483647, 2147483647, 15, 15"
    })
    public void testLargeWeightsPreserveDistribution(final int firstWeight, final int secondWeight,
                                                     final int expectedFirstCount, final int expectedSecondCount) {
        Upstream first = Upstream.builder().url("first:8080").weight(firstWeight).status(true).build();
        Upstream second = Upstream.builder().url("second:8080").weight(secondWeight).status(true).build();
        List<Upstream> upstreamList = List.of(first, second);
        RoundRobinLoadBalancer loadBalancer = new RoundRobinLoadBalancer();
        Map<String, Integer> counts = new HashMap<>();
        for (int i = 0; i < SELECTION_ITERATIONS; i++) {
            Upstream selected = loadBalancer.select(upstreamList, new LoadBalanceData());
            counts.merge(selected.getUrl(), 1, Integer::sum);
        }
        assertEquals(expectedFirstCount, counts.getOrDefault(first.getUrl(), 0));
        assertEquals(expectedSecondCount, counts.getOrDefault(second.getUrl(), 0));
    }

    /**
     * Round robin load balance test.
     */
    @Test
    public void roundRobinLoadBalanceDisorderedWeightTest() {
        List<Upstream> upstreamList =
                Stream.of(50, 20, 30)
                        .map(weight -> Upstream.builder()
                                .url("upstream-" + weight)
                                .weight(weight)
                                .build())
                        .collect(Collectors.toList());

        RoundRobinLoadBalancer roundRobinLoadBalancer = new RoundRobinLoadBalancer();
        Map<String, Integer> countMap = new HashMap<>();
        IntStream.range(0, 120).forEach(i -> {
            Upstream result = roundRobinLoadBalancer.select(upstreamList, new LoadBalanceData());
            int count = countMap.getOrDefault(result.getUrl(), 0);
            countMap.put(result.getUrl(), ++count);
        });
        assertEquals(60, countMap.get("upstream-50").intValue());
    }

    @Test
    public void roundRobinLoadBalanceOrderedWeightTest() {
        List<Upstream> upstreamList =
                Stream.of(20, 30, 50)
                        .map(weight -> Upstream.builder()
                                .url("upstream-" + weight)
                                .weight(weight)
                                .build())
                        .collect(Collectors.toList());

        RoundRobinLoadBalancer roundRobinLoadBalancer = new RoundRobinLoadBalancer();
        Map<String, Integer> countMap = new HashMap<>();
        IntStream.range(0, 120).forEach(i -> {
            Upstream result = roundRobinLoadBalancer.select(upstreamList, new LoadBalanceData());
            int count = countMap.getOrDefault(result.getUrl(), 0);
            countMap.put(result.getUrl(), ++count);
        });
        assertEquals(60, countMap.get("upstream-50").intValue());
    }

    @Test
    public void roundRobinLoadBalanceReversedWeightTest() {
        List<Upstream> upstreamList =
                Stream.of(50, 30, 20)
                        .map(weight -> Upstream.builder()
                                .url("upstream-" + weight)
                                .weight(weight)
                                .build())
                        .collect(Collectors.toList());

        RoundRobinLoadBalancer roundRobinLoadBalancer = new RoundRobinLoadBalancer();
        Map<String, Integer> countMap = new HashMap<>();
        IntStream.range(0, 120).forEach(i -> {
            Upstream result = roundRobinLoadBalancer.select(upstreamList, new LoadBalanceData());
            int count = countMap.getOrDefault(result.getUrl(), 0);
            countMap.put(result.getUrl(), ++count);
        });
        assertEquals(60, countMap.get("upstream-50").intValue());
    }

    @Test
    public void roundRobinLoadBalanceTest() {
        List<Upstream> upstreamList =
                Stream.of(50, 30, 20)
                        .map(weight -> Upstream.builder()
                                .url("upstream-" + weight)
                                .weight(weight)
                                .build())
                        .collect(Collectors.toList());
        List<Upstream> upstreamList2 =
                Stream.of(50, 40, 20)
                        .map(weight -> Upstream.builder()
                                .url("upstream-" + weight)
                                .weight(1)
                                .build())
                        .collect(Collectors.toList());

        RoundRobinLoadBalancer roundRobinLoadBalancer = new RoundRobinLoadBalancer();
        
        // Test with weighted upstream list
        Upstream result1 = roundRobinLoadBalancer.select(upstreamList, new LoadBalanceData());
        assertNotNull(result1, "Selected upstream should not be null");
        assertTrue(upstreamList.contains(result1), "Selected upstream should be from the provided list");
        
        // Test with equal weight upstream list
        Upstream result2 = roundRobinLoadBalancer.select(upstreamList2, new LoadBalanceData());
        assertNotNull(result2, "Selected upstream should not be null");
        assertTrue(upstreamList2.contains(result2), "Selected upstream should be from the provided list");
        
        // Test multiple selections to verify round-robin behavior
        Map<String, Integer> countMap = new HashMap<>();
        IntStream.range(0, SELECTION_ITERATIONS).forEach(i -> {
            Upstream result = roundRobinLoadBalancer.select(upstreamList2, new LoadBalanceData());
            int count = countMap.getOrDefault(result.getUrl(), 0);
            countMap.put(result.getUrl(), ++count);
        });
        
        // With equal weights, distribution should be roughly equal
        assertEquals(3, countMap.size(), "All three upstreams should be selected");
        countMap.values().forEach(count -> 
            assertTrue(count >= 8 && count <= 12, "Distribution should be roughly equal for equal weights"));
    }

    /**
     * Issue #8441: two selectors whose upstream lists share the first upstream's
     * URL must not share smooth weighted round-robin counters. With isolated
     * state each selector's sequence is deterministic, so the exact counts of a
     * smooth cycle hold; on the shared state of the old implementation the
     * shared upstream's weight flips 1↔9 between calls and the ratios deviate.
     */
    @Test
    public void sharedFirstUpstreamStaysIsolatedPerSelector() {
        Upstream sharedLight = Upstream.builder().url("shared-a:8080").weight(1).status(true).build();
        Upstream sharedHeavy = Upstream.builder().url("shared-a:8080").weight(9).status(true).build();
        Upstream b = Upstream.builder().url("b:8080").weight(9).status(true).build();
        Upstream c = Upstream.builder().url("c:8080").weight(1).status(true).build();
        List<Upstream> selectorOne = List.of(sharedLight, b);
        List<Upstream> selectorTwo = List.of(sharedHeavy, c);
        RoundRobinLoadBalancer loadBalancer = new RoundRobinLoadBalancer();
        Map<String, Integer> firstCounts = new HashMap<>();
        Map<String, Integer> secondCounts = new HashMap<>();
        IntStream.range(0, 10000).forEach(i -> {
            firstCounts.merge(loadBalancer.select(selectorOne, new LoadBalanceData()).getUrl(), 1, Integer::sum);
            secondCounts.merge(loadBalancer.select(selectorTwo, new LoadBalanceData()).getUrl(), 1, Integer::sum);
        });
        assertEquals(1000, firstCounts.get("shared-a:8080").intValue());
        assertEquals(9000, firstCounts.get("b:8080").intValue());
        assertEquals(9000, secondCounts.get("shared-a:8080").intValue());
        assertEquals(1000, secondCounts.get("c:8080").intValue());
    }

    /**
     * Issue #8441: parallel selectors sharing an upstream address keep their
     * deterministic ratios, because each thread drives only its own pool.
     */
    @Test
    public void sharedUpstreamSelectorsStayIsolatedUnderConcurrency() throws Exception {
        Upstream sharedLight = Upstream.builder().url("shared-a:8080").weight(1).status(true).build();
        Upstream sharedHeavy = Upstream.builder().url("shared-a:8080").weight(9).status(true).build();
        Upstream b = Upstream.builder().url("b:8080").weight(9).status(true).build();
        Upstream c = Upstream.builder().url("c:8080").weight(1).status(true).build();
        List<Upstream> selectorOne = List.of(sharedLight, b);
        List<Upstream> selectorTwo = List.of(sharedHeavy, c);
        RoundRobinLoadBalancer loadBalancer = new RoundRobinLoadBalancer();
        Map<String, Integer> firstCounts = new HashMap<>();
        Map<String, Integer> secondCounts = new HashMap<>();
        Thread one = new Thread(() -> IntStream.range(0, 10000).forEach(i ->
                firstCounts.merge(loadBalancer.select(selectorOne, new LoadBalanceData()).getUrl(), 1, Integer::sum)));
        Thread two = new Thread(() -> IntStream.range(0, 10000).forEach(i ->
                secondCounts.merge(loadBalancer.select(selectorTwo, new LoadBalanceData()).getUrl(), 1, Integer::sum)));
        one.start();
        two.start();
        one.join();
        two.join();
        assertEquals(1000, firstCounts.get("shared-a:8080").intValue());
        assertEquals(9000, firstCounts.get("b:8080").intValue());
        assertEquals(9000, secondCounts.get("shared-a:8080").intValue());
        assertEquals(1000, secondCounts.get("c:8080").intValue());
    }

    /**
     * Issue #8441: two selectors over the same upstream set with opposite
     * weights must each follow their own configuration, not fight over one
     * pool (the old keying shared the state because the first URL matched).
     */
    @Test
    public void sameUpstreamSetWithDifferentWeightsStaysIsolated() {
        Upstream lightA = Upstream.builder().url("set-a:8080").weight(1).status(true).build();
        Upstream heavyB = Upstream.builder().url("set-b:8080").weight(9).status(true).build();
        Upstream heavyA = Upstream.builder().url("set-a:8080").weight(9).status(true).build();
        Upstream lightB = Upstream.builder().url("set-b:8080").weight(1).status(true).build();
        List<Upstream> selectorOne = List.of(lightA, heavyB);
        List<Upstream> selectorTwo = List.of(heavyA, lightB);
        RoundRobinLoadBalancer loadBalancer = new RoundRobinLoadBalancer();
        Map<String, Integer> firstCounts = new HashMap<>();
        Map<String, Integer> secondCounts = new HashMap<>();
        IntStream.range(0, 10000).forEach(i -> {
            firstCounts.merge(loadBalancer.select(selectorOne, new LoadBalanceData()).getUrl(), 1, Integer::sum);
            secondCounts.merge(loadBalancer.select(selectorTwo, new LoadBalanceData()).getUrl(), 1, Integer::sum);
        });
        assertEquals(1000, firstCounts.get("set-a:8080").intValue());
        assertEquals(9000, firstCounts.get("set-b:8080").intValue());
        assertEquals(9000, secondCounts.get("set-a:8080").intValue());
        assertEquals(1000, secondCounts.get("set-b:8080").intValue());
    }

    /**
     * A configured-weight change rotates the state to a fresh pool, so the new
     * ratio applies from a clean counter instead of a polluted one.
     */
    @Test
    public void weightChangeRotatesToAFreshPool() {
        Upstream a = Upstream.builder().url("wc-a:8080").weight(1).status(true).build();
        Upstream heavyB = Upstream.builder().url("wc-b:8080").weight(9).status(true).build();
        RoundRobinLoadBalancer loadBalancer = new RoundRobinLoadBalancer();
        Map<String, Integer> counts = new HashMap<>();
        List<Upstream> heavy = List.of(a, heavyB);
        IntStream.range(0, 10000).forEach(i ->
                counts.merge(loadBalancer.select(heavy, new LoadBalanceData()).getUrl(), 1, Integer::sum));
        assertEquals(1000, counts.get("wc-a:8080").intValue());
        assertEquals(9000, counts.get("wc-b:8080").intValue());
        List<Upstream> lighter = List.of(a, Upstream.builder().url("wc-b:8080").weight(2).status(true).build());
        IntStream.range(0, 10000).forEach(i ->
                counts.merge(loadBalancer.select(lighter, new LoadBalanceData()).getUrl(), 1, Integer::sum));
        final int aTotal = counts.get("wc-a:8080").intValue();
        final int bTotal = counts.get("wc-b:8080").intValue();
        // 1:2 over the second window of 10000 picks
        assertTrue(Math.abs(aTotal - 1000 - 3333) <= 100, "a count in second window: " + aTotal);
        assertTrue(Math.abs(bTotal - 9000 - 6667) <= 100, "b count in second window: " + bTotal);
    }

    /**
     * Reordering a selector's upstream list starts a fresh pool and the old
     * state never leaks into the new order.
     */
    @Test
    public void reorderedUpstreamListStartsFreshState() {
        Upstream a = Upstream.builder().url("ro-a:8080").weight(1).status(true).build();
        Upstream b = Upstream.builder().url("ro-b:8080").weight(9).status(true).build();
        RoundRobinLoadBalancer loadBalancer = new RoundRobinLoadBalancer();
        List<Upstream> original = List.of(a, b);
        Map<String, Integer> counts = new HashMap<>();
        IntStream.range(0, 10000).forEach(i ->
                counts.merge(loadBalancer.select(original, new LoadBalanceData()).getUrl(), 1, Integer::sum));
        assertEquals(1000, counts.get("ro-a:8080").intValue());
        List<Upstream> reordered = List.of(b, a);
        IntStream.range(0, 10000).forEach(i ->
                counts.merge(loadBalancer.select(reordered, new LoadBalanceData()).getUrl(), 1, Integer::sum));
        assertEquals(1000, counts.get("ro-a:8080").intValue() - 1000);
    }
}
