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

import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.junit.jupiter.api.Test;

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
            Upstream result = roundRobinLoadBalancer.select(upstreamList, "");
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
            Upstream result = roundRobinLoadBalancer.select(upstreamList, "");
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
            Upstream result = roundRobinLoadBalancer.select(upstreamList, "");
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
        Upstream result1 = roundRobinLoadBalancer.select(upstreamList, "");
        assertNotNull(result1, "Selected upstream should not be null");
        assertTrue(upstreamList.contains(result1), "Selected upstream should be from the provided list");
        
        // Test with equal weight upstream list
        Upstream result2 = roundRobinLoadBalancer.select(upstreamList2, "");
        assertNotNull(result2, "Selected upstream should not be null");
        assertTrue(upstreamList2.contains(result2), "Selected upstream should be from the provided list");
        
        // Test multiple selections to verify round-robin behavior
        Map<String, Integer> countMap = new HashMap<>();
        IntStream.range(0, SELECTION_ITERATIONS).forEach(i -> {
            Upstream result = roundRobinLoadBalancer.select(upstreamList2, "");
            int count = countMap.getOrDefault(result.getUrl(), 0);
            countMap.put(result.getUrl(), ++count);
        });
        
        // With equal weights, distribution should be roughly equal
        assertEquals(3, countMap.size(), "All three upstreams should be selected");
        countMap.values().forEach(count -> 
            assertTrue(count >= 8 && count <= 12, "Distribution should be roughly equal for equal weights"));
    }
}