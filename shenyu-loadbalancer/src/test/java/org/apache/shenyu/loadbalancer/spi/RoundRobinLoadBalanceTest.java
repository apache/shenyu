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

/**
 * The type Load balance test.
 */
public final class RoundRobinLoadBalanceTest {

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
        roundRobinLoadBalancer.select(upstreamList, "");
        roundRobinLoadBalancer.select(upstreamList2, "");
    }
}
