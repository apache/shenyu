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
import org.junit.jupiter.params.provider.ValueSource;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * The type random balance test.
 */
public class RandomLoadBalancerTest {

    @Test
    public void randomLoadBalancesWeightEqualTest() {
        final List<Upstream> targets = Stream.of(10, 10, 10)
                .map(weight -> Upstream.builder()
                        .url("upstream-" + weight)
                        .weight(weight)
                        .build())
                .collect(Collectors.toList());
        final RandomLoadBalancer balancer = new RandomLoadBalancer();
        final LoadBalanceData data = new LoadBalanceData();
        for (int index = 0; index < 1000; index++) {
            assertTrue(targets.contains(balancer.doSelect(targets, data)));
        }
    }

    @Test
    public void randomLoadBalancesWeightZeroTest() {
        final List<Upstream> targets = Stream.of(0, 0, 0)
                .map(weight -> Upstream.builder()
                        .url("upstream-" + weight)
                        .weight(weight)
                        .build())
                .collect(Collectors.toList());
        final RandomLoadBalancer balancer = new RandomLoadBalancer();
        final LoadBalanceData data = new LoadBalanceData();
        for (int index = 0; index < 1000; index++) {
            assertTrue(targets.contains(balancer.doSelect(targets, data)));
        }
    }

    @ParameterizedTest
    @ValueSource(ints = {0, 1, 3, 5})
    public void selectsOnlyPositiveWeightAtDifferentPositions(final int heavyIndex) {
        final List<Upstream> targets = IntStream.range(0, 6)
                .mapToObj(index -> Upstream.builder().url("upstream-" + index).weight(index == heavyIndex ? 100 : 0).build())
                .collect(Collectors.toList());
        final RandomLoadBalancer balancer = new RandomLoadBalancer();
        final LoadBalanceData data = new LoadBalanceData();
        for (int index = 0; index < 1000; index++) {
            assertSame(targets.get(heavyIndex), balancer.doSelect(targets, data));
        }
    }

    @ParameterizedTest
    @ValueSource(ints = {0, 100})
    public void selectsSingleUpstream(final int weight) {
        final Upstream target = Upstream.builder().url("single").weight(weight).build();
        final RandomLoadBalancer balancer = new RandomLoadBalancer();
        final LoadBalanceData data = new LoadBalanceData();
        for (int index = 0; index < 100; index++) {
            assertSame(target, balancer.doSelect(List.of(target), data));
        }
    }

    @Test
    public void excludesLeadingZeroWeightWhenRemainingWeightsAreEqualAndPositive() {
        final Upstream zero = Upstream.builder().url("zero").weight(0).build();
        final Upstream first = Upstream.builder().url("first").weight(10).build();
        final Upstream second = Upstream.builder().url("second").weight(10).build();
        final List<Upstream> targets = List.of(zero, first, second);
        final List<Upstream> positiveTargets = List.of(first, second);
        final RandomLoadBalancer balancer = new RandomLoadBalancer();
        final LoadBalanceData data = new LoadBalanceData();
        for (int index = 0; index < 1000; index++) {
            assertTrue(positiveTargets.contains(balancer.doSelect(targets, data)));
        }
    }

    @ParameterizedTest
    @ValueSource(ints = {-1, 1, 3, 5})
    public void readsEachWeightOnceInListOrder(final int heavyIndex) {
        final List<Upstream> targets = IntStream.range(0, 6)
                .mapToObj(index -> Upstream.builder().url("upstream-" + index).weight(index == heavyIndex ? 100 : 0).build())
                .collect(Collectors.toList());
        final List<Upstream> observed = new ArrayList<>();
        final RandomLoadBalancer balancer = new RandomLoadBalancer() {
            @Override
            protected int getWeight(final Upstream upstream) {
                assertTrue(observed.size() < targets.size());
                assertSame(targets.get(observed.size()), upstream);
                observed.add(upstream);
                return super.getWeight(upstream);
            }
        };
        assertTrue(targets.contains(balancer.doSelect(targets, new LoadBalanceData())));
        assertEquals(targets, observed);
    }

    @Test
    public void rejectsEmptyAndNullListsInDoSelect() {
        final RandomLoadBalancer balancer = new RandomLoadBalancer();
        final LoadBalanceData data = new LoadBalanceData();
        assertThrows(IndexOutOfBoundsException.class, () -> balancer.doSelect(List.of(), data));
        assertThrows(NullPointerException.class, () -> balancer.doSelect(null, data));
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
}
