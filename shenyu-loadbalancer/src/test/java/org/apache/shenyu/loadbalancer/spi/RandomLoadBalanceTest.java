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
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * The type random balance test.
 */
public class RandomLoadBalanceTest {

    private List<Upstream> randomLoadBalancesWeightDisordered;

    private List<Upstream> randomLoadBalancesWeightOrdered;

    private List<Upstream> randomLoadBalancesWeightReversed;

    private List<Upstream> randomLoadBalancesWeightEqual;

    private List<Upstream> randomLoadBalancesWeightZero;

    private RandomLoadBalancer randomLoadBalancer;

    @BeforeEach
    public void setUp() {
        this.randomLoadBalancesWeightDisordered = Stream.of(10, 50, 40)
                .map(weight -> Upstream.builder()
                        .url("upstream-" + weight)
                        .weight(weight)
                        .build())
                .collect(Collectors.toList());

        this.randomLoadBalancesWeightOrdered = Stream.of(10, 40, 50)
                .map(weight -> Upstream.builder()
                        .url("upstream-" + weight)
                        .weight(weight)
                        .build())
                .collect(Collectors.toList());

        this.randomLoadBalancesWeightReversed = Stream.of(50, 40, 10)
                .map(weight -> Upstream.builder()
                        .url("upstream-" + weight)
                        .weight(weight)
                        .build())
                .collect(Collectors.toList());

        this.randomLoadBalancesWeightEqual = Stream.of(10, 10, 10)
                .map(weight -> Upstream.builder()
                        .url("upstream-" + weight)
                        .weight(weight)
                        .build())
                .collect(Collectors.toList());

        this.randomLoadBalancesWeightZero = Stream.of(0, 0, 0)
                .map(weight -> Upstream.builder()
                        .url("upstream-" + weight)
                        .weight(weight)
                        .build())
                .collect(Collectors.toList());

        randomLoadBalancer = new RandomLoadBalancer();
    }

    @Test
    public void randomLoadBalancesWeightEqualTest() {
        final Upstream upstreamOrdered = randomLoadBalancer.select(randomLoadBalancesWeightEqual, "");
        assertNotNull(upstreamOrdered);
    }

    @Test
    public void randomLoadBalancesWeightZeroTest() {
        final Upstream upstreamOrdered = randomLoadBalancer.select(randomLoadBalancesWeightZero, "");
        assertNotNull(upstreamOrdered);
    }

    @Test
    public void randomLoadBalanceOrderedWeightTest() {
        final Upstream upstreamOrdered = randomLoadBalancer.select(randomLoadBalancesWeightOrdered, "");
        assertNotNull(upstreamOrdered);
    }

    @Test
    public void randomLoadBalanceDisOrderedWeightTest() {
        final Upstream upstreamDisordered = randomLoadBalancer.select(randomLoadBalancesWeightDisordered, "");
        assertNotNull(upstreamDisordered);
    }

    @Test
    public void randomLoadBalanceReversedWeightTest() {
        final Upstream upstreamReversed = randomLoadBalancer.select(randomLoadBalancesWeightReversed, "");
        assertNotNull(upstreamReversed);
    }
}
