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
import org.apache.shenyu.loadbalancer.factory.LoadBalancerFactory;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Weighted selection must retain the full sum of configured integer weights.
 */
public class WeightedSelectionOverflowTest {

    private static final int SELECTIONS = 4096;

    @Test
    public void testRandomDoesNotSelectZeroWeightWhenBothSumsOverflow() {
        List<Upstream> upstreams = upstreams(Integer.MAX_VALUE, 1, 0, 0, Integer.MAX_VALUE, 2);
        // With int sums, totalWeight is 1 and the first-half sum is Integer.MIN_VALUE.
        // Every possible draw then selects the fourth entry, even though its weight is zero.
        Upstream selected = new RandomLoadBalancer().select(upstreams, new LoadBalanceData());
        assertTrue(selected.getWeight() > 0, "a zero-weight upstream must not receive weighted traffic");
    }

    @ParameterizedTest
    @CsvSource({
            "random, 3, 1, 0",
            "leastActive, 3, 1, 0",
            "shortestResponse, 3, 1, 0",
            "random, 1800000000, 600000000, 0",
            "leastActive, 1800000000, 600000000, 0",
            "shortestResponse, 1800000000, 600000000, 0",
            "random, 2000000000, 2000000000, 1000000000",
            "leastActive, 2000000000, 2000000000, 1000000000",
            "shortestResponse, 2000000000, 2000000000, 1000000000"
    })
    public void testSelectionRespectsWeightsAcrossIntegerOverflow(final String algorithm, final int firstWeight,
                                                                final int secondWeight, final int thirdWeight) {
        List<Upstream> upstreams = upstreams(firstWeight, secondWeight, thirdWeight);
        int[] counts = new int[upstreams.size()];
        LoadBalanceData data = new LoadBalanceData();
        for (int i = 0; i < SELECTIONS; i++) {
            Upstream selected = LoadBalancerFactory.selector(upstreams, algorithm, data);
            counts[upstreams.indexOf(selected)]++;
        }

        long totalWeight = (long) firstWeight + secondWeight + thirdWeight;
        for (int i = 0; i < upstreams.size(); i++) {
            int weight = upstreams.get(i).getWeight();
            if (weight == 0) {
                assertEquals(0, counts[i], "zero-weight upstream selected by " + algorithm);
            } else {
                double expected = (double) weight / totalWeight;
                double actual = (double) counts[i] / SELECTIONS;
                // A wide margin separates the correct distribution from overflow's uniform
                // fallback or single-node bias without pinning the random sequence.
                assertEquals(expected, actual, 0.08, "incorrect weight share for " + algorithm + " upstream " + i);
            }
        }
    }

    private List<Upstream> upstreams(final int... weights) {
        return IntStream.range(0, weights.length)
                .mapToObj(i -> Upstream.builder().protocol("http://").url("upstream-" + i)
                        .weight(weights[i]).status(true).timestamp(0).warmup(0).build())
                .collect(Collectors.toList());
    }
}

