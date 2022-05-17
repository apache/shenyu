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

package org.apache.shenyu.loadbalancer.factory;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import org.apache.shenyu.common.enums.LoadBalanceEnum;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * The type loadBalance utils test.
 */
public final class LoadBalancerFactoryTest {

    /**
     * Load balance util test.
     */
    @Test
    public void loadBalanceUtilsOrderedWeightTest() {
        List<Upstream> upstreamList =
                Stream.of(10, 20, 70)
                        .map(weight -> Upstream.builder()
                                .url("upstream-" + weight)
                                .weight(weight)
                                .build())
                        .collect(Collectors.toList());
        Map<String, Integer> countMap = new HashMap<>();
        IntStream.range(0, 120).forEach(i -> {
            Upstream result = LoadBalancerFactory.selector(upstreamList, LoadBalanceEnum.ROUND_ROBIN.getName(), "");
            int count = countMap.getOrDefault(result.getUrl(), 0);
            countMap.put(result.getUrl(), ++count);
        });
        assertEquals(12, countMap.get("upstream-10").intValue());
    }

    @Test
    public void loadBalanceUtilsDisOrderedWeightTest() {
        List<Upstream> upstreamList =
                Stream.of(70, 10, 20)
                        .map(weight -> Upstream.builder()
                                .url("upstream-" + weight)
                                .weight(weight)
                                .build())
                        .collect(Collectors.toList());
        Map<String, Integer> countMap = new HashMap<>();
        IntStream.range(0, 120).forEach(i -> {
            Upstream result = LoadBalancerFactory.selector(upstreamList, LoadBalanceEnum.ROUND_ROBIN.getName(), "");
            int count = countMap.getOrDefault(result.getUrl(), 0);
            countMap.put(result.getUrl(), ++count);
        });
        assertEquals(12, countMap.get("upstream-10").intValue());
    }

    @Test
    public void loadBalanceUtilsReversedWeightTest() {
        List<Upstream> upstreamList =
                Stream.of(70, 20, 10)
                        .map(weight -> Upstream.builder()
                                .url("upstream-" + weight)
                                .weight(weight)
                                .build())
                        .collect(Collectors.toList());
        Map<String, Integer> countMap = new HashMap<>();
        IntStream.range(0, 120).forEach(i -> {
            Upstream result = LoadBalancerFactory.selector(upstreamList, LoadBalanceEnum.ROUND_ROBIN.getName(), "");
            int count = countMap.getOrDefault(result.getUrl(), 0);
            countMap.put(result.getUrl(), ++count);
        });
        assertEquals(12, countMap.get("upstream-10").intValue());
    }
}
