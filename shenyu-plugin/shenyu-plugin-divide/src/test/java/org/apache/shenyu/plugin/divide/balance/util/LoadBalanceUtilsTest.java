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

package org.apache.shenyu.plugin.divide.balance.util;

import org.apache.shenyu.common.dto.convert.DivideUpstream;
import org.apache.shenyu.plugin.divide.balance.utils.LoadBalanceUtils;
import org.junit.Assert;
import org.junit.Test;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * The type loadBalance utils test.
 */
public final class LoadBalanceUtilsTest {

    /**
     * Load balance util test.
     */
    @Test
    public void loadBalanceUtilsOrderedWeightTest() {
        List<DivideUpstream> upstreamList =
                Stream.of(10, 20, 70)
                        .map(weight -> DivideUpstream.builder()
                                .upstreamUrl("upstream-" + weight)
                                .weight(weight)
                                .build())
                        .collect(Collectors.toList());
        Map<String, Integer> countMap = new HashMap<>();
        for (int i = 0; i < 120; i++) {
            DivideUpstream result = LoadBalanceUtils.selector(upstreamList, "roundRobin", "");
            int count = countMap.getOrDefault(result.getUpstreamUrl(), 0);
            countMap.put(result.getUpstreamUrl(), ++count);
        }
        Assert.assertEquals(12, countMap.get("upstream-10").intValue());
    }

    @Test
    public void loadBalanceUtilsDisOrderedWeightTest() {
        List<DivideUpstream> upstreamList =
                Stream.of(70, 10, 20)
                        .map(weight -> DivideUpstream.builder()
                                .upstreamUrl("upstream-" + weight)
                                .weight(weight)
                                .build())
                        .collect(Collectors.toList());
        Map<String, Integer> countMap = new HashMap<>();
        for (int i = 0; i < 120; i++) {
            DivideUpstream result = LoadBalanceUtils.selector(upstreamList, "roundRobin", "");
            int count = countMap.getOrDefault(result.getUpstreamUrl(), 0);
            countMap.put(result.getUpstreamUrl(), ++count);
        }
        Assert.assertEquals(12, countMap.get("upstream-10").intValue());
    }

    @Test
    public void loadBalanceUtilsReversedWeightTest() {
        List<DivideUpstream> upstreamList =
                Stream.of(70, 20, 10)
                        .map(weight -> DivideUpstream.builder()
                                .upstreamUrl("upstream-" + weight)
                                .weight(weight)
                                .build())
                        .collect(Collectors.toList());
        Map<String, Integer> countMap = new HashMap<>();
        for (int i = 0; i < 120; i++) {
            DivideUpstream result = LoadBalanceUtils.selector(upstreamList, "roundRobin", "");
            int count = countMap.getOrDefault(result.getUpstreamUrl(), 0);
            countMap.put(result.getUpstreamUrl(), ++count);
        }
        Assert.assertEquals(12, countMap.get("upstream-10").intValue());
    }
}
