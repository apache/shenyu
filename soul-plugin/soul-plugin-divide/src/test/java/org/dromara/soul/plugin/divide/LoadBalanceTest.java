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

package org.dromara.soul.plugin.divide;

import org.dromara.soul.common.dto.convert.DivideUpstream;
import org.dromara.soul.plugin.divide.balance.spi.RoundRobinLoadBalance;
import org.junit.Assert;
import org.junit.Test;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * The type Load balance test.
 *
 * @author wanglaomo
 */
public class LoadBalanceTest {
    
    /**
     * Round robin load balance test.
     */
    @Test
    public void roundRobinLoadBalanceTest() {
        List<DivideUpstream> divideUpstreamList =
                Stream.of(50, 20, 30)
                        .map(weight -> {
                            DivideUpstream divideUpstream = new DivideUpstream();
                            divideUpstream.setUpstreamUrl("divide-upstream-" + weight);
                            divideUpstream.setWeight(weight);
                            return divideUpstream;
                        })
                        .collect(Collectors.toList());
        
        RoundRobinLoadBalance roundRobinLoadBalance = new RoundRobinLoadBalance();
        Map<String, Integer> countMap = new HashMap<>();
        for (int i = 0; i < 120; i++) {
            DivideUpstream result = roundRobinLoadBalance.select(divideUpstreamList, "");
            int count = countMap.getOrDefault(result.getUpstreamUrl(), 0);
            countMap.put(result.getUpstreamUrl(), ++count);
        }
        Assert.assertEquals(50, countMap.get("divide-upstream-50").intValue());
    }
}
