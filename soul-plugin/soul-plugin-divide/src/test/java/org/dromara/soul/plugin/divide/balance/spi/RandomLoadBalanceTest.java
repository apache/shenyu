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

package org.dromara.soul.plugin.divide.balance.spi;

import org.dromara.soul.common.dto.convert.DivideUpstream;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * The type random balance test.
 *
 * @author zhanglei
 */
public class RandomLoadBalanceTest {

    private List<DivideUpstream> randomLoadBalances;

    @Before
    public void setUp() {
        this.randomLoadBalances = Stream.of(10, 40, 50)
                .map(weight -> DivideUpstream.builder()
                        .upstreamUrl("divide-upstream-" + weight)
                        .weight(weight)
                        .build())
                .collect(Collectors.toList());
    }

    /**
     * random load balance test.
     */
    @Test
    public void randomLoadBalanceTest() {
        final RandomLoadBalance randomLoadBalance = new RandomLoadBalance();
        DivideUpstream upstream = randomLoadBalance.select(randomLoadBalances, "");
        Assert.assertNotNull(upstream);
    }
}
