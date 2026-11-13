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
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * The type ShortestResponse balance test.
 */
public class ShortestResponseLoadBalancerTest {

    private List<Upstream> upstreamList;

    @Test
    public void testSelectByWeight() {
        this.upstreamList = Stream.of(1, 2)
                .map(weight -> Upstream.builder()
                        .url("upstream-" + weight)
                        .weight(weight)
                        .build())
                .collect(Collectors.toList());
        int select1 = 0;
        int select2 = 0;
        int loop = 10000;
        ShortestResponseLoadBalancer lb = new ShortestResponseLoadBalancer();
        for (int i = 0; i < loop; i++) {
            Upstream upstream = lb.select(upstreamList, new LoadBalanceData());
            if (upstream.getUrl().equals("upstream-1")) {
                select1++;
            }
            if (upstream.getUrl().equals("upstream-2")) {
                select2++;
            }
        }
        Assertions.assertTrue(select1 < select2);
        Assertions.assertEquals(loop, select1 + select2);
    }

    @Test
    public void testSelectByResponse() {
        this.upstreamList = Stream.of(1, 2)
                .map(weight -> Upstream.builder()
                        .url("upstream-" + weight)
                        .build())
                .collect(Collectors.toList());
        int select1 = 0;
        int select2 = 0;
        int loop = 10000;
        ShortestResponseLoadBalancer lb = new ShortestResponseLoadBalancer();
        upstreamList.get(0).getSucceeded().addAndGet(1);
        upstreamList.get(0).getSucceededElapsed().addAndGet(50000);
        for (int i = 0; i < loop; i++) {
            Upstream upstream = lb.select(upstreamList, new LoadBalanceData());
            if (upstream.getUrl().equals("upstream-1")) {
                select1++;
            }
            if (upstream.getUrl().equals("upstream-2")) {
                select2++;
            }
        }
        Assertions.assertTrue(select1 < select2);
        Assertions.assertEquals(loop, select1 + select2);

    }

    @Test
    public void growingInflightShiftsTrafficAwayFromHistoricallyFastUpstream() {
        Upstream fast = Upstream.builder().url("a").weight(1).build();
        Upstream slow = Upstream.builder().url("b").weight(1).build();
        fast.getSucceeded().set(1);
        fast.getSucceededElapsed().set(10);
        slow.getSucceeded().set(1);
        slow.getSucceededElapsed().set(100);
        fast.getInflight().set(1);
        slow.getInflight().set(1);
        ShortestResponseLoadBalancer balancer = new ShortestResponseLoadBalancer();
        int fastHits = 0;
        List<Upstream> upstreams = List.of(fast, slow);
        LoadBalanceData data = new LoadBalanceData();
        for (int i = 0; i < 100; i++) {
            if (balancer.select(upstreams, data) == fast) {
                fastHits++;
            }
        }
        Assertions.assertTrue(fastHits < 100);
        Assertions.assertTrue(fast.getInflight().get() > 1);
        Assertions.assertTrue(slow.getInflight().get() > 1);
        Assertions.assertEquals(2 + 100, fast.getInflight().get() + slow.getInflight().get());
    }

    @Test
    public void singletonSelectionIncrementsInflightOnce() {
        Upstream upstream = Upstream.builder().url("only").weight(1).build();
        long before = upstream.getInflight().get();
        Upstream picked = new ShortestResponseLoadBalancer().select(List.of(upstream), new LoadBalanceData());
        Assertions.assertSame(upstream, picked);
        Assertions.assertEquals(before + 1, upstream.getInflight().get());
    }
}
