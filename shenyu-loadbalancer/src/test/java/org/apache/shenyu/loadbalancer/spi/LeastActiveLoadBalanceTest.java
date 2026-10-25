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

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;

/**
 * The type least activity load balance test.
 */
public class LeastActiveLoadBalanceTest {
    private final List<Upstream> onlyOneList = new ArrayList<>();

    /**
     * build upstream list.
     */
    public void buildUpstreamList() {
        Upstream upstream1 = Upstream.builder()
                .url("baidu.com")
                .protocol("https://")
                .build();
        Upstream upstream2 = Upstream.builder()
                .url("pro.jd.com")
                .protocol("https://")
                .build();
        onlyOneList.add(upstream1);
        onlyOneList.add(upstream2);
    }

    @Test
    public void testResponseTimeBalancer() {
        buildUpstreamList();
        final LeastActiveLoadBalance leastActiveLoadBalance = new LeastActiveLoadBalance();
        List<Upstream> selected = new ArrayList<>();
        for (int i = 0; i < 100; i++) {
            selected.add(leastActiveLoadBalance.doSelect(onlyOneList, new LoadBalanceData()));
        }
        // Every upstream is idle, so both share the traffic.
        Assertions.assertTrue(selected.contains(onlyOneList.get(0)));
        Assertions.assertTrue(selected.contains(onlyOneList.get(1)));
    }

    @Test
    public void testSameAddressVariantsKeepIndependentCounts() {
        Upstream first = Upstream.builder().protocol("dubbo://").url("127.0.0.1:20880").group("blue").version("v1").build();
        Upstream second = Upstream.builder().protocol("dubbo://").url("127.0.0.1:20880").group("green").version("v2").build();
        List<Upstream> upstreamList = List.of(first, second);
        LeastActiveLoadBalance loadBalance = new LeastActiveLoadBalance();

        // The two variants share an address but keep separate active counts.
        first.getInflight().set(5);
        for (int i = 0; i < 20; i++) {
            Assertions.assertSame(second, loadBalance.doSelect(upstreamList, new LoadBalanceData()));
        }
        first.getInflight().set(1);
        second.getInflight().set(5);
        for (int i = 0; i < 20; i++) {
            Assertions.assertSame(first, loadBalance.doSelect(upstreamList, new LoadBalanceData()));
        }
    }

    /**
     * A completed request must stop competing with outstanding work: while the slow upstream holds one
     * request, every selection goes to the idle upstream instead of alternating with the busy one.
     */
    @Test
    public void testSlowOutstandingRequestReceivesNoFurtherTraffic() {
        buildUpstreamList();
        Upstream busy = onlyOneList.get(0);
        Upstream idle = onlyOneList.get(1);
        busy.getInflight().set(2);
        final LeastActiveLoadBalance leastActiveLoadBalance = new LeastActiveLoadBalance();

        for (int i = 0; i < 50; i++) {
            Assertions.assertSame(idle, leastActiveLoadBalance.doSelect(onlyOneList, new LoadBalanceData()));
        }
        // Releasing the outstanding request lets the upstream compete again.
        busy.getInflight().set(1);
        List<Upstream> selected = new ArrayList<>();
        for (int i = 0; i < 100; i++) {
            selected.add(leastActiveLoadBalance.doSelect(onlyOneList, new LoadBalanceData()));
        }
        Assertions.assertTrue(selected.contains(busy));
    }

    /**
     * The selections that preceded a long-running request are completed work. They must not make the busy
     * upstream look idle, whatever the gateway did before that request started.
     */
    @Test
    public void testCumulativeSelectionsDoNotMaskOutstandingWork() {
        buildUpstreamList();
        Upstream busy = onlyOneList.get(0);
        Upstream idle = onlyOneList.get(1);
        final LeastActiveLoadBalance leastActiveLoadBalance = new LeastActiveLoadBalance();
        for (int i = 0; i < 100; i++) {
            leastActiveLoadBalance.doSelect(onlyOneList, new LoadBalanceData());
        }
        busy.getInflight().set(2);
        for (int i = 0; i < 20; i++) {
            Assertions.assertSame(idle, leastActiveLoadBalance.doSelect(onlyOneList, new LoadBalanceData()));
        }
    }

    /**
     * A newly added upstream starts idle, as do the existing ones. Weight, warmup included, decides between
     * the upstreams that share the lowest active count, so the new node is not sent every request.
     */
    @Test
    public void testNewlyAddedUpstreamIsNotFlooded() {
        Upstream a = Upstream.builder().protocol("http://").url("a:80").weight(100).status(true).build();
        Upstream b = Upstream.builder().protocol("http://").url("b:80").weight(100).status(true).build();
        Upstream c = Upstream.builder().protocol("http://").url("c:80").weight(100).status(true).build();
        final LeastActiveLoadBalance leastActiveLoadBalance = new LeastActiveLoadBalance();

        for (int i = 0; i < 100; i++) {
            leastActiveLoadBalance.doSelect(List.of(a, b), new LoadBalanceData());
        }
        int newNodeHits = 0;
        for (int i = 0; i < 50; i++) {
            if (leastActiveLoadBalance.doSelect(List.of(a, b, c), new LoadBalanceData()) == c) {
                newNodeHits++;
            }
        }
        // Equal weights and equal active counts: the new node takes its third of the traffic, not all of it.
        Assertions.assertTrue(newNodeHits < 50, "the new upstream was selected " + newNodeHits + " out of 50 times");
        Assertions.assertTrue(newNodeHits > 0, "the new upstream was never selected");
    }

    /**
     * Upstreams that share the lowest count are picked by weight, so an upstream whose warmup is still
     * ramping up receives proportionally less traffic than a fully warm one.
     */
    @Test
    public void testWeightBreaksTiesAmongLeastActiveUpstreams() {
        Upstream warm = Upstream.builder().protocol("http://").url("warm:80").weight(100).status(true)
                .timestamp(System.currentTimeMillis() - 60_000).warmup(60_000).build();
        // One tenth of its warmup elapsed, so its effective weight is 10 against the fully warm 100.
        Upstream ramping = Upstream.builder().protocol("http://").url("ramping:80").weight(100).status(true)
                .timestamp(System.currentTimeMillis() - 60_000).warmup(600_000).build();
        List<Upstream> upstreamList = List.of(warm, ramping);
        final LeastActiveLoadBalance leastActiveLoadBalance = new LeastActiveLoadBalance();

        int rampingHits = 0;
        for (int i = 0; i < 400; i++) {
            if (leastActiveLoadBalance.doSelect(upstreamList, new LoadBalanceData()) == ramping) {
                rampingHits++;
            }
        }
        Assertions.assertTrue(rampingHits > 0, "the ramping upstream was never selected");
        Assertions.assertTrue(rampingHits < 100, "the ramping upstream was selected " + rampingHits + " out of 400 times");
    }

    @Test
    public void testInflightCountIsReadNotReserved() {
        buildUpstreamList();
        onlyOneList.get(0).setInflight(new AtomicLong(3));
        onlyOneList.get(1).setInflight(new AtomicLong(1));
        final LeastActiveLoadBalance leastActiveLoadBalance = new LeastActiveLoadBalance();
        leastActiveLoadBalance.doSelect(onlyOneList, new LoadBalanceData());
        // Selection leaves the accounting to the request lifecycle.
        Assertions.assertEquals(3, onlyOneList.get(0).getInflight().get());
        Assertions.assertEquals(1, onlyOneList.get(1).getInflight().get());
    }
}
