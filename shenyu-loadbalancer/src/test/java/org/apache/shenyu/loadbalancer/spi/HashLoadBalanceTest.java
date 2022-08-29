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
import org.apache.shenyu.loadbalancer.entity.UpstreamHolder;
import org.apache.shenyu.loadbalancer.util.WeightUtil;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Method;
import java.util.List;
import java.util.SortedMap;
import java.util.concurrent.ConcurrentSkipListMap;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * The type Hash balance test.
 */
public final class HashLoadBalanceTest {

    private Method hash;

    private UpstreamHolder hashLoadBalancesOrdered;

    private UpstreamHolder hashLoadBalancesDisordered;

    private UpstreamHolder hashLoadBalancesReversed;

    private ConcurrentSkipListMap<Long, Upstream> treeMapOrdered;

    private ConcurrentSkipListMap<Long, Upstream> treeMapDisordered;

    private ConcurrentSkipListMap<Long, Upstream> treeMapReversed;

    @BeforeEach
    public void setUp() throws Exception {
        this.hash = HashLoadBalancer.class.getDeclaredMethod("hash", String.class);
        this.hash.setAccessible(true);
        List<Upstream> orderedUpstream = Stream.of(1, 2, 3)
                .map(weight -> Upstream.builder().url("upstream-" + weight).build())
                .collect(Collectors.toList());
        this.hashLoadBalancesOrdered = new UpstreamHolder(WeightUtil.calculateTotalWeight(orderedUpstream), orderedUpstream);
        List<Upstream> disorderedUpstream = Stream.of(2, 1, 3)
                .map(weight -> Upstream.builder().url("upstream-" + weight).build())
                .collect(Collectors.toList());
        this.hashLoadBalancesDisordered = new UpstreamHolder(WeightUtil.calculateTotalWeight(disorderedUpstream), disorderedUpstream);
        List<Upstream> reversedUpstream = Stream.of(3, 2, 1)
                .map(weight -> Upstream.builder().url("upstream-" + weight).build())
                .collect(Collectors.toList());
        this.hashLoadBalancesReversed = new UpstreamHolder(WeightUtil.calculateTotalWeight(reversedUpstream), reversedUpstream);
        this.treeMapOrdered = new ConcurrentSkipListMap<>();
        this.treeMapDisordered = new ConcurrentSkipListMap<>();
        this.treeMapReversed = new ConcurrentSkipListMap<>();
        for (Upstream address : hashLoadBalancesOrdered.getUpstreams()) {
            for (int i = 0; i < 5; i++) {
                String hashKey = "SHENYU-" + address.getUrl() + "-HASH-" + i;
                Object o = hash.invoke(null, hashKey);
                treeMapOrdered.put(Long.parseLong(o.toString()), address);
            }
        }
        for (Upstream address : hashLoadBalancesReversed.getUpstreams()) {
            for (int i = 0; i < 5; i++) {
                String hashKey = "SHENYU-" + address.getUrl() + "-HASH-" + i;
                Object o = hash.invoke(null, hashKey);
                treeMapReversed.put(Long.parseLong(o.toString()), address);
            }
        }
        for (Upstream address : hashLoadBalancesDisordered.getUpstreams()) {
            for (int i = 0; i < 5; i++) {
                String hashKey = "SHENYU-" + address.getUrl() + "-HASH-" + i;
                Object o = hash.invoke(null, hashKey);
                treeMapDisordered.put(Long.parseLong(o.toString()), address);
            }
        }
    }

    /**
     * Hash load balance test.
     */
    @Test
    public void hashLoadBalanceOrderedWeightTest() throws Exception {
        final String ip = "127.0.0.1";
        final HashLoadBalancer hashLoadBalance = new HashLoadBalancer();
        final Upstream upstream = hashLoadBalance.select(hashLoadBalancesOrdered, ip);
        final Long hashKey = Long.parseLong(hash.invoke(null, ip).toString());
        final SortedMap<Long, Upstream> lastRing = treeMapOrdered.tailMap(hashKey);
        final Upstream assertUp = lastRing.get(lastRing.firstKey());
        assertEquals(assertUp.getUrl(), upstream.getUrl());

    }

    @Test
    public void hashLoadBalanceDisorderedWeightTest() throws Exception {
        final String ip = "127.0.0.1";
        final HashLoadBalancer hashLoadBalance = new HashLoadBalancer();
        final Upstream upstream = hashLoadBalance.select(hashLoadBalancesDisordered, ip);
        final Long hashKey = Long.parseLong(hash.invoke(null, ip).toString());
        final SortedMap<Long, Upstream> lastRing = treeMapDisordered.tailMap(hashKey);
        final Upstream assertUp = lastRing.get(lastRing.firstKey());
        assertEquals(assertUp.getUrl(), upstream.getUrl());

    }

    @Test
    public void hashLoadBalanceReversedWeightTest() throws Exception {
        final String ip = "127.0.0.1";
        final HashLoadBalancer hashLoadBalance = new HashLoadBalancer();
        final Upstream divideUpstream = hashLoadBalance.select(hashLoadBalancesReversed, ip);
        final Long hashKey = Long.parseLong(hash.invoke(null, ip).toString());
        final SortedMap<Long, Upstream> lastRing = treeMapReversed.tailMap(hashKey);
        final Upstream assertUp = lastRing.get(lastRing.firstKey());
        assertEquals(assertUp.getUrl(), divideUpstream.getUrl());
    }
}
