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
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.List;
import java.util.SortedMap;
import java.util.concurrent.ConcurrentSkipListMap;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mockStatic;

/**
 * The type Hash balance test.
 */
public final class HashLoadBalanceTest {

    private Method hash;

    private List<Upstream> onlyOneList;

    private List<Upstream> hashLoadBalancesOrdered;

    private List<Upstream> hashLoadBalancesDisordered;

    private List<Upstream> hashLoadBalancesReversed;

    private ConcurrentSkipListMap<Long, Upstream> treeMapOrdered;

    private ConcurrentSkipListMap<Long, Upstream> treeMapDisordered;

    private ConcurrentSkipListMap<Long, Upstream> treeMapReversed;

    @BeforeEach
    public void setUp() throws Exception {
        this.hash = HashLoadBalancer.class.getDeclaredMethod("hash", String.class);
        this.hash.setAccessible(true);
        this.onlyOneList = Stream.of(1)
                .map(weight -> Upstream.builder()
                        .url("upstream-" + weight)
                        .status(false)
                        .build())
                .collect(Collectors.toList());
        this.hashLoadBalancesOrdered = Stream.of(1, 2, 3)
                .map(weight -> Upstream.builder()
                        .url("upstream-" + weight)
                        .build())
                .collect(Collectors.toList());
        this.hashLoadBalancesDisordered = Stream.of(2, 1, 3)
                .map(weight -> Upstream.builder()
                        .url("upstream-" + weight)
                        .build())
                .collect(Collectors.toList());
        this.hashLoadBalancesReversed = Stream.of(3, 2, 1)
                .map(weight -> Upstream.builder()
                        .url("upstream-" + weight)
                        .build())
                .collect(Collectors.toList());
        this.treeMapOrdered = new ConcurrentSkipListMap<>();
        this.treeMapDisordered = new ConcurrentSkipListMap<>();
        this.treeMapReversed = new ConcurrentSkipListMap<>();
        for (Upstream address : hashLoadBalancesOrdered) {
            for (int i = 0; i < 5; i++) {
                String hashKey = "SHENYU-" + address.getUrl() + "-HASH-" + i;
                Object o = hash.invoke(null, hashKey);
                treeMapOrdered.put(Long.parseLong(o.toString()), address);
            }
        }
        for (Upstream address : hashLoadBalancesReversed) {
            for (int i = 0; i < 5; i++) {
                String hashKey = "SHENYU-" + address.getUrl() + "-HASH-" + i;
                Object o = hash.invoke(null, hashKey);
                treeMapReversed.put(Long.parseLong(o.toString()), address);
            }
        }
        for (Upstream address : hashLoadBalancesDisordered) {
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
    public void onlyOneListTest() {
        final String ip = "127.0.0.1";
        final HashLoadBalancer hashLoadBalance = new HashLoadBalancer();
        Assertions.assertNotNull(hashLoadBalance.select(onlyOneList, ip));
        Assertions.assertEquals(hashLoadBalance.getWeight(Upstream.builder().status(false).build()), 0);
        Assertions.assertEquals(hashLoadBalance.getWeight(Upstream.builder()
                .timestamp(System.currentTimeMillis() - 3L)
                .warmup(4)
                .weight(1)
                .build()), 1);
        Assertions.assertEquals(hashLoadBalance.getWeight(Upstream.builder()
                .timestamp(System.currentTimeMillis() + 3L)
                .warmup(4)
                .weight(1)
                .build()), 1);
        Assertions.assertEquals(hashLoadBalance.getWeight(Upstream.builder()
                .timestamp(System.currentTimeMillis() - 3000L)
                .warmup(4000)
                .weight(4)
                .build()), 3);
        Assertions.assertEquals(hashLoadBalance.getWeight(Upstream.builder()
                .timestamp(System.currentTimeMillis() - 3L)
                .warmup(1)
                .weight(1)
                .build()), 1);
        Assertions.assertEquals(hashLoadBalance.getWeight(Upstream.builder()
                .warmup(1)
                .weight(0)
                .build()), 0);
        Assertions.assertEquals(hashLoadBalance.getWeight(Upstream.builder()
                .warmup(0)
                .weight(1)
                .build()), 1);

    }

    /**
     * Hash load balance test.
     */
    @Test
    public void hashLoadBalanceOrderedWeightTest() throws Exception {
        final String ip = "127.0.0.1";
        final HashLoadBalancer hashLoadBalance = new HashLoadBalancer();
        Assertions.assertNull(hashLoadBalance.select(null, ip));
        final Upstream upstream = hashLoadBalance.select(hashLoadBalancesOrdered, ip);
        final Long hashKey = Long.parseLong(hash.invoke(null, ip).toString());
        final SortedMap<Long, Upstream> lastRing = treeMapOrdered.tailMap(hashKey);
        final Upstream assertUp = lastRing.get(lastRing.firstKey());
        assertEquals(assertUp.getUrl(), upstream.getUrl());
    }

    @Test
    public void selectTest() {
        final String ip = "SHENYU-upstream-2-HASH-100";
        final HashLoadBalancer hashLoadBalance = new HashLoadBalancer();
        Assertions.assertNull(hashLoadBalance.select(null, ip));
        final Upstream upstream = hashLoadBalance.select(hashLoadBalancesOrdered, ip);
        assertEquals(treeMapOrdered.firstEntry().getValue().getUrl(), upstream.getUrl());
    }

    @Test
    public void hashErrorTest() throws NoSuchAlgorithmException {
        final String ip = "127.0.0.1";
        MockedStatic<MessageDigest> messageDigestMockedStatic = mockStatic(MessageDigest.class);
        messageDigestMockedStatic.when((MockedStatic.Verification) MessageDigest.getInstance("MD5")).thenThrow(NoSuchAlgorithmException.class);
        assertThrows(InvocationTargetException.class, () -> hash.invoke(null, ip));
        messageDigestMockedStatic.close();
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
