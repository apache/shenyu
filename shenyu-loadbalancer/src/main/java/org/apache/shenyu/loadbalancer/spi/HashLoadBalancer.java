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

import org.apache.shenyu.common.cache.WindowTinyLFUMap;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.loadbalancer.entity.LoadBalanceData;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.spi.Join;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.stream.IntStream;

/**
 * hash algorithm impl.
 */
@Join
public class HashLoadBalancer extends AbstractLoadBalancer {

    /**
     * virtual node used to solve unbalanced load.
     */
    private static final int VIRTUAL_NODE_NUM = 5;

    private static final ThreadLocal<MessageDigest> MD5 = ThreadLocal.withInitial(HashLoadBalancer::newMessageDigest);

    private final Map<List<String>, SortedMap<Long, Integer>> ringCache = new WindowTinyLFUMap<>(Constants.CACHE_MAX_COUNT);
    
    @Override
    public Upstream doSelect(final List<Upstream> upstreamList, final LoadBalanceData data) {
        final List<String> ringKey = new ArrayList<>(upstreamList.size());
        upstreamList.forEach(upstream -> ringKey.add(upstream.getUrl()));
        final SortedMap<Long, Integer> treeMap = ringCache.computeIfAbsent(List.copyOf(ringKey), this::buildRing);
        long hash = hash(data.getIp());
        SortedMap<Long, Integer> lastRing = treeMap.tailMap(hash);
        if (!lastRing.isEmpty()) {
            return upstreamList.get(lastRing.get(lastRing.firstKey()));
        }
        return upstreamList.get(treeMap.get(treeMap.firstKey()));
    }

    private SortedMap<Long, Integer> buildRing(final List<String> upstreamUrls) {
        final SortedMap<Long, Integer> treeMap = new TreeMap<>();
        IntStream.range(0, upstreamUrls.size()).forEach(index ->
                IntStream.range(0, VIRTUAL_NODE_NUM).forEach(virtualNode -> {
                    long addressHash = hash("SHENYU-" + upstreamUrls.get(index) + "-HASH-" + virtualNode);
                    treeMap.put(addressHash, index);
                }));
        return treeMap;
    }

    private static long hash(final String key) {
        MessageDigest md5 = MD5.get();
        md5.reset();
        byte[] keyBytes = key.getBytes(StandardCharsets.UTF_8);
        md5.update(keyBytes);
        byte[] digest = md5.digest();
        // hash code, Truncate to 32-bits
        long hashCode = (long) (digest[3] & 0xFF) << 24
                | ((long) (digest[2] & 0xFF) << 16)
                | ((long) (digest[1] & 0xFF) << 8)
                | (digest[0] & 0xFF);
        return hashCode & 0xffffffffL;
    }

    private static MessageDigest newMessageDigest() {
        try {
            return MessageDigest.getInstance("MD5");
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException("MD5 not supported", e);
        }
    }
}
