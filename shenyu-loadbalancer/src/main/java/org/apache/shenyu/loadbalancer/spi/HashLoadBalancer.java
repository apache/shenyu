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
import org.apache.shenyu.spi.Join;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.List;
import java.util.SortedMap;
import java.util.concurrent.ConcurrentSkipListMap;

/**
 * hash algorithm impl.
 */
@Join
public class HashLoadBalancer extends AbstractLoadBalancer {

    private static final int VIRTUAL_NODE_NUM = 5;

    @Override
    public Upstream doSelect(final List<Upstream> upstreamList, final String ip) {
        final ConcurrentSkipListMap<Long, Upstream> treeMap = new ConcurrentSkipListMap<>();
        for (Upstream upstream : upstreamList) {
            for (int i = 0; i < VIRTUAL_NODE_NUM; i++) {
                long addressHash = hash("SHENYU-" + upstream.getUrl() + "-HASH-" + i);
                treeMap.put(addressHash, upstream);
            }
        }
        long hash = hash(String.valueOf(ip));
        SortedMap<Long, Upstream> lastRing = treeMap.tailMap(hash);
        if (!lastRing.isEmpty()) {
            return lastRing.get(lastRing.firstKey());
        }
        return treeMap.firstEntry().getValue();
    }

    private static long hash(final String key) {
        // md5 byte
        MessageDigest md5;
        try {
            md5 = MessageDigest.getInstance("MD5");
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException("MD5 not supported", e);
        }
        md5.reset();
        byte[] keyBytes;
        keyBytes = key.getBytes(StandardCharsets.UTF_8);
        md5.update(keyBytes);
        byte[] digest = md5.digest();
        // hash code, Truncate to 32-bits
        long hashCode = (long) (digest[3] & 0xFF) << 24
                | ((long) (digest[2] & 0xFF) << 16)
                | ((long) (digest[1] & 0xFF) << 8)
                | (digest[0] & 0xFF);
        return hashCode & 0xffffffffL;
    }
}
