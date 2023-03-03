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
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ConcurrentSkipListMap;
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

    /**
     * selector for storing ip and service providers.
     */
    private final ConcurrentMap<String, ConsistentHashSelector> selectors = new ConcurrentHashMap<>();

    /**
     * consistent hash with virtual node to select upstream.
     *
     * @param upstreamList the upstream list
     * @param ip           the ip
     * @return selected upstream
     */
    @Override
    public Upstream doSelect(final List<Upstream> upstreamList, final String ip) {
        int invokersHashCode = upstreamList.hashCode();
        ConsistentHashSelector selector = selectors.get(ip);
        if (selector == null || selector.identityHashCode != invokersHashCode) {
            selectors.put(ip, new ConsistentHashSelector(upstreamList, invokersHashCode));
            selector = selectors.get(ip);
        }
        return selector.select(ip);
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

    private static final class ConsistentHashSelector {

        private final ConcurrentSkipListMap<Long, Upstream> virtualInvokers;

        private final int identityHashCode;

        ConsistentHashSelector(final List<Upstream> upstreamList, final int identityHashCode) {
            this.virtualInvokers = new ConcurrentSkipListMap<>();
            this.identityHashCode = identityHashCode;
            upstreamList.forEach(upstream -> IntStream.range(0, VIRTUAL_NODE_NUM).forEach(i -> {
                long addressHash = hash("SHENYU-" + upstream.getUrl() + "-HASH-" + i);
                virtualInvokers.put(addressHash, upstream);
            }));
        }

        public Upstream select(final String ip) {
            Map.Entry<Long, Upstream> entry = virtualInvokers.ceilingEntry(hash(ip));
            if (entry == null) {
                entry = virtualInvokers.firstEntry();
            }
            return entry.getValue();
        }
    }
}
