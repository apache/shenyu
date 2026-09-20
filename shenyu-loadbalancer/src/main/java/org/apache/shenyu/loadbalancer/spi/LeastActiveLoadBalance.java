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
import org.apache.shenyu.spi.Join;

import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Collectors;

/**
 * least active algorithm impl.
 */
@Join
public class LeastActiveLoadBalance extends AbstractLoadBalancer {

    private final int recyclePeriod = 60000;

    private final ConcurrentMap<String, ActiveCount> countMap = new ConcurrentHashMap<>(16);

    private final AtomicBoolean updateLock = new AtomicBoolean();

    private volatile long lastRecycle;

    @Override
    protected Upstream doSelect(final List<Upstream> upstreamList, final LoadBalanceData data) {
        long now = System.currentTimeMillis();
        Map<String, Upstream> domainMap = upstreamList.stream()
                .collect(Collectors.toConcurrentMap(Upstream::buildDomain, upstream -> upstream));

        domainMap.keySet().forEach(domain -> {
            ActiveCount activeCount = countMap.computeIfAbsent(domain, key -> new ActiveCount(now));
            activeCount.setLastUpdate(now);
        });

        final String domain = countMap.entrySet().stream()
                // Ensure that the filtered domain is included in the domainMap.
                .filter(entry -> domainMap.containsKey(entry.getKey()))
                .min(Comparator.comparingLong(entry -> entry.getValue().getCount()))
                .map(Map.Entry::getKey)
                .orElse(upstreamList.get(0).buildDomain());

        ActiveCount activeCount = countMap.get(domain);
        if (Objects.nonNull(activeCount)) {
            activeCount.increase();
        }

        // A removed domain's entry lingers for up to recyclePeriod, safely excluded from selection meanwhile.
        if (!updateLock.get() && now - lastRecycle > recyclePeriod && updateLock.compareAndSet(false, true)) {
            try {
                countMap.entrySet().removeIf(item -> now - item.getValue().getLastUpdate() > recyclePeriod);
                lastRecycle = now;
            } finally {
                updateLock.set(false);
            }
        }
        return domainMap.get(domain);
    }

    /**
     * The type Active count.
     */
    protected static class ActiveCount {

        private final AtomicLong count = new AtomicLong(Long.MIN_VALUE);

        private volatile long lastUpdate;

        ActiveCount(final long lastUpdate) {
            this.lastUpdate = lastUpdate;
        }

        /**
         * Increase count.
         */
        void increase() {
            count.addAndGet(1);
        }

        /**
         * Get count.
         *
         * @return the count
         */
        long getCount() {
            return count.get();
        }

        /**
         * Gets last update.
         *
         * @return the last update
         */
        long getLastUpdate() {
            return lastUpdate;
        }

        /**
         * Sets last update.
         *
         * @param lastUpdate the last update
         */
        void setLastUpdate(final long lastUpdate) {
            this.lastUpdate = lastUpdate;
        }
    }
}
