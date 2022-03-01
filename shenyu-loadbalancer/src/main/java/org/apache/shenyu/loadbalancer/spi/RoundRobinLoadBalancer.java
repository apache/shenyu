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

import java.util.List;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Round-robin load balance impl.
 */
@Join
public class RoundRobinLoadBalancer extends AbstractLoadBalancer {

    private final int recyclePeriod = 60000;

    private final ConcurrentMap<String, ConcurrentMap<String, WeightedRoundRobin>> methodWeightMap = new ConcurrentHashMap<>(16);

    private final AtomicBoolean updateLock = new AtomicBoolean();

    @Override
    public Upstream doSelect(final List<Upstream> upstreamList, final String ip) {
        String key = upstreamList.get(0).getUrl();
        ConcurrentMap<String, WeightedRoundRobin> map = methodWeightMap.get(key);
        if (Objects.isNull(map)) {
            methodWeightMap.putIfAbsent(key, new ConcurrentHashMap<>(16));
            map = methodWeightMap.get(key);
        }
        int totalWeight = 0;
        long maxCurrent = Long.MIN_VALUE;
        long now = System.currentTimeMillis();
        Upstream selectedInvoker = null;
        WeightedRoundRobin selectedWeightedRoundRobin = null;
        for (Upstream upstream : upstreamList) {
            String rKey = upstream.getUrl();
            WeightedRoundRobin weightedRoundRobin = map.get(rKey);
            int weight = getWeight(upstream);
            if (Objects.isNull(weightedRoundRobin)) {
                weightedRoundRobin = new WeightedRoundRobin();
                weightedRoundRobin.setWeight(weight);
                map.putIfAbsent(rKey, weightedRoundRobin);
            }
            if (weight != weightedRoundRobin.getWeight()) {
                // weight changed.
                weightedRoundRobin.setWeight(weight);
            }
            long cur = weightedRoundRobin.increaseCurrent();
            weightedRoundRobin.setLastUpdate(now);
            if (cur > maxCurrent) {
                maxCurrent = cur;
                selectedInvoker = upstream;
                selectedWeightedRoundRobin = weightedRoundRobin;
            }
            totalWeight += weight;
        }
        if (!updateLock.get() && upstreamList.size() != map.size() && updateLock.compareAndSet(false, true)) {
            try {
                // copy -> modify -> update reference.
                ConcurrentMap<String, WeightedRoundRobin> newMap = new ConcurrentHashMap<>(map);
                newMap.entrySet().removeIf(item -> now - item.getValue().getLastUpdate() > recyclePeriod);
                methodWeightMap.put(key, newMap);
            } finally {
                updateLock.set(false);
            }
        }
        if (Objects.nonNull(selectedInvoker)) {
            selectedWeightedRoundRobin.sel(totalWeight);
            return selectedInvoker;
        }
        // should not happen here.
        return upstreamList.get(0);
    }

    /**
     * The type Weighted round-robin.
     */
    protected static class WeightedRoundRobin {

        private int weight;

        private final AtomicLong current = new AtomicLong(0);

        private long lastUpdate;

        /**
         * Gets weight.
         *
         * @return the weight
         */
        int getWeight() {
            return weight;
        }

        /**
         * Sets weight.
         *
         * @param weight the weight
         */
        void setWeight(final int weight) {
            this.weight = weight;
            current.set(0);
        }

        /**
         * Increase current long.
         *
         * @return the long
         */
        long increaseCurrent() {
            return current.addAndGet(weight);
        }

        /**
         * Sel.
         *
         * @param total the total
         */
        void sel(final int total) {
            current.addAndGet(-1 * total);
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
