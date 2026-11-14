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
import org.apache.shenyu.common.utils.MapUtils;
import org.apache.shenyu.loadbalancer.entity.LoadBalanceData;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.loadbalancer.entity.UpstreamIdentity;
import org.apache.shenyu.spi.Join;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Round-robin load balance impl.
 */
@Join
public class RoundRobinLoadBalancer extends AbstractLoadBalancer {

    /**
     * State is keyed by the full ordered upstream identity set together with the
     * configured weights (#8441): selectors whose upstream lists share an address
     * never share smooth weighted round-robin counters, and the bounded cache
     * evicts pools orphaned by reorders or weight changes.
     */
    private final Map<List<String>, ConcurrentMap<UpstreamIdentity, WeightedRoundRobin>> methodWeightMap =
            new WindowTinyLFUMap<>(Constants.CACHE_MAX_COUNT);

    @Override
    public Upstream doSelect(final List<Upstream> upstreamList, final LoadBalanceData data) {
        List<String> stateKey = new ArrayList<>(upstreamList.size());
        for (Upstream upstream : upstreamList) {
            // the configured weight, not the warm-up adjusted one, so warm-up
            // ramping does not rotate pools and reset the counters each call
            stateKey.add(upstream.identity().markerKey() + ':' + upstream.getWeight());
        }
        ConcurrentMap<UpstreamIdentity, WeightedRoundRobin> map =
                methodWeightMap.computeIfAbsent(List.copyOf(stateKey), k -> new ConcurrentHashMap<>(16));
        long totalWeight = 0;
        long maxCurrent = Long.MIN_VALUE;
        Upstream selectedInvoker = null;
        WeightedRoundRobin selectedWeightedRoundRobin = null;
        for (Upstream upstream : upstreamList) {
            int weight = getWeight(upstream);
            WeightedRoundRobin weightedRoundRobin = MapUtils.computeIfAbsent(map, upstream.identity(), k -> {
                WeightedRoundRobin roundRobin = new WeightedRoundRobin();
                roundRobin.setWeight(weight);
                return roundRobin;
            });
            if (weight != weightedRoundRobin.getWeight()) {
                // weight changed (warm-up ramping or configuration update).
                weightedRoundRobin.setWeight(weight);
            }
            long cur = weightedRoundRobin.increaseCurrent();
            if (cur > maxCurrent) {
                maxCurrent = cur;
                selectedInvoker = upstream;
                selectedWeightedRoundRobin = weightedRoundRobin;
            }
            totalWeight += weight;
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
        void sel(final long total) {
            current.addAndGet(-total);
        }
    }
}
