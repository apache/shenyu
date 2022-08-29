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

package org.apache.shenyu.loadbalancer.util;

import org.apache.shenyu.loadbalancer.entity.Upstream;

import java.util.List;

/**
 * calculate weight util.
 **/
public class WeightUtil {

    /**
     * Calculate total weight.
     *
     * @param upstreamList the upstreams
     * @return total weight
     */
    public static int calculateTotalWeight(final List<Upstream> upstreamList) {
        // total weight.
        int totalWeight = 0;
        for (Upstream upstream : upstreamList) {
            int weight = getWeight(upstream);
            // Cumulative total weight.
            totalWeight += weight;
        }
        return totalWeight;
    }

    /**
     * Get weight.
     *
     * @param upstream the upstream
     * @return weight for upstream
     */
    public static int getWeight(final Upstream upstream) {
        if (!upstream.isStatus()) {
            return 0;
        }
        return getWeight(upstream.getTimestamp(), upstream.getWarmup(), upstream.getWeight());
    }

    private static int getWeight(final long timestamp, final int warmup, final int weight) {
        if (weight > 0 && timestamp > 0) {
            int uptime = (int) (System.currentTimeMillis() - timestamp);
            if (uptime > 0 && uptime < warmup) {
                return calculateWarmupWeight(uptime, warmup, weight);
            }
        }
        return weight;
    }

    private static int calculateWarmupWeight(final int uptime, final int warmup, final int weight) {
        int ww = (int) ((float) uptime / ((float) warmup / (float) weight));
        return ww < 1 ? 1 : (Math.min(ww, weight));
    }
}
