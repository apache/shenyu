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

import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.loadbalancer.entity.LoadBalanceData;
import org.apache.shenyu.loadbalancer.entity.Upstream;

import java.util.List;

/**
 * The type Abstract load balancer.
 */
public abstract class AbstractLoadBalancer implements LoadBalancer {

    /**
     * Do select upstream.
     *
     * @param upstreamList the upstream list
     * @param data  the data
     * @return the upstream
     */
    protected abstract Upstream doSelect(List<Upstream> upstreamList, LoadBalanceData data);

    @Override
    public Upstream select(final List<Upstream> upstreamList, final LoadBalanceData data) {
        if (CollectionUtils.isEmpty(upstreamList)) {
            return null;
        }
        if (upstreamList.size() == 1) {
            return upstreamList.get(0);
        }
        return doSelect(upstreamList, data);
    }

    protected int getWeight(final Upstream upstream) {
        return getWeight(upstream, System.currentTimeMillis());
    }

    /**
     * Warm-up adjusted weight sampled at a caller-provided instant, so one selection
     * can compare every candidate on the same weight scale even when the millisecond
     * clock ticks mid-comparison.
     *
     * @param upstream the candidate
     * @param nowMillis the selection time
     * @return the effective weight
     */
    protected int getWeight(final Upstream upstream, final long nowMillis) {
        if (!upstream.isStatus()) {
            return 0;
        }
        return getWeight(upstream.getTimestamp(), upstream.getWarmup(), upstream.getWeight(), nowMillis);
    }

    private int getWeight(final long timestamp, final int warmup, final int weight, final long nowMillis) {
        if (weight > 0 && timestamp > 0) {
            long uptime = nowMillis - timestamp;
            if (uptime > 0 && uptime < warmup) {
                return calculateWarmupWeight((int) uptime, warmup, weight);
            }
        }
        return weight;
    }

    private int calculateWarmupWeight(final int uptime, final int warmup, final int weight) {
        int ww = (int) ((float) uptime / ((float) warmup / (float) weight));
        return ww < 1 ? 1 : (Math.min(ww, weight));
    }
}
