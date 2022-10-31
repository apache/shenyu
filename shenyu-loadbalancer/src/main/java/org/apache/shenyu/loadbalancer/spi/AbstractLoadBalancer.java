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

import java.util.List;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.loadbalancer.entity.Upstream;

/**
 * The type Abstract load balancer.
 */
public abstract class AbstractLoadBalancer implements LoadBalancer {

    /**
     * Do select upstream.
     *
     * @param upstreamList the upstream list
     * @param ip           the ip
     * @return the upstream
     */
    protected abstract Upstream doSelect(List<Upstream> upstreamList, String ip);

    @Override
    public Upstream select(final List<Upstream> upstreamList, final String ip) {
        if (CollectionUtils.isEmpty(upstreamList)) {
            return null;
        }
        if (upstreamList.size() == 1) {
            return upstreamList.get(0);
        }
        return doSelect(upstreamList, ip);
    }

    protected int getWeight(final Upstream upstream) {
        if (!upstream.isStatus()) {
            return 0;
        }
        return getWeight(upstream.getTimestamp(), upstream.getWarmup(), upstream.getWeight());
    }

    private int getWeight(final long timestamp, final int warmup, final int weight) {
        if (weight > 0 && timestamp > 0) {
            int uptime = (int) (System.currentTimeMillis() - timestamp);
            if (uptime > 0 && uptime < warmup) {
                return calculateWarmupWeight(uptime, warmup, weight);
            }
        }
        return weight;
    }

    private int calculateWarmupWeight(final int uptime, final int warmup, final int weight) {
        int ww = (int) ((float) uptime / ((float) warmup / (float) weight));
        return ww < 1 ? 1 : (Math.min(ww, weight));
    }
}
