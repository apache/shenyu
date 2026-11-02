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

import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

/**
 * least active algorithm impl.
 *
 * <p>An upstream is active while it is serving a request, which is recorded in {@link Upstream#getInflight()}:
 * the count is increased before a request is forwarded and released on success, error and cancellation. The
 * least active upstream wins; when several upstreams share the lowest count they are picked by weight, warmup
 * included, so a newly added upstream is not sent a catch-up burst while its weight is still ramping up.
 *
 * <p>The divide, websocket and apache dubbo gray routing paths maintain the count. Other callers see every
 * upstream at the same baseline and the choice falls back to weight: the grpc picker, the tcp proxy and the
 * java sdk rebuild {@link Upstream} objects on every call, so a count kept on them could not be shared across
 * requests.
 */
@Join
public class LeastActiveLoadBalance extends AbstractLoadBalancer {

    @Override
    protected Upstream doSelect(final List<Upstream> upstreamList, final LoadBalanceData data) {
        int length = upstreamList.size();
        long leastActive = Long.MAX_VALUE;
        int leastCount = 0;
        int[] leastIndexes = new int[length];
        int[] weights = new int[length];
        int totalWeight = 0;
        int firstWeight = 0;
        boolean sameWeight = true;

        for (int i = 0; i < length; i++) {
            Upstream upstream = upstreamList.get(i);
            long active = upstream.getInflight().get();
            int afterWarmup = getWeight(upstream);
            weights[i] = afterWarmup;
            if (active < leastActive) {
                leastActive = active;
                leastCount = 1;
                leastIndexes[0] = i;
                totalWeight = afterWarmup;
                firstWeight = afterWarmup;
                sameWeight = true;
            } else if (active == leastActive) {
                leastIndexes[leastCount++] = i;
                totalWeight += afterWarmup;
                if (sameWeight && i > 0 && afterWarmup != firstWeight) {
                    sameWeight = false;
                }
            }
        }

        if (leastCount == 1) {
            return upstreamList.get(leastIndexes[0]);
        }
        if (!sameWeight && totalWeight > 0) {
            int offsetWeight = ThreadLocalRandom.current().nextInt(totalWeight);
            for (int i = 0; i < leastCount; i++) {
                int leastIndex = leastIndexes[i];
                offsetWeight -= weights[leastIndex];
                if (offsetWeight < 0) {
                    return upstreamList.get(leastIndex);
                }
            }
        }
        return upstreamList.get(leastIndexes[ThreadLocalRandom.current().nextInt(leastCount)]);
    }
}
