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
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.atomic.AtomicLong;

/**
 * shortestResponse algorithm impl.
 */
@Join
public class ShortestResponseLoadBalancer extends AbstractLoadBalancer {

    @Override
    protected Upstream doSelect(final List<Upstream> upstreamList, final String ip) {
        int length = upstreamList.size();
        long shortestResponse = Long.MAX_VALUE;
        int shortestCount = 0;
        int[] shortestIndexes = new int[length];
        int[] weights = new int[length];
        int totalWeight = 0;
        int firstWeight = 0;
        boolean sameWeight = true;

        for (int i = 0; i < upstreamList.size(); i++) {
            Upstream upstream = upstreamList.get(i);
            AtomicLong inflight = upstream.getInflight();
            // Calculate the estimated response time from the product of active connections and succeeded average elapsed time.
            long estimateResponse = upstream.getSucceededAverageElapsed() * inflight.get();
            int afterWarmup = getWeight(upstream);
            weights[i] = afterWarmup;
            if (estimateResponse < shortestResponse) {
                shortestResponse = estimateResponse;
                shortestCount = 1;
                shortestIndexes[0] = i;
                totalWeight = afterWarmup;
                firstWeight = afterWarmup;
                sameWeight = true;
            } else if (estimateResponse == shortestResponse) {
                shortestIndexes[shortestCount++] = i;
                totalWeight += afterWarmup;
                if (sameWeight && i > 0
                        && afterWarmup != firstWeight) {
                    sameWeight = false;
                }
            }
        }

        if (shortestCount == 1) {
            return upstreamList.get(shortestIndexes[0]);
        }
        if (!sameWeight && totalWeight > 0) {
            int offsetWeight = ThreadLocalRandom.current().nextInt(totalWeight);
            for (int i = 0; i < shortestCount; i++) {
                int shortestIndex = shortestIndexes[i];
                offsetWeight -= weights[shortestIndex];
                if (offsetWeight < 0) {
                    return upstreamList.get(shortestIndex);
                }
            }
        }
        return upstreamList.get(shortestIndexes[ThreadLocalRandom.current().nextInt(shortestCount)]);
    }
}
