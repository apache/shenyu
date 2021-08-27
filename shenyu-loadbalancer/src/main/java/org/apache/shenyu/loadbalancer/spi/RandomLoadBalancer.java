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
import java.util.Random;

/**
 * random algorithm impl.
 */
@Join
public class RandomLoadBalancer extends AbstractLoadBalancer {

    private static final Random RANDOM = new Random();

    @Override
    public Upstream doSelect(final List<Upstream> upstreamList, final String ip) {
        int totalWeight = calculateTotalWeight(upstreamList);
        boolean sameWeight = isAllUpStreamSameWeight(upstreamList);
        if (totalWeight > 0 && !sameWeight) {
            return random(totalWeight, upstreamList);
        }
        // If the weights are the same or the weights are 0 then random
        return random(upstreamList);
    }

    private boolean isAllUpStreamSameWeight(final List<Upstream> upstreamList) {
        boolean sameWeight = true;
        int length = upstreamList.size();
        for (int i = 0; i < length; i++) {
            int weight = getWeight(upstreamList.get(i));
            if (i > 0 && weight != getWeight(upstreamList.get(i - 1))) {
                // Calculate whether the weight of ownership is the same
                sameWeight = false;
                break;
            }
        }
        return sameWeight;
    }

    private int calculateTotalWeight(final List<Upstream> upstreamList) {
        // total weight
        int totalWeight = 0;
        for (Upstream divideUpstream : upstreamList) {
            int weight = getWeight(divideUpstream);
            // Cumulative total weight
            totalWeight += weight;
        }
        return totalWeight;
    }

    private Upstream random(final int totalWeight, final List<Upstream> upstreamList) {
        // If the weights are not the same and the weights are greater than 0, then random by the total number of weights
        int offset = RANDOM.nextInt(totalWeight);
        // Determine which segment the random value falls on
        for (Upstream divideUpstream : upstreamList) {
            offset -= getWeight(divideUpstream);
            if (offset < 0) {
                return divideUpstream;
            }
        }
        return upstreamList.get(0);
    }

    private Upstream random(final List<Upstream> upstreamList) {
        return upstreamList.get(RANDOM.nextInt(upstreamList.size()));
    }
}
