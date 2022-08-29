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

import java.security.SecureRandom;
import java.util.List;

/**
 * random algorithm impl.
 */
@Join
public class RandomLoadBalancer extends AbstractLoadBalancer {

    private static final SecureRandom RANDOM = new SecureRandom();

    @Override
    public Upstream doSelect(final List<Upstream> upstreamList, final String ip) {
        int length = upstreamList.size();
        // every upstream has the same weight?
        boolean sameWeight = true;
        // the weight of every upstream
        int[] weights = new int[length];
        int firstUpstreamWeight = getWeight(upstreamList.get(0));
        weights[0] = firstUpstreamWeight;
        // init the totalWeight
        int totalWeight = firstUpstreamWeight;
        for (int i = 1; i < length; i++) {
            int currentUpstreamWeight = getWeight(upstreamList.get(i));
            weights[i] = currentUpstreamWeight;
            totalWeight += currentUpstreamWeight;
            if (sameWeight && currentUpstreamWeight != firstUpstreamWeight) {
                // Calculate whether the weight of ownership is the same.
                sameWeight = false;
            }
        }
        if (totalWeight > 0 && !sameWeight) {
            return random(totalWeight, weights, upstreamList);
        }
        return random(upstreamList);
    }

    private Upstream random(final int totalWeight, final int[] weights, final List<Upstream> upstreamList) {
        // If the weights are not the same and the weights are greater than 0, then random by the total number of weights.
        int offset = RANDOM.nextInt(totalWeight);
        // Determine which segment the random value falls on
        for (int i = 0; i < weights.length; i++) {
            offset -= weights[i];
            if (offset < 0) {
                return upstreamList.get(i);
            }
        }
        return random(upstreamList);
    }

    private Upstream random(final List<Upstream> upstreamList) {
        return upstreamList.get(RANDOM.nextInt(upstreamList.size()));
    }
}
