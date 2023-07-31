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

import java.security.SecureRandom;
import java.util.Arrays;
import java.util.List;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.spi.Join;

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
            totalWeight += currentUpstreamWeight;
            weights[i] = totalWeight;
            if (sameWeight && currentUpstreamWeight != firstUpstreamWeight) {
                // Calculate whether the weight of ownership is the same.
                sameWeight = false;
            }
        }
        if (totalWeight > 0 && !sameWeight) {
            return random(totalWeight, weights, upstreamList, length);
        }
        return random(upstreamList);
    }

    private Upstream random(final int totalWeight, final int[] weights, final List<Upstream> upstreamList, final int length) {
        // If the weights are not the same and the weights are greater than 0, then random by the total number of weights.
        int offset = RANDOM.nextInt(totalWeight);
        if (length <= 4) {
            for (int i = 0; i < length; i++) {
                if (offset < weights[i]) {
                    return upstreamList.get(i);
                }
            }
        } else {
            int i = Arrays.binarySearch(weights, offset);
            if (i < 0) {
                i = -i - 1;
            } else {
                while (weights[i + 1] == offset) {
                    i++;
                }
                i++;
            }
            return upstreamList.get(i);
        }
        return random(upstreamList);
    }

    private Upstream random(final List<Upstream> upstreamList) {
        return upstreamList.get(RANDOM.nextInt(upstreamList.size()));
    }
}
