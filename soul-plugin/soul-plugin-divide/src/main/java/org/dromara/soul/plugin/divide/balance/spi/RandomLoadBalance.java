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

package org.dromara.soul.plugin.divide.balance.spi;

import org.dromara.soul.common.dto.convert.DivideUpstream;
import org.dromara.soul.spi.Join;

import java.util.List;
import java.util.Random;

/**
 * random algorithm impl.
 *
 * @author xiaoyu(Myth)
 */
@Join
public class RandomLoadBalance extends AbstractLoadBalance {

    private static final Random RANDOM = new Random();

    @Override
    public DivideUpstream doSelect(final List<DivideUpstream> upstreamList, final String ip) {
        // total number
        int length = upstreamList.size();
        // total weight
        int totalWeight = 0;
        // Whether the weights are the same
        boolean sameWeight = true;
        for (int i = 0; i < length; i++) {
            int weight = upstreamList.get(i).getWeight();
            // Cumulative total weight
            totalWeight += weight;
            if (sameWeight && i > 0
                    && weight != upstreamList.get(i - 1).getWeight()) {
                // Calculate whether the weight of ownership is the same
                sameWeight = false;
            }
        }
        if (totalWeight > 0 && !sameWeight) {
            // If the weights are not the same and the weights are greater than 0, then random by the total number of weights
            int offset = RANDOM.nextInt(totalWeight);
            // Determine which segment the random value falls on
            for (DivideUpstream divideUpstream : upstreamList) {
                offset -= divideUpstream.getWeight();
                if (offset < 0) {
                    return divideUpstream;
                }
            }
        }
        // If the weights are the same or the weights are 0 then random
        return upstreamList.get(RANDOM.nextInt(length));
    }
}
