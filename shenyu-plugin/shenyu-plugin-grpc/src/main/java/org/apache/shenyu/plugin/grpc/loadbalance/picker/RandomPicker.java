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

package org.apache.shenyu.plugin.grpc.loadbalance.picker;

import io.grpc.LoadBalancer;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.plugin.grpc.loadbalance.SubChannelCopy;

import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

/**
 * RandomPicker.
 */
public class RandomPicker extends AbstractReadyPicker {

    public RandomPicker(final List<LoadBalancer.Subchannel> list) {
        super(list);
    }

    @Override
    protected SubChannelCopy pick(final List<SubChannelCopy> list) {
        if (CollectionUtils.isEmpty(list)) {
            return null;
        }
        if (list.size() == 1) {
            return list.get(0);
        }
        int index = getRandomIndexByWeight(list);
        return list.get(index);
    }

    private int getRandomIndexByWeight(final List<SubChannelCopy> list) {
        final int sumWeight = list.stream().mapToInt(SubChannelCopy::getWeight).sum();
        if (sumWeight <= 0) {
            return ThreadLocalRandom.current().nextInt(list.size());
        }
        int randomInt = ThreadLocalRandom.current().nextInt(sumWeight);
        int sumI = 0;
        for (int i = 0; i < list.size(); i++) {
            sumI += list.get(i).getWeight();
            if (randomInt < sumI) {
                return i;
            }
        }
        return list.size() - 1;
    }
}
