/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.web.balance.spi;

import com.alibaba.dubbo.common.utils.AtomicPositiveInteger;
import org.dromara.soul.common.dto.convert.DivideUpstream;
import org.dromara.soul.common.enums.LoadBalanceEnum;
import org.dromara.soul.web.balance.LoadBalance;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

/**
 * RoundRobin  LoadBalance Impl.
 *
 * @author wanglaomo
 */
public class RoundRobinLoadBalance implements LoadBalance {

    private final ConcurrentMap<String, AtomicPositiveInteger> sequences = new ConcurrentHashMap<>();

    @Override
    public DivideUpstream select(final List<DivideUpstream> upstreamList, final String ip) {
        List<DivideUpstream> resultUpstreamList = upstreamList;
        String key = resultUpstreamList.get(0).getUpstreamUrl();
        // 总个数
        int length = resultUpstreamList.size();
        // 最大权重
        int maxWeight = 0;
        // 最小权重
        int minWeight = Integer.MAX_VALUE;
        // 权重总值
        int weightSum = 0;
        final LinkedHashMap<DivideUpstream, IntegerWrapper> weightMap = new LinkedHashMap<>();
        for (int i = 0; i < length; i++) {
            int weight = resultUpstreamList.get(i).getWeight();
            // 累计最大权重
            maxWeight = Math.max(maxWeight, weight);
            // 累计最小权重
            minWeight = Math.min(minWeight, weight);
            if(weight > 0) {
                weightMap.put(resultUpstreamList.get(i), new IntegerWrapper(weight));
                weightSum += weight;
            }
        }

        AtomicPositiveInteger sequence = sequences.get(key);
        if (sequence == null) {
            sequences.putIfAbsent(key, new AtomicPositiveInteger());
            sequence = sequences.get(key);
        }
        int currentSequence = sequence.getAndIncrement();
        // 权重不一样
        if (maxWeight > 0 && minWeight < maxWeight) {
            int mod = currentSequence % weightSum;
            for (int i = 0; i < maxWeight; i++) {
                for(Map.Entry<DivideUpstream, IntegerWrapper> entry : weightMap.entrySet()) {
                    final DivideUpstream divideUpstream = entry.getKey();
                    IntegerWrapper weight = entry.getValue();
                    if(mod == 0 && weight.getValue() > 0) {
                        return divideUpstream;
                    }
                    if(weight.getValue() > 0) {
                        weight.decrement();
                        mod--;
                    }
                }
            }
        }

        // 取模轮循
        return resultUpstreamList.get(sequence.getAndIncrement() % length);
    }

    /**
     *  get algorithm name.
     *
     * @return algorithm name
     */
    @Override
    public String algorithm() {
        return LoadBalanceEnum.ROUND_ROBIN.getName();
    }

    private static final class IntegerWrapper {
        private int value;

        public IntegerWrapper(int value) {
            this.value = value;
        }

        public int getValue() {
            return value;
        }

        public void setValue(int value) {
            this.value = value;
        }

        public void decrement() {
            this.value--;
        }
    }
}
