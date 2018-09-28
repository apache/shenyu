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

import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.stream.Collectors;

/**
 * RoundRobin  LoadBalance Impl.
 * @author xiaoyu(Myth)
 */
public class RoundRobinLoadBalance implements LoadBalance {

    private final ConcurrentMap<String, AtomicPositiveInteger> sequences = new ConcurrentHashMap<>();

    private final ConcurrentMap<String, AtomicPositiveInteger> weightSequences = new ConcurrentHashMap<>();

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
        for (int i = 0; i < length; i++) {
            int weight = resultUpstreamList.get(i).getWeight();
            // 累计最大权重
            maxWeight = Math.max(maxWeight, weight);
            // 累计最小权重
            minWeight = Math.min(minWeight, weight);
        }
        // 权重不一样
        if (maxWeight > 0 && minWeight < maxWeight) {
            AtomicPositiveInteger weightSequence = weightSequences.get(key);
            if (weightSequence == null) {
                weightSequences.putIfAbsent(key, new AtomicPositiveInteger());
                weightSequence = weightSequences.get(key);
            }
            int currentWeight = weightSequence.getAndIncrement() % maxWeight;
            //筛选权重大于当前权重基数的Invoker

            final List<DivideUpstream> weightInvokers =
                    resultUpstreamList.stream()
                            .filter(divideHandle -> divideHandle.getWeight() > currentWeight)
                            .collect(Collectors.toList());
            int weightLength = weightInvokers.size();
            if (weightLength == 1) {
                return weightInvokers.get(0);
            } else if (weightLength > 1) {
                resultUpstreamList = weightInvokers;
                length = resultUpstreamList.size();
            }
        }
        AtomicPositiveInteger sequence = sequences.get(key);
        if (sequence == null) {
            sequences.putIfAbsent(key, new AtomicPositiveInteger());
            sequence = sequences.get(key);
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
}
