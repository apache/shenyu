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

package org.apache.shenyu.agent.plugin.logging.common.sampler;

import org.springframework.http.server.reactive.ServerHttpRequest;

import java.util.BitSet;
import java.util.Random;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * used for sample log.
 */
public class CountingSampler implements Sampler {

    private final AtomicInteger counter;

    private final BitSet sampleDecisions;

    /**
     * Fills a bitset with decisions according to the supplied probability.
     */
    CountingSampler(final float probability) {
        this(probability, new Random());
    }

    /**
     * Fills a bitset with decisions according to the probability using the supplied {@link Random}.
     */
    CountingSampler(final float probability, final Random random) {
        counter = new AtomicInteger();
        int outOf100 = (int) (probability * 100.0f);
        this.sampleDecisions = randomBitSet(100, outOf100, random);
    }

    /**
     * loops over the pre-canned decisions, resetting to zero when it gets to the end.
     */
    @Override
    public boolean isSampled(final ServerHttpRequest request) {
        return sampleDecisions.get(mod(counter.getAndIncrement(), 100));
    }

    /**
     * Returns a non-negative mod.
     */
    private int mod(final int dividend, final int divisor) {
        int result = dividend % divisor;
        return result >= 0 ? result : divisor + result;
    }

    /**
     * Reservoir sampling algorithm borrowed from Stack Overflow.
     * http://stackoverflow.com/questions/12817946/generate-a-random-bitset-with-n-1s
     */
    /**
     * Reservoir sampling algorithm borrowed from Stack Overflow.
     * http://stackoverflow.com/questions/12817946/generate-a-random-bitset-with-n-1s
     *
     * @param size        bitmap size
     * @param cardinality cardinality
     * @param rnd         random
     * @return bitSet
     */
    private BitSet randomBitSet(final int size, final int cardinality, final Random rnd) {
        BitSet result = new BitSet(size);
        int[] chosen = new int[cardinality];
        int i;
        for (i = 0; i < cardinality; ++i) {
            chosen[i] = i;
            result.set(i);
        }
        for (; i < size; ++i) {
            int j = rnd.nextInt(i + 1);
            if (j < cardinality) {
                result.clear(chosen[j]);
                result.set(i);
                chosen[j] = i;
            }
        }
        return result;
    }

}
