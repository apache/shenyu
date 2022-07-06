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

package org.apache.shenyu.plugin.logging.common.sampler;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.math.NumberUtils;
import org.springframework.http.server.reactive.ServerHttpRequest;

import java.util.BitSet;
import java.util.Random;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * used for sample log.
 * reference resources： http://stackoverflow.com/questions/12817946/generate-a-random-bitset-with-n-1s
 */
public class CountSampler implements Sampler {

    private final AtomicInteger counter;

    private final BitSet sampleDecisions;

    /**
     * Fills a bitset with decisions according to the supplied probability.
     */
    public CountSampler(final float probability) {
        counter = new AtomicInteger();
        int percent = (int) (probability * 100.0f);
        this.sampleDecisions = genRandomBitSet(100, percent);
    }

    /**
     * loops over the pre-canned decisions, resetting to zero when it gets to the end.
     */
    @Override
    public boolean isSampled(final ServerHttpRequest request) {
        return sampleDecisions.get(mod(counter.getAndIncrement()));
    }

    /**
     * Returns a non-negative mod.
     */
    private int mod(final int dividend) {
        int result = dividend % 100;
        return result >= 0 ? result : 100 + result;
    }

    /**
     * gen random bitSet.
     * reference resources： http://stackoverflow.com/questions/12817946/generate-a-random-bitset-with-n-1s
     *
     * @param size        bitmap size
     * @param cardinality cardinality
     * @return bitSet
     */
    private BitSet genRandomBitSet(final int size, final int cardinality) {
        BitSet result = new BitSet(size);
        int[] chosen = new int[cardinality];
        int i;
        for (i = 0; i < cardinality; ++i) {
            chosen[i] = i;
            result.set(i);
        }
        Random random = new Random();
        for (; i < size; ++i) {
            int j = random.nextInt(i + 1);
            if (j < cardinality) {
                result.clear(chosen[j]);
                result.set(i);
                chosen[j] = i;
            }
        }
        return result;
    }

    /**
     * create a sampler instance.
     *
     * @param probability probability
     * @return sampler instance
     */
    public static Sampler create(final String probability) {
        if (StringUtils.isBlank(probability)) {
            return ALWAYS_SAMPLE;
        }
        if ("0".equals(probability)) {
            return NEVER_SAMPLE;
        }
        if ("1".equals(probability) || "1.0".equals(probability) || "1.0.0".equals(probability)) {
            return ALWAYS_SAMPLE;
        }
        float parseProbability = NumberUtils.toFloat(probability, 1);
        if (parseProbability < 0.01f || parseProbability > 1) {
            throw new IllegalArgumentException(
                    "probability should be between 0.01 and 1: was " + probability);
        }
        return new CountSampler(parseProbability);
    }

}
