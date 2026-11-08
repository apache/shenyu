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

package org.apache.shenyu.plugin.ai.proxy.enhanced.evaluation;

import org.apache.commons.lang3.StringUtils;

import java.util.BitSet;
import java.util.Random;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Deterministic probability gate of the evaluation sampler: a pre-generated
 * 100-slot decision bitset walked by an atomic counter, so a rate of {@code 0.01}
 * samples exactly one decision in every hundred (mechanism of the logging
 * family's count sampler, scoped to the evaluation pipeline).
 *
 * <p>A blank or absent rate samples everything; a rate outside {@code [0, 1]} is
 * rejected at construction instead of silently clamped.</p>
 */
public final class AiEvaluationSampler {

    private final AtomicInteger counter = new AtomicInteger();

    private final BitSet sampleDecisions;

    public AiEvaluationSampler(final String rate) {
        float probability = parse(rate);
        int percent = Math.round(probability * 100.0f);
        this.sampleDecisions = genRandomBitSet(100, percent);
    }

    /**
     * Whether this decision is sampled.
     *
     * @return the pre-canned decision for the next counter value
     */
    public boolean isSampled() {
        return sampleDecisions.get(mod(counter.getAndIncrement()));
    }

    private static float parse(final String rate) {
        if (StringUtils.isBlank(rate) || "1".equals(rate.trim()) || "1.0".equals(rate.trim())) {
            return 1.0f;
        }
        float probability;
        try {
            probability = Float.parseFloat(rate.trim());
        } catch (NumberFormatException error) {
            throw new IllegalArgumentException("evaluation sampleRate is not a number: " + rate, error);
        }
        if (probability < 0f || probability > 1f) {
            throw new IllegalArgumentException("evaluation sampleRate must be within [0, 1]: " + rate);
        }
        return probability;
    }

    private int mod(final int dividend) {
        int result = dividend % 100;
        return result >= 0 ? result : 100 + result;
    }

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
}
