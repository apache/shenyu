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

package org.apache.shenyu.common.metrics;

import org.junit.jupiter.api.Test;

import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.LongStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class AiUsageTest {

    @Test
    void reportedUsageCarriesProviderSourceAndCumulativeSemantics() {
        final AiUsage usage = new AiUsage(1L, 2L, null, 1L, Map.of("reasoning_tokens", 1L), AiUsage.Status.REPORTED);
        assertEquals(AiUsage.Source.PROVIDER_REPORTED, usage.source());
        assertEquals(AiUsage.Aggregation.CUMULATIVE_SNAPSHOT, usage.aggregation());
    }

    @Test
    void unavailableUsageRemainsExplicitAndCannotClaimEstimateSemantics() {
        assertEquals(AiUsage.Status.UNKNOWN, AiUsage.UNKNOWN.status());
        assertThrows(IllegalArgumentException.class, () -> new AiUsage(null, null, null, null, Map.of(),
                AiUsage.Status.UNKNOWN, AiUsage.Source.GATEWAY_ESTIMATE, AiUsage.Aggregation.CUMULATIVE_SNAPSHOT));
        assertThrows(NullPointerException.class, () -> new AiUsage(1L, null, null, null, Map.of(),
                AiUsage.Status.REPORTED, null, AiUsage.Aggregation.CUMULATIVE_SNAPSHOT));
    }

    @Test
    void providerCountsAreBoundedValidAndDefensivelyCopied() {
        Map<String, Long> mutable = new java.util.HashMap<>(Map.of("reasoning_tokens", 1L));
        final AiUsage usage = new AiUsage(1L, 1L, 2L, null, mutable, AiUsage.Status.REPORTED);
        mutable.put("reasoning_tokens", 9L);
        assertEquals(1L, usage.providerCounts().get("reasoning_tokens"));
        assertThrows(UnsupportedOperationException.class, () -> usage.providerCounts().put("new_tokens", 1L));
        assertThrows(IllegalArgumentException.class, () -> new AiUsage(1L, 1L, 2L, null,
                Map.of("bad_tokens", -1L), AiUsage.Status.REPORTED));
        assertThrows(IllegalArgumentException.class, () -> new AiUsage(1L, 1L, 2L, null,
                LongStream.range(0, 33).boxed().collect(Collectors.toMap(index -> "tokens_" + index, index -> index)),
                AiUsage.Status.REPORTED));
        assertThrows(NullPointerException.class, () -> new AiUsage(1L, 1L, 2L, null,
                Map.of("bad", null), AiUsage.Status.REPORTED));
    }
}
