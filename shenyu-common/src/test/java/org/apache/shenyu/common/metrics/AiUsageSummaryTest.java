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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

class AiUsageSummaryTest {

    @Test
    void preservesPartialCoverageAndReportedZero() {
        AiUsageSummary totals = AiUsageSummary.EMPTY;
        totals = add(totals, new AiUsage(0L, null, null, null, Map.of(), AiUsage.Status.REPORTED));
        totals = add(totals, AiUsage.UNKNOWN);
        assertEquals(2, totals.attemptCount());
        assertEquals(1, totals.reportedAttemptCount());
        assertEquals(1, totals.unreportedAttemptCount());
        assertEquals(Long.valueOf(0L), totals.promptTokens());
        assertEquals(1, totals.promptTokenReports());
        assertNull(totals.completionTokens());
        assertEquals(0, totals.completionTokenReports());
    }

    @Test
    void marksLongOverflowExplicitlyWithoutFabricatingATotal() {
        AiUsageSummary overflow = new AiUsageSummary(1, 1, 0, Long.MAX_VALUE, 0L, null, null,
                1, 1, 0, 0, 0, false);
        AiUsageSummary result = add(overflow, new AiUsage(1L, 0L, null, null, Map.of(), AiUsage.Status.REPORTED));
        assertNull(result.promptTokens());
        assertEquals(Long.valueOf(0L), result.completionTokens());
        assertEquals(1, result.promptTokenReports());
        assertEquals(2, result.completionTokenReports());
        assertEquals(true, result.overflow());
    }

    private AiUsageSummary add(final AiUsageSummary totals, final AiUsage usage) {
        return new AiUsageSummary.Aggregator(totals).add(usage).build();
    }
}
