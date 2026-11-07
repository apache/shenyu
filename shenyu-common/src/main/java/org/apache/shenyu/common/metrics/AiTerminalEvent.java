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

import org.apache.shenyu.common.dto.AgentRequestContext;

/**
 * One logical terminal event after recovery and response writing. Usage belongs
 * only to the last started attempt, while usageTotals exposes bounded sums over
 * every frozen provider-reported attempt snapshot.
 * @param context final actual target and trusted request facts
 * @param outcome terminal classification
 * @param httpStatus assigned downstream status, not proof of delivery
 * @param elapsedMillis monotonic logical duration
 * @param attemptCount actual upstream attempts, zero for early rejections
 * @param usage last attempt's explicit snapshot, or unknown
 * @param usageTotals explicit cross-attempt totals and coverage counts
 */
public record AiTerminalEvent(AgentRequestContext context, AiCallOutcome outcome, Integer httpStatus,
                              long elapsedMillis, int attemptCount, AiUsage usage, AiUsageSummary usageTotals) {

    /**
     * Preserve the original terminal constructor, keeping unavailable per-attempt totals unknown.
     * @param context trusted request
     * @param outcome terminal classification
     * @param httpStatus assigned status
     * @param elapsedMillis elapsed time
     * @param attemptCount known attempt count
     * @param usage last attempt snapshot
     */
    public AiTerminalEvent(final AgentRequestContext context, final AiCallOutcome outcome, final Integer httpStatus,
                           final long elapsedMillis, final int attemptCount, final AiUsage usage) {
        this(context, outcome, httpStatus, elapsedMillis, attemptCount, usage,
                new AiUsageSummary(attemptCount, 0, attemptCount, null, null, null, null, 0, 0, 0, 0, 0, false));
    }
}
