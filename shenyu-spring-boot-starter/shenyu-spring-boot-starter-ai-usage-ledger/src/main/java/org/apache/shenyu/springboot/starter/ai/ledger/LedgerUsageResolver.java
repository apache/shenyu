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

package org.apache.shenyu.springboot.starter.ai.ledger;

import org.apache.shenyu.common.ledger.LedgerRecord;

import org.apache.shenyu.common.metrics.AiAttemptEvent;
import org.apache.shenyu.common.metrics.AiUsage;

import java.util.Objects;

/** Shared-normalized-usage port for #7955; never parses or subscribes to a response body. */
@FunctionalInterface
public interface LedgerUsageResolver {

    /**
     * Map a final attempt snapshot, not a terminal rollup.
     * @param event existing normalized observation
     * @return explicit quantity provenance
     */
    LedgerRecord.Usage resolve(AiAttemptEvent event);

    /**
     * Adapt the shared usage contract without treating estimates or deltas as actual totals.
     * @return default adapter
     */
    static LedgerUsageResolver currentContract() {
        return event -> {
            AiUsage usage = event.usage();
            if (Objects.isNull(usage) || usage.status() != AiUsage.Status.REPORTED) {
                return LedgerRecord.Usage.unavailable(Objects.isNull(usage) ? LedgerRecord.Quality.UNKNOWN
                        : switch (usage.status()) {
                        case INVALID, LIMIT_EXCEEDED -> LedgerRecord.Quality.INVALID;
                        case UNSUPPORTED -> LedgerRecord.Quality.UNSUPPORTED;
                        default -> LedgerRecord.Quality.UNKNOWN;
                    });
            }
            if (usage.aggregation() != AiUsage.Aggregation.CUMULATIVE_SNAPSHOT) {
                return LedgerRecord.Usage.unavailable(LedgerRecord.Quality.UNSUPPORTED);
            }
            try {
                boolean estimated = usage.source() == AiUsage.Source.GATEWAY_ESTIMATE;
                return new LedgerRecord.Usage(usage.promptTokens(), usage.completionTokens(), usage.totalTokens(), usage.cachedPromptTokens(),
                        usage.providerCounts(), estimated ? LedgerRecord.Quality.ESTIMATED : LedgerRecord.Quality.REPORTED,
                        estimated ? LedgerRecord.Origin.GATEWAY_ESTIMATE : LedgerRecord.Origin.PROVIDER_REPORTED);
            } catch (IllegalArgumentException error) {
                return LedgerRecord.Usage.unavailable(LedgerRecord.Quality.INVALID);
            }
        };
    }
}
