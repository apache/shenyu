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

import java.util.Objects;

/**
 * Bounded logical totals derived only from frozen attempt snapshots. Category sums
 * contain every known value, while report counts make partial coverage explicit.
 * Per-attempt events remain authoritative; this summary never invents a missing value.
 *
 * @param attemptCount started upstream attempts
 * @param reportedAttemptCount attempts carrying a provider-reported usage snapshot
 * @param unreportedAttemptCount unknown, invalid, unsupported or truncated attempts
 * @param promptTokens sum of known input counts
 * @param completionTokens sum of known output counts
 * @param totalTokens sum of provider-reported totals
 * @param cachedPromptTokens sum of known cached-input counts
 * @param promptTokenReports attempts reporting an input count
 * @param completionTokenReports attempts reporting an output count
 * @param totalTokenReports attempts reporting a total count
 * @param cachedPromptTokenReports attempts reporting a cached-input count
 * @param providerCountReports provider-specific category values retained
 * @param overflow true when a category sum exceeded a signed long
 */
public record AiUsageSummary(
        int attemptCount,
        int reportedAttemptCount,
        int unreportedAttemptCount,
        Long promptTokens,
        Long completionTokens,
        Long totalTokens,
        Long cachedPromptTokens,
        int promptTokenReports,
        int completionTokenReports,
        int totalTokenReports,
        int cachedPromptTokenReports,
        int providerCountReports,
        boolean overflow) {

    public static final AiUsageSummary EMPTY = new AiUsageSummary(0, 0, 0,
            null, null, null, null, 0, 0, 0, 0, 0, false);

    static final class Aggregator {

        private int attemptCount;

        private int reportedAttemptCount;

        private long promptTokens;

        private long completionTokens;

        private long totalTokens;

        private long cachedPromptTokens;

        private int promptTokenReports;

        private int completionTokenReports;

        private int totalTokenReports;

        private int cachedPromptTokenReports;

        private int providerCountReports;

        private boolean promptSeen;

        private boolean completionSeen;

        private boolean totalSeen;

        private boolean cachedSeen;

        private boolean promptOverflow;

        private boolean completionOverflow;

        private boolean totalOverflow;

        private boolean cachedOverflow;

        Aggregator(final AiUsageSummary summary) {
            attemptCount = summary.attemptCount();
            reportedAttemptCount = summary.reportedAttemptCount();
            promptTokens = value(summary.promptTokens());
            completionTokens = value(summary.completionTokens());
            totalTokens = value(summary.totalTokens());
            cachedPromptTokens = value(summary.cachedPromptTokens());
            promptTokenReports = summary.promptTokenReports();
            completionTokenReports = summary.completionTokenReports();
            totalTokenReports = summary.totalTokenReports();
            cachedPromptTokenReports = summary.cachedPromptTokenReports();
            providerCountReports = summary.providerCountReports();
            promptOverflow = summary.overflow() && Objects.isNull(summary.promptTokens());
            completionOverflow = summary.overflow() && Objects.isNull(summary.completionTokens());
            totalOverflow = summary.overflow() && Objects.isNull(summary.totalTokens());
            cachedOverflow = summary.overflow() && Objects.isNull(summary.cachedPromptTokens());
            promptSeen = summary.promptTokenReports() > 0;
            completionSeen = summary.completionTokenReports() > 0;
            totalSeen = summary.totalTokenReports() > 0;
            cachedSeen = summary.cachedPromptTokenReports() > 0;
        }

        Aggregator add(final AiUsage usage) {
            attemptCount++;
            if (usage.status() == AiUsage.Status.REPORTED) {
                reportedAttemptCount++;
                promptOverflow |= count(usage.promptTokens(), Category.PROMPT);
                completionOverflow |= count(usage.completionTokens(), Category.COMPLETION);
                totalOverflow |= count(usage.totalTokens(), Category.TOTAL);
                cachedOverflow |= count(usage.cachedPromptTokens(), Category.CACHED);
                providerCountReports += usage.providerCounts().size();
            }
            return this;
        }

        private boolean count(final Long amount, final Category category) {
            if (Objects.isNull(amount)) {
                return false;
            }
            try {
                switch (category) {
                    case PROMPT:
                        promptTokens = Math.addExact(promptTokens, amount);
                        promptTokenReports++;
                        promptSeen = true;
                        return promptOverflow;
                    case COMPLETION:
                        completionTokens = Math.addExact(completionTokens, amount);
                        completionTokenReports++;
                        completionSeen = true;
                        return completionOverflow;
                    case TOTAL:
                        totalTokens = Math.addExact(totalTokens, amount);
                        totalTokenReports++;
                        totalSeen = true;
                        return totalOverflow;
                    case CACHED:
                    default:
                        cachedPromptTokens = Math.addExact(cachedPromptTokens, amount);
                        cachedPromptTokenReports++;
                        cachedSeen = true;
                        return cachedOverflow;
                }
            } catch (ArithmeticException error) {
                return true;
            }
        }

        AiUsageSummary build() {
            return new AiUsageSummary(attemptCount, reportedAttemptCount, attemptCount - reportedAttemptCount,
                    result(promptTokens, promptSeen, promptOverflow),
                    result(completionTokens, completionSeen, completionOverflow),
                    result(totalTokens, totalSeen, totalOverflow),
                    result(cachedPromptTokens, cachedSeen, cachedOverflow),
                    promptTokenReports, completionTokenReports, totalTokenReports, cachedPromptTokenReports,
                    providerCountReports, promptOverflow || completionOverflow || totalOverflow || cachedOverflow);
        }

        private static long value(final Long amount) {
            return Objects.requireNonNullElse(amount, 0L);
        }

        private static Long result(final long amount, final boolean seen, final boolean overflow) {
            return overflow || !seen ? null : amount;
        }

        private enum Category {
            PROMPT, COMPLETION, TOTAL, CACHED
        }
    }
}
