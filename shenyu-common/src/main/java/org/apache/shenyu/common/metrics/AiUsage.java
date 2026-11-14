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

import java.util.Map;
import java.util.Objects;

/**
 * Explicit upstream token counts, never estimates. Null means unknown, not zero.
 * Total is never calculated from the other fields. A snapshot does not establish
 * chargeability or successful delivery, especially on a cancelled stream.
 * @param promptTokens upstream-reported input count
 * @param completionTokens upstream-reported output count
 * @param totalTokens upstream-reported total
 * @param cachedPromptTokens upstream-reported cached-input count
 * @param providerCounts bounded provider-specific nonnegative counts
 * @param status bounded observation quality
 * @param source whether counts came from the provider or a gateway estimate
 * @param aggregation how multiple usage snapshots must be combined
 */
public record AiUsage(Long promptTokens, Long completionTokens, Long totalTokens, Long cachedPromptTokens,
                      Map<String, Long> providerCounts, Status status, Source source, Aggregation aggregation) {

    public static final AiUsage UNKNOWN = new AiUsage(null, null, null, null, Map.of(), Status.UNKNOWN, null, Aggregation.UNKNOWN);

    public AiUsage(final Long promptTokens, final Long completionTokens, final Long totalTokens, final Status status) {
        this(promptTokens, completionTokens, totalTokens, null, Map.of(), status,
                status == Status.REPORTED ? Source.PROVIDER_REPORTED : null,
                status == Status.REPORTED ? Aggregation.CUMULATIVE_SNAPSHOT : Aggregation.UNKNOWN);
    }

    public AiUsage(final Long promptTokens, final Long completionTokens, final Long totalTokens,
                   final Long cachedPromptTokens, final Map<String, Long> providerCounts, final Status status) {
        this(promptTokens, completionTokens, totalTokens, cachedPromptTokens, providerCounts, status,
                status == Status.REPORTED ? Source.PROVIDER_REPORTED : null,
                status == Status.REPORTED ? Aggregation.CUMULATIVE_SNAPSHOT : Aggregation.UNKNOWN);
    }

    public AiUsage {
        providerCounts = Map.copyOf(Objects.requireNonNull(providerCounts, "provider counts are required"));
        if (providerCounts.size() > 32) {
            throw new IllegalArgumentException("provider count categories are bounded to 32");
        }
        providerCounts.forEach(AiUsage::validateProviderCount);
        if (status == Status.REPORTED) {
            Objects.requireNonNull(source, "reported usage source is required");
            if (aggregation == Aggregation.UNKNOWN) {
                throw new IllegalArgumentException("reported usage aggregation is required");
            }
        } else if (Objects.nonNull(source) || aggregation != Aggregation.UNKNOWN) {
            throw new IllegalArgumentException("unavailable usage has no source or aggregation semantics");
        }
    }

    private static void validateProviderCount(final String name, final Long value) {
        if (Objects.isNull(name) || name.isBlank() || name.length() > 128 || !name.equals(name.strip())
                || name.chars().anyMatch(Character::isISOControl)) {
            throw new IllegalArgumentException("provider count name must be bounded nonblank text");
        }
        if (Objects.isNull(value)) {
            throw new IllegalArgumentException("provider count value is required");
        }
        if (value < 0) {
            throw new IllegalArgumentException("provider count value must not be negative");
        }
    }

    /**
     * Quality of the raw upstream observation, not a billing decision.
     */
    public enum Status {
        UNKNOWN, REPORTED, INVALID, LIMIT_EXCEEDED, UNSUPPORTED
    }

    /**
     * Origin of the counts. ShenYu currently emits only provider-reported values.
     */
    public enum Source {
        PROVIDER_REPORTED, GATEWAY_ESTIMATE
    }

    /**
     * Snapshot semantics. Incremental values are not claimed by a supported route.
     */
    public enum Aggregation {
        UNKNOWN, CUMULATIVE_SNAPSHOT, INCREMENTAL
    }
}
