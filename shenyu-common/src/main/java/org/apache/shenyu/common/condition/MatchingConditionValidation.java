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

package org.apache.shenyu.common.condition;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * Overall matching-condition structural validation result.
 *
 * @param diagnostics immutable bounded diagnostics
 */
public record MatchingConditionValidation(List<MatchingConditionDiagnostic> diagnostics) {

    private static final int MAX_DIAGNOSTICS = 16;

    /**
     * Creates a result with bounded diagnostics that retain the aggregate severity of all supplied diagnostics.
     */
    public MatchingConditionValidation {
        Objects.requireNonNull(diagnostics, "diagnostics");
        diagnostics = bounded(diagnostics);
    }

    /**
     * Returns the aggregate validation status.
     *
     * @return invalid when any diagnostic is invalid, otherwise unknown legacy when any identifier is unknown, otherwise valid
     */
    public Status status() {
        return deriveStatus(diagnostics);
    }

    /**
     * Returns whether known-invalid structure was found.
     *
     * @return true when validation status is invalid
     */
    public boolean isInvalid() {
        return Status.INVALID == status();
    }

    private static List<MatchingConditionDiagnostic> bounded(final List<MatchingConditionDiagnostic> diagnostics) {
        List<MatchingConditionDiagnostic> result = new ArrayList<>(
                diagnostics.subList(0, Math.min(diagnostics.size(), MAX_DIAGNOSTICS)));
        Status aggregateStatus = deriveStatus(diagnostics);
        if (result.size() == MAX_DIAGNOSTICS && aggregateStatus != deriveStatus(result)) {
            MatchingConditionDiagnostic representative = diagnostics.stream()
                    .filter(diagnostic -> MatchingConditionDiagnostic.Severity.INVALID == diagnostic.severity())
                    .findFirst().orElseThrow();
            result.set(MAX_DIAGNOSTICS - 1, representative);
        }
        return List.copyOf(result);
    }

    private static Status deriveStatus(final List<MatchingConditionDiagnostic> diagnostics) {
        if (diagnostics.stream().anyMatch(diagnostic -> MatchingConditionDiagnostic.Severity.INVALID == diagnostic.severity())) {
            return Status.INVALID;
        }
        if (diagnostics.stream().anyMatch(diagnostic -> MatchingConditionDiagnostic.Severity.UNKNOWN_LEGACY == diagnostic.severity())) {
            return Status.UNKNOWN_LEGACY;
        }
        return Status.VALID;
    }

    /**
     * Aggregate validation status.
     */
    public enum Status {

        /** No structural diagnostics. */
        VALID,

        /** At least one known-invalid diagnostic. */
        INVALID,

        /** No known-invalid diagnostics and at least one unknown identifier. */
        UNKNOWN_LEGACY
    }
}
