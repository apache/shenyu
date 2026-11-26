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

import com.google.re2j.Pattern;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.utils.DateUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * Pure structural validator for matching-condition candidates.
 */
public final class MatchingConditionValidator {

    private MatchingConditionValidator() {
    }

    /**
     * Validates matching conditions without changing or normalizing them.
     *
     * @param candidates condition candidates in stored order
     * @return bounded aggregate structural validation
     */
    public static MatchingConditionValidation validate(final List<MatchingConditionCandidate> candidates) {
        Objects.requireNonNull(candidates, "candidates");
        List<MatchingConditionDiagnostic> diagnostics = new ArrayList<>();
        for (int index = 0; index < candidates.size(); index++) {
            validateCandidate(Objects.requireNonNull(candidates.get(index), "candidate"), index, diagnostics);
        }
        return new MatchingConditionValidation(diagnostics);
    }

    private static void validateCandidate(final MatchingConditionCandidate candidate, final int index,
                                          final List<MatchingConditionDiagnostic> diagnostics) {
        String root = "conditions[" + index + "].";
        if (StringUtils.isBlank(candidate.source())) {
            add(diagnostics, root + "paramType", MatchingConditionDiagnostic.Code.SOURCE_REQUIRED,
                    MatchingConditionDiagnostic.Severity.INVALID);
            return;
        }
        MatchingConditionSource source = MatchingConditionCatalog.source(candidate.source()).orElse(null);
        if (Objects.isNull(source)) {
            add(diagnostics, root + "paramType", MatchingConditionDiagnostic.Code.UNKNOWN_SOURCE,
                    MatchingConditionDiagnostic.Severity.UNKNOWN_LEGACY);
            return;
        }
        validateKnownSource(candidate, source, root, diagnostics);
    }

    private static void validateKnownSource(final MatchingConditionCandidate candidate, final MatchingConditionSource source,
                                            final String root, final List<MatchingConditionDiagnostic> diagnostics) {
        if (StringUtils.isBlank(candidate.operator())) {
            add(diagnostics, root + "operator", MatchingConditionDiagnostic.Code.OPERATOR_REQUIRED,
                    MatchingConditionDiagnostic.Severity.INVALID);
            return;
        }
        MatchingConditionOperator operator = MatchingConditionCatalog.operator(candidate.operator()).orElse(null);
        if (Objects.isNull(operator)) {
            add(diagnostics, root + "operator", MatchingConditionDiagnostic.Code.UNKNOWN_OPERATOR,
                    MatchingConditionDiagnostic.Severity.UNKNOWN_LEGACY);
            return;
        }
        validateKnownOperator(candidate, source, operator, root, diagnostics);
    }

    private static void validateKnownOperator(final MatchingConditionCandidate candidate, final MatchingConditionSource source,
                                              final MatchingConditionOperator operator, final String root,
                                              final List<MatchingConditionDiagnostic> diagnostics) {
        if (source.paramNameRequired() && operator.requiresValue() && StringUtils.isBlank(candidate.paramName())) {
            add(diagnostics, root + "paramName", MatchingConditionDiagnostic.Code.NAME_REQUIRED,
                    MatchingConditionDiagnostic.Severity.INVALID);
        }
        if (operator.requiresValue() && StringUtils.isBlank(candidate.paramValue())) {
            add(diagnostics, root + "paramValue", MatchingConditionDiagnostic.Code.VALUE_REQUIRED,
                    MatchingConditionDiagnostic.Severity.INVALID);
            return;
        }
        if (StringUtils.isBlank(candidate.paramValue())) {
            return;
        }
        validateFormattedValue(candidate, operator, root, diagnostics);
    }

    private static void validateFormattedValue(final MatchingConditionCandidate candidate, final MatchingConditionOperator operator,
                                               final String root,
                                               final List<MatchingConditionDiagnostic> diagnostics) {
        if (MatchingConditionValueFormat.REGEX == operator.valueFormat()) {
            try {
                Pattern.compile(candidate.paramValue().trim());
            } catch (RuntimeException ignored) {
                add(diagnostics, root + "paramValue", MatchingConditionDiagnostic.Code.INVALID_REGEX,
                        MatchingConditionDiagnostic.Severity.INVALID);
            }
            return;
        }
        if (MatchingConditionValueFormat.DATE_TIME == operator.valueFormat()) {
            try {
                DateUtils.parseLocalDateTime(candidate.paramValue().trim());
            } catch (RuntimeException ignored) {
                add(diagnostics, root + "paramValue", MatchingConditionDiagnostic.Code.INVALID_DATE_TIME,
                        MatchingConditionDiagnostic.Severity.INVALID);
            }
        }
    }

    private static void add(final List<MatchingConditionDiagnostic> diagnostics, final String path,
                            final MatchingConditionDiagnostic.Code code,
                            final MatchingConditionDiagnostic.Severity severity) {
        diagnostics.add(new MatchingConditionDiagnostic(path, code, severity));
    }
}
