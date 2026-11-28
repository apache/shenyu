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

package org.apache.shenyu.admin.validation;

import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.apache.shenyu.admin.model.dto.RuleConditionDTO;
import org.apache.shenyu.admin.model.dto.SelectorConditionDTO;
import org.apache.shenyu.common.condition.MatchingConditionCandidate;
import org.apache.shenyu.common.condition.MatchingConditionDiagnostic;
import org.apache.shenyu.common.condition.MatchingConditionValidation;
import org.apache.shenyu.common.condition.MatchingConditionValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Applies opt-in shared matching-condition validation at Admin write boundaries.
 *
 * <p>The static flag is startup configuration bound from
 * {@code shenyu.matching-condition.strict-validation}. It is static because the two service interface default
 * methods are the only shared write boundary and cannot receive injected beans.</p>
 */
public final class MatchingConditionEnforcement {

    private static final int MAX_MESSAGE_DIAGNOSTICS = 4;

    private static final String MESSAGE_PREFIX = "Matching condition validation failed: ";

    private static final Logger LOG = LoggerFactory.getLogger(MatchingConditionEnforcement.class);

    private static volatile boolean strictValidation;

    private MatchingConditionEnforcement() {
    }

    /**
     * Sets the startup-configured strict validation mode.
     *
     * @param strictValidation true to reject known-invalid conditions
     */
    public static void setStrictValidation(final boolean strictValidation) {
        MatchingConditionEnforcement.strictValidation = strictValidation;
    }

    /**
     * Returns the startup-configured strict validation mode.
     *
     * @return true when known-invalid conditions are rejected
     */
    public static boolean isStrictValidation() {
        return strictValidation;
    }

    /**
     * Checks selector conditions without changing their configured values.
     *
     * @param conditions selector conditions, or null
     */
    public static void checkSelectorConditions(final List<SelectorConditionDTO> conditions) {
        check(conditions, condition -> new MatchingConditionCandidate(condition.getParamType(),
                condition.getOperator(), condition.getParamName(), condition.getParamValue()));
    }

    /**
     * Checks rule conditions without changing their configured values.
     *
     * @param conditions rule conditions, or null
     */
    public static void checkRuleConditions(final List<RuleConditionDTO> conditions) {
        check(conditions, condition -> new MatchingConditionCandidate(condition.getParamType(),
                condition.getOperator(), condition.getParamName(), condition.getParamValue()));
    }

    private static <T> void check(final List<T> conditions, final Function<T, MatchingConditionCandidate> mapper) {
        if (Objects.isNull(conditions) || conditions.isEmpty()) {
            return;
        }
        MatchingConditionValidation validation = MatchingConditionValidator.validate(
                conditions.stream().map(mapper).collect(Collectors.toList()));
        if (!validation.isInvalid()) {
            return;
        }
        String details = summary(validation.diagnostics());
        if (!strictValidation) {
            LOG.warn("Matching condition structural issues detected (report-only, not rejected): {}", details);
            return;
        }
        throw new ShenyuAdminException(MESSAGE_PREFIX + details);
    }

    private static String summary(final List<MatchingConditionDiagnostic> diagnostics) {
        String details = diagnostics.stream().limit(MAX_MESSAGE_DIAGNOSTICS)
                .map(diagnostic -> diagnostic.path() + "=" + diagnostic.code()).collect(Collectors.joining(", "));
        if (diagnostics.size() > MAX_MESSAGE_DIAGNOSTICS) {
            details += ", ...";
        }
        return details;
    }
}
