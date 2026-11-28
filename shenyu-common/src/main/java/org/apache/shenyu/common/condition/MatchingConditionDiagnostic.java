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

import java.util.Objects;

/**
 * Bounded matching-condition diagnostic containing only a declared path and stable classifications.
 *
 * @param path condition field path
 * @param code stable diagnostic code
 * @param severity diagnostic severity
 */
public record MatchingConditionDiagnostic(String path, Code code, Severity severity) {

    private static final String PATH_PATTERN = "[A-Za-z0-9_.\\[\\]]{1,128}";

    /**
     * Creates a diagnostic without configured values or parser messages.
     */
    public MatchingConditionDiagnostic {
        if (Objects.isNull(path) || !path.matches(PATH_PATTERN)) {
            throw new IllegalArgumentException("Invalid matching-condition diagnostic path");
        }
        Objects.requireNonNull(code, "code");
        Objects.requireNonNull(severity, "severity");
    }

    /**
     * Stable structural diagnostic codes.
     */
    public enum Code {

        /** Source identifier is required. */
        SOURCE_REQUIRED,

        /** Operator identifier is required. */
        OPERATOR_REQUIRED,

        /** Parameter name is required. */
        NAME_REQUIRED,

        /** Parameter value is required. */
        VALUE_REQUIRED,

        /** Configured regular expression is invalid. */
        INVALID_REGEX,

        /** Configured date-time is invalid. */
        INVALID_DATE_TIME,

        /** Source identifier is not in the built-in catalog. */
        UNKNOWN_SOURCE,

        /** Operator identifier is not in the built-in catalog. */
        UNKNOWN_OPERATOR
    }

    /**
     * Diagnostic severity and compatibility classification.
     */
    public enum Severity {

        /** Known invalid structure. */
        INVALID,

        /** Unknown identifier retained for legacy or custom compatibility. */
        UNKNOWN_LEGACY
    }
}
