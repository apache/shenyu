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

package org.apache.shenyu.plugin.ai.pii.detector;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import java.util.Objects;

/**
 * The deterministic builtin detectors. Every pattern is anchored on both sides
 * by a digit boundary or a character class, so an eleven digit national id
 * segment never surfaces as a phone number, and the two checksum validated
 * types only report candidates whose checksum is valid. No claim of universal
 * personally identifiable information detection is made: this is a bounded,
 * reviewed set.
 */
public final class BuiltinPiiDetectors {

    /**
     * The email address detector type.
     */
    public static final String EMAIL = "EMAIL";

    /**
     * The mainland China mobile phone number detector type.
     */
    public static final String PHONE_CN = "PHONE_CN";

    /**
     * The mainland China resident id number detector type, checksum validated.
     */
    public static final String NATIONAL_ID_CN = "NATIONAL_ID_CN";

    /**
     * The payment card number detector type, Luhn validated.
     */
    public static final String CREDIT_CARD = "CREDIT_CARD";

    private static final Map<String, PiiDetector> BUILTINS = build();

    private BuiltinPiiDetectors() {
    }

    private static Map<String, PiiDetector> build() {
        Map<String, PiiDetector> detectors = new LinkedHashMap<>();
        detectors.put(EMAIL, new RegexPiiDetector(EMAIL,
                Pattern.compile("[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}")));
        detectors.put(PHONE_CN, new RegexPiiDetector(PHONE_CN,
                Pattern.compile("(?<!\\d)1[3-9]\\d{9}(?!\\d)")));
        detectors.put(NATIONAL_ID_CN, new RegexPiiDetector(NATIONAL_ID_CN,
                Pattern.compile("(?<![0-9Xx])\\d{6}(?:19|20)\\d{2}(?:0[1-9]|1[0-2])(?:0[1-9]|[12]\\d|3[01])\\d{3}[0-9Xx](?!\\d)"),
                PiiChecksum::isValidMod11));
        detectors.put(CREDIT_CARD, new RegexPiiDetector(CREDIT_CARD,
                Pattern.compile("(?<!\\d)\\d{13,19}(?!\\d)"),
                PiiChecksum::isValidLuhn));
        return Collections.unmodifiableMap(detectors);
    }

    /**
     * All the builtin detectors in a stable order.
     *
     * @return the detectors by type
     */
    public static Map<String, PiiDetector> all() {
        return BUILTINS;
    }

    /**
     * The enabled builtin detectors. An empty or null selection enables all of
     * them; an entry that names no builtin detector is skipped with a warning
     * left to the caller, which knows the rule it is compiling.
     *
     * @param enabledTypes the configured types, empty means all
     * @return the enabled detectors by type
     */
    public static Map<String, PiiDetector> select(final List<String> enabledTypes) {
        if (Objects.isNull(enabledTypes) || enabledTypes.isEmpty()) {
            return BUILTINS;
        }
        Map<String, PiiDetector> selected = new LinkedHashMap<>();
        for (String type : enabledTypes) {
            PiiDetector detector = BUILTINS.get(type);
            if (Objects.nonNull(detector)) {
                selected.put(type, detector);
            }
        }
        return selected;
    }
}
