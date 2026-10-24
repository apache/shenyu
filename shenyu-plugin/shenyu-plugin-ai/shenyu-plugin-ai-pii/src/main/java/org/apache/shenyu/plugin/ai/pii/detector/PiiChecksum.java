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

import java.util.Objects;
/**
 * The checksum validations that turn a digit pattern match into a detection
 * with an acceptable false positive rate. A national id number is only
 * reported when its mod 11-2 checksum is valid, a card number only when its
 * Luhn checksum is valid.
 */

public final class PiiChecksum {

    private static final int[] MOD11_2_WEIGHTS = {7, 9, 10, 5, 8, 4, 2, 1, 6, 3, 7, 9, 10, 5, 8, 4, 2};

    private static final char[] MOD11_2_CHECK_CODES = {'1', '0', 'X', '9', '8', '7', '6', '5', '4', '3', '2'};

    private PiiChecksum() {
    }

    /**
     * The mod 11-2 checksum of a Chinese resident id number: the 18th
     * character must equal the check code of the first 17 digits.
     *
     * @param candidate an 18 character candidate, digits with an optional x as the last character
     * @return true when the checksum is valid
     */
    public static boolean isValidMod11(final String candidate) {
        if (Objects.isNull(candidate) || candidate.length() != 18) {
            return false;
        }
        int sum = 0;
        for (int i = 0; i < MOD11_2_WEIGHTS.length; i++) {
            char digit = candidate.charAt(i);
            if (digit < '0' || digit > '9') {
                return false;
            }
            sum += (digit - '0') * MOD11_2_WEIGHTS[i];
        }
        char expected = MOD11_2_CHECK_CODES[sum % 11];
        char given = Character.toUpperCase(candidate.charAt(17));
        return given == expected;
    }

    /**
     * The Luhn checksum of a payment card number.
     *
     * @param candidate the candidate digits
     * @return true when the checksum is valid
     */
    public static boolean isValidLuhn(final String candidate) {
        if (Objects.isNull(candidate) || candidate.isEmpty()) {
            return false;
        }
        int sum = 0;
        boolean doubleDigit = false;
        for (int i = candidate.length() - 1; i >= 0; i--) {
            char digit = candidate.charAt(i);
            if (digit < '0' || digit > '9') {
                return false;
            }
            int value = digit - '0';
            if (doubleDigit) {
                value *= 2;
                if (value > 9) {
                    value -= 9;
                }
            }
            sum += value;
            doubleDigit = !doubleDigit;
        }
        return sum % 10 == 0;
    }
}
