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

package org.apache.shenyu.plugin.mock.util;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test case for {@link MockUtil}.
 */
public final class MockUtilTest {

    @ParameterizedTest
    @CsvSource({
        "0, 2147483647",
        "-2147483648, 2147483647",
        "-2147483648, 0",
        "-2147483648, -1",
        "2147483647, 2147483647",
        "-2147483648, -2147483648",
        "0, 0",
        "10, 15"
    })
    public void randomIntShouldSupportInclusiveIntegerBounds(final int min, final int max) {
        for (int i = 0; i < 100; i++) {
            int value = MockUtil.randomInt(min, max);
            assertTrue(value >= min && value <= max, "Generated value must remain within the inclusive bounds");
        }
    }

    @ParameterizedTest
    @CsvSource({"1, 0", "2147483647, -2147483648"})
    public void randomIntShouldRejectReversedBounds(final int min, final int max) {
        assertThrows(IllegalArgumentException.class, () -> MockUtil.randomInt(min, max));
    }

    @Test
    public void zhShouldReturnLengthsWithinTheInclusiveRange() {
        for (int i = 0; i < 500; i++) {
            String value = MockUtil.zh(2, 3);
            int length = value.length();
            assertTrue(length >= 2 && length <= 3, "zh(2,3) length must be 2 or 3 but was " + length);
        }
    }

    @Test
    public void zhShouldCoverTheUpperHalfOfTheRange() {
        // the sibling en() samples [min, max] inclusively; over 500 draws zh(2,5) must reach 4 or 5
        int maxLength = 0;
        for (int i = 0; i < 500; i++) {
            maxLength = Math.max(maxLength, MockUtil.zh(2, 5).length());
        }
        assertTrue(maxLength >= 4, "zh(2,5) must be able to produce lengths up to 5 but max observed was " + maxLength);
    }
}
