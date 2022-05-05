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

package org.apache.shenyu.common.concurrent;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Test cases for MemoryLimitCalculator.
 */
public class MemoryLimitCalculatorTest {

    @Test
    public void testCalculateWhenIllegalPercentage() {
        float largerThanOne = 2;
        float zero = 0;
        float lessThanZero = -1;
        assertThrows(IllegalArgumentException.class, () -> MemoryLimitCalculator.calculate(largerThanOne));
        assertThrows(IllegalArgumentException.class, () -> MemoryLimitCalculator.calculate(zero));
        assertThrows(IllegalArgumentException.class, () -> MemoryLimitCalculator.calculate(lessThanZero));
    }

    @Test
    public void testCalculate() {
        float percentage = 0.5f;
        assertEquals((long) (MemoryLimitCalculator.maxAvailable() * percentage), MemoryLimitCalculator.calculate(percentage));
    }

    @Test
    public void testDefaultCalculate() {
        assertEquals((long) (MemoryLimitCalculator.maxAvailable() * 0.8), MemoryLimitCalculator.defaultLimit());
    }
}
