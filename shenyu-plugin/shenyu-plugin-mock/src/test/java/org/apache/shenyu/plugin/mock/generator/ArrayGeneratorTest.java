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

package org.apache.shenyu.plugin.mock.generator;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * The test case for {@link ArrayGenerator}.
 */
public final class ArrayGeneratorTest {

    private final ArrayGenerator arrayGenerator = new ArrayGenerator();

    @Test
    public void testGenerate() {
        arrayGenerator.parseRule("array|\"test\"|3");
        String generate = arrayGenerator.generate();
        assertEquals("\"test\",\"test\",\"test\"", generate);
    }

    @Test
    public void testNestRuleGenerate() {
        arrayGenerator.parseRule("array|{\"ints\":${array|10|3}}|3");
        String generate = arrayGenerator.generate();
        assertEquals("{\"ints\":[10,10,10]},{\"ints\":[10,10,10]},{\"ints\":[10,10,10]}", generate);
    }

    @Test
    public void testMatch() {
        assertTrue(arrayGenerator.match("array|111|2"));
        assertTrue(arrayGenerator.match("array|${int|10-20}|2"));
        assertFalse(arrayGenerator.match("array|10|a"));
        assertFalse(arrayGenerator.match("array|10|"));
    }
}
