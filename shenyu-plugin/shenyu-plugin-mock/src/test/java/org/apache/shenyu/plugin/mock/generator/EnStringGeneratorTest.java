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

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * The test case for {@link EnStringGenerator}.
 */
public final class EnStringGeneratorTest {

    private final EnStringGenerator generator = new EnStringGenerator();

    @Test
    public void testGenerate() {
        int max = 10;
        int min = 5;
        String enString = generator.generate("en|" + min + "-" + max, null);
        assertTrue(enString.matches("[a-zA-Z]{" + min + "," + max + "}"));
    }

    @Test
    public void testMatch() {
        assertTrue(generator.match("en|10-12"));
        assertFalse(generator.match("en"));
        assertFalse(generator.match("en|"));
        assertFalse(generator.match("en|10.1-12.1"));
        assertFalse(generator.match("en|10"));
        assertFalse(generator.match("en|-10"));
    }
}
