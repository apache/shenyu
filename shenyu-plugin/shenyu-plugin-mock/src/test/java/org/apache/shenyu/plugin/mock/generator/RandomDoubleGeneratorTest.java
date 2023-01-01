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

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * The test case for {@link RandomDoubleGenerator}.
 */
public final class RandomDoubleGeneratorTest {

    private final RandomDoubleGenerator generator = new RandomDoubleGenerator();

    @Test
    public void generate() {
        String doubleValue = generator.generate("double|10.5-12.0", null);
        assertNotNull(doubleValue);
        String formated = generator.generate("double|10.5-12.0|￥%.2f", null);
        assertAll(
            () -> assertNotNull(formated),
            () -> assertTrue(formated.matches("^￥\\d+.\\d{2}$"))
        );

    }

    @Test
    public void match() {
        assertTrue(generator.match("double|10-15"));
        assertTrue(generator.match("double|10.1-15"));
        assertTrue(generator.match("double|10.1-15.2"));
        assertTrue(generator.match("double|10.1-15.2|"));
        assertTrue(generator.match("double|10.1-15.2|%.2f"));
        assertFalse(generator.match("double"));
        assertFalse(generator.match("double|10.2.1-"));
    }
}
