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

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * The test case for {@link CurrentTimeGenerator}.
 */
public final class CurrentTimeGeneratorTest {
    
    private final CurrentTimeGenerator generator = new CurrentTimeGenerator();
    
    @Test
    public void testGenerate() {
        generator.parseRule("${current}");
        Assertions.assertTrue(generator.generate().matches("^\\d{4}(-\\d{2}){2} \\d{2}(:\\d{2}){2}$"));
        generator.parseRule("current|YYYY-MM-dd");
        Assertions.assertTrue(generator.generate().matches("^\\d{4}(-\\d{2}){2}$"));
    }
    
    @Test
    public void testMatch() {
        Assertions.assertTrue(generator.match("current"));
        Assertions.assertFalse(generator.match("current|"));
        Assertions.assertTrue(generator.match("current|YYYY-MM-dd"));
    }
}
