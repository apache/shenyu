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

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

/**
 * The test case for {@link PhoneGenerator }.
 *
 * @date 2022/6/20 14:48
 */
public final class PhoneGeneratorTest {
    
    private final PhoneGenerator generator = new PhoneGenerator();
    
    @Test
    void testGenerate() {
        generator.parseRule("phone");
        String phone = generator.generate();
        assertTrue(phone.matches("^1[3-9]\\d{9}$"));
    }
    
    @Test
    void match() {
        assertTrue(generator.match("phone"));
        assertFalse(generator.match("mobile"));
        
    }
}
