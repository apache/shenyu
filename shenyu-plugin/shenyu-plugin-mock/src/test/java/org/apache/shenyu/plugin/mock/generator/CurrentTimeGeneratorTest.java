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

import java.text.SimpleDateFormat;
import java.util.Date;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * The test case for {@link CurrentTimeGenerator}.
 */
public class CurrentTimeGeneratorTest {
    
    private final CurrentTimeGenerator generator = new CurrentTimeGenerator();
    
    @Test
    public void testGenerate() {
        generator.parseRule("${current}");
        Assertions.assertEquals(generator.generate(), currentTime("YYYY-MM-dd HH:mm:ss"));
        generator.parseRule("current|YYYY-MM-dd");
        Assertions.assertEquals(generator.generate(), currentTime("YYYY-MM-dd"));
        
    }
    
    private String currentTime(final String format) {
        return new SimpleDateFormat(format).format(new Date());
    }
    
    @Test
    public void testMatch() {
        Assertions.assertTrue(generator.match("current"));
        Assertions.assertFalse(generator.match("current|"));
        Assertions.assertTrue(generator.match("current|YYYY-MM-dd"));
    }
}
