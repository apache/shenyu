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

import java.util.Objects;
import org.junit.jupiter.api.Test;

/**
 * The test case for {@link RangeDataGenerator}.
 */
public final class RangeDataGeneratorTest {
    
    private final RangeDataGenerator generator = new RangeDataGenerator();
    
    @Test
    public void generate() {
        generator.parseRule("list|[shenyu,gateway]");
        String rangeData = generator.generate();
        assertTrue(Objects.equals("shenyu", rangeData) || Objects.equals("gateway", rangeData));
    }
    
    @Test
    public void testListDataContainComma() {
        generator.parseRule("list|[shen\\,yu,gate\\,way]");
        String rangeData = generator.generate();
        assertTrue(Objects.equals("shen,yu", rangeData) || Objects.equals("gate,way", rangeData));
    }
    
    @Test
    public void match() {
        assertTrue(generator.match("list|[shen\\,yu,gate\\,way]"));
        assertTrue(generator.match("list|[shenyu,gateway]"));
        assertFalse(generator.match("list|[shenyu,gateway"));
        assertFalse(generator.match("list|[]"));
        assertFalse(generator.match("list|[ ]"));
        
    }
}
