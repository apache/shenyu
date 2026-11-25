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

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;

import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * The test case for {@link RangeDataGenerator}.
 */
public final class RangeDataGeneratorTest {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    private final RangeDataGenerator generator = new RangeDataGenerator();

    @Test
    public void generate() {
        String rangeData = generator.generate("list|[shenyu,gateway]", null);
        assertTrue(Objects.equals("shenyu", rangeData) || Objects.equals("gateway", rangeData));
    }

    @Test
    public void testListDataContainComma() {
        String rangeData = generator.generate("list|[shen\\,yu,gate\\,way]", null);
        assertTrue(Objects.equals("shen,yu", rangeData) || Objects.equals("gate,way", rangeData));
    }

    @Test
    public void testRenderListValueWithQuotes() throws JsonProcessingException {
        assertRenderedValue("say \"hello\"", "say \"hello\"");
    }

    @Test
    public void testRenderListValueWithBackslash() throws JsonProcessingException {
        assertRenderedValue("C:\\temp", "C:\\temp");
    }

    @Test
    public void testRenderListValueWithEscapedComma() throws JsonProcessingException {
        assertRenderedValue("shen\\,yu", "shen,yu");
    }

    @Test
    public void match() {
        assertTrue(generator.match("list|[shen\\,yu,gate\\,way]"));
        assertTrue(generator.match("list|[shenyu,gateway]"));
        assertFalse(generator.match("list|[shenyu,gateway"));
        assertFalse(generator.match("list|[]"));
        assertFalse(generator.match("list|[ ]"));

    }

    private void assertRenderedValue(final String listValue, final String expected) throws JsonProcessingException {
        String response = GeneratorFactory.dealRule("{\"value\":${list|[" + listValue + "]}}", null);
        assertEquals(expected, MAPPER.readTree(response).get("value").textValue());
    }
}
