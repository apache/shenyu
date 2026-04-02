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

package org.apache.shenyu.admin.jpa.converter;

import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class MapStringConverterTest {

    private final MapStringConverter converter = new MapStringConverter();

    @Test
    void testConvertToDatabaseColumnNull() {
        String result = converter.convertToDatabaseColumn(null);
        assertNull(result);
    }

    @Test
    void testConvertToDatabaseColumnEmpty() {
        String result = converter.convertToDatabaseColumn(Collections.emptyMap());
        assertEquals("{}", result);
    }

    @Test
    void testConvertToDatabaseColumnWithValues() {
        Map<String, String> map = new HashMap<>();
        map.put("key1", "value1");
        map.put("key2", "value2");
        String result = converter.convertToDatabaseColumn(map);
        assertTrue(result.contains("key1"));
        assertTrue(result.contains("value1"));
        assertTrue(result.contains("key2"));
        assertTrue(result.contains("value2"));
    }

    @Test
    void testConvertToEntityAttributeNull() {
        Map<String, String> result = converter.convertToEntityAttribute(null);
        assertNull(result);
    }

    @Test
    void testConvertToEntityAttributeEmpty() {
        Map<String, String> result = converter.convertToEntityAttribute("{}");
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void testConvertToEntityAttributeWithValues() {
        String json = "{\"key1\":\"value1\",\"key2\":\"value2\"}";
        Map<String, String> result = converter.convertToEntityAttribute(json);
        assertNotNull(result);
        assertEquals(2, result.size());
        assertEquals("value1", result.get("key1"));
        assertEquals("value2", result.get("key2"));
    }

    @Test
    void testRoundTripConversion() {
        Map<String, String> original = new HashMap<>();
        original.put("key1", "value1");
        original.put("key2", "value2");
        String dbValue = converter.convertToDatabaseColumn(original);
        Map<String, String> result = converter.convertToEntityAttribute(dbValue);
        assertEquals(original, result);
    }
}
