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

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;


class ListByteConverterTest {

    private final ListByteConverter converter = new ListByteConverter();

    @Test
    void testConvertToDatabaseColumnNull() {
        String result = converter.convertToDatabaseColumn(null);
        assertNull(result);
    }

    @Test
    void testConvertToDatabaseColumnEmpty() {
        String result = converter.convertToDatabaseColumn(Collections.emptyList());
        assertEquals("[]", result);
    }

    @Test
    void testConvertToDatabaseColumnWithValues() {
        List<Byte> list = Arrays.asList((byte) 1, (byte) 2, (byte) 3);
        String result = converter.convertToDatabaseColumn(list);
        assertEquals("[1,2,3]", result);
    }

    @Test
    void testConvertToEntityAttributeNull() {
        List<Byte> result = converter.convertToEntityAttribute(null);
        assertNull(result);
    }

    @Test
    void testConvertToEntityAttributeEmpty() {
        List<Byte> result = converter.convertToEntityAttribute("[]");
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void testConvertToEntityAttributeWithValues() {
        List<Byte> result = converter.convertToEntityAttribute("[1,2,3]");
        assertNotNull(result);
        assertEquals(3, result.size());
        assertEquals(Byte.valueOf((byte) 1), result.get(0));
        assertEquals(Byte.valueOf((byte) 2), result.get(1));
        assertEquals(Byte.valueOf((byte) 3), result.get(2));
    }

    @Test
    void testRoundTripConversion() {
        List<Byte> original = Arrays.asList((byte) 1, (byte) 2, (byte) 3);
        String dbValue = converter.convertToDatabaseColumn(original);
        List<Byte> result = converter.convertToEntityAttribute(dbValue);
        assertEquals(original, result);
    }
}
