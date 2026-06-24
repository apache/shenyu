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

package org.apache.shenyu.plugin.record.body;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public final class BodyWriterTest {

    private static final String TEST_DATA = "hello shenyu";

    private BodyWriter bodyWriter;

    @BeforeEach
    public void setUp() {
        bodyWriter = new BodyWriter();
    }

    @Test
    public void testWriteAndOutput() {
        bodyWriter.write(ByteBuffer.wrap(TEST_DATA.getBytes(StandardCharsets.UTF_8)));
        assertFalse(bodyWriter.isEmpty());
        assertEquals(TEST_DATA.length(), bodyWriter.size());
        assertEquals(TEST_DATA, bodyWriter.output());
    }

    @Test
    public void testEmptyWriterProperties() {
        assertTrue(bodyWriter.isEmpty());
        assertEquals(0, bodyWriter.size());
        assertFalse(bodyWriter.isSizeExceeded());
    }

    @Test
    public void testMaxSizeExceeded() {
        int maxSize = 5;
        BodyWriter limitedWriter = new BodyWriter(maxSize);
        limitedWriter.write(ByteBuffer.wrap("hello world".getBytes(StandardCharsets.UTF_8)));
        assertTrue(limitedWriter.isSizeExceeded());
    }

    @Test
    public void testMaxSizeExactWriteDoesNotExceed() {
        int maxSize = 5;
        BodyWriter limitedWriter = new BodyWriter(maxSize);
        limitedWriter.write(ByteBuffer.wrap("hello".getBytes(StandardCharsets.UTF_8)));
        assertFalse(limitedWriter.isSizeExceeded());
        assertEquals(5, limitedWriter.size());
    }

    @Test
    public void testWriteAfterSizeExceededIsDropped() {
        int maxSize = 5;
        BodyWriter limitedWriter = new BodyWriter(maxSize);
        limitedWriter.write(ByteBuffer.wrap("hello world".getBytes(StandardCharsets.UTF_8)));
        assertTrue(limitedWriter.isSizeExceeded());
        int sizeBefore = limitedWriter.size();
        limitedWriter.write(ByteBuffer.wrap("extra".getBytes(StandardCharsets.UTF_8)));
        assertEquals(sizeBefore, limitedWriter.size());
    }

    @Test
    public void testMultipleWritesConcatenates() {
        bodyWriter.write(ByteBuffer.wrap("hello".getBytes(StandardCharsets.UTF_8)));
        bodyWriter.write(ByteBuffer.wrap(" shenyu".getBytes(StandardCharsets.UTF_8)));
        assertEquals(TEST_DATA, bodyWriter.output());
    }

    @Test
    public void testBelowLimitNotExceeded() {
        BodyWriter limitedWriter = new BodyWriter(1024);
        limitedWriter.write(ByteBuffer.wrap(TEST_DATA.getBytes(StandardCharsets.UTF_8)));
        assertFalse(limitedWriter.isSizeExceeded());
    }

    @Test
    public void testDefaultConstructorNoLimit() {
        BodyWriter unlimitedWriter = new BodyWriter();
        byte[] largeData = new byte[1024];
        unlimitedWriter.write(ByteBuffer.wrap(largeData));
        assertFalse(unlimitedWriter.isSizeExceeded());
        assertEquals(1024, unlimitedWriter.size());
    }

    @Test
    public void testOutputPreventsFurtherWrites() {
        bodyWriter.write(ByteBuffer.wrap(TEST_DATA.getBytes(StandardCharsets.UTF_8)));
        String result = bodyWriter.output();
        assertEquals(TEST_DATA, result);
        int sizeBefore = bodyWriter.size();
        bodyWriter.write(ByteBuffer.wrap("extra".getBytes(StandardCharsets.UTF_8)));
        assertEquals(sizeBefore, bodyWriter.size());
    }

    @Test
    public void testOutputReturnsEmptyStringWhenEmpty() {
        assertEquals("", bodyWriter.output());
    }
}