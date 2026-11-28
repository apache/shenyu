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

package org.apache.shenyu.plugin.shadow.capture;

import org.junit.jupiter.api.Test;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DefaultDataBufferFactory;

import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * test case for {@link ShadowBodyCapture}.
 */
public final class ShadowBodyCaptureTest {

    private final DefaultDataBufferFactory factory = new DefaultDataBufferFactory();

    @Test
    public void testCopiesBodyAndKeepsReadPosition() {
        DataBuffer dataBuffer = factory.wrap("hello".getBytes(StandardCharsets.UTF_8));
        final int position = dataBuffer.readPosition();
        ShadowBodyCapture capture = new ShadowBodyCapture(16);
        capture.append(dataBuffer);
        assertArrayEquals("hello".getBytes(StandardCharsets.UTF_8), capture.bytes());
        assertFalse(capture.isTruncated());
        assertEquals(position, dataBuffer.readPosition());
    }

    @Test
    public void testStopsAtBound() {
        ShadowBodyCapture capture = new ShadowBodyCapture(2);
        capture.append(factory.wrap("hello".getBytes(StandardCharsets.UTF_8)));
        assertTrue(capture.isTruncated());
        assertEquals(2, capture.bytes().length);
    }

    @Test
    public void testZeroBoundNeverCopies() {
        ShadowBodyCapture capture = new ShadowBodyCapture(0);
        capture.append(factory.wrap("hello".getBytes(StandardCharsets.UTF_8)));
        assertTrue(capture.isTruncated());
        assertEquals(0, capture.bytes().length);
    }

    @Test
    public void testEmptyBodyIsNotTruncated() {
        ShadowBodyCapture capture = new ShadowBodyCapture(8);
        assertFalse(capture.isTruncated());
        assertEquals(0, capture.bytes().length);
    }
}
