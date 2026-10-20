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

package org.apache.shenyu.plugin.logging.common.body;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.test.util.ReflectionTestUtils;

import java.io.ByteArrayOutputStream;
import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

/**
 * The Test Case For BodyWriter.
 */
public class BodyWriterTest {

    private BodyWriter writer;

    private String sendString;

    private ByteBuffer byteBuffer;

    @BeforeEach
    public void setUp() throws UnsupportedEncodingException {
        this.writer = new BodyWriter();
        this.sendString = "hello, shenyu";
        byteBuffer = ByteBuffer.wrap(sendString.getBytes(StandardCharsets.UTF_8));
    }

    @Test
    public void testWrite() {
        ByteBuffer byteBuffer = ByteBuffer.wrap(sendString.getBytes(StandardCharsets.UTF_8));
        writer.write(byteBuffer.asReadOnlyBuffer());
        String res = writer.output();
        Assertions.assertEquals(res, "hello, shenyu");
    }

    @Test
    public void testIsEmpty() {
        Assertions.assertTrue(writer.isEmpty());
    }

    @Test
    public void testSize() {
        writer.write(byteBuffer.asReadOnlyBuffer());
        int size = writer.size();
        Assertions.assertEquals(size, 13);
    }

    @Test
    public void testOutput() {
        writer.write(byteBuffer.asReadOnlyBuffer());
        String res = writer.output();
        Assertions.assertEquals(res, "hello, shenyu");
    }

    @Test
    public void testSplitUtf8BodyAtLimit() {
        BodyWriter limitedWriter = new BodyWriter(6);
        // two 3-byte UTF-8 characters, split inside the first character
        final String body = Character.toString(0x20AC) + Character.toString(0x2122);
        byte[] bytes = body.getBytes(StandardCharsets.UTF_8);
        limitedWriter.write(ByteBuffer.wrap(bytes, 0, 1).asReadOnlyBuffer());
        limitedWriter.write(ByteBuffer.wrap(bytes, 1, bytes.length - 1).asReadOnlyBuffer());
        Assertions.assertFalse(limitedWriter.isLimitExceeded());
        Assertions.assertEquals(6, limitedWriter.size());
        Assertions.assertEquals(body, limitedWriter.output());
    }

    @Test
    public void testOverflowDropsBufferAndKeepsCounting() {
        BodyWriter limitedWriter = new BodyWriter(4);
        limitedWriter.write(ByteBuffer.wrap("test".getBytes(StandardCharsets.UTF_8)));
        Assertions.assertFalse(limitedWriter.isLimitExceeded());
        Assertions.assertEquals(4, bufferedSize(limitedWriter));
        limitedWriter.write(ByteBuffer.wrap(new byte[1]));
        Assertions.assertTrue(limitedWriter.isLimitExceeded());
        assertBufferReleased(limitedWriter);
        for (int i = 0; i < 64; i++) {
            limitedWriter.write(ByteBuffer.wrap(new byte[1024]));
        }
        Assertions.assertEquals(5 + 64 * 1024, limitedWriter.size());
        assertBufferReleased(limitedWriter);
        Assertions.assertFalse(limitedWriter.isEmpty());
        Assertions.assertEquals("", limitedWriter.output());
    }

    @Test
    public void testOversizedChunkIsNotBufferedAndSourceUntouched() {
        BodyWriter limitedWriter = new BodyWriter(4);
        ByteBuffer buffer = ByteBuffer.allocate(1024 * 1024);
        limitedWriter.write(buffer.asReadOnlyBuffer());
        Assertions.assertEquals(0, buffer.position());
        Assertions.assertEquals(1024 * 1024, limitedWriter.size());
        Assertions.assertTrue(limitedWriter.isLimitExceeded());
        assertBufferReleased(limitedWriter);
        Assertions.assertEquals("", limitedWriter.output());
    }

    @ParameterizedTest
    @ValueSource(ints = {0, -1, Integer.MIN_VALUE})
    public void testNonPositiveLimitBuffersNothing(final int limit) {
        BodyWriter limitedWriter = new BodyWriter(limit);
        limitedWriter.write(ByteBuffer.allocate(0));
        Assertions.assertTrue(limitedWriter.isEmpty());
        Assertions.assertFalse(limitedWriter.isLimitExceeded());
        limitedWriter.write(ByteBuffer.wrap("hello".getBytes(StandardCharsets.UTF_8)));
        Assertions.assertEquals(5, limitedWriter.size());
        Assertions.assertTrue(limitedWriter.isLimitExceeded());
        assertBufferReleased(limitedWriter);
        Assertions.assertEquals("", limitedWriter.output());
    }

    @Test
    public void testObservedSizeIsCapped() {
        BodyWriter limitedWriter = new BodyWriter(4);
        ByteBuffer buffer = ByteBuffer.allocate(1024 * 1024);
        for (int i = 0; i < 2049; i++) {
            limitedWriter.write(buffer.asReadOnlyBuffer());
        }
        Assertions.assertEquals(Integer.MAX_VALUE, limitedWriter.size());
        Assertions.assertTrue(limitedWriter.isLimitExceeded());
        Assertions.assertEquals("", limitedWriter.output());
    }

    @Test
    public void testOverflowDetectedWhenSizeSaturatesAtMaxLimit() {
        BodyWriter maxLimitWriter = new BodyWriter(Integer.MAX_VALUE);
        // simulate a body that has already reached the int range without allocating 2 GiB
        ReflectionTestUtils.setField(maxLimitWriter, "size", (long) Integer.MAX_VALUE - 1);
        maxLimitWriter.write(ByteBuffer.wrap(new byte[2]));
        Assertions.assertEquals(Integer.MAX_VALUE, maxLimitWriter.size());
        Assertions.assertTrue(maxLimitWriter.isLimitExceeded());
        assertBufferReleased(maxLimitWriter);
        Assertions.assertEquals("", maxLimitWriter.output());
    }

    @Test
    public void testOutputStopsFurtherWrites() {
        BodyWriter limitedWriter = new BodyWriter(4);
        limitedWriter.write(ByteBuffer.wrap("abc".getBytes(StandardCharsets.UTF_8)));
        Assertions.assertEquals("abc", limitedWriter.output());
        limitedWriter.write(ByteBuffer.wrap("hello".getBytes(StandardCharsets.UTF_8)));
        Assertions.assertEquals(3, limitedWriter.size());
        Assertions.assertFalse(limitedWriter.isLimitExceeded());
        Assertions.assertEquals("abc", limitedWriter.output());
    }

    @Test
    public void testConcurrentWriteAndOutputDoNotFail() throws Exception {
        ExecutorService executor = Executors.newFixedThreadPool(2);
        try {
            for (int round = 0; round < 200; round++) {
                BodyWriter concurrentWriter = new BodyWriter(64);
                CountDownLatch start = new CountDownLatch(1);
                Future<?> writes = executor.submit(() -> {
                    start.await();
                    for (int i = 0; i < 100; i++) {
                        concurrentWriter.write(ByteBuffer.wrap("abcdefgh".getBytes(StandardCharsets.UTF_8)));
                    }
                    return null;
                });
                Future<String> output = executor.submit(() -> {
                    start.await();
                    return concurrentWriter.output();
                });
                start.countDown();
                writes.get(5, TimeUnit.SECONDS);
                String body = output.get(5, TimeUnit.SECONDS);
                // output sees either a consistent prefix within the limit, or nothing after overflow
                Assertions.assertTrue(body.length() <= 64 && body.length() % 8 == 0, body);
            }
        } finally {
            executor.shutdownNow();
        }
    }

    @Test
    public void testOverflowReleasesBufferReferences() {
        BodyWriter limitedWriter = new BodyWriter(512 * 1024);
        ByteBuffer chunk = ByteBuffer.allocate(8192);
        for (int i = 0; i < 64; i++) {
            limitedWriter.write(chunk.asReadOnlyBuffer());
        }
        Assertions.assertFalse(limitedWriter.isLimitExceeded());
        Assertions.assertEquals(512 * 1024, bufferedSize(limitedWriter));
        limitedWriter.write(ByteBuffer.wrap(new byte[1]));
        Assertions.assertTrue(limitedWriter.isLimitExceeded());
        // the writer no longer references the buffered array, so it is collectable before the request ends
        assertBufferReleased(limitedWriter);
        // later writes only count bytes and must not allocate a new buffer
        for (int i = 0; i < 8; i++) {
            limitedWriter.write(chunk.asReadOnlyBuffer());
        }
        assertBufferReleased(limitedWriter);
        Assertions.assertEquals(512 * 1024 + 1 + 8 * 8192, limitedWriter.size());
        Assertions.assertEquals("", limitedWriter.output());
        assertBufferReleased(limitedWriter);
    }

    @Test
    public void testEmptyBodyDoesNotAllocateBuffer() {
        BodyWriter limitedWriter = new BodyWriter(4);
        limitedWriter.write(ByteBuffer.allocate(0));
        Assertions.assertNull(ReflectionTestUtils.getField(limitedWriter, "stream"));
        Assertions.assertEquals("", limitedWriter.output());
    }

    private static void assertBufferReleased(final BodyWriter bodyWriter) {
        Assertions.assertNull(ReflectionTestUtils.getField(bodyWriter, "stream"));
        Assertions.assertNull(ReflectionTestUtils.getField(bodyWriter, "channel"));
    }

    private static int bufferedSize(final BodyWriter bodyWriter) {
        ByteArrayOutputStream stream = (ByteArrayOutputStream) ReflectionTestUtils.getField(bodyWriter, "stream");
        Assertions.assertNotNull(stream);
        return stream.size();
    }
}
