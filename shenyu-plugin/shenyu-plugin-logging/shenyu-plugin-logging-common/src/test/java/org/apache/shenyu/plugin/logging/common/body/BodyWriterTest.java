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

import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;

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
}
