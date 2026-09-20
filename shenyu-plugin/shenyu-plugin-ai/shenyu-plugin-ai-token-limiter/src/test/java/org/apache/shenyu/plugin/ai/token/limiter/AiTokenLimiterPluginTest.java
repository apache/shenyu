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

package org.apache.shenyu.plugin.ai.token.limiter;

import org.junit.jupiter.api.Test;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for {@link AiTokenLimiterPlugin}.
 */
class AiTokenLimiterPluginTest {

    @Test
    void testBodyWriterRetainsOnlyTailBytes() {
        AiTokenLimiterPlugin.BodyWriter writer = new AiTokenLimiterPlugin.BodyWriter(8);

        assertTrue(writer.isEmpty());
        writer.write(ByteBuffer.wrap("012345".getBytes(StandardCharsets.UTF_8)));
        writer.write(ByteBuffer.wrap("6789".getBytes(StandardCharsets.UTF_8)));

        assertFalse(writer.isEmpty());
        assertEquals("23456789", writer.output());
    }

    @Test
    void testBodyWriterKeepsTailOfLargeChunk() {
        AiTokenLimiterPlugin.BodyWriter writer = new AiTokenLimiterPlugin.BodyWriter(8);

        writer.write(ByteBuffer.wrap("0123456789".getBytes(StandardCharsets.UTF_8)));

        assertEquals("23456789", writer.output());
    }
}
