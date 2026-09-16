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

package org.apache.shenyu.plugin.mcp.server.transport;

import org.junit.jupiter.api.Test;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.codec.ServerSentEvent;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;

import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;

/**
 * Test cases for {@link SseEventFormatter}.
 */
final class SseEventFormatterTest {

    @Test
    void testFormatEventUsesUtf8() {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/mcp/sse").build());
        ServerSentEvent<String> event = ServerSentEvent.builder("{\"message\":\"你好，世界\"}")
                .event("message")
                .build();

        DataBuffer buffer = SseEventFormatter.formatEvent(event, exchange);

        assertArrayEquals("event: message\ndata: {\"message\":\"你好，世界\"}\n\n".getBytes(StandardCharsets.UTF_8),
                readAndRelease(buffer));
    }

    @Test
    void testFormatCommentUsesUtf8() {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/mcp/sse").build());

        DataBuffer buffer = SseEventFormatter.formatComment("保持连接", exchange);

        assertArrayEquals(": 保持连接\n\n".getBytes(StandardCharsets.UTF_8), readAndRelease(buffer));
    }

    private static byte[] readAndRelease(final DataBuffer buffer) {
        try {
            byte[] bytes = new byte[buffer.readableByteCount()];
            buffer.read(bytes);
            return bytes;
        } finally {
            DataBufferUtils.release(buffer);
        }
    }
}
