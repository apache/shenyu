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

package org.apache.shenyu.common.utils;

import io.netty.buffer.Unpooled;
import io.netty.channel.embedded.EmbeddedChannel;
import io.netty.handler.codec.http.HttpContent;
import io.netty.handler.codec.http.HttpRequest;
import io.netty.handler.codec.http.HttpRequestDecoder;
import io.netty.handler.codec.http.LastHttpContent;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Ensures the resolved Netty decoder rejects ambiguous HTTP chunk framing.
 */
class NettyHttpCodecSecurityRegressionTest {

    @ParameterizedTest
    @ValueSource(strings = {"1 z", "1\t2"})
    void testRejectsWhitespaceInsideChunkSize(final String chunkSize) {
        // GHSA-j4mg-hqgv-34qc: post-digit whitespace must not truncate the chunk size.
        EmbeddedChannel channel = new EmbeddedChannel(new HttpRequestDecoder());
        try {
            channel.writeInbound(Unpooled.copiedBuffer(chunkedRequest(chunkSize), StandardCharsets.US_ASCII));
            HttpRequest request = channel.readInbound();
            assertTrue(request.decoderResult().isSuccess());
            HttpContent content = channel.readInbound();
            try {
                assertTrue(content.decoderResult().isFailure());
            } finally {
                content.release();
            }
        } finally {
            channel.finishAndReleaseAll();
        }
    }

    @Test
    void testAcceptsValidChunkExtension() {
        EmbeddedChannel channel = new EmbeddedChannel(new HttpRequestDecoder());
        try {
            channel.writeInbound(Unpooled.copiedBuffer(chunkedRequest("1;name=value"), StandardCharsets.US_ASCII));
            HttpRequest request = channel.readInbound();
            assertTrue(request.decoderResult().isSuccess());
            HttpContent content = channel.readInbound();
            try {
                assertTrue(content.decoderResult().isSuccess());
                assertEquals("a", content.content().toString(StandardCharsets.US_ASCII));
            } finally {
                content.release();
            }
            LastHttpContent last = channel.readInbound();
            try {
                assertTrue(last.decoderResult().isSuccess());
            } finally {
                last.release();
            }
        } finally {
            channel.finishAndReleaseAll();
        }
    }

    private String chunkedRequest(final String chunkSize) {
        return "POST / HTTP/1.1\r\nHost: localhost\r\nTransfer-Encoding: chunked\r\n\r\n"
                + chunkSize + "\r\na\r\n0\r\n\r\n";
    }
}
