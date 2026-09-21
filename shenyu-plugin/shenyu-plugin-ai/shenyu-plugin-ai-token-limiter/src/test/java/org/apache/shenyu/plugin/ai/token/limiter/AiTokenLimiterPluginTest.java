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

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.convert.rule.AiTokenLimiterHandle;
import org.apache.shenyu.plugin.ai.token.limiter.handler.AiTokenLimiterPluginHandler;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.zip.GZIPOutputStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
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

    @Test
    void testHandlerRuleWithNullFieldsFillsDefaults() {
        // Test for issue #6513: null fields should be filled with defaults
        AiTokenLimiterPluginHandler handler = new AiTokenLimiterPluginHandler();

        RuleData ruleData = RuleData.builder()
                .id("test-rule-id")
                .selectorId("test-selector-id")
                .name("test-rule")
                .handle("{\"aiTokenLimitType\":\"uri\",\"keyName\":\"default\"}")
                .build();

        handler.handlerRule(ruleData);

        AiTokenLimiterHandle cached = AiTokenLimiterPluginHandler.CACHED_HANDLE.get()
                .obtainHandle(CacheKeyUtils.INST.getKey(ruleData));

        assertNotNull(cached);
        assertEquals("uri", cached.getAiTokenLimitType());
        assertEquals("default", cached.getKeyName());
        // These should be filled with defaults
        assertNotNull(cached.getTokenLimit());
        assertNotNull(cached.getTimeWindowSeconds());
        assertEquals(Long.valueOf(100L), cached.getTokenLimit());
        assertEquals(Long.valueOf(60L), cached.getTimeWindowSeconds());
    }

    @Test
    void testGzipDecoderWithHeaderSpanningBuffers() throws IOException {
        // Test for issue #6515: gzip header spanning multiple buffers
        String sseContent = "data: {\"id\":\"test\",\"usage\":{\"completion_tokens\":50}}\n\n";
        byte[] compressed = compressGzip(sseContent);

        // Split at byte 8 - header boundary
        byte[] chunk1 = new byte[8];
        byte[] chunk2 = new byte[compressed.length - 8];
        System.arraycopy(compressed, 0, chunk1, 0, 8);
        System.arraycopy(compressed, 8, chunk2, 0, chunk2.length);

        GzipStreamDecoder decoder = new GzipStreamDecoder();
        byte[] result1 = decoder.decode(chunk1);
        // Header incomplete
        assertEquals(0, result1.length);

        byte[] result2 = decoder.decode(chunk2);
        // Should decompress now
        assertTrue(result2.length > 0);
        assertEquals(sseContent, new String(result2, StandardCharsets.UTF_8));
        decoder.close();
    }

    @Test
    void testGzipDecoderWithCompleteHeaderInFirstBuffer() throws IOException {
        String sseContent = "data: {\"id\":\"test\",\"usage\":{\"completion_tokens\":50}}\n\n";
        byte[] compressed = compressGzip(sseContent);

        GzipStreamDecoder decoder = new GzipStreamDecoder();
        byte[] result = decoder.decode(compressed);
        assertEquals(sseContent, new String(result, StandardCharsets.UTF_8));
        decoder.close();
    }

    @Test
    void testGzipDecoderWithLargeFirstChunk() throws IOException {
        // Regression: first chunk far larger than header buffer limit (266)
        // historically would incorrectly abandon entire response
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < 200; i++) {
            sb.append("data: {\"id\":\"chatcmpl-").append(i)
                    .append("\",\"choices\":[{\"delta\":{\"content\":\"token-").append(i * 7919).append("\"}}]}\n\n");
        }
        sb.append("data: {\"usage\":{\"completion_tokens\":75}}\n\n");
        String sseContent = sb.toString();

        byte[] compressed = compressGzip(sseContent);
        assertTrue(compressed.length > 266, "Compressed data must exceed header buffer size");

        GzipStreamDecoder decoder = new GzipStreamDecoder();
        byte[] result = decoder.decode(compressed);
        decoder.close();

        assertEquals(sseContent, new String(result, StandardCharsets.UTF_8));
    }

    @Test
    void testEndToEndGzipDecompressionAcrossMultipleChunks() throws IOException {
        // End-to-end test: verify full decompression pipeline with 3 chunks
        String sseContent = buildSseContentWithTokens(75);
        byte[] compressed = compressGzip(sseContent);
        byte[][] chunks = splitIntoThreeChunks(compressed);

        GzipStreamDecoder decoder = new GzipStreamDecoder();
        StringBuilder decompressed = new StringBuilder();

        for (byte[] chunk : chunks) {
            byte[] result = decoder.decode(chunk);
            if (result.length > 0) {
                decompressed.append(new String(result, StandardCharsets.UTF_8));
            }
        }
        decoder.close();

        // Verify decompression succeeded
        assertEquals(sseContent, decompressed.toString());
        assertTrue(decompressed.toString().contains("completion_tokens\":75"));
    }

    private String buildSseContentWithTokens(final int tokens) {
        return "data: {\"id\":\"chatcmpl-1\",\"choices\":[{\"delta\":{\"content\":\"Hello\"}}]}\n\n"
                + "data: {\"id\":\"chatcmpl-1\",\"choices\":[{\"delta\":{\"content\":\" World\"}}]}\n\n"
                + "data: {\"id\":\"chatcmpl-1\",\"choices\":[{\"delta\":{}}],"
                + "\"usage\":{\"completion_tokens\":" + tokens + ",\"prompt_tokens\":10,\"total_tokens\":" + (tokens + 10) + "}}\n\n"
                + "data: [DONE]\n\n";
    }

    private byte[] compressGzip(final String content) throws IOException {
        ByteArrayOutputStream compressedStream = new ByteArrayOutputStream();
        try (GZIPOutputStream gzipOutputStream = new GZIPOutputStream(compressedStream)) {
            gzipOutputStream.write(content.getBytes(StandardCharsets.UTF_8));
        }
        return compressedStream.toByteArray();
    }

    private byte[][] splitIntoThreeChunks(final byte[] data) {
        byte[] chunk1 = new byte[8];
        int chunk2Size = (data.length - 8) / 2;
        byte[] chunk2 = new byte[chunk2Size];
        byte[] chunk3 = new byte[data.length - 8 - chunk2Size];

        System.arraycopy(data, 0, chunk1, 0, 8);
        System.arraycopy(data, 8, chunk2, 0, chunk2Size);
        System.arraycopy(data, 8 + chunk2Size, chunk3, 0, chunk3.length);

        return new byte[][]{chunk1, chunk2, chunk3};
    }
}
