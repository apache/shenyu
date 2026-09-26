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
import org.apache.shenyu.infra.redis.serializer.ShenyuRedisSerializationContext;
import org.apache.shenyu.plugin.ai.token.limiter.handler.AiTokenLimiterPluginHandler;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.data.redis.core.ReactiveRedisTemplate;
import org.springframework.data.redis.core.ReactiveValueOperations;
import org.springframework.data.redis.serializer.RedisSerializationContext;
import reactor.core.publisher.Mono;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.lang.reflect.Method;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
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

    @Test
    void testRecordTokensUsageGivesTheCounterItsWindowWhenItIsCreated() throws Exception {
        // Regression for #6649: the window is applied when the counter is created and never re-issued, otherwise a
        // sustained traffic keeps pushing it forward and the token budget of the window is never reset.
        ReactiveRedisTemplate<String, String> redisTemplate = mockRedisTemplate();
        ReactiveValueOperations<String, String> valueOperations = mockValueOperations();
        Mockito.when(redisTemplate.opsForValue()).thenReturn(valueOperations);
        Mockito.when(valueOperations.setIfAbsent("key", "0", Duration.ofSeconds(60L))).thenReturn(Mono.just(true));
        Mockito.when(valueOperations.increment("key", 10L)).thenReturn(Mono.just(30L));

        recordTokensUsage(new AiTokenLimiterPlugin(), redisTemplate, "key", 10L, 60L);

        // the counter is created together with its window, and its value goes through the template's String value
        // serializer, so it has to be a String (a Long would throw at runtime)
        Mockito.verify(valueOperations).setIfAbsent("key", "0", Duration.ofSeconds(60L));
        Mockito.verify(valueOperations).increment("key", 10L);
        Mockito.verify(redisTemplate, Mockito.never()).getExpire(Mockito.anyString());
        Mockito.verify(redisTemplate, Mockito.never()).expire(Mockito.anyString(), Mockito.any(Duration.class));
    }

    @Test
    void testRecordTokensUsageKeepsTheWindowOfAnExistingCounter() throws Exception {
        // An existing counter that already has a window keeps it: re-issuing the expiration would make the window
        // slide forward on every request, which is the bug this change fixes.
        ReactiveRedisTemplate<String, String> redisTemplate = mockRedisTemplate();
        ReactiveValueOperations<String, String> valueOperations = mockValueOperations();
        Mockito.when(redisTemplate.opsForValue()).thenReturn(valueOperations);
        Mockito.when(valueOperations.setIfAbsent("key", "0", Duration.ofSeconds(60L))).thenReturn(Mono.just(false));
        Mockito.when(redisTemplate.getExpire("key")).thenReturn(Mono.just(Duration.ofSeconds(30L)));
        Mockito.when(valueOperations.increment("key", 10L)).thenReturn(Mono.just(30L));

        recordTokensUsage(new AiTokenLimiterPlugin(), redisTemplate, "key", 10L, 60L);

        Mockito.verify(redisTemplate).getExpire("key");
        Mockito.verify(redisTemplate, Mockito.never()).expire(Mockito.anyString(), Mockito.any(Duration.class));
        Mockito.verify(valueOperations).increment("key", 10L);
    }

    @Test
    void testRecordTokensUsageGivesAWindowToACounterThatHasNone() throws Exception {
        // A counter written by an earlier version, or by another path, can exist without an expiration: it is given
        // one once, instead of on every increment.
        ReactiveRedisTemplate<String, String> redisTemplate = mockRedisTemplate();
        ReactiveValueOperations<String, String> valueOperations = mockValueOperations();
        Mockito.when(redisTemplate.opsForValue()).thenReturn(valueOperations);
        Mockito.when(valueOperations.setIfAbsent("key", "0", Duration.ofSeconds(60L))).thenReturn(Mono.just(false));
        Mockito.when(redisTemplate.getExpire("key")).thenReturn(Mono.just(Duration.ofSeconds(-1L)));
        Mockito.when(redisTemplate.expire("key", Duration.ofSeconds(60L))).thenReturn(Mono.just(true));
        Mockito.when(valueOperations.increment("key", 10L)).thenReturn(Mono.just(30L));

        recordTokensUsage(new AiTokenLimiterPlugin(), redisTemplate, "key", 10L, 60L);

        Mockito.verify(redisTemplate).expire("key", Duration.ofSeconds(60L));
        Mockito.verify(valueOperations).increment("key", 10L);
    }

    @Test
    void testTheCounterValueSurvivesTheTemplateValueSerializer() {
        // The template built by the handler serializes values with StringRedisSerializer, so the counter is written
        // as a String: the serializer's bridge method casts to String, and a Long would throw at runtime. This is the
        // class of mistake a mock-only test cannot catch.
        RedisSerializationContext.SerializationPair<String> valuePair =
                ShenyuRedisSerializationContext.stringSerializationContext().getValueSerializationPair();

        assertEquals("0", valuePair.read(valuePair.write("0")));
    }

    @SuppressWarnings("unchecked")
    private ReactiveValueOperations<String, String> mockValueOperations() {
        return Mockito.mock(ReactiveValueOperations.class);
    }

    @SuppressWarnings("unchecked")
    private ReactiveRedisTemplate<String, String> mockRedisTemplate() {
        return Mockito.mock(ReactiveRedisTemplate.class);
    }

    private void recordTokensUsage(final AiTokenLimiterPlugin plugin, final ReactiveRedisTemplate<String, String> redisTemplate,
                                   final String cacheKey, final Long tokens, final Long windowSeconds) throws Exception {
        Method method = AiTokenLimiterPlugin.class.getDeclaredMethod("recordTokensUsage",
                ReactiveRedisTemplate.class, String.class, Long.class, Long.class);
        method.setAccessible(true);
        method.invoke(plugin, redisTemplate, cacheKey, tokens, windowSeconds);
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
