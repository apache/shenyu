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
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.Mockito;
import org.reactivestreams.Publisher;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.data.redis.core.ReactiveRedisTemplate;
import org.springframework.data.redis.core.script.RedisScript;
import org.springframework.data.redis.serializer.RedisSerializationContext;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import reactor.core.Disposable;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.zip.GZIPOutputStream;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for {@link AiTokenLimiterPlugin}.
 */
class AiTokenLimiterPluginTest {

    private static final String USAGE_EVENT = "data: {\"usage\":{\"completion_tokens\":12}}\n\n";

    private static final String RESPONSES_COMPLETED_EVENT = "event: response.completed\n"
            + "data: {\"type\":\"response.completed\",\"sequence_number\":4,\"response\":{\"id\":\"resp_1\",\"object\":\"response\","
            + "\"status\":\"completed\",\"usage\":{\"input_tokens\":3,\"output_tokens\":12,\"total_tokens\":15}}}\n\n";

    private static final String RESPONSES_BODY = "{\"id\":\"resp_1\",\"object\":\"response\",\"status\":\"completed\","
            + "\"output\":[],\"usage\":{\"input_tokens\":3,\"output_tokens\":12,\"total_tokens\":15}}";

    @Test
    void testResponsesStreamEventRecordsOutputTokens() {
        assertEventStreamUsage(RESPONSES_COMPLETED_EVENT,
                new byte[][] {RESPONSES_COMPLETED_EVENT.getBytes(StandardCharsets.UTF_8)}, false, false, List.of(12L));
    }

    @Test
    void testResponsesStreamEventAcrossBuffersRecordsOutputTokens() {
        byte[] content = RESPONSES_COMPLETED_EVENT.getBytes(StandardCharsets.UTF_8);
        int split = RESPONSES_COMPLETED_EVENT.indexOf("\"output_tokens\"");
        byte[][] chunks = {Arrays.copyOfRange(content, 0, split), Arrays.copyOfRange(content, split, content.length)};
        for (boolean flush : new boolean[] {false, true}) {
            assertEventStreamUsage(RESPONSES_COMPLETED_EVENT, chunks, flush, false, List.of(12L));
        }
    }

    @Test
    void testCompleteResponsesBodyRecordsOutputTokens() {
        byte[] content = RESPONSES_BODY.getBytes(StandardCharsets.UTF_8);
        assertResponseUsage(content, new byte[][] {content}, MediaType.APPLICATION_JSON, false, false, List.of(12L));
    }

    @Test
    void testResponsesUsageIsNotReadFromMaxOutputTokens() {
        String body = "{\"id\":\"resp_1\",\"object\":\"response\",\"max_output_tokens\":99,\"usage\":{\"input_tokens\":3,"
                + "\"output_tokens\":12,\"total_tokens\":15}}";
        assertJsonUsage(body, List.of(12L));
    }

    @Test
    void testResponsesUsageWithoutOutputTokensRecordsNothing() {
        String body = "{\"id\":\"resp_1\",\"object\":\"response\",\"status\":\"incomplete\",\"max_output_tokens\":99}";
        assertJsonUsage(body, List.of());
    }

    @Test
    void testMetadataTokenFieldsDoNotRecordUsage() {
        assertJsonUsage("{\"metadata\":{\"output_tokens\":123},\"choices\":[]}", List.of());
        assertJsonUsage("{\"metadata\":{\"completion_tokens\":123},\"choices\":[]}", List.of());
        assertJsonUsage("{\"choices\":[{\"message\":{\"content\":\"{\\\"output_tokens\\\":123}\"}}]}", List.of());
        assertJsonUsage("{\"choices\":[{\"message\":{\"tool_calls\":[{\"function\":{\"arguments\":\"{\\\"completion_tokens\\\":123}\"}}]}}]}", List.of());
    }

    @Test
    void testMetadataTokenFieldsCannotOverrideUsage() {
        assertJsonUsage("{\"usage\":{\"completion_tokens\":12},\"metadata\":{\"output_tokens\":123}}", List.of(12L));
        assertJsonUsage("{\"metadata\":{\"output_tokens\":123},\"usage\":{\"completion_tokens\":12}}", List.of(12L));
        assertJsonUsage("{\"usage\":{\"output_tokens\":12},\"output_tokens\":123}", List.of(12L));
    }

    @Test
    void testReportedZeroIsDifferentFromUnknownUsage() {
        assertJsonUsage("{\"usage\":{\"completion_tokens\":0}}", List.of(0L));
        assertJsonUsage("{\"usage\":{}}", List.of());
        assertJsonUsage("{\"usage\":null}", List.of());
        assertJsonUsage("{}", List.of());
    }

    @ParameterizedTest
    @ValueSource(strings = {"-1", "1.5", "\"12\"", "null", "true", "{}", "[]", "9223372036854775808"})
    void testInvalidTokenCountsDoNotRecordUsage(final String count) {
        assertJsonUsage("{\"usage\":{\"completion_tokens\":" + count + "}}", List.of());
    }

    @Test
    void testAmbiguousJsonDoesNotRecordUsage() {
        assertJsonUsage("{\"usage\":{\"completion_tokens\":12,\"completion_tokens\":13}}", List.of());
        assertJsonUsage("{\"usage\":{\"completion_tokens\":12}}{}", List.of());
        assertJsonUsage("{\"usage\":{\"completion_tokens\":12}}garbage", List.of());
    }

    @Test
    void testLastCompleteStreamUsageIsRecordedOnce() {
        String stream = "data: {\"usage\":{\"completion_tokens\":2}}\n\n"
                + "data: {\"choices\":[]}\n\n"
                + "data: {\"usage\":{\"completion_tokens\":12}}\n\n"
                + "data: [DONE]\n\n";
        assertEventStreamUsage(stream, splitEveryByte(stream.getBytes(StandardCharsets.UTF_8)), true, false, List.of(12L));
    }

    @Test
    void testIncompleteStreamEventDoesNotRecordUsage() {
        String stream = "data: {\"usage\":{\"completion_tokens\":12}}";
        assertEventStreamUsage(stream, new byte[][] {stream.getBytes(StandardCharsets.UTF_8)}, false, false, List.of());
    }

    @ParameterizedTest
    @CsvSource({"false, field", "true, field", "true, number"})
    void testResponseUsageAcrossBuffers(final boolean flush, final String boundary) {
        byte[] content = USAGE_EVENT.getBytes(StandardCharsets.UTF_8);
        int split = USAGE_EVENT.indexOf("field".equals(boundary) ? "tokens" : "2");
        byte[][] chunks = {Arrays.copyOfRange(content, 0, split), Arrays.copyOfRange(content, split, content.length)};
        assertResponseUsage(content, chunks, MediaType.TEXT_EVENT_STREAM, flush, false, List.of(12L));
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void testResponseUsageAcrossGzipBuffers(final boolean flush) throws IOException {
        byte[] compressed = compressGzip(USAGE_EVENT);
        assertResponseUsage(compressed, splitIntoThreeChunks(compressed), MediaType.TEXT_EVENT_STREAM, flush, true, List.of(12L));
    }

    @Test
    void testGzipJsonUsesProtocolUsageOnly() throws IOException {
        String body = "{\"usage\":{\"output_tokens\":12},\"metadata\":{\"output_tokens\":123}}";
        byte[] compressed = compressGzip(body);
        assertResponseUsage(compressed, splitIntoThreeChunks(compressed), MediaType.APPLICATION_JSON,
                false, true, List.of(12L));

        String metadataOnly = "{\"metadata\":{\"output_tokens\":123}}";
        byte[] compressedMetadata = compressGzip(metadataOnly);
        assertResponseUsage(compressedMetadata, splitIntoThreeChunks(compressedMetadata), MediaType.APPLICATION_JSON,
                false, true, List.of());
    }

    @Test
    void testCompleteUsageBeforeCancellationIsRecorded() {
        assertCancellationUsage(USAGE_EVENT, List.of(12L));
        assertCancellationUsage("data: {\"usage\":{\"completion_tokens\":12}}", List.of());
    }

    @Test
    void testFlushUsageIsFinalizedAfterOuterPublisherCompletes() {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/"));
        exchange.getResponse().getHeaders().setContentType(MediaType.TEXT_EVENT_STREAM);
        ByteArrayOutputStream forwarded = new ByteArrayOutputStream();
        exchange.getResponse().setWriteHandler(body -> body.doOnNext(buffer -> {
            byte[] bytes = new byte[buffer.readableByteCount()];
            buffer.read(bytes);
            forwarded.writeBytes(bytes);
            DataBufferUtils.release(buffer);
        }).then());
        List<Long> recorded = new ArrayList<>();
        List<Throwable> errors = new ArrayList<>();
        AiTokenLimiterPlugin.AiStatisticServerHttpResponse response = new AiTokenLimiterPlugin.AiStatisticServerHttpResponse(
                exchange, exchange.getResponse(), recorded::add);
        Sinks.Many<Publisher<? extends DataBuffer>> body = Sinks.many().unicast().onBackpressureBuffer();
        response.writeAndFlushWith(body.asFlux()).subscribe(unused -> { }, errors::add);
        int split = USAGE_EVENT.indexOf("tokens");
        body.tryEmitNext(Mono.just(exchange.getResponse().bufferFactory().wrap(USAGE_EVENT.substring(0, split).getBytes(StandardCharsets.UTF_8)))).orThrow();
        assertEquals(USAGE_EVENT.substring(0, split), forwarded.toString(StandardCharsets.UTF_8));
        assertTrue(recorded.isEmpty());
        body.tryEmitNext(Mono.just(exchange.getResponse().bufferFactory().wrap(USAGE_EVENT.substring(split).getBytes(StandardCharsets.UTF_8)))).orThrow();
        assertEquals(USAGE_EVENT, forwarded.toString(StandardCharsets.UTF_8));
        assertTrue(recorded.isEmpty());

        body.tryEmitComplete().orThrow();
        assertTrue(errors.isEmpty());
        assertEquals(List.of(12L), recorded);
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
    void testRecordTokensUsageExecutesOneAtomicScriptWithStringArguments() {
        ReactiveRedisTemplate<String, String> redisTemplate = mockRedisTemplate();
        Mockito.when(redisTemplate.execute(Mockito.<RedisScript<Long>>any(), Mockito.eq(List.of("key")), Mockito.eq(List.of("10", "60"))))
                .thenReturn(Flux.just(30L));

        assertEquals(30L, new AiTokenLimiterPlugin().recordTokensUsage(redisTemplate, "key", 10L, 60L).block());
        Mockito.verify(redisTemplate).execute(Mockito.<RedisScript<Long>>any(), Mockito.eq(List.of("key")), Mockito.eq(List.of("10", "60")));
        Mockito.verifyNoMoreInteractions(redisTemplate);
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

    private void assertJsonUsage(final String body, final List<Long> expectedTokens) {
        byte[] content = body.getBytes(StandardCharsets.UTF_8);
        assertResponseUsage(content, new byte[][] {content}, MediaType.APPLICATION_JSON, false, false, expectedTokens);
    }

    private void assertEventStreamUsage(final String expected, final byte[][] chunks, final boolean flush,
                                        final boolean gzip, final List<Long> expectedTokens) {
        assertResponseUsage(expected.getBytes(StandardCharsets.UTF_8), chunks, MediaType.TEXT_EVENT_STREAM,
                flush, gzip, expectedTokens);
    }

    private void assertResponseUsage(final byte[] expected, final byte[][] chunks, final MediaType mediaType,
                                     final boolean flush, final boolean gzip, final List<Long> expectedTokens) {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/"));
        exchange.getResponse().getHeaders().setContentType(mediaType);
        if (gzip) {
            exchange.getResponse().getHeaders().set(HttpHeaders.CONTENT_ENCODING, "gzip");
        }
        List<Long> recorded = new ArrayList<>();
        AiTokenLimiterPlugin.AiStatisticServerHttpResponse response = new AiTokenLimiterPlugin.AiStatisticServerHttpResponse(
                exchange, exchange.getResponse(), recorded::add);
        Flux<DataBuffer> body = Flux.fromArray(chunks).map(exchange.getResponse().bufferFactory()::wrap);
        Mono<Void> completion = flush ? response.writeAndFlushWith(body.map(Mono::just)) : response.writeWith(body);
        completion.block();

        assertEquals(expectedTokens, recorded);
        byte[] forwarded = DataBufferUtils.join(exchange.getResponse().getBody()).map(buffer -> {
            byte[] bytes = new byte[buffer.readableByteCount()];
            buffer.read(bytes);
            DataBufferUtils.release(buffer);
            return bytes;
        }).block();
        assertArrayEquals(expected, forwarded);
    }

    private void assertCancellationUsage(final String content, final List<Long> expectedTokens) {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/"));
        exchange.getResponse().getHeaders().setContentType(MediaType.TEXT_EVENT_STREAM);
        exchange.getResponse().setWriteHandler(body -> body.doOnNext(DataBufferUtils::release).then());
        List<Long> recorded = new ArrayList<>();
        AiTokenLimiterPlugin.AiStatisticServerHttpResponse response = new AiTokenLimiterPlugin.AiStatisticServerHttpResponse(
                exchange, exchange.getResponse(), recorded::add);
        Flux<DataBuffer> body = Flux.concat(
                Mono.just(exchange.getResponse().bufferFactory().wrap(content.getBytes(StandardCharsets.UTF_8))),
                Mono.never());
        Disposable subscription = response.writeWith(body).subscribe();
        assertTrue(recorded.isEmpty());

        subscription.dispose();
        assertEquals(expectedTokens, recorded);
    }

    @SuppressWarnings("unchecked")
    private ReactiveRedisTemplate<String, String> mockRedisTemplate() {
        return Mockito.mock(ReactiveRedisTemplate.class);
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

    private byte[][] splitEveryByte(final byte[] data) {
        byte[][] chunks = new byte[data.length][];
        for (int i = 0; i < data.length; i++) {
            chunks[i] = new byte[] {data[i]};
        }
        return chunks;
    }
}
