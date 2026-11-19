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
import org.mockito.Mockito;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import reactor.core.Disposable;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.zip.GZIPOutputStream;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Bounded gzip observation for issue #8385: a small compressed chunk must not
 * materialize its full expansion before the usage-inspection budget is checked,
 * and observer exhaustion must stop further inflation.
 */
class GzipBoundedObservationTest {

    private static final Duration TIMEOUT = Duration.ofSeconds(10);

    private static final int BUDGET = 64 * 1024;

    private static final int SLICE_SIZE = 4096;

    @Test
    void highExpansionChunkStopsAtTheObserverBudget() throws IOException {
        // The issue's reproduction: ~8 KiB compressed expands to 8 MiB of ordinary content.
        byte[] payload = new byte[8 * 1024 * 1024];
        Arrays.fill(payload, (byte) 'a');
        byte[] compressed = compress(payload);
        assertTrue(compressed.length < BUDGET, "fixture must be highly compressible");

        long[] fed = {0L};
        int[] maxSlice = {0};
        GzipStreamDecoder decoder = new GzipStreamDecoder();
        try {
            decoder.decode(compressed, slice -> {
                fed[0] += slice.length;
                maxSlice[0] = Math.max(maxSlice[0], slice.length);
                return fed[0] <= BUDGET;
            });
            // The budget is checked per slice, so at most one extra slice may be fed,
            // never the whole 8 MiB expansion.
            assertTrue(fed[0] <= BUDGET + SLICE_SIZE, "observation must stay within budget + one slice, fed=" + fed[0]);
            assertTrue(maxSlice[0] <= SLICE_SIZE, "slices must stay bounded");

            // Exhaustion abandons the decoder: later chunks are not inflated at all.
            long before = fed[0];
            decoder.decode(compressed, slice -> {
                fed[0] += slice.length;
                return true;
            });
            assertEquals(before, fed[0], "an exhausted decoder must not inflate further input");
        } finally {
            decoder.close();
        }
    }

    @Test
    void manySmallChunksStillReassembleTheFullStream() throws IOException {
        byte[] payload = new byte[1024 * 1024];
        for (int i = 0; i < payload.length; i++) {
            payload[i] = (byte) ('a' + i % 26);
        }
        byte[] compressed = compress(payload);

        ByteArrayOutputStream collected = new ByteArrayOutputStream();
        GzipStreamDecoder decoder = new GzipStreamDecoder();
        try {
            int chunkSize = 512;
            for (int offset = 0; offset < compressed.length; offset += chunkSize) {
                byte[] chunk = Arrays.copyOfRange(compressed, offset, Math.min(offset + chunkSize, compressed.length));
                decoder.decode(chunk, slice -> {
                    collected.writeBytes(slice);
                    return true;
                });
            }
        } finally {
            decoder.close();
        }
        assertArrayEquals(payload, collected.toByteArray());
    }

    @Test
    void closeIsIdempotentAndDecodeAfterCloseIsIgnored() throws IOException {
        byte[] compressed = compress("data: {\"usage\":{\"completion_tokens\":12}}\n\n".getBytes(StandardCharsets.UTF_8));
        GzipStreamDecoder decoder = new GzipStreamDecoder();
        AtomicInteger calls = new AtomicInteger();
        decoder.decode(compressed, slice -> {
            calls.incrementAndGet();
            return true;
        });
        assertTrue(calls.get() > 0);
        decoder.close();
        decoder.close();
        decoder.decode(compressed, slice -> {
            calls.incrementAndGet();
            return true;
        });
        assertEquals(1, calls.get(), "a closed decoder must not observe further input");
    }

    @Test
    void malformedCompressionAbandonsObservationWithoutFailingTheResponse() throws IOException {
        // A valid gzip header followed by bytes that are not a valid deflate stream.
        byte[] broken = new byte[32];
        System.arraycopy(new byte[] {0x1f, (byte) 0x8b, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, (byte) 0xff}, 0, broken, 0, 10);
        Arrays.fill(broken, 10, broken.length, (byte) 0xff);

        GzipStreamDecoder decoder = new GzipStreamDecoder();
        try {
            AtomicInteger calls = new AtomicInteger();
            decoder.decode(broken, slice -> {
                calls.incrementAndGet();
                return true;
            });
            // Observation is abandoned, not propagated as an error; later chunks stay ignored.
            decoder.decode(compress("data: {}\n\n".getBytes(StandardCharsets.UTF_8)), slice -> {
                calls.incrementAndGet();
                return true;
            });
            assertEquals(0, calls.get());
        } finally {
            decoder.close();
        }
    }

    @Test
    void oversizedGzipBodyReportsUnknownUsageAndForwardsBytesUnchanged() throws IOException {
        // Usage sits beyond the inspection budget: the oversized policy must report
        // unavailable usage (nothing recorded), not zero, and never buffer the whole body.
        byte[] padding = new byte[2 * BUDGET];
        Arrays.fill(padding, (byte) 'a');
        byte[] body = new byte[padding.length + 64];
        System.arraycopy(padding, 0, body, 0, padding.length);
        byte[] tail = "{\"usage\":{\"completion_tokens\":12}}".getBytes(StandardCharsets.UTF_8);
        System.arraycopy(tail, 0, body, padding.length, tail.length);
        byte[] compressed = compress(body);

        MockServerWebExchange exchange = gzipExchange(MediaType.APPLICATION_JSON);
        List<Long> recorded = new ArrayList<>();
        AiTokenLimiterPlugin.AiStatisticServerHttpResponse observer = new AiTokenLimiterPlugin.AiStatisticServerHttpResponse(
                exchange, exchange.getResponse(), recorded::add);
        observer.writeWith(Flux.just(exchange.getResponse().bufferFactory().wrap(compressed))).block(TIMEOUT);

        assertTrue(recorded.isEmpty(), "usage beyond the budget must stay unavailable, not be recorded as zero");
        assertArrayEquals(compressed, forwarded(exchange));
    }

    @Test
    void gzipStreamWithinBudgetStillRecordsUsage() throws IOException {
        byte[] compressed = compress("data: {\"usage\":{\"completion_tokens\":12}}\n\ndata: [DONE]\n\n"
                .getBytes(StandardCharsets.UTF_8));
        MockServerWebExchange exchange = gzipExchange(MediaType.TEXT_EVENT_STREAM);
        List<Long> recorded = new ArrayList<>();
        AiTokenLimiterPlugin.AiStatisticServerHttpResponse observer = new AiTokenLimiterPlugin.AiStatisticServerHttpResponse(
                exchange, exchange.getResponse(), recorded::add);
        byte[][] chunks = splitEvery(compressed, 7);
        observer.writeAndFlushWith(Flux.fromArray(chunks).map(chunk ->
                Mono.just(exchange.getResponse().bufferFactory().wrap(chunk)))).block(TIMEOUT);

        assertEquals(List.of(12L), recorded);
        assertArrayEquals(compressed, forwarded(exchange));
    }

    @Test
    void decoderIsClosedExactlyOnceOnSuccessErrorAndCancellation() throws IOException {
        byte[] compressed = compress("data: {\"usage\":{\"completion_tokens\":12}}\n\n".getBytes(StandardCharsets.UTF_8));

        // Success path.
        GzipStreamDecoder successDecoder = Mockito.spy(new GzipStreamDecoder());
        MockServerWebExchange successExchange = gzipExchange(MediaType.TEXT_EVENT_STREAM);
        observerWithDecoder(successExchange, successDecoder, new ArrayList<>())
                .writeWith(Flux.just(successExchange.getResponse().bufferFactory().wrap(compressed))).block(TIMEOUT);
        Mockito.verify(successDecoder, Mockito.times(1)).close();

        // Error path: the body fails mid-stream.
        GzipStreamDecoder errorDecoder = Mockito.spy(new GzipStreamDecoder());
        MockServerWebExchange errorExchange = gzipExchange(MediaType.TEXT_EVENT_STREAM);
        List<Throwable> errors = new ArrayList<>();
        observerWithDecoder(errorExchange, errorDecoder, new ArrayList<>())
                .writeWith(Flux.concat(
                        Mono.just(errorExchange.getResponse().bufferFactory().wrap(compressed)),
                        Mono.error(new IllegalStateException("body failed"))))
                .subscribe(unused -> { }, errors::add);
        assertEquals(1, errors.size());
        Mockito.verify(errorDecoder, Mockito.times(1)).close();

        // Cancellation path: the subscriber disposes before the body completes.
        GzipStreamDecoder cancelDecoder = Mockito.spy(new GzipStreamDecoder());
        MockServerWebExchange cancelExchange = gzipExchange(MediaType.TEXT_EVENT_STREAM);
        cancelExchange.getResponse().setWriteHandler(body -> body.doOnNext(DataBufferUtils::release).then());
        Disposable subscription = observerWithDecoder(cancelExchange, cancelDecoder, new ArrayList<>())
                .writeWith(Flux.concat(
                        Mono.just(cancelExchange.getResponse().bufferFactory().wrap(compressed)),
                        Mono.never()))
                .subscribe();
        subscription.dispose();
        Mockito.verify(cancelDecoder, Mockito.times(1)).close();
    }

    private AiTokenLimiterPlugin.AiStatisticServerHttpResponse observerWithDecoder(final MockServerWebExchange exchange,
                                                                                   final GzipStreamDecoder decoder,
                                                                                   final List<Long> recorded) {
        AiTokenLimiterPlugin.AiStatisticServerHttpResponse observer = Mockito.spy(
                new AiTokenLimiterPlugin.AiStatisticServerHttpResponse(exchange, exchange.getResponse(), recorded::add));
        Mockito.doReturn(decoder).when(observer).createDecoder();
        return observer;
    }

    private MockServerWebExchange gzipExchange(final MediaType mediaType) {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/"));
        exchange.getResponse().getHeaders().setContentType(mediaType);
        exchange.getResponse().getHeaders().set(HttpHeaders.CONTENT_ENCODING, "gzip");
        return exchange;
    }

    private byte[] forwarded(final MockServerWebExchange exchange) {
        return DataBufferUtils.join(exchange.getResponse().getBody()).map(buffer -> {
            byte[] bytes = new byte[buffer.readableByteCount()];
            buffer.read(bytes);
            DataBufferUtils.release(buffer);
            return bytes;
        }).block(TIMEOUT);
    }

    private byte[] compress(final byte[] payload) throws IOException {
        ByteArrayOutputStream compressedStream = new ByteArrayOutputStream();
        try (GZIPOutputStream gzipOutputStream = new GZIPOutputStream(compressedStream)) {
            gzipOutputStream.write(payload);
        }
        return compressedStream.toByteArray();
    }

    private byte[][] splitEvery(final byte[] data, final int chunkSize) {
        int count = (data.length + chunkSize - 1) / chunkSize;
        byte[][] chunks = new byte[count][];
        for (int i = 0; i < count; i++) {
            chunks[i] = Arrays.copyOfRange(data, i * chunkSize, Math.min((i + 1) * chunkSize, data.length));
        }
        return chunks;
    }
}
