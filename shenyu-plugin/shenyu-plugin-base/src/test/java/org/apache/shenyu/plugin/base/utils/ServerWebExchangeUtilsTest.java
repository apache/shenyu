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

package org.apache.shenyu.plugin.base.utils;

import io.netty.buffer.ByteBuf;
import io.netty.buffer.EmptyByteBuf;
import io.netty.buffer.PooledByteBufAllocator;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.core.io.buffer.DataBufferFactory;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.core.io.buffer.NettyDataBuffer;
import org.springframework.core.io.buffer.NettyDataBufferFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.codec.ServerCodecConfigurer;
import org.springframework.http.server.reactive.ServerHttpResponseDecorator;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Pooled-buffer ownership of rewritten request bodies.
 */
public final class ServerWebExchangeUtilsTest {

    private final List<ByteBuf> allocations = new ArrayList<>();

    private final NettyDataBufferFactory factory = new NettyDataBufferFactory(PooledByteBufAllocator.DEFAULT) {
        @Override
        public NettyDataBuffer allocateBuffer(final int initialCapacity) {
            NettyDataBuffer buffer = super.allocateBuffer(initialCapacity);
            allocations.add(buffer.getNativeBuffer());
            return buffer;
        }
    };

    @AfterEach
    public void releaseTestBuffers() {
        allocations.stream().filter(buffer -> buffer.refCnt() > 0 && !(buffer instanceof EmptyByteBuf)).forEach(ByteBuf::release);
    }

    @Test
    public void testShortCircuitDoesNotAllocateRewrittenBody() {
        ServerWebExchange rewritten = rewrite(exchange("original", StandardCharsets.UTF_8), body -> Mono.just(body + " changed"));
        rewritten.getResponse().setStatusCode(HttpStatus.FORBIDDEN);
        rewritten.getResponse().setComplete().block();

        assertTrue(allocations.isEmpty(), "An unread rewritten body must not allocate a pooled buffer");
    }

    @ParameterizedTest
    @ValueSource(strings = {"UTF-8", "UTF-16LE", "ISO-8859-1"})
    public void testForwardingPreservesBodyCharsetAndHeaders(final String charsetName) {
        Charset charset = Charset.forName(charsetName);
        ServerWebExchange original = exchange("original", charset);
        ServerWebExchange rewritten = rewrite(original, body -> Mono.just(body + "-é"));

        assertEquals("original-é", readBody(rewritten, charset));
        assertEquals(original.getRequest().getHeaders().getContentType(), rewritten.getRequest().getHeaders().getContentType());
        assertEquals("kept", rewritten.getRequest().getHeaders().getFirst("X-Test"));
        assertEquals(-1, rewritten.getRequest().getHeaders().getContentLength());
        assertEquals("chunked", rewritten.getRequest().getHeaders().getFirst(HttpHeaders.TRANSFER_ENCODING));
        assertEquals(1, allocations.size());
        assertBuffersReleased();
    }

    @Test
    public void testRepeatedSubscriptionsEncodeFreshBuffers() {
        AtomicInteger conversions = new AtomicInteger();
        ServerWebExchange rewritten = rewrite(exchange("original", StandardCharsets.UTF_8), body -> {
            conversions.incrementAndGet();
            return Mono.just(body + " changed");
        });

        assertEquals("original changed", readBody(rewritten, StandardCharsets.UTF_8));
        assertEquals("original changed", readBody(rewritten, StandardCharsets.UTF_8));
        assertEquals(1, conversions.get(), "Re-reading the transformed body must not repeat conversion side effects");
        assertEquals(2, allocations.size());
        assertBuffersReleased();
    }

    @Test
    public void testCancellationBeforeBodyDemandDoesNotAllocate() {
        ServerWebExchange rewritten = rewrite(exchange("original", StandardCharsets.UTF_8), body -> Mono.just(body + " changed"));

        StepVerifier.create(rewritten.getRequest().getBody(), 0).thenCancel().verify();

        assertTrue(allocations.isEmpty());
    }

    @Test
    public void testCancellationAfterConsumptionReleasesBuffer() {
        ServerWebExchange rewritten = rewrite(exchange("original", StandardCharsets.UTF_8), Mono::just);

        StepVerifier.create(rewritten.getRequest().getBody(), 0)
                .thenRequest(1)
                .assertNext(buffer -> {
                    assertEquals("original", buffer.toString(StandardCharsets.UTF_8));
                    DataBufferUtils.release(buffer);
                }).thenCancel().verify();

        assertEquals(1, allocations.size());
        assertBuffersReleased();
    }

    @Test
    public void testCancellationDuringConversionDoesNotAllocate() {
        StepVerifier.create(ServerWebExchangeUtils.rewriteRequestBody(exchange("original", StandardCharsets.UTF_8),
                ServerCodecConfigurer.create().getReaders(), body -> Mono.never())).thenCancel().verify();

        assertTrue(allocations.isEmpty());
    }

    @Test
    public void testConversionFailureIsPreservedWithoutAllocation() {
        IllegalStateException failure = new IllegalStateException("conversion failed");
        StepVerifier.create(ServerWebExchangeUtils.rewriteRequestBody(exchange("original", StandardCharsets.UTF_8),
                ServerCodecConfigurer.create().getReaders(), body -> Mono.error(failure)))
                .expectErrorSatisfies(error -> assertSame(failure, error))
                .verify();

        assertTrue(allocations.isEmpty());
    }

    @Test
    public void testEmptyInputCanBeRewritten() {
        ServerWebExchange rewritten = rewrite(exchange("", StandardCharsets.UTF_8), body -> Mono.just(body + "replacement"));

        assertEquals("replacement", readBody(rewritten, StandardCharsets.UTF_8));
        assertBuffersReleased();
    }

    @Test
    public void testEmptyOutputCanBeRead() {
        ServerWebExchange rewritten = rewrite(exchange("original", StandardCharsets.UTF_8), body -> Mono.just(""));

        assertEquals("", readBody(rewritten, StandardCharsets.UTF_8));
        assertBuffersReleased();
    }

    private ServerWebExchange exchange(final String body, final Charset charset) {
        byte[] bytes = body.getBytes(charset);
        MockServerWebExchange original = MockServerWebExchange.from(MockServerHttpRequest.post("/test")
                .contentType(new MediaType("text", "plain", charset)).contentLength(bytes.length).header("X-Test", "kept")
                .body(Flux.just(new org.springframework.core.io.buffer.DefaultDataBufferFactory().wrap(bytes))));
        return original.mutate().response(new ServerHttpResponseDecorator(original.getResponse()) {
            @Override
            public DataBufferFactory bufferFactory() {
                return factory;
            }
        }).build();
    }

    private ServerWebExchange rewrite(final ServerWebExchange exchange, final Function<String, Mono<String>> convert) {
        return ServerWebExchangeUtils.rewriteRequestBody(exchange, ServerCodecConfigurer.create().getReaders(), convert).block(Duration.ofSeconds(5));
    }

    private String readBody(final ServerWebExchange exchange, final Charset charset) {
        return DataBufferUtils.join(exchange.getRequest().getBody()).map(buffer -> {
            try {
                return buffer.toString(charset);
            } finally {
                DataBufferUtils.release(buffer);
            }
        }).defaultIfEmpty("").block(Duration.ofSeconds(5));
    }

    private void assertBuffersReleased() {
        // EmptyByteBuf is a shared, unreleasable zero-capacity buffer, not a pooled allocation.
        allocations.stream().filter(buffer -> !(buffer instanceof EmptyByteBuf)).forEach(buffer -> assertEquals(0, buffer.refCnt()));
    }
}
