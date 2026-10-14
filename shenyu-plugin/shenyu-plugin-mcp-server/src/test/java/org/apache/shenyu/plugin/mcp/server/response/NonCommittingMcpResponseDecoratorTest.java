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

package org.apache.shenyu.plugin.mcp.server.response;

import com.google.gson.JsonObject;
import io.netty.buffer.PooledByteBufAllocator;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DefaultDataBufferFactory;
import org.springframework.core.io.buffer.NettyDataBuffer;
import org.springframework.core.io.buffer.NettyDataBufferFactory;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.mock.http.server.reactive.MockServerHttpResponse;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

/**
 * Test cases for {@link NonCommittingMcpResponseDecorator}.
 */
@ExtendWith(MockitoExtension.class)
public final class NonCommittingMcpResponseDecoratorTest {

    @Mock
    private ServerHttpResponse delegate;

    private final DefaultDataBufferFactory bufferFactory = new DefaultDataBufferFactory();

    // ---------------------------------------------------------------
    // Chunks are accumulated incrementally instead of via collectList
    // ---------------------------------------------------------------

    @Test
    void testWriteWithAccumulatesChunksAndCompletesFuture() throws Exception {
        final CompletableFuture<String> future = new CompletableFuture<>();
        final NonCommittingMcpResponseDecorator decorator =
                new NonCommittingMcpResponseDecorator(delegate, "session-1", future, null);

        decorator.writeWith(Flux.just(buffer("part-1,"), buffer("part-2"))).block();

        assertEquals("part-1,part-2", future.get(5, TimeUnit.SECONDS));
        verify(delegate, never()).writeWith(any());
    }

    @Test
    void testWriteAndFlushWithProcessesInnerPublishersInOrder() throws Exception {
        final CompletableFuture<String> future = new CompletableFuture<>();
        final NonCommittingMcpResponseDecorator decorator =
                new NonCommittingMcpResponseDecorator(delegate, "session-1", future, null);

        decorator.writeAndFlushWith(Flux.just(Flux.just(buffer("part-1,")), Flux.just(buffer("part-2")))).block();

        assertEquals("part-1,part-2", future.get(5, TimeUnit.SECONDS));
        verify(delegate, never()).writeAndFlushWith(any());
    }

    @Test
    void testWriteWithAppliesResponseTemplateOnAccumulatedBody() throws Exception {
        final JsonObject contentTemplate = new JsonObject();
        contentTemplate.addProperty("type", "text");
        contentTemplate.addProperty("text", "${result}");
        final JsonObject responseTemplate = new JsonObject();
        responseTemplate.add("content", contentTemplate);

        final CompletableFuture<String> future = new CompletableFuture<>();
        final NonCommittingMcpResponseDecorator decorator =
                new NonCommittingMcpResponseDecorator(delegate, "session-1", future, responseTemplate);

        decorator.writeWith(Flux.just(buffer("{\"result\":"), buffer("\"ok\"}"))).block();

        assertEquals("{\"content\":{\"type\":\"text\",\"text\":\"ok\"}}", future.get(5, TimeUnit.SECONDS));
    }

    @Test
    void testWriteWithCompletesFutureExceptionallyOnError() {
        final CompletableFuture<String> future = new CompletableFuture<>();
        final NonCommittingMcpResponseDecorator decorator =
                new NonCommittingMcpResponseDecorator(delegate, "session-1", future, null);

        StepVerifier.create(decorator.writeWith(Flux.error(new IllegalStateException("boom"))))
                .verifyError(IllegalStateException.class);

        assertTrue(future.isCompletedExceptionally());
    }

    @Test
    void testSetCompleteCompletesFutureWithEmptyBody() throws Exception {
        final CompletableFuture<String> future = new CompletableFuture<>();
        final NonCommittingMcpResponseDecorator decorator =
                new NonCommittingMcpResponseDecorator(delegate, "session-1", future, null);

        StepVerifier.create(decorator.setComplete()).verifyComplete();

        assertEquals("", future.get(5, TimeUnit.SECONDS));
    }

    // ---------------------------------------------
    // Emitted buffers must always be released again
    // ---------------------------------------------

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void releasesCollectedBufferOnUpstreamError(final boolean flush) {
        NettyDataBuffer buffer = new NettyDataBufferFactory(PooledByteBufAllocator.DEFAULT).allocateBuffer();
        buffer.write("partial".getBytes(StandardCharsets.UTF_8));
        CompletableFuture<String> future = new CompletableFuture<>();
        NonCommittingMcpResponseDecorator decorator = new NonCommittingMcpResponseDecorator(new MockServerHttpResponse(), "session", future, null);
        IllegalStateException failure = new IllegalStateException("upstream failed");
        Flux<NettyDataBuffer> body = Flux.concat(Mono.just(buffer), Mono.error(failure));

        StepVerifier.create(flush ? decorator.writeAndFlushWith(Flux.just(body)) : decorator.writeWith(body))
                .expectErrorMatches(error -> error == failure).verify();

        assertEquals(0, buffer.getNativeBuffer().refCnt());
        assertTrue(future.isCompletedExceptionally());
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void releasesCollectedBufferOnCancellation(final boolean flush) {
        NettyDataBuffer buffer = new NettyDataBufferFactory(PooledByteBufAllocator.DEFAULT).allocateBuffer();
        buffer.write("partial".getBytes(StandardCharsets.UTF_8));
        NonCommittingMcpResponseDecorator decorator = new NonCommittingMcpResponseDecorator(new MockServerHttpResponse(), "session", new CompletableFuture<>(), null);
        Flux<NettyDataBuffer> body = Flux.concat(Mono.just(buffer), Mono.never());

        StepVerifier.create(flush ? decorator.writeAndFlushWith(Flux.just(body)) : decorator.writeWith(body))
                .thenCancel().verify();

        assertEquals(0, buffer.getNativeBuffer().refCnt());
    }

    @Test
    public void testWriteWithReleasesCollectedBuffers() {
        NettyDataBufferFactory nettyBufferFactory = new NettyDataBufferFactory(PooledByteBufAllocator.DEFAULT);
        NettyDataBuffer buffer = (NettyDataBuffer) nettyBufferFactory.wrap("response".getBytes(StandardCharsets.UTF_8));
        CompletableFuture<String> responseFuture = new CompletableFuture<>();
        NonCommittingMcpResponseDecorator decorator = new NonCommittingMcpResponseDecorator(
                new MockServerHttpResponse(), "session", responseFuture, null);

        decorator.writeWith(Mono.just(buffer)).block();

        assertEquals("response", responseFuture.join());
        assertEquals(0, buffer.getNativeBuffer().refCnt());
    }

    private DataBuffer buffer(final String content) {
        return bufferFactory.wrap(content.getBytes(StandardCharsets.UTF_8));
    }
}
