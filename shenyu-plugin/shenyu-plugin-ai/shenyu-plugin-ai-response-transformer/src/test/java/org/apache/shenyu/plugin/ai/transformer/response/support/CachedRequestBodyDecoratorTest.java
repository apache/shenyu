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

package org.apache.shenyu.plugin.ai.transformer.response.support;

import org.junit.jupiter.api.Test;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.core.io.buffer.DefaultDataBufferFactory;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Sinks;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Test for CachedRequestBodyDecorator.
 */
class CachedRequestBodyDecoratorTest {

    private static final DefaultDataBufferFactory BUFFER_FACTORY = new DefaultDataBufferFactory();

    @Test
    void testForwardsAndCapturesBody() {
        ServerHttpRequest request = MockServerHttpRequest.post("/orders")
                .body(Flux.just(BUFFER_FACTORY.wrap("{\"customer\":\"alice\"}".getBytes(StandardCharsets.UTF_8))));
        CachedRequestBodyDecorator decorator = new CachedRequestBodyDecorator(request);

        assertEquals("{\"customer\":\"alice\"}", readBody(decorator.getBody()));
        assertEquals("{\"customer\":\"alice\"}", decorator.bodyAsString());
    }

    @Test
    void testCapturesMultipleBuffersInOrder() {
        ServerHttpRequest request = MockServerHttpRequest.post("/orders")
                .body(Flux.just(BUFFER_FACTORY.wrap("{\"id\":".getBytes(StandardCharsets.UTF_8)),
                        BUFFER_FACTORY.wrap("\"A\"}".getBytes(StandardCharsets.UTF_8))));
        CachedRequestBodyDecorator decorator = new CachedRequestBodyDecorator(request);

        assertEquals("{\"id\":\"A\"}", readBody(decorator.getBody()));
        assertEquals("{\"id\":\"A\"}", decorator.bodyAsString());
    }

    @Test
    void testOverflowStillForwardsButDropsSnapshot() {
        ServerHttpRequest request = MockServerHttpRequest.post("/orders")
                .body(Flux.just(BUFFER_FACTORY.wrap("0123456789".getBytes(StandardCharsets.UTF_8))));
        CachedRequestBodyDecorator decorator = new CachedRequestBodyDecorator(request, 4);

        assertEquals("0123456789", readBody(decorator.getBody()));
        assertEquals("", decorator.bodyAsString());
    }

    @Test
    void testEmptyBody() {
        ServerHttpRequest request = MockServerHttpRequest.post("/orders").build();
        CachedRequestBodyDecorator decorator = new CachedRequestBodyDecorator(request);

        assertEquals("", readBody(decorator.getBody()));
        assertEquals("", decorator.bodyAsString());
    }

    @Test
    void testCancellationCapturesOnlyForwardedBytes() {
        Sinks.Many<DataBuffer> bodySink = Sinks.many().unicast().onBackpressureBuffer();
        ServerHttpRequest request = MockServerHttpRequest.post("/orders").body(bodySink.asFlux());
        CachedRequestBodyDecorator decorator = new CachedRequestBodyDecorator(request);

        StepVerifier.create(decorator.getBody())
                .then(() -> bodySink.tryEmitNext(BUFFER_FACTORY.wrap("{\"customer\":".getBytes(StandardCharsets.UTF_8))))
                .assertNext(dataBuffer -> DataBufferUtils.release(dataBuffer))
                .thenCancel()
                .verify();

        assertEquals("{\"customer\":", decorator.bodyAsString());
    }

    private String readBody(final Flux<DataBuffer> body) {
        return body.map(dataBuffer -> {
            byte[] bytes = new byte[dataBuffer.readableByteCount()];
            dataBuffer.read(bytes);
            DataBufferUtils.release(dataBuffer);
            return new String(bytes, StandardCharsets.UTF_8);
        }).collectList().map(parts -> String.join("", parts)).block();
    }
}
