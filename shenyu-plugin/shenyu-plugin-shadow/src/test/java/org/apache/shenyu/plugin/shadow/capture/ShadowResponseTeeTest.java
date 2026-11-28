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

package org.apache.shenyu.plugin.shadow.capture;

import org.junit.jupiter.api.Test;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DefaultDataBufferFactory;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.http.server.reactive.MockServerHttpResponse;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * test case for {@link ShadowResponseTee}.
 */
public final class ShadowResponseTeeTest {

    private final DefaultDataBufferFactory factory = new DefaultDataBufferFactory();

    @Test
    public void testCapturesPublisherBodyWithoutReplacingIt() {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/x").build());
        MockServerHttpResponse primary = exchange.getResponse();
        ShadowBodyCapture capture = new ShadowBodyCapture(16);
        ServerWebExchange observed = ShadowResponseTee.tee(exchange, capture);
        DataBuffer buffer = factory.wrap("hello".getBytes(StandardCharsets.UTF_8));
        StepVerifier.create(observed.getResponse().writeWith(Mono.just(buffer))).verifyComplete();
        assertArrayEquals("hello".getBytes(StandardCharsets.UTF_8), capture.bytes());
        StepVerifier.create(primary.getBodyAsString())
                .expectNext("hello")
                .verifyComplete();
    }

    @Test
    public void testStopsAtBound() {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/x").build());
        ShadowBodyCapture capture = new ShadowBodyCapture(2);
        ServerWebExchange observed = ShadowResponseTee.tee(exchange, capture);
        DataBuffer buffer = factory.wrap("payload".getBytes(StandardCharsets.UTF_8));
        StepVerifier.create(observed.getResponse().writeWith(Mono.just(buffer))).verifyComplete();
        assertEquals(2, capture.bytes().length);
        assertTrue(capture.isTruncated());
    }
}
