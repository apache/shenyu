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


package org.apache.shenyu.plugin.response.strategy;

import io.netty.buffer.ByteBufAllocator;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.NettyDataBuffer;
import org.springframework.core.io.buffer.NettyDataBufferFactory;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;

class WebClientChainCleanupTest {

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void releasesUnsubscribedBodyAfterReactiveOrSynchronousChainFailure(final boolean synchronous) {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/"));
        NettyDataBuffer buffer = new NettyDataBufferFactory(ByteBufAllocator.DEFAULT).allocateBuffer();
        AtomicInteger subscriptions = new AtomicInteger();
        Flux<DataBuffer> body = Flux.<DataBuffer>just(buffer).doOnSubscribe(subscription -> subscriptions.incrementAndGet());
        exchange.getAttributes().put(Constants.CLIENT_RESPONSE_ATTR, ResponseEntity.ok(body));
        ShenyuPluginChain chain = ignored -> {
            if (synchronous) {
                throw new IllegalStateException("chain failed");
            }
            return Mono.error(new IllegalStateException("chain failed"));
        };

        StepVerifier.create(new WebClientMessageWriter().writeWith(exchange, chain)).expectErrorMessage("chain failed").verify();

        assertEquals(1, subscriptions.get());
        assertEquals(0, buffer.getNativeBuffer().refCnt());
    }

    @Test
    void releasesUnsubscribedBodyWhenChainIsCancelled() {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/"));
        NettyDataBuffer buffer = new NettyDataBufferFactory(ByteBufAllocator.DEFAULT).allocateBuffer();
        AtomicInteger subscriptions = new AtomicInteger();
        Flux<DataBuffer> body = Flux.<DataBuffer>just(buffer).doOnSubscribe(subscription -> subscriptions.incrementAndGet());
        exchange.getAttributes().put(Constants.CLIENT_RESPONSE_ATTR, ResponseEntity.ok(body));

        StepVerifier.create(new WebClientMessageWriter().writeWith(exchange, ignored -> Mono.never())).thenCancel().verify();

        assertEquals(1, subscriptions.get());
        assertEquals(0, buffer.getNativeBuffer().refCnt());
    }

    @Test
    void preservesFailureWhenNoUpstreamResponseExists() {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/"));
        StepVerifier.create(new WebClientMessageWriter().writeWith(exchange, ignored -> Mono.error(new IllegalStateException("chain failed"))))
                .expectErrorMessage("chain failed").verify();
    }
}
