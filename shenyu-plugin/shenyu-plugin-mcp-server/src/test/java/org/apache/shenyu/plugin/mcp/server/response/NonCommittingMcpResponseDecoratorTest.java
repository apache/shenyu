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

import io.netty.buffer.PooledByteBufAllocator;
import org.junit.jupiter.api.Test;
import org.springframework.core.io.buffer.NettyDataBuffer;
import org.springframework.core.io.buffer.NettyDataBufferFactory;
import org.springframework.mock.http.server.reactive.MockServerHttpResponse;
import reactor.core.publisher.Mono;

import java.nio.charset.StandardCharsets;
import java.util.concurrent.CompletableFuture;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Test cases for {@link NonCommittingMcpResponseDecorator}.
 */
public final class NonCommittingMcpResponseDecoratorTest {

    @Test
    public void testWriteWithReleasesCollectedBuffers() {
        NettyDataBufferFactory bufferFactory = new NettyDataBufferFactory(PooledByteBufAllocator.DEFAULT);
        NettyDataBuffer buffer = (NettyDataBuffer) bufferFactory.wrap("response".getBytes(StandardCharsets.UTF_8));
        CompletableFuture<String> responseFuture = new CompletableFuture<>();
        NonCommittingMcpResponseDecorator decorator = new NonCommittingMcpResponseDecorator(
                new MockServerHttpResponse(), "session", responseFuture, null);

        decorator.writeWith(Mono.just(buffer)).block();

        assertEquals("response", responseFuture.join());
        assertEquals(0, buffer.getNativeBuffer().refCnt());
    }
}
