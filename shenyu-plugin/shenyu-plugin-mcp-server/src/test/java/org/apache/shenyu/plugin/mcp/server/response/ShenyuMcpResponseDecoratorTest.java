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

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DefaultDataBufferFactory;
import org.springframework.http.server.reactive.ServerHttpResponse;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.nio.charset.StandardCharsets;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link ShenyuMcpResponseDecorator}.
 */
@ExtendWith(MockitoExtension.class)
class ShenyuMcpResponseDecoratorTest {

    @Mock
    private ServerHttpResponse delegate;

    private final DefaultDataBufferFactory bufferFactory = new DefaultDataBufferFactory();

    @Test
    void testWriteWithCompletesFutureWithAllChunks() throws Exception {
        when(delegate.writeWith(any())).thenAnswer(invocation -> Flux.from(invocation.getArgument(0)).then());

        final CompletableFuture<String> future = new CompletableFuture<>();
        final ShenyuMcpResponseDecorator decorator =
                new ShenyuMcpResponseDecorator(delegate, "session-1", future, null);

        decorator.writeWith(Flux.just(buffer("part-1,"), buffer("part-2"))).block();

        assertEquals("part-1,part-2", future.get(5, TimeUnit.SECONDS));
    }

    @Test
    void testSetCompleteCompletesFutureWithAccumulatedBody() {
        when(delegate.setComplete()).thenReturn(Mono.empty());

        final CompletableFuture<String> future = new CompletableFuture<>();
        final ShenyuMcpResponseDecorator decorator =
                new ShenyuMcpResponseDecorator(delegate, "session-1", future, null);

        decorator.setComplete().block();

        assertEquals("", future.getNow(""));
    }

    private DataBuffer buffer(final String content) {
        return bufferFactory.wrap(content.getBytes(StandardCharsets.UTF_8));
    }
}
