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
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DefaultDataBufferFactory;
import org.springframework.http.server.reactive.ServerHttpResponse;
import reactor.core.publisher.Flux;
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
 * Test case for {@link NonCommittingMcpResponseDecorator}.
 */
@ExtendWith(MockitoExtension.class)
class NonCommittingMcpResponseDecoratorTest {

    @Mock
    private ServerHttpResponse delegate;

    private final DefaultDataBufferFactory bufferFactory = new DefaultDataBufferFactory();

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

    private DataBuffer buffer(final String content) {
        return bufferFactory.wrap(content.getBytes(StandardCharsets.UTF_8));
    }
}
