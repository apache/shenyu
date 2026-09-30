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

package org.apache.shenyu.plugin.ai.proxy.enhanced.service;

import org.junit.jupiter.api.Test;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.springframework.ai.openai.api.OpenAiApi;
import org.springframework.ai.openai.api.OpenAiApi.ChatCompletionRequest;
import org.springframework.core.io.buffer.DefaultDataBufferFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.web.reactive.function.client.ClientResponse;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;
import reactor.core.Disposable;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.Optional;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Exercises the real Spring AI stream operators without opening sockets.
 */
class AiProxyStreamCancellationTest {

    private static final String EVENT = """
            data: {"id":"stream-1","object":"chat.completion.chunk","created":0,"model":"fixture","choices":[{"index":0,"delta":{"content":"hello"}}]}

            """;

    @Test
    void testCancelAfterFirstEventReachesRawBody() {
        verifyCancellation(Duration.ZERO);
    }

    @Test
    void testAsynchronousCancelAfterFirstEventReachesRawBody() {
        verifyCancellation(Duration.ofMillis(100));
    }

    private void verifyCancellation(final Duration cancellationDelay) {
        final AtomicBoolean cancelled = new AtomicBoolean();
        final Flux<String> events = Flux.just(EVENT).concatWith(Flux.never()).doOnCancel(() -> cancelled.set(true));
        final OpenAiApi api = createApi(events);
        StepVerifier.create(stream(api))
                .expectNextCount(1)
                .thenAwait(cancellationDelay)
                .thenCancel()
                .verify(Duration.ofSeconds(3));
        assertTrue(cancelled.get(), "Cancellation must reach the raw WebClient response, not only the SDK output");
    }

    @Test
    void testCancellationBeforeFirstEvent() {
        final AtomicBoolean cancelled = new AtomicBoolean();
        final OpenAiApi api = createApi(Flux.<String>never().doOnCancel(() -> cancelled.set(true)));
        StepVerifier.create(stream(api)).thenAwait(Duration.ofMillis(100)).thenCancel().verify(Duration.ofSeconds(3));
        assertTrue(cancelled.get());
    }

    @Test
    void testSharedClientSubscriptionsHaveIndependentCancellation() {
        final List<AtomicBoolean> cancellations = new ArrayList<>();
        final OpenAiApi api = createApi(Flux.defer(() -> {
            final AtomicBoolean cancelled = new AtomicBoolean();
            cancellations.add(cancelled);
            return Flux.just(EVENT).concatWith(Flux.never()).doOnCancel(() -> cancelled.set(true));
        }));
        final Flux<OpenAiApi.ChatCompletionChunk> shared = stream(api);
        final AtomicInteger received = new AtomicInteger();
        final Disposable first = shared.subscribe(chunk -> received.incrementAndGet());
        final Disposable second = shared.subscribe(chunk -> received.incrementAndGet());
        try {
            assertEquals(2, received.get());
            first.dispose();
            assertTrue(cancellations.get(0).get());
            assertFalse(cancellations.get(1).get(), "Cancelling one subscriber must not cancel another on the same cached client");
            second.dispose();
            assertTrue(cancellations.get(1).get());
        } finally {
            first.dispose();
            second.dispose();
        }
    }

    @Test
    void testFallbackCancellationReachesRawBody() {
        final OpenAiApi failing = mock(OpenAiApi.class);
        final ChatCompletionRequest request = mock(ChatCompletionRequest.class);
        when(failing.chatCompletionStream(request)).thenReturn(Flux.error(new IllegalStateException("fixture failure")));
        final AtomicBoolean cancelled = new AtomicBoolean();
        final OpenAiApi fallback = createApi(Flux.just(EVENT).concatWith(Flux.never()).doOnCancel(() -> cancelled.set(true)));
        final AiCommonConfig config = new AiCommonConfig();
        config.setModel("fixture");
        final AiProxyExecutorService.FallbackContext context = new AiProxyExecutorService.FallbackContext(fallback, config);
        StepVerifier.create(new AiProxyExecutorService().executeDirectStream(failing, Optional.of(context), request,
                "{\"messages\":[{\"role\":\"user\",\"content\":\"test\"}]}", true))
                .expectNextCount(1).thenAwait(Duration.ofMillis(100)).thenCancel().verify(Duration.ofSeconds(3));
        assertTrue(cancelled.get());
    }

    @Test
    void testNormalStreamStillCompletes() {
        StepVerifier.create(stream(createApi(Flux.just(EVENT)))).expectNextCount(1).verifyComplete();
    }

    private OpenAiApi createApi(final Flux<String> events) {
        return OpenAiApi.builder().apiKey("fixture-key")
                .webClientBuilder(WebClient.builder().filter(AiStreamCancellation.responseFilter()).exchangeFunction(request -> Mono.just(ClientResponse.create(HttpStatus.OK)
                        .header(HttpHeaders.CONTENT_TYPE, MediaType.TEXT_EVENT_STREAM_VALUE)
                        .body(events.map(event -> DefaultDataBufferFactory.sharedInstance.wrap(event.getBytes(StandardCharsets.UTF_8))))
                        .build())))
                .build();
    }

    private Flux<OpenAiApi.ChatCompletionChunk> stream(final OpenAiApi api) {
        final ChatCompletionRequest request = mock(ChatCompletionRequest.class);
        when(request.stream()).thenReturn(true);
        return new AiProxyExecutorService().executeDirectStream(api, Optional.empty(), request, "{}", true);
    }
}
