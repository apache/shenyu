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

import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiChat;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProviderFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAi;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.WebClientAiProxyTransport;
import org.junit.jupiter.api.Test;
import org.springframework.core.io.buffer.DefaultDataBufferFactory;
import org.springframework.http.HttpStatus;
import org.springframework.web.reactive.function.client.ClientResponse;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.Disposable;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Cancellation regressions through raw WebClient transport and Engine response scope.
 */
class AiProxyStreamCancellationTest {

    private static final String EVENT = "data: {\"id\":\"stream-1\",\"vendor\":true}\n\n";

    @Test
    void testCancelAfterFirstEventReachesRawBody() {
        verifyCancellation(Duration.ZERO, true);
    }

    @Test
    void testAsynchronousCancelAfterFirstEventReachesRawBody() {
        verifyCancellation(Duration.ofMillis(100), true);
    }

    @Test
    void testCancellationBeforeFirstEvent() {
        verifyCancellation(Duration.ofMillis(100), false);
    }

    private void verifyCancellation(final Duration delay, final boolean event) {
        AtomicBoolean cancelled = new AtomicBoolean();
        Flux<String> source = (event ? Flux.just(EVENT).concatWith(Flux.never()) : Flux.<String>never()).doOnCancel(() -> cancelled.set(true));
        StepVerifier.create(stream(engine(source), new AtomicInteger())).thenAwait(delay).thenCancel().verify(Duration.ofSeconds(3));
        assertTrue(cancelled.get());
    }

    @Test
    void testSharedClientSubscriptionsHaveIndependentCancellation() {
        List<AtomicBoolean> cancellations = new ArrayList<>();
        AiProxyEngine engine = engine(Flux.defer(() -> {
            AtomicBoolean cancelled = new AtomicBoolean();
            cancellations.add(cancelled);
            return Flux.just(EVENT).concatWith(Flux.never()).doOnCancel(() -> cancelled.set(true));
        }));
        AtomicInteger received = new AtomicInteger();
        Mono<Void> shared = stream(engine, received);
        Disposable first = shared.subscribe();
        Disposable second = shared.subscribe();
        try {
            assertEquals(2, received.get());
            first.dispose();
            assertTrue(cancellations.get(0).get());
            assertFalse(cancellations.get(1).get());
            second.dispose();
            assertTrue(cancellations.get(1).get());
        } finally {
            first.dispose();
            second.dispose();
        }
    }

    @Test
    void testNormalStreamStillCompletes() {
        AtomicInteger received = new AtomicInteger();
        StepVerifier.create(stream(engine(Flux.just(EVENT)), received)).verifyComplete();
        assertEquals(2, received.get());
    }

    private AiProxyEngine engine(final Flux<String> events) {
        WebClient client = WebClient.builder().exchangeFunction(request -> Mono.just(ClientResponse.create(HttpStatus.OK)
                .body(events.map(event -> DefaultDataBufferFactory.sharedInstance.wrap(event.getBytes(StandardCharsets.UTF_8)))).build())).build();
        return new AiProxyEngine(new AiProxyProtocolFactory(List.of(new OpenAiChat())), new AiProxyProviderFactory(List.of(new OpenAi())),
                new WebClientAiProxyTransport(client), Duration.ZERO);
    }

    private Mono<Void> stream(final AiProxyEngine engine, final AtomicInteger received) {
        AiCommonConfig config = new AiCommonConfig();
        config.setProvider("OpenAI");
        config.setBaseUrl("https://upstream.example");
        config.setApiKey("fixture-key");
        config.setStream(true);
        return engine.execute("{\"messages\":[{\"role\":\"user\",\"content\":\"hello\"}]}", config, Optional.empty(),
                response -> response.getBody().doOnNext(bytes -> received.incrementAndGet()).then());
    }
}
