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

import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.junit.jupiter.api.Test;
import reactor.core.Disposable;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for {@link AiResponseConsumer}.
 */
class AiResponseConsumerTest {

    private final AiResponseConsumer responseConsumer = new AiResponseConsumer();

    @Test
    void shouldPreserveFirstBodyElementAndMarkOutputAsEmitted() {
        byte[] first = "first".getBytes(StandardCharsets.UTF_8);
        byte[] second = "second".getBytes(StandardCharsets.UTF_8);
        AiUpstreamResponse response = new AiUpstreamResponse(200, Map.of("Content-Type", List.of("text/event-stream")),
                Flux.just(first, second));
        AtomicBoolean emitted = new AtomicBoolean();
        List<byte[]> received = new ArrayList<>();

        StepVerifier.create(responseConsumer.consume(response,
                        ready -> ready.getBody().doOnNext(received::add).then(), emitted))
                .verifyComplete();

        assertTrue(emitted.get());
        assertEquals(List.of(first, second), received);
    }

    @Test
    void shouldPropagateFirstBodyErrorWithoutInvokingConsumer() {
        IllegalStateException failure = new IllegalStateException("upstream failed");
        AiUpstreamResponse response = new AiUpstreamResponse(200, Map.of(), Flux.error(failure));
        AtomicBoolean emitted = new AtomicBoolean();
        AtomicBoolean invoked = new AtomicBoolean();

        StepVerifier.create(responseConsumer.consume(response, ready -> {
            invoked.set(true);
            return Mono.empty();
        }, emitted)).expectErrorMatches(error -> error == failure).verify();

        assertFalse(invoked.get());
        assertFalse(emitted.get());
    }

    @Test
    void shouldPropagateCancellationToUpstreamBody() {
        AtomicBoolean cancelled = new AtomicBoolean();
        AiUpstreamResponse response = new AiUpstreamResponse(200, Map.of(),
                Flux.<byte[]>never().doOnCancel(() -> cancelled.set(true)));

        Disposable subscription = responseConsumer.consume(response, ready -> ready.getBody().then(), new AtomicBoolean())
                .subscribe();
        subscription.dispose();

        assertTrue(cancelled.get());
    }
}
