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

package org.apache.shenyu.plugin.ai.proxy.enhanced.protocol;

import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiProxyException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.junit.jupiter.api.Test;
import reactor.core.publisher.Flux;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * OpenAI incremental SSE compatibility tests.
 */
class OpenAiChatResponseTest {

    @Test
    void handlesSplitUtf8CrLfMultilineDataAndOneDoneMarker() {
        String wire = ": keepalive\r\ndata: {\r\ndata: \"text\":\"你好\",\"unknown\":42}\r\n\r\n"
                + "data: [DONE]\r\n\r\ndata: {\"ignored\":true}\n\n";
        List<byte[]> bytes = new ArrayList<>();
        for (byte value : wire.getBytes(StandardCharsets.UTF_8)) {
            bytes.add(new byte[] {value});
        }
        StepVerifier.create(encode(Flux.fromIterable(bytes)))
                .expectNext("data: {\ndata: \"text\":\"你好\",\"unknown\":42}\n\n", "data: [DONE]\n\n")
                .verifyComplete();
    }

    @Test
    void retainsAllEventsInTheSameNetworkChunk() {
        StepVerifier.create(encode(Flux.just(("data: {\"first\":1}\n\ndata: {\"second\":2}\n\ndata: [DONE]\n\n").getBytes(StandardCharsets.UTF_8))))
                .expectNext("data: {\"first\":1}\n\n", "data: {\"second\":2}\n\n", "data: [DONE]\n\n").verifyComplete();
    }

    @Test
    void addsDoneOnlyOnSuccessfulCompletion() {
        StepVerifier.create(encode(Flux.just("data: {\"usage\":{\"total_tokens\":3}}".getBytes(StandardCharsets.UTF_8))))
                .expectNext("data: {\"usage\":{\"total_tokens\":3}}\n\n", "data: [DONE]\n\n").verifyComplete();
        StepVerifier.create(encode(Flux.empty())).expectNext("data: [DONE]\n\n").verifyComplete();
        IllegalStateException failure = new IllegalStateException("upstream closed");
        StepVerifier.create(encode(Flux.just("data: {\"text\":\"hi\"}\n\n".getBytes(StandardCharsets.UTF_8)).concatWith(Flux.error(failure))))
                .expectNext("data: {\"text\":\"hi\"}\n\n").expectErrorMatches(error -> error == failure).verify();
    }

    @Test
    void doesNotDiscardAValidEventBeforeAMalformedEventInTheSameChunk() {
        StepVerifier.create(encode(Flux.just("data: {\"valid\":true}\n\ndata: invalid\n\n".getBytes(StandardCharsets.UTF_8))))
                .expectNext("data: {\"valid\":true}\n\n").expectError(AiProxyException.class).verify();
    }

    @Test
    void doneCancelsAnUpstreamThatRemainsOpen() {
        AtomicBoolean cancelled = new AtomicBoolean();
        StepVerifier.create(encode(Flux.just("data: [DONE]\n\n".getBytes(StandardCharsets.UTF_8)).concatWith(Flux.never()).doOnCancel(() -> cancelled.set(true))))
                .expectNext("data: [DONE]\n\n").expectComplete().verify(Duration.ofSeconds(3));
        assertTrue(cancelled.get());
    }

    @Test
    void keepsNormalAndErrorBodiesAndHeadersUnchanged() {
        for (int status : List.of(200, 400, 500)) {
            AiUpstreamResponse response = new AiUpstreamResponse(status, Map.of("X-Extra", List.of("kept")), Flux.just(new byte[] {1}));
            assertSame(response, new OpenAiChat().encodeResponse(response, status != 200));
        }
        AiUpstreamResponse response = new OpenAiChat().encodeResponse(new AiUpstreamResponse(200,
                Map.of("Content-Length", List.of("42"), "X-Extra", List.of("kept")), Flux.empty()), true);
        assertEquals(List.of("kept"), response.getHeaders().get("X-Extra"));
        assertEquals(List.of("text/event-stream"), response.getHeaders().get("Content-Type"));
        assertTrue(response.getHeaders().keySet().stream().noneMatch(name -> name.equalsIgnoreCase("Content-Length")));
    }

    @Test
    void rejectsAJsonResponseToAStreamingRequestAndReleasesItsBody() {
        AtomicBoolean cancelled = new AtomicBoolean();
        AiUpstreamResponse upstream = new AiUpstreamResponse(200, Map.of("Content-Type", List.of("application/json")),
                Flux.just("{\"unexpected\":true}".getBytes(StandardCharsets.UTF_8)).concatWith(Flux.never()).doOnCancel(() -> cancelled.set(true)));
        StepVerifier.create(new OpenAiChat().encodeResponse(upstream, true).getBody()).expectError(AiProxyException.class).verify();
        assertTrue(cancelled.get());
    }

    private Flux<String> encode(final Flux<byte[]> body) {
        return new OpenAiChat().encodeResponse(new AiUpstreamResponse(200, Map.of(), body), true)
                .getBody().map(bytes -> new String(bytes, StandardCharsets.UTF_8));
    }
}
