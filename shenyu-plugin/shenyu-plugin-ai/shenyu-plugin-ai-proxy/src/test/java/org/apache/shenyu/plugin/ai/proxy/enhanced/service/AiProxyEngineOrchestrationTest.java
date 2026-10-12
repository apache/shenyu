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

import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiProxyException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocol;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiChat;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProvider;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProviderFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAi;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAiCompatible;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiProxyTransport;
import org.junit.jupiter.api.Test;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Retry, fallback, response adaptation and cancellation across the engine boundary.
 */
class AiProxyEngineOrchestrationTest {

    private static final String BODY = "{\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}],\"unknown\":42}";

    @Test
    void normalRequestsRetryThreeTimesAndPreserveTheFinalErrorBody() {
        AtomicInteger calls = new AtomicInteger();
        List<String> received = new ArrayList<>();
        AiProxyEngine engine = engine(request -> {
            calls.incrementAndGet();
            return Mono.just(response(503, Flux.just(bytes("upstream unavailable"))));
        });
        StepVerifier.create(engine.execute(BODY, config(false, "main"), Optional.empty(), response -> {
            assertEquals(503, response.getStatusCode());
            assertEquals(List.of("kept"), response.getHeaders().get("X-Upstream"));
            return consume(response, received);
        })).verifyComplete();
        assertEquals(4, calls.get());
        assertEquals(List.of("upstream unavailable"), received);
    }

    @Test
    void streamingRetriesOnceAndRebuildsFallbackUsingItsProviderConfiguration() {
        List<AiUpstreamRequest> requests = new ArrayList<>();
        AiCommonConfig fallback = config(false, "fallback");
        fallback.setProvider("DeepSeek");
        fallback.setBaseUrl("https://fallback.example/v1");
        fallback.setApiKey("fallback-key");
        fallback.setTemperature(0.7);
        fallback.setMaxTokens(23);
        List<String> received = new ArrayList<>();
        AiProxyEngine engine = engine(request -> {
            requests.add(request);
            return Mono.just(requests.size() <= 2 ? response(429, Flux.just(bytes("busy")))
                    : response(200, Flux.just(bytes("data: {\"vendor\":true}\n\n"))));
        });
        StepVerifier.create(engine.execute(BODY, config(true, "main"), Optional.of(fallback), response -> consume(response, received))).verifyComplete();
        assertEquals(3, requests.size());
        AiUpstreamRequest retried = requests.get(2);
        assertEquals("https://fallback.example/v1/chat/completions", retried.getUri().toString());
        assertEquals(List.of("Bearer fallback-key"), retried.getHeaders().get("Authorization"));
        String json = new String(retried.getBody(), StandardCharsets.UTF_8);
        assertTrue(json.contains("fallback"));
        assertTrue(json.contains("\"temperature\":0.7"));
        assertTrue(json.contains("\"max_tokens\":23"));
        assertTrue(json.contains("\"unknown\":42"));
        assertTrue(json.contains("\"stream\":true"));
        assertEquals(List.of("data: {\"vendor\":true}\n\n", "data: [DONE]\n\n"), received);
    }

    @Test
    void clientErrorsAreForwardedWithoutRetryAndMayUseConfiguredFallback() {
        AtomicInteger calls = new AtomicInteger();
        AiProxyEngine engine = engine(request -> {
            calls.incrementAndGet();
            return Mono.just(response(401, Flux.just(bytes("unauthorized"))));
        });
        StepVerifier.create(engine.execute(BODY, config(false, "main"), Optional.empty(), response -> response.getBody().then())).verifyComplete();
        assertEquals(1, calls.get());
        calls.set(0);
        StepVerifier.create(engine.execute(BODY, config(false, "main"), Optional.of(config(false, "fallback")), response -> response.getBody().then())).verifyComplete();
        assertEquals(2, calls.get());
    }

    @Test
    void retriesTransportFailuresBeforeAnyOutput() {
        AtomicInteger calls = new AtomicInteger();
        List<String> received = new ArrayList<>();
        AiProxyEngine engine = engine(request -> Mono.just(response(200,
                calls.incrementAndGet() == 1 ? Flux.error(networkFailure()) : Flux.just(bytes("data: {\"ok\":true}\n\n")))));
        StepVerifier.create(engine.execute(BODY, config(true, "main"), Optional.empty(), response -> consume(response, received))).verifyComplete();
        assertEquals(2, calls.get());
        assertEquals(List.of("data: {\"ok\":true}\n\n", "data: [DONE]\n\n"), received);
    }

    @Test
    void neverRetriesOrFallsBackAfterBodyEmission() {
        AtomicInteger calls = new AtomicInteger();
        List<String> received = new ArrayList<>();
        AiProxyEngine engine = engine(request -> {
            calls.incrementAndGet();
            return Mono.just(response(200, Flux.just(bytes("data: {\"first\":true}\n\n")).concatWith(Flux.error(networkFailure()))));
        });
        StepVerifier.create(engine.execute(BODY, config(true, "main"), Optional.of(config(true, "fallback")), response -> consume(response, received)))
                .expectError(AiProxyException.class).verify();
        assertEquals(1, calls.get());
        assertEquals(List.of("data: {\"first\":true}\n\n"), received);
    }

    @Test
    void fallbackFailureDoesNotRestartThePrimary() {
        AtomicInteger calls = new AtomicInteger();
        AiProxyEngine engine = engine(request -> {
            calls.incrementAndGet();
            return Mono.error(networkFailure());
        });
        StepVerifier.create(engine.execute(BODY, config(true, "main"), Optional.of(config(true, "fallback")), response -> response.getBody().then()))
                .expectError(AiProxyException.class).verify();
        assertEquals(3, calls.get());
    }

    @Test
    void cancellationBeforeAndAfterTheFirstEventReachesTheActiveTransport() {
        for (boolean firstEvent : List.of(false, true)) {
            AtomicBoolean cancelled = new AtomicBoolean();
            Flux<byte[]> upstream = (firstEvent ? Flux.just(bytes("data: {\"ok\":true}\n\n")).concatWith(Flux.never()) : Flux.<byte[]>never())
                    .doOnCancel(() -> cancelled.set(true));
            AiProxyEngine engine = engine(request -> Mono.just(response(200, upstream)));
            StepVerifier.create(engine.execute(BODY, config(true, "main"), Optional.empty(), response -> response.getBody().then()))
                    .thenAwait(Duration.ofMillis(20)).thenCancel().verify(Duration.ofSeconds(3));
            assertTrue(cancelled.get());
        }
    }

    @Test
    void validationFailuresDoNotExecuteOrFallBack() {
        AtomicInteger calls = new AtomicInteger();
        AiProxyEngine engine = engine(request -> {
            calls.incrementAndGet();
            return Mono.error(networkFailure());
        });
        StepVerifier.create(engine.execute("invalid", config(true, "main"), Optional.of(config(true, "fallback")), response -> response.getBody().then()))
                .expectError(ShenyuException.class).verify();
        assertEquals(0, calls.get());
    }

    @Test
    void providerAndProtocolBothAdaptTheResponse() {
        AtomicBoolean providerAdapted = new AtomicBoolean();
        AtomicBoolean protocolEncoded = new AtomicBoolean();
        AiProxyProvider provider = mock(AiProxyProvider.class);
        when(provider.name()).thenReturn("custom");
        when(provider.supports("openai-chat")).thenReturn(true);
        when(provider.buildRequest(org.mockito.ArgumentMatchers.any(), org.mockito.ArgumentMatchers.any())).thenReturn(new AiUpstreamRequest());
        when(provider.adaptResponse(org.mockito.ArgumentMatchers.any())).thenAnswer(invocation -> {
            providerAdapted.set(true);
            return invocation.getArgument(0);
        });
        AiProxyProtocol protocol = mock(AiProxyProtocol.class);
        when(protocol.name()).thenReturn("openai-chat");
        when(protocol.matches(org.mockito.ArgumentMatchers.any())).thenReturn(true);
        when(protocol.decodeRequest(org.mockito.ArgumentMatchers.any(), org.mockito.ArgumentMatchers.any())).thenAnswer(invocation ->
                new OpenAiChat().decodeRequest(invocation.getArgument(0), invocation.getArgument(1)));
        when(protocol.encodeResponse(org.mockito.ArgumentMatchers.any(), org.mockito.ArgumentMatchers.anyBoolean())).thenAnswer(invocation -> {
            assertTrue(providerAdapted.get());
            protocolEncoded.set(true);
            return invocation.getArgument(0);
        });
        AiCommonConfig config = config(false, "model");
        config.setProvider("custom");
        AiProxyEngine engine = new AiProxyEngine(new AiProxyProtocolFactory(List.of(protocol)), new AiProxyProviderFactory(List.of(provider)),
                request -> Mono.just(response(200, Flux.just(bytes("ok")))), Duration.ZERO);
        StepVerifier.create(engine.execute(BODY, config, Optional.empty(), response -> response.getBody().then())).verifyComplete();
        assertTrue(protocolEncoded.get());
        when(provider.supports("openai-chat")).thenReturn(false);
        providerAdapted.set(false);
        StepVerifier.create(engine.execute(BODY, config, Optional.empty(), response -> response.getBody().then())).expectError(ShenyuException.class).verify();
        assertFalse(providerAdapted.get());
    }

    @Test
    void cancellationDuringFallbackReachesOnlyTheActiveBody() {
        AtomicInteger calls = new AtomicInteger();
        AtomicBoolean cancelled = new AtomicBoolean();
        AiProxyEngine engine = engine(request -> {
            if (calls.incrementAndGet() <= 2) {
                return Mono.error(networkFailure());
            }
            return Mono.just(response(200, Flux.just(bytes("data: {\"fallback\":true}\n\n"))
                    .concatWith(Flux.never()).doOnCancel(() -> cancelled.set(true))));
        });
        StepVerifier.create(engine.execute(BODY, config(true, "main"), Optional.of(config(true, "fallback")), response -> response.getBody().then()))
                .thenAwait(Duration.ofMillis(30)).thenCancel().verify(Duration.ofSeconds(3));
        assertEquals(3, calls.get());
        assertTrue(cancelled.get());
    }

    private AiProxyEngine engine(final AiProxyTransport transport) {
        return new AiProxyEngine(new AiProxyProtocolFactory(List.of(new OpenAiChat())),
                new AiProxyProviderFactory(List.of(new OpenAi(), new OpenAiCompatible("DeepSeek"))), transport, Duration.ZERO);
    }

    private AiCommonConfig config(final boolean stream, final String model) {
        AiCommonConfig config = new AiCommonConfig();
        config.setProvider("OpenAI");
        config.setBaseUrl("https://main.example");
        config.setApiKey("main-key");
        config.setModel(model);
        config.setStream(stream);
        return config;
    }

    private AiUpstreamResponse response(final int status, final Flux<byte[]> body) {
        return new AiUpstreamResponse(status, Map.of("X-Upstream", List.of("kept")), body);
    }

    private Mono<Void> consume(final AiUpstreamResponse response, final List<String> received) {
        return response.getBody().doOnNext(bytes -> received.add(new String(bytes, StandardCharsets.UTF_8))).then();
    }

    private byte[] bytes(final String value) {
        return value.getBytes(StandardCharsets.UTF_8);
    }

    private AiProxyException networkFailure() {
        return new AiProxyException(AiProxyException.Kind.TRANSPORT, "test failure", null);
    }
}
