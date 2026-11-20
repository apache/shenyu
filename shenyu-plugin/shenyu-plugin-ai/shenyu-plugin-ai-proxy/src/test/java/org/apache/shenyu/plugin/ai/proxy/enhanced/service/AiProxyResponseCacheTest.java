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

import org.apache.shenyu.common.dto.AgentRequestContext;
import org.apache.shenyu.common.metrics.AiTerminalEvent;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.proxy.enhanced.cache.AiExactResponseCache;
import org.apache.shenyu.plugin.ai.proxy.enhanced.cache.AiResponseCacheProperties;
import org.apache.shenyu.plugin.ai.proxy.enhanced.cache.AiResponseCacheScope;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiChat;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProviderFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAi;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiProxyTransport;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import static org.apache.shenyu.plugin.ai.proxy.enhanced.AiProxyTestUtils.protocolRouteResolver;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Real engine preparation, upstream ownership and observation on misses and hits.
 */
class AiProxyResponseCacheTest {

    private static final String REQUEST = "{\"model\":\"chat-model\",\"messages\":[{\"role\":\"user\",\"content\":\"hello\"}]}";

    private static final String RESPONSE = "{\"id\":\"chatcmpl-test\",\"object\":\"chat.completion\",\"model\":\"chat-model\","
            + "\"choices\":[{\"index\":0,\"message\":{\"role\":\"assistant\",\"content\":\"answer\"},\"finish_reason\":\"stop\"}],"
            + "\"usage\":{\"prompt_tokens\":4,\"completion_tokens\":2,\"total_tokens\":6}}";

    private static final AiResponseCacheScope SCOPE = new AiResponseCacheScope("tenant", "subject", "access", "policy-1", "prompt-1", "selector", "rule");

    private final AtomicInteger upstreamCalls = new AtomicInteger();

    private final List<AiTerminalEvent> terminals = new ArrayList<>();

    private AiCommonConfig config;

    private AiExactResponseCache cache;

    private AiProxyEngine engine;

    @BeforeEach
    void setUp() {
        AiResponseCacheProperties properties = new AiResponseCacheProperties();
        properties.setEnabled(true);
        cache = new AiExactResponseCache(properties);
        config = new AiCommonConfig();
        config.setProvider("OpenAI");
        config.setProtocol(OpenAiChat.NAME);
        config.setBaseUrl("https://api.example.org");
        config.setApiKey("test-only-key");
        config.setTemperature(null);
        config.setStream(false);
        engine = engine(request -> {
            upstreamCalls.incrementAndGet();
            return Mono.just(response(Flux.just(RESPONSE.getBytes(StandardCharsets.UTF_8))));
        });
    }

    @Test
    void hitPreservesBodyButDoesNotCreateAnUpstreamAttemptOrReportOldUsage() {
        assertEquals(RESPONSE, call(SCOPE));
        assertEquals(RESPONSE, call(SCOPE));
        assertEquals(1, upstreamCalls.get());
        assertEquals(2, terminals.size());
        assertEquals(1, terminals.get(0).attemptCount());
        assertEquals(0, terminals.get(1).attemptCount());
        assertEquals(org.apache.shenyu.common.metrics.AiUsage.UNKNOWN, terminals.get(1).usage());
    }

    @Test
    void providerOverridesCredentialsAndEndpointPartitionTheKey() {
        call(SCOPE);
        config.setModel("other-model");
        call(SCOPE);
        config.setTemperature(0.1);
        call(SCOPE);
        config.setApiKey("other-test-only-key");
        call(SCOPE);
        config.setBaseUrl("https://other.example.org");
        call(SCOPE);
        call(SCOPE);
        assertEquals(5, upstreamCalls.get());
    }

    @Test
    void diagnosticIdentityIsNotCacheAuthorization() {
        AgentRequestContext diagnostic = new AgentRequestContext("id", "llm", "selector", "rule").withIdentity("tenant", "subject");
        for (int index = 0; index < 2; index++) {
            engine.execute(REQUEST, config, Optional.empty(), response -> response.getBody().then())
                    .contextWrite(context -> context.put(AgentRequestContext.REACTOR_KEY, diagnostic)).block();
        }
        assertEquals(2, upstreamCalls.get());
    }

    @Test
    void approvedScopesDoNotShareAcrossTenantSubjectOrRoute() {
        call(SCOPE);
        call(new AiResponseCacheScope("other", "subject", "access", "policy-1", "prompt-1", "selector", "rule"));
        call(new AiResponseCacheScope("tenant", "other", "access", "policy-1", "prompt-1", "selector", "rule"));
        call(new AiResponseCacheScope("tenant", "subject", "access", "policy-1", "prompt-1", "selector", "other"));
        call(SCOPE);
        assertEquals(4, upstreamCalls.get());
    }

    @Test
    void truncatedSseAndSynthesizedCompletionAreNeverCached() {
        config.setStream(true);
        engine = engine(request -> {
            upstreamCalls.incrementAndGet();
            return Mono.just(new AiUpstreamResponse(200, Map.of("Content-Type", List.of("text/event-stream")),
                    Flux.just("data: {\"choices\":[]}\n\n".getBytes(StandardCharsets.UTF_8))));
        });
        call(SCOPE);
        call(SCOPE);
        assertEquals(2, upstreamCalls.get());
        assertEquals(0, cache.stats().writes());
    }

    @Test
    void fallbackConfiguredRequestsNeverReadOrPopulatePrimaryCache() {
        call(SCOPE);
        for (int index = 0; index < 2; index++) {
            engine.execute(REQUEST, config, Optional.of(config), response -> response.getBody().then())
                    .contextWrite(context -> context.put(AiResponseCacheScope.REACTOR_KEY, SCOPE)).block();
        }
        assertEquals(3, upstreamCalls.get());
    }

    @Test
    void cacheHitStillHonorsTheRequestDeadline() {
        call(SCOPE);
        StepVerifier.withVirtualTime(() -> engine.execute(REQUEST, config, Optional.empty(), response ->
                response.getBody().then(Mono.never()))
                .contextWrite(context -> context.put(AiResponseCacheScope.REACTOR_KEY, SCOPE)))
                .thenAwait(new AiProxyRetryProperties().getRequestDeadline().plusSeconds(1))
                .verifyError(org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiTransportException.class);
        assertEquals(1, upstreamCalls.get());
    }

    @Test
    void responseWriteFailureDoesNotCacheOrRetryAnEmittedResponse() {
        StepVerifier.create(engine.execute(REQUEST, config, Optional.empty(), response ->
                response.getBody().then(Mono.error(new IllegalStateException("downstream write failed"))))
                .contextWrite(context -> context.put(AiResponseCacheScope.REACTOR_KEY, SCOPE)))
                .verifyError(IllegalStateException.class);
        assertEquals(RESPONSE, call(SCOPE));
        assertEquals(2, upstreamCalls.get());
    }

    @Test
    void cancelledResponseStillCancelsTheOwnedUpstreamAndDoesNotPublish() {
        AtomicInteger cancelled = new AtomicInteger();
        engine = engine(request -> {
            upstreamCalls.incrementAndGet();
            return Mono.just(response(Flux.just(RESPONSE.getBytes(StandardCharsets.UTF_8))
                    .concatWith(Flux.never()).doOnCancel(cancelled::incrementAndGet)));
        });
        StepVerifier.create(engine.execute(REQUEST, config, Optional.empty(), response -> response.getBody().then())
                .contextWrite(context -> context.put(AiResponseCacheScope.REACTOR_KEY, SCOPE)))
                .thenCancel().verify();
        assertEquals(1, cancelled.get());
        assertEquals(0, cache.stats().writes());
    }

    private AiProxyEngine engine(final AiProxyTransport transport) {
        return new AiProxyEngine(new AiProxyProtocolFactory(List.of(new OpenAiChat())),
                new AiProxyProviderFactory(List.of(new OpenAi())), protocolRouteResolver(), transport,
                new AiProxyRetryProperties(), terminals::add, event -> { }, cache);
    }

    private AiUpstreamResponse response(final Flux<byte[]> body) {
        return new AiUpstreamResponse(200, Map.of("Content-Type", List.of("application/json")), body);
    }

    private String call(final AiResponseCacheScope scope) {
        AtomicReference<String> result = new AtomicReference<>();
        engine.execute(REQUEST, config, Optional.empty(), response -> response.getBody()
                .map(bytes -> new String(bytes, StandardCharsets.UTF_8)).collectList()
                .doOnNext(parts -> result.set(String.join("", parts))).then())
                .contextWrite(context -> context.put(AiResponseCacheScope.REACTOR_KEY, scope)).block();
        return result.get();
    }
}
