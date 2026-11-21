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
import org.apache.shenyu.common.metrics.AiAttemptEvent;
import org.apache.shenyu.common.metrics.AiCallObserver;
import org.apache.shenyu.common.metrics.AiCallOutcome;
import org.apache.shenyu.common.metrics.AiRequestObservation;
import org.apache.shenyu.common.metrics.AiTerminalEvent;
import org.apache.shenyu.common.metrics.AiUsage;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiChat;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiResponses;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProviderFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAi;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiProxyTransport;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiTransportException;
import org.junit.jupiter.api.Test;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.SignalType;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.apache.shenyu.plugin.ai.proxy.enhanced.AiProxyTestUtils.protocolRouteResolver;

class AiProxyObservationTest {

    private static final String BODY = "{\"messages\":[{\"role\":\"user\",\"content\":\"hello\"}]}";

    @Test
    void configuredRetryLimitRetainsAttemptAndTerminalObservation() throws InterruptedException {
        final Sink sink = new Sink();
        final AtomicInteger calls = new AtomicInteger();
        final AiProxyEngine engine = new AiProxyEngine(new AiProxyProtocolFactory(List.of(new OpenAiChat())),
                new AiProxyProviderFactory(List.of(new OpenAi())),
                protocolRouteResolver(),
                request -> Mono.just(response(calls.incrementAndGet() <= 2 ? 503 : 200, Flux.just(bytes("{}")))),
                new AiProxyRetryProperties(1, 0, Duration.ZERO), sink);
        engine.execute(BODY, config(false, "primary"), Optional.of(config(false, "fallback")), result -> result.getBody().then()).block();
        assertTrue(sink.finished.await(5, TimeUnit.SECONDS));
        assertEquals(3, calls.get());
        assertEquals(List.of(AiAttemptEvent.Kind.PRIMARY, AiAttemptEvent.Kind.RETRY, AiAttemptEvent.Kind.FALLBACK),
                sink.attempts.stream().map(AiAttemptEvent::kind).toList());
        assertEquals(1, sink.events.size());
        assertEquals(3, sink.events.get(0).attemptCount());
        assertEquals(AiCallOutcome.SUCCESS, sink.events.get(0).outcome());
    }

    @Test
    void observedTlsFailureDoesNotRetryOrFallback() throws InterruptedException {
        final Sink sink = new Sink();
        final AtomicInteger calls = new AtomicInteger();
        final AiProxyEngine engine = engine(request -> {
            calls.incrementAndGet();
            return Mono.error(new AiTransportException(AiTransportException.Kind.TLS_FAILURE, "fixture", null));
        }, sink);
        StepVerifier.create(engine.execute(BODY, config(false, "primary"), Optional.of(config(false, "fallback")),
                result -> result.getBody().then())).expectError(AiTransportException.class).verify();
        assertTrue(sink.finished.await(5, TimeUnit.SECONDS));
        assertEquals(1, calls.get());
        assertEquals(1, sink.attempts.size());
        assertEquals(AiAttemptEvent.Kind.PRIMARY, sink.attempts.get(0).kind());
        assertEquals(1, sink.events.size());
    }

    @Test
    void observedNonRetryableStatusForwardsOriginalBodyWithoutFallback() {
        final Sink sink = new Sink();
        final AtomicInteger calls = new AtomicInteger();
        final AiProxyEngine engine = engine(request -> {
            calls.incrementAndGet();
            return Mono.just(response(401, Flux.just(bytes("unauthorized"))));
        }, sink);
        engine.execute(BODY, config(false, "primary"), Optional.of(config(false, "fallback")), result -> {
            assertEquals(401, result.getStatusCode());
            return result.getBody().doOnNext(body -> assertEquals("unauthorized", new String(body, StandardCharsets.UTF_8))).then();
        }).block();
        assertEquals(1, calls.get());
        assertEquals(1, sink.attempts.size());
        assertEquals(1, sink.events.size());
    }

    @Test
    void recordsFourPrimaryAttemptsThenFallbackAndOnlyItsUsageAtLogicalEnd() throws InterruptedException {
        final Sink sink = new Sink();
        final AtomicInteger calls = new AtomicInteger();
        final AtomicInteger subscriptions = new AtomicInteger();
        final AiProxyEngine engine = engine(request -> Mono.just(response(calls.incrementAndGet() <= 4 ? 429 : 200, Flux.defer(() -> {
            subscriptions.incrementAndGet();
            return Flux.just(bytes("{\"choices\":[],\"usage\":{\"prompt_tokens\":" + calls.get() + "}}"));
        }))), sink);
        engine.execute(BODY, config(false, "primary"), Optional.of(config(false, "fallback")), result -> result.getBody().then()).block();
        assertTrue(sink.finished.await(5, TimeUnit.SECONDS));
        assertEquals(5, calls.get());
        assertEquals(5, subscriptions.get());
        assertEquals(List.of(AiAttemptEvent.Kind.PRIMARY, AiAttemptEvent.Kind.RETRY, AiAttemptEvent.Kind.RETRY,
                AiAttemptEvent.Kind.RETRY, AiAttemptEvent.Kind.FALLBACK), sink.attempts.stream().map(AiAttemptEvent::kind).toList());
        assertEquals(List.of(1, 2, 3, 4, 5), sink.attempts.stream().map(AiAttemptEvent::attemptNumber).toList());
        assertEquals(AiCallOutcome.RATE_LIMITED, sink.attempts.get(0).outcome());
        assertEquals(1L, sink.attempts.get(0).usage().promptTokens());
        assertEquals(1, sink.events.size());
        assertEquals(5L, sink.events.get(0).usage().promptTokens());
        assertNull(sink.events.get(0).usage().totalTokens());
        assertEquals("fallback", sink.events.get(0).context().getTarget());
        assertEquals(AiCallOutcome.SUCCESS, sink.events.get(0).outcome());
        assertEquals(1, sink.attempts.stream().map(event -> event.context().getRequestId()).distinct().count());
    }

    @Test
    void inheritedGatewayOwnerAloneFinishesLogicalRequest() {
        final Sink sink = new Sink();
        final AgentRequestContext base = new AgentRequestContext("trusted-request", "llm", "selector", "rule").withIdentity("tenant", "subject");
        final AiRequestObservation owner = new AiRequestObservation(sink, base);
        final AiProxyEngine engine = engine(request -> Mono.deferContextual(view -> {
            AgentRequestContext context = view.get(AgentRequestContext.REACTOR_KEY);
            assertEquals("subject", context.getSubject());
            assertEquals("actual", context.getTarget());
            return Mono.just(response(200, Flux.just(bytes("{}"))));
        }), sink);
        engine.execute(BODY, config(false, "actual"), Optional.empty(), result -> result.getBody().then())
                .contextWrite(context -> context.put(AiRequestObservation.REACTOR_KEY, owner)).block();
        assertEquals(1, sink.attempts.size());
        assertTrue(sink.events.isEmpty());
        owner.finish(SignalType.ON_COMPLETE, 200);
        assertEquals(1, sink.events.size());
        assertEquals("actual", sink.events.get(0).context().getTarget());
        assertNull(sink.events.get(0).context().getProtocolVersion());
        assertEquals(OpenAiChat.NAME, sink.attempts.get(0).protocol());
        assertNull(base.getTarget());
    }

    @Test
    void cancellationAfterRawUsageProducesOneAttemptAndOneLogicalCancellation() {
        final Sink sink = new Sink();
        final AtomicBoolean cancelled = new AtomicBoolean();
        final AiProxyEngine engine = engine(request -> Mono.just(response(200, Flux.just(bytes("data: {\"usage\":{\"total_tokens\":8}}\n\n"))
                .concatWith(Flux.never()).doOnCancel(() -> cancelled.set(true)))), sink);
        final AtomicInteger chunks = new AtomicInteger();
        StepVerifier.create(engine.execute(BODY, config(true, "main"), Optional.empty(), result -> result.getBody().doOnNext(chunk -> chunks.incrementAndGet()).then()))
                .then(() -> assertEquals(1, chunks.get())).thenCancel().verify();
        assertTrue(cancelled.get());
        assertEquals(1, sink.attempts.size());
        assertEquals(AiCallOutcome.CANCELLED, sink.attempts.get(0).outcome());
        assertEquals(1, sink.events.size());
        assertEquals(AiCallOutcome.CANCELLED, sink.events.get(0).outcome());
        assertEquals(8L, sink.events.get(0).usage().totalTokens());
    }

    @Test
    void preparationRejectionDoesNotFabricateUpstreamAttempt() {
        final Sink sink = new Sink();
        final AiProxyEngine engine = engine(request -> Mono.error(new AssertionError("must not call transport")), sink);
        StepVerifier.create(engine.execute("not json", config(false, "main"), Optional.empty(), result -> result.getBody().then())).expectError().verify();
        assertTrue(sink.attempts.isEmpty());
        assertEquals(0, sink.events.get(0).attemptCount());
        assertEquals(AiCallOutcome.REJECTED, sink.events.get(0).outcome());
    }

    @Test
    void responseConversionCannotFabricateRawUsageAndSubscriptionsAreIsolated() {
        final Sink sink = new Sink();
        final AiProxyEngine engine = engine(request -> Mono.just(response(200, Flux.just(bytes("{\"id\":\"fixture\",\"choices\":[]}")))), sink);
        final AiCommonConfig config = config(false, "main");
        config.setProtocol(OpenAiResponses.NAME);
        final Mono<Void> execution = engine.execute("{\"input\":\"hello\"}", config, Optional.empty(), result -> result.getBody().then());
        execution.block();
        execution.block();
        assertEquals(2, sink.events.size());
        assertNotEquals(sink.events.get(0).context().getRequestId(), sink.events.get(1).context().getRequestId());
        assertNull(sink.events.get(0).context().getTenant());
        assertNull(sink.events.get(0).context().getSubject());
        assertEquals(AiUsage.Status.UNKNOWN, sink.events.get(0).usage().status());
    }

    @Test
    void exhaustedTransportTimeoutHasOneTerminalAndNoReportedUsage() throws InterruptedException {
        final Sink sink = new Sink();
        final AiProxyEngine engine = engine(request -> Mono.error(new AiTransportException(
                AiTransportException.Kind.RESPONSE_HEADER_TIMEOUT, "private upstream message", null)), sink);
        StepVerifier.create(engine.execute(BODY, config(false, "main"), Optional.empty(), result -> result.getBody().then())).expectError(AiTransportException.class).verify();
        assertTrue(sink.finished.await(5, TimeUnit.SECONDS));
        assertEquals(4, sink.attempts.size());
        assertFalse(sink.attempts.stream().anyMatch(event -> event.outcome() != AiCallOutcome.TIMEOUT));
        assertEquals(1, sink.events.size());
        assertEquals(AiCallOutcome.TIMEOUT, sink.events.get(0).outcome());
        assertEquals(AiUsage.Status.UNKNOWN, sink.events.get(0).usage().status());
    }

    @Test
    void cancellingRetryBackoffDoesNotInventAnotherAttempt() {
        final Sink sink = new Sink();
        StepVerifier.withVirtualTime(() -> {
            AiProxyEngine engine = new AiProxyEngine(new AiProxyProtocolFactory(List.of(new OpenAiChat())), new AiProxyProviderFactory(List.of(new OpenAi())),
                    protocolRouteResolver(),
                    request -> Mono.just(response(503, Flux.just(bytes("{}")))), Duration.ofSeconds(30), sink);
            return engine.execute(BODY, config(false, "main"), Optional.empty(), result -> result.getBody().then());
        }).then(() -> assertEquals(1, sink.attempts.size())).thenCancel().verify();
        assertEquals(1, sink.events.size());
        assertEquals(1, sink.events.get(0).attemptCount());
        assertEquals(AiCallOutcome.CANCELLED, sink.events.get(0).outcome());
    }

    @Test
    void throwingObserversDoNotChangeResponseOrRetryPolicy() {
        final AtomicInteger calls = new AtomicInteger();
        final AiCallObserver observer = new AiCallObserver() {
            @Override
            public void record(final AiTerminalEvent event) {
                throw new IllegalStateException("private observer text");
            }

            @Override
            public void recordAttempt(final AiAttemptEvent event) {
                throw new IllegalStateException("private observer text");
            }
        };
        final AiProxyEngine engine = engine(request -> Mono.just(response(calls.incrementAndGet() <= 1 ? 503 : 200, Flux.just(bytes("{}")))), observer);
        StepVerifier.create(engine.execute(BODY, config(false, "main"), Optional.empty(), result -> result.getBody().then())).verifyComplete();
        assertEquals(2, calls.get());
    }

    private AiProxyEngine engine(final AiProxyTransport transport, final AiCallObserver observer) {
        return new AiProxyEngine(new AiProxyProtocolFactory(List.of(new OpenAiChat(), new OpenAiResponses())),
                new AiProxyProviderFactory(List.of(new OpenAi())), protocolRouteResolver(), transport, Duration.ZERO, observer);
    }

    private AiCommonConfig config(final boolean streaming, final String model) {
        final AiCommonConfig config = new AiCommonConfig();
        config.setProvider("OpenAI");
        config.setBaseUrl("https://fixture.invalid");
        config.setApiKey("fixture-key");
        config.setModel(model);
        config.setStream(streaming);
        return config;
    }

    private AiUpstreamResponse response(final int status, final Flux<byte[]> body) {
        return new AiUpstreamResponse(status, Map.of(), body);
    }

    private byte[] bytes(final String text) {
        return text.getBytes(StandardCharsets.UTF_8);
    }

    private static final class Sink implements AiCallObserver {

        private final List<AiAttemptEvent> attempts = new ArrayList<>();

        private final List<AiTerminalEvent> events = new ArrayList<>();

        private final CountDownLatch finished = new CountDownLatch(1);

        @Override
        public void record(final AiTerminalEvent event) {
            events.add(event);
            finished.countDown();
        }

        @Override
        public void recordAttempt(final AiAttemptEvent event) {
            attempts.add(event);
        }
    }
}
