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

import org.apache.shenyu.common.metrics.AiAttemptEvent;
import org.apache.shenyu.common.metrics.AiCallObserver;
import org.apache.shenyu.common.metrics.AiCallOutcome;
import org.apache.shenyu.common.metrics.AiStreamCompletion;
import org.apache.shenyu.common.metrics.AiTerminalEvent;
import org.apache.shenyu.common.metrics.AiUsage;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiProxyException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiStreamCompletionProperties;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiChat;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiResponses;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProviderFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAi;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAiCompatible;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Stream;

import static org.apache.shenyu.plugin.ai.proxy.enhanced.AiProxyTestUtils.protocolRouteResolver;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AiStreamCompletionTest {

    private static final String BODY = "{\"messages\":[{\"role\":\"user\",\"content\":\"fixture\"}]}";

    @ParameterizedTest
    @MethodSource("compatibilityCases")
    void compatibilityRetainsFramingButNeverUpgradesRawEvidence(final String wire, final AiStreamCompletion.Evidence evidence,
                                                               final AiStreamCompletion.Framing framing) {
        Probe probe = new Probe(AiStreamCompletionProperties.Mode.COMPATIBILITY, Flux.just(bytes(wire)));
        List<String> output = new ArrayList<>();
        probe.execute(output).block();
        assertEquals(evidence, probe.attempts.get(0).completion().evidence());
        assertEquals(framing, probe.attempts.get(0).completion().framing());
        assertEquals(probe.attempts.get(0).completion(), probe.terminals.get(0).completion());
        assertEquals(AiCallOutcome.SUCCESS, probe.terminals.get(0).outcome(), "legacy outcome is transport/HTTP completion only");
        assertEquals(AiUsage.Status.UNKNOWN, probe.terminals.get(0).usage().status(), "a framing marker invents no usage");
        assertEquals(1, output.stream().filter("data: [DONE]\n\n"::equals).count());
        assertEquals(evidence == AiStreamCompletion.Evidence.UPSTREAM_MARKER ? AiProxyAttemptEvent.Reason.COMPLETED
                : AiProxyAttemptEvent.Reason.STREAM_COMPLETION_UNCONFIRMED, probe.reasons.get(probe.reasons.size() - 1));
        assertEquals(1, probe.calls.get());
    }

    @ParameterizedTest
    @ValueSource(strings = {"", "data: {}\n\n", "data: {\"choices\":[{\"index\":0,\"finish_reason\":\"stop\"}]}\n\n", "data: {}"})
    void strictModeFailsMissingMarkersWithoutSynthesizingOrReplaying(final String wire) {
        Probe probe = new Probe(AiStreamCompletionProperties.Mode.STRICT, Flux.just(bytes(wire)));
        List<String> output = new ArrayList<>();
        StepVerifier.create(probe.execute(output)).expectErrorMatches(error -> error instanceof AiProxyException failure
                && failure.getKind() == AiProxyException.Kind.PROTOCOL).verify();
        assertFalse(output.contains("data: [DONE]\n\n"));
        assertEquals(AiStreamCompletion.Framing.NONE, probe.attempts.get(0).completion().framing());
        assertEquals(AiCallOutcome.ERROR, probe.terminals.get(0).outcome());
        assertEquals(1, probe.calls.get(), "a configured fallback must not replay a protocol failure");
    }

    @Test
    void strictModeAcceptsSplitUtf8AndMarkerAndCancelsTheRemainingSource() {
        String wire = "data: {\"choices\":[{\"delta\":{\"content\":\"你好\"}}]}\r\n\r\ndata: [DONE]\r\n\r\n";
        List<byte[]> parts = new ArrayList<>();
        for (byte value : bytes(wire)) {
            parts.add(new byte[]{value});
        }
        AtomicBoolean cancelled = new AtomicBoolean();
        Probe probe = new Probe(AiStreamCompletionProperties.Mode.STRICT,
                Flux.fromIterable(parts).concatWith(Flux.never()).doOnCancel(() -> cancelled.set(true)));
        List<String> output = new ArrayList<>();
        probe.execute(output).block(Duration.ofSeconds(5));
        assertTrue(cancelled.get());
        assertEquals(2, output.size());
        assertTrue(output.get(0).contains("你好"));
        assertEquals(AiStreamCompletion.Evidence.UPSTREAM_MARKER, probe.attempts.get(0).completion().evidence());
        assertEquals(AiStreamCompletion.Framing.PASSTHROUGH_MARKER, probe.attempts.get(0).completion().framing());
    }

    @Test
    void transportFailureAndMalformedPartialEventDoNotAcquireSyntheticSuccessOrReplay() {
        IllegalStateException disconnected = new IllegalStateException("private upstream text");
        Probe transport = new Probe(AiStreamCompletionProperties.Mode.COMPATIBILITY,
                Flux.just(bytes("data: {}\n\n")).concatWith(Flux.error(disconnected)));
        List<String> output = new ArrayList<>();
        StepVerifier.create(transport.execute(output)).expectErrorMatches(error -> error == disconnected).verify();
        assertEquals(AiStreamCompletion.Evidence.TRANSPORT_FAILURE, transport.attempts.get(0).completion().evidence());
        assertEquals(AiStreamCompletion.Framing.NONE, transport.attempts.get(0).completion().framing());
        assertEquals(List.of("data: {}\n\n"), output);
        assertEquals(1, transport.calls.get());
        Probe malformed = new Probe(AiStreamCompletionProperties.Mode.COMPATIBILITY, Flux.just(bytes("data: {\"truncated\":")));
        StepVerifier.create(malformed.execute(new ArrayList<>())).expectError(AiProxyException.class).verify();
        assertEquals(AiStreamCompletion.Evidence.INCOMPLETE, malformed.attempts.get(0).completion().evidence());
        assertEquals(AiStreamCompletion.Framing.NONE, malformed.attempts.get(0).completion().framing());
        assertEquals(AiCallOutcome.ERROR, malformed.terminals.get(0).outcome());
    }

    @Test
    void cancellationHasNoMarkerAndRepeatedConcurrentSubscriptionsOwnSeparateState() {
        Probe never = new Probe(AiStreamCompletionProperties.Mode.COMPATIBILITY, Flux.never());
        StepVerifier.create(never.execute(new ArrayList<>())).thenCancel().verify();
        assertEquals(AiCallOutcome.CANCELLED, never.terminals.get(0).outcome());
        assertEquals(AiStreamCompletion.Evidence.SOURCE_CANCELLED, never.attempts.get(0).completion().evidence());
        assertEquals(AiStreamCompletion.Framing.NONE, never.attempts.get(0).completion().framing());
        Probe probe = new Probe(AiStreamCompletionProperties.Mode.COMPATIBILITY, Flux.defer(() -> Flux.just(bytes("data: {}\n\n"))));
        Mono<Void> cold = probe.execute(new CopyOnWriteArrayList<>());
        Mono.when(cold.subscribeOn(reactor.core.scheduler.Schedulers.parallel()), cold.subscribeOn(reactor.core.scheduler.Schedulers.parallel())).block();
        assertEquals(2, probe.calls.get());
        assertEquals(2, probe.attempts.size());
        assertNotEquals(probe.terminals.get(0).context().getRequestId(), probe.terminals.get(1).context().getRequestId());
        assertTrue(probe.attempts.stream().allMatch(event -> event.completion().evidence() == AiStreamCompletion.Evidence.UNKNOWN));
    }

    @Test
    void convertedResponsesAndUninspectableRawEventsCannotInventUpstreamEvidence() {
        Probe converted = new Probe(AiStreamCompletionProperties.Mode.COMPATIBILITY, Flux.just(bytes(
                "data: {\"id\":\"fixture\",\"object\":\"chat.completion.chunk\",\"choices\":[{\"index\":0,\"delta\":{\"content\":\"hello\"}}]}\n\n")));
        AiCommonConfig config = config();
        config.setProtocol(OpenAiResponses.NAME);
        config.setProvider("fixture-chat");
        converted.engine.execute("{\"input\":\"hello\"}", config, Optional.empty(), response -> response.getBody().then()).block();
        assertEquals(AiStreamCompletion.Evidence.UNKNOWN, converted.attempts.get(0).completion().evidence());
        assertEquals(AiUsage.Status.UNKNOWN, converted.attempts.get(0).usage().status());
        String oversized = "data: {\"opaque\":\"" + "x".repeat(AiUpstreamUsageTap.MAX_BYTES) + "\"}\n\ndata: [DONE]\n\n";
        Probe bounded = new Probe(AiStreamCompletionProperties.Mode.COMPATIBILITY, Flux.just(bytes(oversized)));
        bounded.execute(new ArrayList<>()).block();
        assertEquals(AiStreamCompletion.Evidence.UNKNOWN, bounded.attempts.get(0).completion().evidence());
        assertEquals(AiUsage.Status.LIMIT_EXCEEDED, bounded.attempts.get(0).usage().status());
        assertEquals(AiStreamCompletion.Framing.PASSTHROUGH_MARKER, bounded.attempts.get(0).completion().framing());
    }

    private static Stream<Arguments> compatibilityCases() {
        return Stream.of(
                Arguments.of("", AiStreamCompletion.Evidence.UNKNOWN, AiStreamCompletion.Framing.SYNTHESIZED_MARKER),
                Arguments.of("data: {}\n\n", AiStreamCompletion.Evidence.UNKNOWN, AiStreamCompletion.Framing.SYNTHESIZED_MARKER),
                Arguments.of("data: {\"choices\":[{\"index\":0,\"finish_reason\":\"stop\"}]}\n\n",
                        AiStreamCompletion.Evidence.FINISH_REASON, AiStreamCompletion.Framing.SYNTHESIZED_MARKER),
                Arguments.of("data: {}", AiStreamCompletion.Evidence.INCOMPLETE, AiStreamCompletion.Framing.SYNTHESIZED_MARKER),
                Arguments.of("data: [DONE]\n\n", AiStreamCompletion.Evidence.UPSTREAM_MARKER, AiStreamCompletion.Framing.PASSTHROUGH_MARKER));
    }

    private static byte[] bytes(final String value) {
        return value.getBytes(StandardCharsets.UTF_8);
    }

    private static AiCommonConfig config() {
        AiCommonConfig config = new AiCommonConfig();
        config.setProvider("OpenAI");
        config.setBaseUrl("https://fixture.invalid");
        config.setApiKey("fixture-key");
        config.setModel("fixture");
        config.setStream(true);
        return config;
    }

    private static final class Probe implements AiCallObserver {

        private final List<AiAttemptEvent> attempts = new CopyOnWriteArrayList<>();

        private final List<AiTerminalEvent> terminals = new CopyOnWriteArrayList<>();

        private final List<AiProxyAttemptEvent.Reason> reasons = new CopyOnWriteArrayList<>();

        private final AtomicInteger calls = new AtomicInteger();

        private final AiProxyEngine engine;

        private Probe(final AiStreamCompletionProperties.Mode mode, final Flux<byte[]> body) {
            engine = new AiProxyEngine(new AiProxyProtocolFactory(List.of(new OpenAiChat(mode), new OpenAiResponses())),
                    new AiProxyProviderFactory(List.of(new OpenAi(), new OpenAiCompatible("fixture-chat"))), protocolRouteResolver(), request -> {
                        calls.incrementAndGet();
                        return Mono.just(new AiUpstreamResponse(200, Map.of(), body));
                    }, new AiProxyRetryProperties(1, 1, Duration.ZERO), this, event -> reasons.add(event.reason()));
        }

        private Mono<Void> execute(final List<String> output) {
            return engine.execute(BODY, config(), Optional.of(config()), response -> response.getBody()
                    .doOnNext(bytes -> output.add(new String(bytes, StandardCharsets.UTF_8))).then());
        }

        @Override
        public void record(final AiTerminalEvent event) {
            terminals.add(event);
        }

        @Override
        public void recordAttempt(final AiAttemptEvent event) {
            attempts.add(event);
        }
    }
}
