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

import ch.qos.logback.classic.Logger;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.read.ListAppender;
import org.slf4j.LoggerFactory;
import org.apache.shenyu.common.dto.convert.rule.AiStructuredOutputConfig;
import org.apache.shenyu.common.metrics.AiAttemptEvent;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiProxyException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.WebClientAiProxyTransport;
import io.netty.buffer.PooledByteBufAllocator;
import org.springframework.core.io.buffer.NettyDataBuffer;
import org.springframework.core.io.buffer.NettyDataBufferFactory;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.HttpStatus;
import org.springframework.web.reactive.function.client.ClientResponse;
import org.springframework.web.reactive.function.client.WebClient;
import org.apache.shenyu.common.metrics.AiCallObserver;
import org.apache.shenyu.common.metrics.AiCallOutcome;
import org.apache.shenyu.common.metrics.AiTerminalEvent;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiChat;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProviderFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProvider;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProviderCapabilities;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAi;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiProxyTransport;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiTransportException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.junit.jupiter.params.provider.EnumSource;
import org.reactivestreams.Subscription;
import reactor.test.publisher.TestPublisher;
import org.junit.jupiter.params.provider.CsvSource;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.apache.shenyu.plugin.ai.proxy.enhanced.AiProxyTestUtils.protocolRouteResolver;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertFalse;

class AiProxyCredentialFailureFallbackTest {

    private static final String REQUEST = "{\"messages\":[{\"role\":\"user\",\"content\":\"hello\"}]}";

    private static final String REJECTED = "{\"error\":{\"code\":\"credential_revoked\",\"message\":\"private-fixture-message\"}}";

    @ParameterizedTest
    @CsvSource({"401,false", "403,false", "401,true", "403,true"})
    void skipsPrimaryRetriesAndUsesFallbackCredential(final int status, final boolean streaming) {
        List<AiUpstreamRequest> requests = new ArrayList<>();
        AtomicInteger primarySubscriptions = new AtomicInteger();
        AiProxyEngine engine = engine(request -> {
            requests.add(request);
            if ("primary.example".equals(request.getUri().getHost())) {
                return Mono.just(response(status, Flux.defer(() -> {
                    primarySubscriptions.incrementAndGet();
                    return Flux.just(bytes(REJECTED));
                })));
            }
            return Mono.just(new AiUpstreamResponse(200, Map.of("Content-Type", List.of(streaming ? "text/event-stream" : "application/json")),
                    Flux.just(bytes(streaming ? "data: [DONE]\n\n" : "{}"))));
        }, properties(status));
        StepVerifier.create(engine.execute(REQUEST, config("primary", streaming), Optional.of(config("fallback", streaming)), response -> {
            assertEquals(200, response.getStatusCode());
            return response.getBody().then();
        })).verifyComplete();
        assertEquals(2, requests.size());
        assertEquals(1, primarySubscriptions.get());
        assertEquals(List.of("Bearer fixture-primary-secret"), requests.get(0).getHeaders().get("Authorization"));
        assertEquals(List.of("Bearer fixture-fallback-secret"), requests.get(1).getHeaders().get("Authorization"));
    }


    @ParameterizedTest
    @CsvSource({"code,credential_revoked,true", "type,authentication_failed,true", "code,credential_expired,true",
        "type,credential_revoked,false", "code,authentication_failed,false", "type,unknown,false", "both,unused,true"})
    void appliesMultipleFieldRulesBeforeFallback(final String field, final String value, final boolean matched) {
        AiProxyRetryProperties properties = properties(401);
        AiProxyRetryProperties.CredentialFailureDeclaration second = properties(401).getCredentialFailureFallback().getDeclarations().get(0);
        second.setType("error.type");
        second.setValue("authentication_failed");
        AiProxyRetryProperties.CredentialFailureDeclaration third = properties(401).getCredentialFailureFallback().getDeclarations().get(0);
        third.setValue("credential_expired");
        properties.getCredentialFailureFallback().setDeclarations(List.of(properties.getCredentialFailureFallback().getDeclarations().get(0), second, third));
        AtomicInteger calls = new AtomicInteger();
        AtomicInteger subscriptions = new AtomicInteger();
        byte[] original = bytes("both".equals(field)
                ? "{\"error\":{\"code\":\"credential_revoked\",\"type\":\"authentication_failed\"}}"
                : "{\"error\":{\"" + field + "\":\"" + value + "\"}}");
        AiProxyEngine engine = engine(request -> calls.incrementAndGet() == 1
                ? Mono.just(response(401, Flux.defer(() -> {
                    subscriptions.incrementAndGet();
                    return Flux.just(original);
                }))) : Mono.just(response(200, Flux.just(bytes("{}")))), properties);
        StepVerifier.create(engine.execute(REQUEST, config("primary", false), Optional.of(config("fallback", false)), response -> {
            assertEquals(matched ? 200 : 401, response.getStatusCode());
            return response.getBody().doOnNext(body -> assertArrayEquals(matched ? bytes("{}") : original, body)).then();
        })).verifyComplete();
        assertEquals(matched ? 2 : 1, calls.get());
        assertEquals(1, subscriptions.get());
    }

    @ParameterizedTest
    @ValueSource(strings = {"disabled", "no-fallback", "wrong-endpoint", "non-json"})
    void bypassesInspectionWhenNotApplicable(final String scenario) {
        final AiProxyRetryProperties properties = properties(401);
        properties.getCredentialFailureFallback().setEnabled(!"disabled".equals(scenario));
        if ("wrong-endpoint".equals(scenario)) {
            properties.getCredentialFailureFallback().getDeclarations().get(0).setEndpoint("https://other.example/v1/chat/completions");
        }
        final TestPublisher<byte[]> source = TestPublisher.create();
        final AtomicInteger calls = new AtomicInteger();
        final AtomicInteger delivered = new AtomicInteger();
        final AiProxyEngine engine = engine(request -> {
            calls.incrementAndGet();
            return Mono.just(new AiUpstreamResponse(401, Map.of("Content-Type", List.of("non-json".equals(scenario) ? "text/plain" : "application/json")), source.flux()));
        }, properties);
        final Optional<AiCommonConfig> fallback = "no-fallback".equals(scenario) ? Optional.empty() : Optional.of(config("fallback", false));
        StepVerifier.create(engine.execute(REQUEST, config("primary", false), fallback,
                response -> response.getBody().doOnNext(bytes -> delivered.incrementAndGet()).then()))
                .then(() -> source.next(bytes("first")))
                .then(() -> assertEquals(1, delivered.get()))
                .thenCancel().verify();
        source.assertCancelled();
        assertEquals(1, calls.get());
    }

    @ParameterizedTest
    @ValueSource(strings = {"{}", "{", "", "{\"error\":{\"code\":\"permission_denied\"}}",
        "{\"error\":{\"code\":\"credential_revoked\",\"code\":\"credential_revoked\"}}",
        "{\"error\":{\"code\":\"credential_revoked\"}} trailing", "oversized"})
    void forwardsUnknownErrorsIntact(final String value) {
        final byte[] original = bytes("oversized".equals(value) ? REJECTED + " ".repeat(AiProxyCredentialFailureInspector.MAX_INSPECTION_BYTES) : value);
        final AtomicInteger calls = new AtomicInteger();
        final AtomicInteger subscriptions = new AtomicInteger();
        final ByteArrayOutputStream received = new ByteArrayOutputStream();
        final AiProxyEngine engine = engine(request -> {
            calls.incrementAndGet();
            return Mono.just(response(401, Flux.defer(() -> {
                subscriptions.incrementAndGet();
                return Flux.just(original);
            })));
        }, properties(401));
        StepVerifier.create(engine.execute(REQUEST, config("primary", false), Optional.of(config("fallback", false)), response -> {
            assertEquals(401, response.getStatusCode());
            assertEquals(List.of("kept"), response.getHeaders().get("X-Fixture"));
            return response.getBody().doOnNext(received::writeBytes).then();
        })).verifyComplete();
        assertEquals(1, calls.get());
        assertEquals(1, subscriptions.get());
        assertArrayEquals(original, received.toByteArray());
    }

    @ParameterizedTest
    @EnumSource(value = AiProxyRetryProperties.ReplayMode.class, names = {"NEVER", "PROVIDER_DECLARED"})
    void preservesClassifiedBodyWhenReplayIsBlocked(final AiProxyRetryProperties.ReplayMode mode) {
        final AiProxyRetryProperties properties = properties(401);
        properties.getReplay().setResponseStatus(mode);
        final AtomicInteger calls = new AtomicInteger();
        final ByteArrayOutputStream received = new ByteArrayOutputStream();
        final AiProxyEngine engine = engine(request -> {
            calls.incrementAndGet();
            return Mono.just(response(401, Flux.just(bytes(REJECTED))));
        }, properties);
        final String toolRequest = REQUEST.substring(0, REQUEST.length() - 1) + ",\"tools\":[{\"type\":\"function\",\"function\":{\"name\":\"fixture\"}}]}";
        StepVerifier.create(engine.execute(toolRequest, config("primary", false), Optional.of(config("fallback", false)), response -> {
            assertEquals(401, response.getStatusCode());
            return response.getBody().doOnNext(received::writeBytes).then();
        })).verifyComplete();
        assertEquals(1, calls.get());
        assertArrayEquals(bytes(REJECTED), received.toByteArray());
    }

    @ParameterizedTest
    @ValueSource(ints = {401, 403, 503, 0})
    void neverRetriesOrRecursivelySwitchesFallback(final int status) {
        final AtomicInteger calls = new AtomicInteger();
        final AiTransportException failure = networkFailure();
        final AiProxyEngine engine = engine(request -> {
            if (calls.incrementAndGet() == 1) {
                return Mono.just(response(401, Flux.just(bytes(REJECTED))));
            }
            return status == 0 ? Mono.error(failure) : Mono.just(response(status, Flux.just(bytes(REJECTED))));
        }, properties(401));
        final Mono<Void> exchange = engine.execute(REQUEST, config("primary", false), Optional.of(config("fallback", false)), response -> {
            assertEquals(status, response.getStatusCode());
            return response.getBody().then();
        });
        if (status == 0) {
            StepVerifier.create(exchange).expectErrorMatches(error -> error == failure).verify();
        } else {
            StepVerifier.create(exchange).verifyComplete();
        }
        assertEquals(2, calls.get());
    }

    @Test
    void stopsFurtherPrimaryRetriesAfterAnEarlierRetry() {
        final List<String> targets = new ArrayList<>();
        final AiProxyEngine engine = engine(request -> {
            targets.add(request.getUri().getHost());
            return Mono.just(response(targets.size() == 1 ? 503 : targets.size() == 2 ? 401 : 200,
                    Flux.just(bytes(targets.size() == 2 ? REJECTED : "{}"))));
        }, properties(401));
        StepVerifier.create(engine.execute(REQUEST, config("primary", false), Optional.of(config("fallback", false)), response -> {
            assertEquals(200, response.getStatusCode());
            return response.getBody().then();
        })).verifyComplete();
        assertEquals(List.of("primary.example", "primary.example", "fallback.example"), targets);
    }

    @Test
    void cancellationWhileInspectingReachesThePrimaryAndDoesNotStartFallback() {
        final TestPublisher<byte[]> source = TestPublisher.create();
        final AtomicInteger calls = new AtomicInteger();
        final AiProxyEngine engine = engine(request -> {
            calls.incrementAndGet();
            return Mono.just(response(401, source.flux()));
        }, properties(401));
        StepVerifier.create(engine.execute(REQUEST, config("primary", false), Optional.of(config("fallback", false)), response -> response.getBody().then()))
                .then(() -> source.next(bytes(REJECTED))).thenCancel().verify();
        source.assertCancelled();
        assertEquals(1, calls.get());
    }

    @Test
    void cancellationBeforeHeadersReachesOriginalTransport() {
        final AtomicInteger calls = new AtomicInteger();
        final AtomicBoolean cancelled = new AtomicBoolean();
        final AiProxyEngine engine = engine(request -> {
            calls.incrementAndGet();
            return Mono.<AiUpstreamResponse>never().doOnCancel(() -> cancelled.set(true));
        }, properties(401));
        StepVerifier.create(engine.execute(REQUEST, config("primary", false), Optional.of(config("fallback", false)), response -> response.getBody().then()))
                .thenCancel().verify();
        assertTrue(cancelled.get());
        assertEquals(1, calls.get());
    }

    @Test
    void cancellationAtClassificationBoundaryPreventsFallbackSubscription() {
        final TestPublisher<byte[]> source = TestPublisher.create();
        final AtomicInteger calls = new AtomicInteger();
        final AtomicReference<Subscription> subscription = new AtomicReference<>();
        final AiProxyEngine engine = engine(request -> {
            calls.incrementAndGet();
            return Mono.just(response(401, source.flux()));
        }, properties(401), null, event -> {
            if (event.reason() == AiProxyAttemptEvent.Reason.CREDENTIAL_FAILURE) {
                subscription.get().cancel();
            }
        });
        StepVerifier.create(engine.execute(REQUEST, config("primary", false), Optional.of(config("fallback", false)), response -> response.getBody().then())
                        .doOnSubscribe(subscription::set))
                .then(() -> source.next(bytes(REJECTED)).complete()).thenCancel()
                .verifyThenAssertThat().hasNotDroppedErrors();
        assertEquals(1, calls.get());
    }

    @Test
    void cancellationDuringFallbackReachesItsBodyWithoutAnotherAttempt() {
        final TestPublisher<byte[]> source = TestPublisher.create();
        final AtomicInteger calls = new AtomicInteger();
        final AiProxyEngine engine = engine(request -> Mono.just(calls.incrementAndGet() == 1
                ? response(401, Flux.just(bytes(REJECTED))) : response(200, source.flux())), properties(401));
        StepVerifier.create(engine.execute(REQUEST, config("primary", false), Optional.of(config("fallback", false)), response -> response.getBody().then()))
                .then(() -> assertEquals(2, calls.get())).thenCancel().verify();
        source.assertCancelled();
        assertEquals(2, calls.get());
    }

    @Test
    void deadlineCancelsInspectionWithoutAnAdditionalAttempt() {
        final AiProxyRetryProperties properties = properties(401);
        properties.setRequestDeadline(Duration.ofSeconds(1));
        final AtomicInteger calls = new AtomicInteger();
        final AtomicBoolean cancelled = new AtomicBoolean();
        final AiProxyEngine engine = engine(request -> {
            calls.incrementAndGet();
            return Mono.just(response(401, Flux.<byte[]>never().doOnCancel(() -> cancelled.set(true))));
        }, properties);
        StepVerifier.create(engine.execute(REQUEST, config("primary", false), Optional.of(config("fallback", false)), response -> response.getBody().then()))
                .expectError().verify(Duration.ofSeconds(5));
        assertEquals(1, calls.get());
        assertTrue(cancelled.get());
    }

    @Test
    void doesNotReplayBodyReadFailureAfterForwardingAnyPrefix() {
        final AtomicInteger calls = new AtomicInteger();
        final AiTransportException failure = networkFailure();
        final ByteArrayOutputStream received = new ByteArrayOutputStream();
        final AiProxyEngine engine = engine(request -> {
            calls.incrementAndGet();
            return Mono.just(response(401, Flux.just(bytes(REJECTED)).concatWith(Flux.error(failure))));
        }, properties(401));
        StepVerifier.create(engine.execute(REQUEST, config("primary", false), Optional.of(config("fallback", false)),
                response -> response.getBody().doOnNext(received::writeBytes).then()))
                .expectErrorMatches(error -> error == failure).verify();
        assertEquals(1, calls.get());
        assertArrayEquals(bytes(REJECTED), received.toByteArray());
    }

    @Test
    void keepsPrimaryFailureAndBoundedReasonsWhenFallbackSucceeds() {
        final List<AiAttemptEvent> attempts = new ArrayList<>();
        final List<AiTerminalEvent> terminal = new ArrayList<>();
        final List<AiProxyAttemptEvent> reasons = new ArrayList<>();
        final AiCallObserver observer = new AiCallObserver() {
            @Override
            public void record(final AiTerminalEvent event) {
                terminal.add(event);
            }

            @Override
            public void recordAttempt(final AiAttemptEvent event) {
                attempts.add(event);
            }
        };
        final AtomicInteger calls = new AtomicInteger();
        final AiProxyEngine engine = engine(request -> Mono.just(calls.incrementAndGet() == 1
                ? response(401, Flux.just(bytes(REJECTED))) : response(200, Flux.just(bytes("{}")))), properties(401), observer, reasons::add);
        StepVerifier.create(engine.execute(REQUEST, config("primary", false), Optional.of(config("fallback", false)), response -> response.getBody().then())).verifyComplete();
        assertEquals(List.of(401, 200), attempts.stream().map(AiAttemptEvent::httpStatus).toList());
        assertEquals(List.of(AiCallOutcome.REJECTED, AiCallOutcome.SUCCESS), attempts.stream().map(AiAttemptEvent::outcome).toList());
        assertEquals(1, terminal.size());
        assertEquals(AiCallOutcome.SUCCESS, terminal.get(0).outcome());
        assertEquals(1, reasons.stream().filter(event -> event.reason() == AiProxyAttemptEvent.Reason.CREDENTIAL_FAILURE).count());
        assertEquals(1, reasons.stream().filter(event -> event.reason() == AiProxyAttemptEvent.Reason.FALLBACK_CREDENTIAL).count());
        final String rendered = attempts.toString() + terminal + reasons;
        assertFalse(rendered.contains("fixture-primary-secret"));
        assertFalse(rendered.contains("fixture-fallback-secret"));
        assertFalse(rendered.contains("private-fixture-message"));
        assertFalse(rendered.contains("credential_revoked"));
    }

    @Test
    void repairStatusFailureDoesNotUseCredentialFallback() {
        final AiStructuredOutputConfig policy = new AiStructuredOutputConfig();
        policy.setSchema("{\"type\":\"object\",\"required\":[\"answer\"]}");
        policy.setRepairEnabled(true);
        policy.setRepairPrompt("Fix the output");
        policy.setRepairMaxAttempts(1);
        final AiCommonConfig primary = config("primary", false);
        primary.setStructuredOutput(policy);
        final List<AiProxyAttemptEvent> events = new ArrayList<>();
        final AtomicInteger calls = new AtomicInteger();
        final AiProxyEngine engine = engine(request -> Mono.just(calls.incrementAndGet() == 1
                ? response(200, Flux.just(bytes("{\"choices\":[{\"message\":{\"content\":\"{}\"},\"finish_reason\":\"stop\"}]}")))
                : response(401, Flux.just(bytes(REJECTED)))), properties(401), null, events::add);
        StepVerifier.create(engine.execute(REQUEST, primary, Optional.of(config("fallback", false)), response -> response.getBody().then()))
                .expectError(AiProxyException.class).verify();
        assertEquals(2, calls.get());
        assertTrue(events.stream().anyMatch(event -> event.reason() == AiProxyAttemptEvent.Reason.CONTENT_REPAIR));
        assertTrue(events.stream().noneMatch(event -> event.reason() == AiProxyAttemptEvent.Reason.CREDENTIAL_FAILURE
                || event.reason() == AiProxyAttemptEvent.Reason.FALLBACK_CREDENTIAL));
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void realTransportReleasesPooledBuffersOnClassificationOrOversize(final boolean oversized) {
        final NettyDataBufferFactory buffers = new NettyDataBufferFactory(PooledByteBufAllocator.DEFAULT);
        final List<NettyDataBuffer> allocated = new ArrayList<>();
        final AtomicInteger calls = new AtomicInteger();
        final WebClient client = WebClient.builder().exchangeFunction(request -> {
            final boolean primary = calls.incrementAndGet() == 1;
            final byte[] content = bytes(primary ? REJECTED + (oversized ? " ".repeat(AiProxyCredentialFailureInspector.MAX_INSPECTION_BYTES) : "") : "{}");
            final NettyDataBuffer buffer = buffers.allocateBuffer(content.length);
            buffer.write(content);
            allocated.add(buffer);
            return Mono.just(ClientResponse.create(primary ? HttpStatus.UNAUTHORIZED : HttpStatus.OK)
                    .header("Content-Type", "application/json").body(Flux.just(buffer)).build());
        }).build();
        try {
            final AiProxyEngine engine = engine(new WebClientAiProxyTransport(client), properties(401));
            StepVerifier.create(engine.execute(REQUEST, config("primary", false), Optional.of(config("fallback", false)), response -> {
                assertEquals(oversized ? 401 : 200, response.getStatusCode());
                return response.getBody().then();
            })).verifyComplete();
            assertEquals(oversized ? 1 : 2, calls.get());
            allocated.forEach(buffer -> assertEquals(0, buffer.getNativeBuffer().refCnt()));
        } finally {
            allocated.forEach(DataBufferUtils::release);
        }
    }

    @Test
    void credentialFailureCannotBypassFallbackCapabilities() {
        final AiProxyProvider limited = mock(AiProxyProvider.class, CALLS_REAL_METHODS);
        when(limited.name()).thenReturn("limited");
        final AiProviderCapabilities chat = AiProviderCapabilities.chat();
        when(limited.capabilities()).thenReturn(new AiProviderCapabilities(chat.protocolBindings(), Set.of(), chat.features()));
        final AtomicInteger calls = new AtomicInteger();
        final AiProxyEngine engine = new AiProxyEngine(new AiProxyProtocolFactory(List.of(new OpenAiChat())),
                new AiProxyProviderFactory(List.of(new OpenAi(), limited)), protocolRouteResolver(), request -> {
                    calls.incrementAndGet();
                    return Mono.just(response(401, Flux.just(bytes(REJECTED))));
                }, properties(401));
        final AiCommonConfig fallback = config("fallback", false);
        fallback.setProvider("limited");
        StepVerifier.create(engine.execute(REQUEST, config("primary", false), Optional.of(fallback), response -> response.getBody().then()))
                .expectErrorMatches(error -> error.getMessage().contains("operation"))
                .verify();
        assertEquals(1, calls.get());
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void classificationDoesNotLogCredentialsOrRawErrors(final boolean malformed) {
        final Logger logger = (Logger) LoggerFactory.getLogger("org.apache.shenyu.plugin.ai.proxy");
        final ListAppender<ILoggingEvent> messages = new ListAppender<>();
        messages.start();
        logger.addAppender(messages);
        try {
            final AtomicInteger calls = new AtomicInteger();
            final String body = malformed ? "invalid fixture-primary-secret private-fixture-message" : REJECTED;
            final AiProxyEngine engine = engine(request -> Mono.just(calls.incrementAndGet() == 1
                    ? response(401, Flux.just(bytes(body))) : response(200, Flux.just(bytes("{}")))), properties(401));
            StepVerifier.create(engine.execute(REQUEST, config("primary", false), Optional.of(config("fallback", false)), response -> response.getBody().then()))
                    .verifyComplete();
            final String rendered = messages.list.stream().map(ILoggingEvent::getFormattedMessage).reduce("", String::concat);
            assertFalse(rendered.contains("fixture-primary-secret"));
            assertFalse(rendered.contains("fixture-fallback-secret"));
            assertFalse(rendered.contains("private-fixture-message"));
            assertFalse(rendered.contains("credential_revoked"));
        } finally {
            logger.detachAppender(messages);
            messages.stop();
        }
    }

    private AiTransportException networkFailure() {
        return new AiTransportException(AiTransportException.Kind.IO_FAILURE, "fixture read failure", null);
    }

    private AiProxyRetryProperties properties(final int status) {
        final AiProxyRetryProperties properties = new AiProxyRetryProperties(3, 3, Duration.ZERO);
        AiProxyRetryProperties.CredentialFailureDeclaration declaration = new AiProxyRetryProperties.CredentialFailureDeclaration();
        declaration.setEndpoint("https://primary.example/v1/chat/completions");
        declaration.setProtocol(OpenAiChat.NAME);
        declaration.setStatus(status);
        declaration.setValue("credential_revoked");
        properties.getCredentialFailureFallback().setDeclarations(List.of(declaration));
        properties.getCredentialFailureFallback().setEnabled(true);
        return properties;
    }

    private AiProxyEngine engine(final AiProxyTransport transport, final AiProxyRetryProperties properties) {
        return engine(transport, properties, null, event -> {
        });
    }

    private AiProxyEngine engine(final AiProxyTransport transport, final AiProxyRetryProperties properties,
                                 final AiCallObserver observer, final AiProxyAttemptListener listener) {
        return new AiProxyEngine(new AiProxyProtocolFactory(List.of(new OpenAiChat())),
                new AiProxyProviderFactory(List.of(new OpenAi())), protocolRouteResolver(), transport, properties, observer, listener);
    }

    private AiCommonConfig config(final String target, final boolean streaming) {
        AiCommonConfig config = new AiCommonConfig();
        config.setProvider("OpenAI");
        config.setBaseUrl("https://" + target + ".example");
        config.setApiKey("fixture-" + target + "-secret");
        config.setModel(target);
        config.setStream(streaming);
        return config;
    }

    private AiUpstreamResponse response(final int status, final Flux<byte[]> body) {
        return new AiUpstreamResponse(status, Map.of("Content-Type", List.of("application/json"), "X-Fixture", List.of("kept")), body);
    }

    private byte[] bytes(final String text) {
        return text.getBytes(StandardCharsets.UTF_8);
    }
}
