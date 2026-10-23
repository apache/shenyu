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

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiProxyException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.PreparedAiRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.ShenyuAiRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProtocolRequestOverrides;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocol;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiChat;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiResponses;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProvider;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProviderFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAi;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAiCompatible;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiProxyTransport;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiTransportException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
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
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.apache.shenyu.plugin.ai.proxy.enhanced.AiProxyTestUtils.protocolRouteResolver;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Retry, fallback, response adaptation and cancellation across the engine boundary.
 */
class AiProxyEngineOrchestrationTest {

    private static final String BODY = "{\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}],\"unknown\":42}";

    @Test
    void convertsResponsesAfterRetryAndFallbackWithoutSharingStreamState() {
        List<AiUpstreamRequest> requests = new ArrayList<>();
        AiCommonConfig fallback = config(false, "fallback");
        fallback.setProvider("DeepSeek");
        fallback.setMaxTokens(23);
        AiProxyEngine engine = engine(request -> {
            requests.add(request);
            return Mono.just(requests.size() <= 2 ? response(503, Flux.just(bytes("busy"))) : response(200,
                    Flux.just(bytes("data: {\"model\":\"fallback\",\"choices\":[{\"index\":0,\"delta\":{\"content\":\"ok\"},\"finish_reason\":\"stop\"}]}\n\n"))));
        });
        AiCommonConfig primary = config(false, "main");
        primary.setProtocol(OpenAiResponses.NAME);
        List<String> received = new ArrayList<>();
        StepVerifier.create(engine.execute("{\"input\":\"hi\",\"stream\":true}", primary, Optional.of(fallback),
                response -> consume(response, received))).verifyComplete();
        assertEquals(3, requests.size());
        assertArrayEquals(requests.get(0).getBody(), requests.get(1).getBody());
        String body = new String(requests.get(2).getBody(), StandardCharsets.UTF_8);
        assertTrue(body.contains("\"model\":\"fallback\""));
        assertTrue(body.contains("\"max_tokens\":23"));
        assertFalse(body.contains("max_completion_tokens"));
        assertEquals(1, received.stream().filter(event -> event.startsWith("event: response.created\n")).count());
        assertEquals(1, received.stream().filter(event -> event.startsWith("event: response.completed\n")).count());
    }

    @Test
    void doesNotRetryResponsesAfterAConvertedEvent() {
        AtomicInteger calls = new AtomicInteger();
        List<String> received = new ArrayList<>();
        AiProxyEngine engine = engine(request -> {
            calls.incrementAndGet();
            return Mono.just(response(200, Flux.just(bytes("data: {\"choices\":[{\"index\":0,\"delta\":{\"content\":\"first\"}}]}\n\n"))
                    .concatWith(Flux.error(networkFailure()))));
        });
        AiCommonConfig primary = config(true, "main");
        primary.setProvider("DeepSeek");
        primary.setProtocol(OpenAiResponses.NAME);
        StepVerifier.create(engine.execute("{\"input\":\"hi\",\"stream\":true}", primary, Optional.of(config(true, "fallback")),
                response -> consume(response, received))).expectError(AiProxyException.class).verify();
        assertEquals(1, calls.get());
        assertTrue(received.stream().anyMatch(event -> event.startsWith("event: response.created\n")));
        assertTrue(received.stream().noneMatch(event -> event.startsWith("event: response.completed\n")));
    }

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
    void configuredRetryCountsControlPrimaryAttempts() {
        AtomicInteger nonStreamingCalls = new AtomicInteger();
        AiProxyEngine nonStreamingEngine = engine(request -> {
            nonStreamingCalls.incrementAndGet();
            return Mono.just(response(503, Flux.just(bytes("unavailable"))));
        }, new AiProxyRetryProperties(2, 0, Duration.ZERO));

        StepVerifier.create(nonStreamingEngine.execute(BODY, config(false, "main"), Optional.empty(),
                response -> response.getBody().then())).verifyComplete();
        assertEquals(3, nonStreamingCalls.get());

        AtomicInteger streamingCalls = new AtomicInteger();
        AiProxyEngine streamingEngine = engine(request -> {
            streamingCalls.incrementAndGet();
            return Mono.error(networkFailure());
        }, new AiProxyRetryProperties(2, 0, Duration.ZERO));

        StepVerifier.create(streamingEngine.execute(BODY, config(true, "main"), Optional.empty(),
                response -> response.getBody().then())).expectError(AiTransportException.class).verify();
        assertEquals(1, streamingCalls.get());
    }

    @Test
    void nonStreamingRetriesUseExponentialBackoff() {
        AtomicInteger calls = new AtomicInteger();
        AiProxyEngine engine = engine(request -> {
            int attempt = calls.incrementAndGet();
            return Mono.just(attempt < 4
                    ? response(503, Flux.just(bytes("unavailable")))
                    : response(200, Flux.just(bytes("ok"))));
        }, new AiProxyRetryProperties(3, 1, Duration.ofSeconds(1)));

        StepVerifier.withVirtualTime(() -> engine.execute(BODY, config(false, "main"), Optional.empty(),
                        response -> response.getBody().then()))
                .expectSubscription()
                .then(() -> assertEquals(1, calls.get()))
                .thenAwait(Duration.ofMillis(999))
                .then(() -> assertEquals(1, calls.get()))
                .thenAwait(Duration.ofMillis(1))
                .then(() -> assertEquals(2, calls.get()))
                .thenAwait(Duration.ofSeconds(2))
                .then(() -> assertEquals(3, calls.get()))
                .thenAwait(Duration.ofSeconds(4))
                .then(() -> assertEquals(4, calls.get()))
                .verifyComplete();
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

    @ParameterizedTest
    @ValueSource(ints = {400, 401, 403, 404, 408, 422})
    void clientErrorsAreForwardedWithoutRetryOrFallback(final int status) {
        AtomicInteger calls = new AtomicInteger();
        List<String> received = new ArrayList<>();
        AiProxyEngine engine = engine(request -> {
            calls.incrementAndGet();
            return Mono.just(response(status, Flux.just(bytes("client error"))));
        });

        StepVerifier.create(engine.execute(BODY, config(false, "main"), Optional.of(config(false, "fallback")), response -> {
            assertEquals(status, response.getStatusCode());
            assertEquals(List.of("kept"), response.getHeaders().get("X-Upstream"));
            return consume(response, received);
        })).verifyComplete();

        assertEquals(1, calls.get());
        assertEquals(List.of("client error"), received);
    }

    @ParameterizedTest
    @ValueSource(ints = {429, 500, 502, 503, 504, 599})
    void retryableStatusesRetryBeforeUsingFallback(final int status) {
        AtomicInteger primaryCalls = new AtomicInteger();
        AtomicInteger fallbackCalls = new AtomicInteger();
        List<String> received = new ArrayList<>();
        AiProxyEngine engine = engine(request -> {
            String requestBody = new String(request.getBody(), StandardCharsets.UTF_8);
            if (requestBody.contains("\"model\":\"fallback\"")) {
                fallbackCalls.incrementAndGet();
                return Mono.just(response(200, Flux.just(bytes("fallback response"))));
            }
            primaryCalls.incrementAndGet();
            return Mono.just(response(status, Flux.just(bytes("retryable error"))));
        });

        StepVerifier.create(engine.execute(BODY, config(false, "main"), Optional.of(config(false, "fallback")),
                response -> consume(response, received))).verifyComplete();

        assertEquals(4, primaryCalls.get());
        assertEquals(1, fallbackCalls.get());
        assertEquals(List.of("fallback response"), received);
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

    @ParameterizedTest
    @CsvSource({
        "CONNECT_TIMEOUT, 2",
        "RESPONSE_HEADER_TIMEOUT, 2",
        "STREAM_IDLE_TIMEOUT, 2",
        "STREAM_DURATION_TIMEOUT, 1",
        "RESPONSE_TOO_LARGE, 1",
        "DNS_FAILURE, 1",
        "TLS_FAILURE, 1",
        "CONNECTION_RESET, 2",
        "IO_FAILURE, 2",
        "UNKNOWN, 1"
    })
    void transportFailuresFollowTheRetryWhitelist(final AiTransportException.Kind kind, final int expectedCalls) {
        AtomicInteger calls = new AtomicInteger();
        AiProxyEngine engine = engine(request -> {
            calls.incrementAndGet();
            return Mono.error(transportFailure(kind));
        });

        StepVerifier.create(engine.execute(BODY, config(true, "main"), Optional.empty(), response -> response.getBody().then()))
                .expectErrorSatisfies(error -> assertEquals(kind,
                        assertInstanceOf(AiTransportException.class, error).getKind()))
                .verify();

        assertEquals(expectedCalls, calls.get());
    }

    @ParameterizedTest
    @CsvSource({
        "CONNECT_TIMEOUT, 2, true",
        "RESPONSE_HEADER_TIMEOUT, 2, true",
        "STREAM_IDLE_TIMEOUT, 2, false",
        "STREAM_DURATION_TIMEOUT, 1, false",
        "RESPONSE_TOO_LARGE, 1, true",
        "DNS_FAILURE, 1, false",
        "TLS_FAILURE, 1, false",
        "CONNECTION_RESET, 2, true",
        "IO_FAILURE, 2, true",
        "UNKNOWN, 1, false"
    })
    void transportFailuresFollowTheFallbackWhitelist(final AiTransportException.Kind kind,
                                                      final int expectedPrimaryCalls,
                                                      final boolean fallbackExpected) {
        AtomicInteger primaryCalls = new AtomicInteger();
        AtomicInteger fallbackCalls = new AtomicInteger();
        AiProxyEngine engine = engine(request -> {
            String requestBody = new String(request.getBody(), StandardCharsets.UTF_8);
            if (requestBody.contains("\"model\":\"fallback\"")) {
                fallbackCalls.incrementAndGet();
                return Mono.just(response(200, Flux.just(bytes("fallback response"))));
            }
            primaryCalls.incrementAndGet();
            return Mono.error(transportFailure(kind));
        });

        Mono<Void> execution = engine.execute(BODY, config(true, "main"), Optional.of(config(true, "fallback")),
                response -> response.getBody().then());
        if (fallbackExpected) {
            StepVerifier.create(execution).verifyComplete();
        } else {
            StepVerifier.create(execution)
                    .expectErrorSatisfies(error -> assertEquals(kind,
                            assertInstanceOf(AiTransportException.class, error).getKind()))
                    .verify();
        }

        assertEquals(expectedPrimaryCalls, primaryCalls.get());
        assertEquals(fallbackExpected ? 1 : 0, fallbackCalls.get());
    }

    @Test
    void responseTooLargeDoesNotRetryTheSameProvider() {
        AtomicInteger calls = new AtomicInteger();
        AiProxyEngine engine = engine(request -> {
            calls.incrementAndGet();
            return Mono.error(responseTooLargeFailure());
        });

        StepVerifier.create(engine.execute(BODY, config(false, "main"), Optional.empty(), response -> response.getBody().then()))
                .expectErrorSatisfies(error -> assertEquals(AiTransportException.Kind.RESPONSE_TOO_LARGE,
                        assertInstanceOf(AiTransportException.class, error).getKind()))
                .verify();
        assertEquals(1, calls.get());
    }

    @Test
    void responseTooLargeCanUseConfiguredFallback() {
        AtomicInteger calls = new AtomicInteger();
        AtomicBoolean fallbackUsed = new AtomicBoolean();
        AiProxyEngine engine = engine(request -> {
            calls.incrementAndGet();
            String requestBody = new String(request.getBody(), StandardCharsets.UTF_8);
            if (!requestBody.contains("\"model\":\"fallback\"")) {
                return Mono.error(responseTooLargeFailure());
            }
            fallbackUsed.set(true);
            return Mono.just(response(200, Flux.just(bytes("fallback response"))));
        });

        StepVerifier.create(engine.execute(BODY, config(false, "main"), Optional.of(config(false, "fallback")),
                response -> response.getBody().then())).verifyComplete();
        assertEquals(2, calls.get());
        assertTrue(fallbackUsed.get());
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
                .expectError(AiTransportException.class).verify();
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
                .expectError(AiTransportException.class).verify();
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
    void providerResponseAdapterAndProtocolConvertTheResponseInOrder() {
        List<String> conversions = new ArrayList<>();
        AtomicBoolean supported = new AtomicBoolean(true);
        AiProxyProvider provider = new AiProxyProvider() {
            @Override
            public String name() {
                return "custom";
            }

            @Override
            public boolean supports(final String protocol) {
                return supported.get();
            }

            @Override
            public AiUpstreamRequest buildRequest(final ShenyuAiRequest request, final AiCommonConfig config) {
                return new AiUpstreamRequest();
            }

            @Override
            public PreparedAiRequest prepareRequest(final ShenyuAiRequest request, final AiCommonConfig config) {
                return new PreparedAiRequest(buildRequest(request, config), response -> {
                    conversions.add("provider");
                    return response;
                });
            }
        };
        OpenAiChat delegate = new OpenAiChat();
        AiProxyProtocol protocol = new AiProxyProtocol() {
            @Override
            public String name() {
                return delegate.name();
            }

            @Override
            public boolean matches(final JsonNode payload) {
                return delegate.matches(payload);
            }

            @Override
            public ShenyuAiRequest decodeRequest(final String rawBody, final ObjectNode payload) {
                return delegate.decodeRequest(rawBody, payload);
            }

            @Override
            public AiUpstreamResponse encodeResponse(final AiUpstreamResponse response, final boolean stream) {
                conversions.add("protocol");
                return response;
            }
        };
        AiCommonConfig config = config(false, "model");
        config.setProvider("custom");
        AiProxyEngine engine = new AiProxyEngine(new AiProxyProtocolFactory(List.of(protocol)), new AiProxyProviderFactory(List.of(provider)),
                protocolRouteResolver(), request -> Mono.just(response(200, Flux.just(bytes("ok")))), Duration.ZERO);
        StepVerifier.create(engine.execute(BODY, config, Optional.empty(), response -> response.getBody().then())).verifyComplete();
        assertEquals(List.of("provider", "protocol"), conversions);
        supported.set(false);
        conversions.clear();
        StepVerifier.create(engine.execute(BODY, config, Optional.empty(), response -> response.getBody().then())).expectError(ShenyuException.class).verify();
        assertTrue(conversions.isEmpty());
    }

    @Test
    void preparesTheConvertedTargetProtocolBeforeTheProvider() throws Exception {
        AtomicInteger clientPrepared = new AtomicInteger();
        AtomicInteger prepared = new AtomicInteger();
        OpenAiResponses responses = new OpenAiResponses();
        AiProxyProtocol clientProtocol = new AiProxyProtocol() {
            @Override
            public String name() {
                return responses.name();
            }

            @Override
            public boolean matches(final JsonNode payload) {
                return responses.matches(payload);
            }

            @Override
            public ShenyuAiRequest decodeRequest(final String rawBody, final ObjectNode payload) {
                return responses.decodeRequest(rawBody, payload);
            }

            @Override
            public ObjectNode prepareOutgoingRequest(final ObjectNode payload, final AiProtocolRequestOverrides overrides) {
                clientPrepared.incrementAndGet();
                return payload;
            }
        };
        OpenAiChat chat = new OpenAiChat();
        AiProxyProtocol targetProtocol = new AiProxyProtocol() {
            @Override
            public String name() {
                return chat.name();
            }

            @Override
            public boolean matches(final JsonNode payload) {
                return chat.matches(payload);
            }

            @Override
            public ShenyuAiRequest decodeRequest(final String rawBody, final ObjectNode payload) {
                return chat.decodeRequest(rawBody, payload);
            }

            @Override
            public ObjectNode prepareOutgoingRequest(final ObjectNode payload, final AiProtocolRequestOverrides overrides) {
                prepared.incrementAndGet();
                payload.put("target_prepared", true);
                return payload;
            }
        };
        AiProxyProvider provider = new AiProxyProvider() {
            @Override
            public String name() {
                return "custom";
            }

            @Override
            public AiUpstreamRequest buildRequest(final ShenyuAiRequest request, final AiCommonConfig config) {
                AiUpstreamRequest upstream = new AiUpstreamRequest();
                upstream.setBody(request.getRawBody().getBytes(StandardCharsets.UTF_8));
                return upstream;
            }
        };
        AiProxyEngine engine = new AiProxyEngine(
                new AiProxyProtocolFactory(List.of(clientProtocol, targetProtocol)),
                new AiProxyProviderFactory(List.of(provider)), protocolRouteResolver(), request -> Mono.empty());
        AiCommonConfig config = config(false, "model");
        config.setProvider("custom");
        config.setProtocol(OpenAiResponses.NAME);

        AiUpstreamRequest upstream = engine.buildRequest("{\"input\":\"hi\"}", config);

        assertEquals(0, clientPrepared.get());
        assertEquals(1, prepared.get());
        assertTrue(new ObjectMapper().readTree(upstream.getBody()).path("target_prepared").asBoolean());
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
        return new AiProxyEngine(new AiProxyProtocolFactory(List.of(new OpenAiChat(), new OpenAiResponses())),
                new AiProxyProviderFactory(List.of(new OpenAi(), new OpenAiCompatible("DeepSeek"))),
                protocolRouteResolver(), transport, Duration.ZERO);
    }

    private AiProxyEngine engine(final AiProxyTransport transport, final AiProxyRetryProperties retryProperties) {
        return new AiProxyEngine(new AiProxyProtocolFactory(List.of(new OpenAiChat())),
                new AiProxyProviderFactory(List.of(new OpenAi(), new OpenAiCompatible("DeepSeek"))),
                protocolRouteResolver(), transport, retryProperties);
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

    private AiTransportException networkFailure() {
        return transportFailure(AiTransportException.Kind.IO_FAILURE);
    }

    private AiTransportException responseTooLargeFailure() {
        return transportFailure(AiTransportException.Kind.RESPONSE_TOO_LARGE);
    }

    private AiTransportException transportFailure(final AiTransportException.Kind kind) {
        return new AiTransportException(kind, "test transport failure: " + kind, null);
    }
}
