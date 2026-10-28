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

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.apache.shenyu.common.dto.AgentRequestContext;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.metrics.AiAttemptEvent;
import org.apache.shenyu.common.metrics.AiCallObserver;
import org.apache.shenyu.common.metrics.AiRequestObservation;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.proxy.enhanced.conversion.AiResponseAdapter;
import org.apache.shenyu.plugin.ai.proxy.enhanced.conversion.AiProtocolConverterFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiProxyException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.PreparedAiRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.ShenyuAiRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocol;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.routing.AiProtocolRoute;
import org.apache.shenyu.plugin.ai.proxy.enhanced.routing.AiProtocolRouteResolver;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProvider;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProviderFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiProxyTransport;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiTransportException;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.SignalType;

import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;

/**
 * Coordinates the complete reactive AI exchange and owns retry and fallback policy.
 */
public final class AiProxyEngine {

    private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper().enable(DeserializationFeature.FAIL_ON_TRAILING_TOKENS);

    private static final AiProxyRetryPolicy RETRY_POLICY = new AiProxyRetryPolicy();

    private final AiProxyProtocolFactory aiProxyProtocolFactory;

    private final AiProxyProviderFactory aiProxyProviderFactory;

    private final AiProtocolRouteResolver aiProtocolRouteResolver;

    private final AiProxyTransport aiProxyTransport;

    private final AiProxyRetryProperties retryProperties;

    private final AiCallObserver observer;
    private final AiProxyAttemptListener attemptListener;

    public AiProxyEngine(final AiProxyProtocolFactory protocols, final AiProxyProviderFactory providers, final AiProxyTransport transport) {
        this(protocols, providers, defaultRouteResolver(), transport, new AiProxyRetryProperties(), null, event -> {
        });
    }

    public AiProxyEngine(final AiProxyProtocolFactory protocols, final AiProxyProviderFactory providers,
                         final AiProxyTransport transport, final AiProxyRetryProperties retryProperties) {
        this(protocols, providers, defaultRouteResolver(), transport, retryProperties, null, event -> {
        });
    }

    public AiProxyEngine(final AiProxyProtocolFactory protocols, final AiProxyProviderFactory providers,
                         final AiProxyTransport transport, final Duration retryBackoff) {
        this(protocols, providers, defaultRouteResolver(), transport, new AiProxyRetryProperties(3, 1, retryBackoff), null, event -> {
        });
    }

    public AiProxyEngine(final AiProxyProtocolFactory protocols, final AiProxyProviderFactory providers,
                         final AiProxyTransport transport, final Duration retryBackoff, final AiCallObserver observer) {
        this(protocols, providers, defaultRouteResolver(), transport, new AiProxyRetryProperties(3, 1, retryBackoff), observer, event -> {
        });
    }

    public AiProxyEngine(final AiProxyProtocolFactory protocols, final AiProxyProviderFactory providers,
                         final AiProxyTransport transport, final AiProxyRetryProperties retryProperties,
                         final AiCallObserver observer) {
        this(protocols, providers, defaultRouteResolver(), transport, retryProperties, observer, event -> {
        });
    }

    public AiProxyEngine(
            final AiProxyProtocolFactory protocols,
            final AiProxyProviderFactory providers,
            final AiProtocolRouteResolver routeResolver,
            final AiProxyTransport transport) {
        this(
                protocols,
                providers,
                routeResolver,
                transport,
                new AiProxyRetryProperties());
    }

    public AiProxyEngine(final AiProxyProtocolFactory protocols, final AiProxyProviderFactory providers,
                         final AiProtocolRouteResolver routeResolver, final AiProxyTransport transport,
                         final AiProxyRetryProperties retryProperties) {
        this(protocols, providers, routeResolver, transport, retryProperties, event -> {
        });
    }

    /**
     * Create an AI proxy engine with retry policy integration.
     *
     * @param protocols protocol registry
     * @param providers provider registry
     * @param routeResolver protocol routing resolver
     * @param transport upstream transport
     * @param retryProperties retry properties
     * @param attemptListener bounded attempt observer
     */
    public AiProxyEngine(final AiProxyProtocolFactory protocols, final AiProxyProviderFactory providers,
                         final AiProtocolRouteResolver routeResolver, final AiProxyTransport transport,
                         final AiProxyRetryProperties retryProperties,
                         final AiProxyAttemptListener attemptListener) {
        this(protocols, providers, routeResolver, transport, retryProperties, null, attemptListener);
    }

    public AiProxyEngine(final AiProxyProtocolFactory protocols, final AiProxyProviderFactory providers,
                         final AiProtocolRouteResolver routeResolver, final AiProxyTransport transport,
                         final AiProxyRetryProperties retryProperties, final AiCallObserver observer) {
        this(protocols, providers, routeResolver, transport, retryProperties, observer, event -> {
        });
    }

    public AiProxyEngine(final AiProxyProtocolFactory protocols, final AiProxyProviderFactory providers,
                         final AiProtocolRouteResolver routeResolver, final AiProxyTransport transport,
                         final AiProxyRetryProperties retryProperties, final AiCallObserver observer,
                         final AiProxyAttemptListener attemptListener) {
        this.aiProxyProtocolFactory = Objects.requireNonNull(protocols, "protocols must not be null");
        this.aiProxyProviderFactory = Objects.requireNonNull(providers, "providers must not be null");
        this.aiProtocolRouteResolver = Objects.requireNonNull(routeResolver, "routeResolver must not be null");
        this.aiProxyTransport = Objects.requireNonNull(transport, "transport must not be null");
        this.retryProperties = Objects.requireNonNull(retryProperties, "retryProperties must not be null");
        this.attemptListener = Objects.requireNonNull(attemptListener, "attemptListener must not be null");
        this.observer = observer;
    }

    public AiProxyEngine(final AiProxyProtocolFactory protocols, final AiProxyProviderFactory providers,
                         final AiProtocolConverterFactory converters, final AiProxyTransport transport,
                         final AiProxyRetryProperties retryProperties, final AiCallObserver observer) {
        this(protocols, providers, new AiProtocolRouteResolver(converters), transport, retryProperties, observer, event -> {
        });
    }

    public AiProxyEngine(final AiProxyProtocolFactory protocols, final AiProxyProviderFactory providers,
                         final AiProtocolRouteResolver routeResolver, final AiProxyTransport transport,
                         final Duration retryBackoff) {
        this(protocols, providers, routeResolver, transport, new AiProxyRetryProperties(3, 1, retryBackoff));
    }

    public AiProxyEngine(final AiProxyProtocolFactory protocols, final AiProxyProviderFactory providers,
                         final AiProtocolRouteResolver routeResolver, final AiProxyTransport transport,
                         final Duration retryBackoff, final AiCallObserver observer) {
        this(protocols, providers, routeResolver, transport, new AiProxyRetryProperties(3, 1, retryBackoff), observer);
    }

    public AiProxyEngine(final AiProxyProtocolFactory protocols, final AiProxyProviderFactory providers,
                         final AiProtocolConverterFactory converters, final AiProxyTransport transport,
                         final Duration retryBackoff, final AiCallObserver observer) {
        this(protocols, providers, converters, transport, new AiProxyRetryProperties(3, 1, retryBackoff), observer);
    }

    private static AiProtocolRouteResolver defaultRouteResolver() {
        return new AiProtocolRouteResolver(new AiProtocolConverterFactory(java.util.List.of(
                new org.apache.shenyu.plugin.ai.proxy.enhanced.conversion.ConnectResponsesToChatConverter())));
    }

    /**
     * Execute and consume a complete exchange with retry and fallback before body emission.
     * Keeping the consumer inside this scope preserves upstream lifetime and backpressure.
     *
     * @param requestBody original client request
     * @param primaryConfig resolved primary configuration
     * @param fallbackConfig resolved fallback configuration
     * @param responseConsumer response boundary consuming the reactive body once
     * @return completion or a classified failure
     */
    public Mono<Void> execute(final String requestBody, final AiCommonConfig primaryConfig,
                              final Optional<AiCommonConfig> fallbackConfig,
                              final Function<AiUpstreamResponse, Mono<Void>> responseConsumer) {
        return Mono.deferContextual(view -> {
            AiRequestObservation inherited = view.getOrDefault(AiRequestObservation.REACTOR_KEY, null);
            AiRequestObservation observation = Objects.nonNull(inherited) ? inherited
                    : new AiRequestObservation(observer, AiObservationSupport.context(view, null, null));
            AtomicReference<Integer> status = new AtomicReference<>();
            return Mono.defer(() -> {
                long deadlineNanos = System.nanoTime() + retryProperties.getRequestDeadline().toNanos();
                String invocationId = UUID.randomUUID().toString();
                PreparedExchange primary = prepare(requestBody, new AiCommonConfig(primaryConfig));
                Optional<AiCommonConfig> fallback = fallbackConfig.map(config -> {
                    AiCommonConfig copy = new AiCommonConfig(config);
                    copy.setStream(primary.stream());
                    // Fallback changes the upstream, never the already selected client protocol.
                    copy.setProtocol(primary.clientProtocol());
                    return copy;
                });
                final int maxRetries = retryProperties.maxRetries(primary.stream());
                final boolean fallbackProviderDeclaredReplaySafe = fallback
                        .map(config -> aiProxyProviderFactory.getProvider(providerName(config))
                                .capabilities().isReplaySafe(primary.operation()))
                        .orElse(false);
                Execution execution = new Execution(requestBody, response -> {
                    status.set(response.getStatusCode());
                    return responseConsumer.apply(response);
                }, new AtomicBoolean(), new AtomicInteger(), invocationId, deadlineNanos, observation);
                return attempt(execution, primary, fallback, maxRetries, maxRetries,
                        AiProxyAttemptEvent.Phase.PRIMARY, fallbackProviderDeclaredReplaySafe);
            }).doOnError(error -> observation.failure(AiObservationSupport.outcome(error)))
                    .doFinally(signal -> {
                        if (Objects.isNull(inherited)) {
                            observation.finish(signal, status.get());
                        }
                    }).contextWrite(context -> context.put(AiRequestObservation.REACTOR_KEY, observation)
                            .put(AgentRequestContext.REACTOR_KEY, observation.getContext()));
        });
    }

    /**
     * Decode and adapt a request without opening an HTTP connection.
     *
     * @param requestBody original client body
     * @param config resolved configuration
     * @return upstream request
     */
    public AiUpstreamRequest buildRequest(final String requestBody, final AiCommonConfig config) {
        return prepare(requestBody, config).request();
    }

    private Mono<Void> attempt(final Execution execution, final PreparedExchange prepared,
                               final Optional<AiCommonConfig> fallback, final int maxRetries, final int retriesRemaining,
                               final AiProxyAttemptEvent.Phase phase,
                               final boolean fallbackProviderDeclaredReplaySafe) {
        final int currentAttempt = execution.attemptIndex().getAndIncrement();
        long deadlineStartNanos = System.nanoTime();
        long remainingNanos = execution.deadlineNanos() - deadlineStartNanos;
        if (remainingNanos <= 0L) {
            emit(execution.invocationId(), currentAttempt, phase,
                    AiProxyAttemptEvent.Reason.REQUEST_DEADLINE_EXCEEDED, null);
            return Mono.error(new AiProxyException(AiProxyException.Kind.TRANSPORT,
                    "AI proxy request deadline exceeded before attempt", null));
        }
        return Mono.defer(() -> {
            // A configured protocol identifier is not a negotiated protocol version.
            AgentRequestContext target = execution.observation().getContext()
                    .withTarget(selectedModel(prepared.request()), prepared.operation().name(), null);
            AiAttemptEvent.Kind kind = phase == AiProxyAttemptEvent.Phase.FALLBACK
                    ? AiAttemptEvent.Kind.FALLBACK : currentAttempt == 0
                    ? AiAttemptEvent.Kind.PRIMARY : AiAttemptEvent.Kind.RETRY;
            AiRequestObservation.Attempt observedAttempt = execution.observation()
                    .startAttempt(target, kind, prepared.provider(), prepared.clientProtocol(), prepared.stream());
            return aiProxyTransport.execute(prepared.request())
                    .flatMap(response -> {
                        observedAttempt.status(response.getStatusCode());
                        AiUpstreamResponse observed = new AiUpstreamUsageTap(
                                observedAttempt, response.getHeaders(), prepared.stream()).attach(response);
                        return handleResponse(observed, prepared, fallback, retriesRemaining,
                                execution, currentAttempt, phase, fallbackProviderDeclaredReplaySafe);
                    })
                    .timeout(Duration.ofNanos(Math.max(1L, remainingNanos - elapsedSince(deadlineStartNanos))),
                            Mono.error(new AiTransportException(
                                    AiTransportException.Kind.RESPONSE_HEADER_TIMEOUT,
                                    "AI proxy request deadline exceeded", null)))
                    .doOnSuccess(ignored -> observedAttempt.finish(SignalType.ON_COMPLETE, null))
                    .doOnError(error -> observedAttempt.finish(SignalType.ON_ERROR,
                            error instanceof UpstreamStatusFailure ? null : AiObservationSupport.outcome(error)))
                    .doOnCancel(() -> observedAttempt.finish(SignalType.CANCEL, null))
                    .contextWrite(context -> context.put(AgentRequestContext.REACTOR_KEY, target))
                    .onErrorResume(error -> resumeAttempt(error, prepared, fallback, maxRetries,
                            retriesRemaining, execution, currentAttempt, phase, fallbackProviderDeclaredReplaySafe));
        });
    }

    private Mono<Void> handleResponse(final AiUpstreamResponse response, final PreparedExchange prepared,
                                      final Optional<AiCommonConfig> fallback, final int retriesRemaining,
                                      final Execution execution, final int currentAttempt,
                                      final AiProxyAttemptEvent.Phase phase,
                                      final boolean fallbackProviderDeclaredReplaySafe) {
        return Mono.defer(() -> {
            int status = response.getStatusCode();
            if (status >= 400) {
                return handleStatusFailure(response, prepared, fallback, retriesRemaining,
                        execution, currentAttempt, phase, fallbackProviderDeclaredReplaySafe);
            }
            AiUpstreamResponse adapted = prepared.responseAdapter().adapt(response);
            return consumeResponse(adapted, execution.responseConsumer(), execution.emitted())
                    .doOnSuccess(ignored -> emit(execution.invocationId(), currentAttempt, phase,
                            AiProxyAttemptEvent.Reason.COMPLETED, null));
        });
    }

    private Mono<Void> resumeAttempt(final Throwable error, final PreparedExchange prepared,
                                     final Optional<AiCommonConfig> fallback, final int maxRetries,
                                     final int retriesRemaining,
                                     final Execution execution,
                                     final int currentAttempt, final AiProxyAttemptEvent.Phase phase,
                                     final boolean fallbackProviderDeclaredReplaySafe) {
        return Mono.defer(() -> {
            final AiProxyRetryPolicy.Decision decision;
            final Map<String, List<String>> responseHeaders;
            if (execution.emitted().get()) {
                emit(execution.invocationId(), currentAttempt, phase,
                        AiProxyAttemptEvent.Reason.OUTPUT_COMMITTED, null);
                return Mono.error(error);
            }
            if (error instanceof AiTransportException transportException) {
                responseHeaders = Map.of();
                decision = RETRY_POLICY.decideTransport(new AiProxyRetryPolicy.FailureContext(
                        transportException.getKind(), null, Map.of(), retriesRemaining,
                        fallback.isPresent(), prepared.conservativeReplaySafe(), prepared.operation(),
                        retryProperties.getReplay(), prepared.providerDeclaredReplaySafe(),
                        fallbackProviderDeclaredReplaySafe));
            } else if (error instanceof UpstreamStatusFailure statusFailure) {
                responseHeaders = statusFailure.headers;
                decision = RETRY_POLICY.decideStatus(new AiProxyRetryPolicy.FailureContext(
                        null, statusFailure.status, statusFailure.headers, retriesRemaining,
                        fallback.isPresent(), prepared.conservativeReplaySafe(), prepared.operation(),
                        retryProperties.getReplay(), prepared.providerDeclaredReplaySafe(),
                        fallbackProviderDeclaredReplaySafe));
            } else {
                return Mono.error(error);
            }
            if (decision == AiProxyRetryPolicy.Decision.RETRY) {
                return retry(error, prepared, fallback, maxRetries, retriesRemaining,
                        currentAttempt, execution, phase,
                        fallbackProviderDeclaredReplaySafe, responseHeaders);
            }
            if (decision == AiProxyRetryPolicy.Decision.FALLBACK) {
                return fallback(error, prepared, fallback, currentAttempt, execution, phase, responseHeaders);
            }
            if (decision == AiProxyRetryPolicy.Decision.PROPAGATE_ERROR
                    && error instanceof AiTransportException) {
                emit(execution.invocationId(), currentAttempt, phase,
                        AiProxyAttemptEvent.Reason.NON_RETRYABLE_TRANSPORT, null);
            }
            return Mono.error(error);
        });
    }

    private Mono<Void> retry(final Throwable error, final PreparedExchange prepared,
                             final Optional<AiCommonConfig> fallback, final int maxRetries,
                             final int retriesRemaining, final int currentAttempt,
                             final Execution execution,
                             final AiProxyAttemptEvent.Phase phase, final boolean fallbackProviderDeclaredReplaySafe,
                             final Map<String, List<String>> responseHeaders) {
        return Mono.defer(() -> {
            final int retryIndex = maxRetries - retriesRemaining;
            final Duration retryAfter = AiProxyRetryPolicy.retryAfter(responseHeaders, retryProperties);
            if (AiProxyRetryPolicy.hasRetryAfter(responseHeaders)
                    && !retryProperties.acceptsRetryAfter(retryAfter)) {
                emit(execution.invocationId(), currentAttempt, phase,
                        AiProxyAttemptEvent.Reason.REPLAY_BLOCKED, null);
                return Mono.error(error);
            }
            Duration delay = retryProperties.retryDelay(prepared.stream(), retryIndex, System.nanoTime());
            if (Objects.nonNull(retryAfter) && retryAfter.compareTo(delay) > 0) {
                delay = retryAfter;
            }
            Duration remaining = Duration.ofNanos(execution.deadlineNanos() - System.nanoTime());
            if (remaining.isNegative() || remaining.isZero() || delay.compareTo(remaining) >= 0) {
                emit(execution.invocationId(), currentAttempt, phase,
                        AiProxyAttemptEvent.Reason.REQUEST_DEADLINE_EXCEEDED, retryAfter);
                return Mono.error(new AiProxyException(AiProxyException.Kind.TRANSPORT,
                        "AI proxy request deadline exceeded before retry", error));
            }
            emit(execution.invocationId(), currentAttempt, phase,
                    error instanceof AiTransportException
                            ? AiProxyAttemptEvent.Reason.RETRY_TRANSPORT : AiProxyAttemptEvent.Reason.RETRY_STATUS,
                    retryAfter);
            return Mono.delay(delay).then(attempt(execution, prepared, fallback, maxRetries,
                    retriesRemaining - 1, phase, fallbackProviderDeclaredReplaySafe));
        });
    }

    private Mono<Void> fallback(final Throwable error, final PreparedExchange prepared,
                                final Optional<AiCommonConfig> fallback,
                                final int currentAttempt, final Execution execution,
                                final AiProxyAttemptEvent.Phase phase,
                                final Map<String, List<String>> responseHeaders) {
        return Mono.defer(() -> {
            Duration remaining = Duration.ofNanos(execution.deadlineNanos() - System.nanoTime());
            if (remaining.isNegative() || remaining.isZero()) {
                emit(execution.invocationId(), currentAttempt, phase,
                        AiProxyAttemptEvent.Reason.REQUEST_DEADLINE_EXCEEDED,
                        AiProxyRetryPolicy.retryAfter(responseHeaders, retryProperties));
                return Mono.error(new AiProxyException(AiProxyException.Kind.TRANSPORT,
                        "AI proxy request deadline exceeded before fallback", error));
            }
            emit(execution.invocationId(), currentAttempt, phase,
                    error instanceof AiTransportException
                            ? AiProxyAttemptEvent.Reason.FALLBACK_TRANSPORT : AiProxyAttemptEvent.Reason.FALLBACK_STATUS,
                    AiProxyRetryPolicy.retryAfter(responseHeaders, retryProperties));
            return attempt(execution, prepare(execution.requestBody(), fallback.get()),
                    Optional.empty(), 0, 0, AiProxyAttemptEvent.Phase.FALLBACK, false);
        });
    }

    private Mono<Void> handleStatusFailure(final AiUpstreamResponse response, final PreparedExchange prepared,
                                           final Optional<AiCommonConfig> fallback, final int retriesRemaining,
                                           final Execution execution,
                                           final int currentAttempt,
                                           final AiProxyAttemptEvent.Phase phase,
                                           final boolean fallbackProviderDeclaredReplaySafe) {
        final AiProxyRetryPolicy.Decision decision =
                RETRY_POLICY.decideStatus(new AiProxyRetryPolicy.FailureContext(
                        null, response.getStatusCode(), response.getHeaders(), retriesRemaining,
                        fallback.isPresent(), prepared.conservativeReplaySafe(), prepared.operation(),
                        retryProperties.getReplay(), prepared.providerDeclaredReplaySafe(),
                        fallbackProviderDeclaredReplaySafe));
        if (decision == AiProxyRetryPolicy.Decision.FORWARD_RESPONSE) {
            emit(execution.invocationId(), currentAttempt, phase,
                    AiProxyAttemptEvent.Reason.NON_RETRYABLE_RESPONSE, null);
            return consumeResponse(response, execution.responseConsumer(), execution.emitted());
        }
        return response.getBody().then(Mono.error(new UpstreamStatusFailure(
                response.getStatusCode(), response.getHeaders())));
    }

    private void emit(final String invocationId, final int attemptIndex, final AiProxyAttemptEvent.Phase phase,
                      final AiProxyAttemptEvent.Reason reason, final Duration retryAfter) {
        try {
            attemptListener.onAttempt(new AiProxyAttemptEvent(invocationId, attemptIndex, phase, reason, retryAfter));
        } catch (RuntimeException ignored) {
            // Observation must never change the AI request outcome.
        }
    }

    private Mono<Void> consumeResponse(
            final AiUpstreamResponse response,
            final Function<AiUpstreamResponse, Mono<Void>> consumer,
            final AtomicBoolean emitted) {
        return response.getBody()
                .switchOnFirst((signal, body) -> {
                    if (signal.isOnError()) {
                        return Flux.error(
                                Objects.requireNonNull(
                                        signal.getThrowable()));
                    }

                    final AiUpstreamResponse ready =
                            new AiUpstreamResponse(
                                    response.getStatusCode(),
                                    response.getHeaders(),
                                    body.doOnNext(
                                            bytes -> emitted.set(true)));

                    return Mono.defer(
                            () -> consumer.apply(ready)).flux();
                })
                .then();
    }

    private PreparedExchange prepare(final String requestBody, final AiCommonConfig config) {
        if (Objects.isNull(requestBody) || requestBody.isBlank()) {
            throw new ShenyuException("AI request body must not be empty");
        }
        final JsonNode root;
        try {
            root = OBJECT_MAPPER.readTree(requestBody);
        } catch (JsonProcessingException error) {
            throw new ShenyuException("AI request body must be valid JSON", error);
        }
        if (!root.isObject()) {
            throw new ShenyuException("AI request body must be a JSON object");
        }
        ObjectNode payload = (ObjectNode) root;
        String upstreamBody = requestBody;
        if (payload.has("fallbackConfig")) {
            payload.remove("fallbackConfig");
            upstreamBody = payload.toString();
        }
        AiProxyProtocol protocol = Objects.isNull(config.getProtocol()) || config.getProtocol().isBlank()
                ? aiProxyProtocolFactory.detect(payload) : aiProxyProtocolFactory.getProtocol(config.getProtocol());
        if (!protocol.matches(payload)) {
            throw new ShenyuException("Request body does not match AI protocol: " + protocol.name());
        }
        ShenyuAiRequest decoded = applyModelOverride(protocol.decodeRequest(upstreamBody, payload), protocol, config);
        AiProxyProvider provider = aiProxyProviderFactory.getProvider(providerName(config));
        boolean stream = decoded.hasStream() ? decoded.isStream() : Boolean.TRUE.equals(config.getStream());
        AiProtocolRoute route = aiProtocolRouteResolver.resolve(
                decoded, provider.capabilities().protocols(), stream);
        ShenyuAiRequest providerRequest = route.conversion().request();
        if (!provider.supports(providerRequest.getProtocol())) {
            throw new ShenyuException("Unsupported AI upstream protocol/provider combination: "
                    + providerRequest.getProtocol() + " -> " + provider.name());
        }
        provider.capabilities().validate(providerRequest, stream);
        try {
            PreparedAiRequest preparedRequest = provider.prepareRequest(providerRequest, config);
            AiResponseAdapter responseAdapter = response -> protocol.encodeResponse(
                    route.conversion().responseAdapter().adapt(
                            preparedRequest.responseAdapter().adapt(response)), stream);
            boolean providerDeclaredReplaySafe = provider.capabilities()
                    .isReplaySafe(providerRequest.getOperation());
            return new PreparedExchange(preparedRequest.request(), responseAdapter,
                    route.clientProtocol(), route.targetProtocol(), stream,
                    AiProxyRetryPolicy.replaySafe(providerRequest), providerRequest.getOperation(),
                    providerDeclaredReplaySafe, provider.name());
        } catch (IllegalArgumentException error) {
            throw new AiProxyException(AiProxyException.Kind.PROVIDER, "Invalid AI provider configuration", error);
        }
    }

    private String selectedModel(final AiUpstreamRequest request) {
        // Only inspect the already constructed upstream request, not a second subscription to either HTTP body.
        // A custom provider may use a non-JSON body; no target is fabricated in that case.
        try {
            JsonNode payload = OBJECT_MAPPER.readTree(request.getBody());
            JsonNode model = Objects.isNull(payload) ? null : payload.get("model");
            return Objects.nonNull(model) && model.isTextual() ? model.textValue() : null;
        } catch (Exception ignored) {
            return null;
        }
    }

    private ShenyuAiRequest applyModelOverride(final ShenyuAiRequest request, final AiProxyProtocol protocol,
                                               final AiCommonConfig config) {
        String configuredModel = config.getModel();
        if (Objects.isNull(configuredModel) || configuredModel.isBlank()
                || configuredModel.equals(request.getModel())) {
            return request;
        }
        ObjectNode effectivePayload = request.getRawPayload().deepCopy();
        effectivePayload.put("model", configuredModel);
        return protocol.decodeRequest(effectivePayload.toString(), effectivePayload);
    }

    private String providerName(final AiCommonConfig config) {
        return Objects.isNull(config.getProvider()) || config.getProvider().isBlank()
                ? "openai" : config.getProvider();
    }

    private long elapsedSince(final long startNanos) {
        return System.nanoTime() - startNanos;
    }

    /**
     * Mutable state shared by the primary and fallback phases of one logical request.
     *
     * @param requestBody original client request body
     * @param responseConsumer client response boundary
     * @param emitted whether response bytes have reached the client
     * @param attemptIndex attempt sequence shared by primary and fallback
     * @param invocationId opaque logical invocation identifier
     * @param deadlineNanos absolute shared request deadline
     * @param observation subscription-local terminal observation
     */
    private record Execution(
            String requestBody,
            Function<AiUpstreamResponse, Mono<Void>> responseConsumer,
            AtomicBoolean emitted,
            AtomicInteger attemptIndex,
            String invocationId,
            long deadlineNanos,
            AiRequestObservation observation) {
    }

    private record PreparedExchange(
            AiUpstreamRequest request,
            AiResponseAdapter responseAdapter,
            String clientProtocol,
            String targetProtocol,
            boolean stream,
            boolean conservativeReplaySafe,
            ShenyuAiRequest.Operation operation,
            boolean providerDeclaredReplaySafe,
            String provider) {
    }

    private static final class UpstreamStatusFailure extends RuntimeException {

        private static final long serialVersionUID = 1L;

        private final int status;

        private final Map<String, List<String>> headers;

        private UpstreamStatusFailure(final int status, final Map<String, List<String>> headers) {
            super("AI upstream returned HTTP " + status);
            this.status = status;
            this.headers = Objects.requireNonNull(headers, "headers must not be null");
        }
    }
}
