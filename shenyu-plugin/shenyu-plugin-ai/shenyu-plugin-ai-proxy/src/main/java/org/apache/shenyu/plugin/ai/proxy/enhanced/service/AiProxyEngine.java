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
import java.util.Objects;
import java.util.Optional;
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

    public AiProxyEngine(final AiProxyProtocolFactory protocols, final AiProxyProviderFactory providers,
                         final AiProtocolRouteResolver routeResolver, final AiProxyTransport transport) {
        this(protocols, providers, routeResolver, transport, new AiProxyRetryProperties());
    }

    public AiProxyEngine(final AiProxyProtocolFactory protocols, final AiProxyProviderFactory providers,
                         final AiProtocolRouteResolver routeResolver, final AiProxyTransport transport, final AiProxyRetryProperties retryProperties) {
        this(protocols, providers, routeResolver, transport, retryProperties, null);
    }

    public AiProxyEngine(final AiProxyProtocolFactory protocols, final AiProxyProviderFactory providers,
                        final AiProtocolRouteResolver routeResolver, final AiProxyTransport transport, final Duration retryBackoff) {
        this(protocols, providers, routeResolver, transport, retryBackoff, null);
    }

    public AiProxyEngine(final AiProxyProtocolFactory protocols, final AiProxyProviderFactory providers,
                        final AiProtocolRouteResolver routeResolver, final AiProxyTransport transport, final Duration retryBackoff, final AiCallObserver observer) {
        this(protocols, providers, routeResolver, transport, new AiProxyRetryProperties(3, 1, retryBackoff), observer);
    }

    public AiProxyEngine(final AiProxyProtocolFactory protocols, final AiProxyProviderFactory providers,
                         final AiProtocolRouteResolver routeResolver, final AiProxyTransport transport,
                         final AiProxyRetryProperties retryProperties, final AiCallObserver observer) {
        this.aiProxyProtocolFactory = Objects.requireNonNull(protocols, "protocols must not be null");
        this.aiProxyProviderFactory = Objects.requireNonNull(providers, "providers must not be null");
        this.aiProtocolRouteResolver = Objects.requireNonNull(routeResolver, "routeResolver must not be null");
        this.aiProxyTransport = Objects.requireNonNull(transport, "transport must not be null");
        this.retryProperties = Objects.requireNonNull(retryProperties, "retryProperties must not be null");
        this.observer = observer;
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
                PreparedExchange primary = prepare(requestBody, new AiCommonConfig(primaryConfig));
                Optional<AiCommonConfig> fallback = fallbackConfig.map(config -> {
                    AiCommonConfig copy = new AiCommonConfig(config);
                    copy.setStream(primary.stream());
                    // Fallback changes the upstream, never the already selected client protocol.
                    copy.setProtocol(primary.clientProtocol());
                    return copy;
                });
                final int maxRetries = retryProperties.maxRetries(primary.stream());
                return attempt(requestBody, primary, fallback, maxRetries, maxRetries, response -> {
                    status.set(response.getStatusCode());
                    return responseConsumer.apply(response);
                }, new AtomicBoolean(), observation, AiAttemptEvent.Kind.PRIMARY);
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

    private Mono<Void> attempt(final String requestBody, final PreparedExchange prepared,
                               final Optional<AiCommonConfig> fallback, final int maxRetries, final int retriesRemaining,
                               final Function<AiUpstreamResponse, Mono<Void>> consumer, final AtomicBoolean emitted,
                               final AiRequestObservation observation, final AiAttemptEvent.Kind kind) {
        return Mono.defer(() -> {
            // A configured protocol identifier is not a negotiated protocol version.
            AgentRequestContext target = observation.getContext().withTarget(prepared.target(), prepared.operation(), null);
            AiRequestObservation.Attempt attempt = observation.startAttempt(target, kind, prepared.provider(), prepared.clientProtocol(), prepared.stream());
            return exchange(prepared, fallback, retriesRemaining, consumer, emitted, attempt)
                    .doOnSuccess(ignored -> attempt.finish(SignalType.ON_COMPLETE, null))
                    .doOnError(error -> attempt.finish(SignalType.ON_ERROR, error instanceof UpstreamStatusFailure ? null : AiObservationSupport.outcome(error)))
                    .doOnCancel(() -> attempt.finish(SignalType.CANCEL, null))
                    .contextWrite(context -> context.put(AgentRequestContext.REACTOR_KEY, target));
        }).onErrorResume(error -> {
            final AiProxyRetryPolicy.Decision decision;
            if (emitted.get()) {
                return Mono.error(error);
            }
            if (error instanceof AiTransportException transportException) {
                decision = RETRY_POLICY.decideTransport(transportException.getKind(), retriesRemaining, fallback.isPresent());
            } else if (error instanceof UpstreamStatusFailure statusFailure) {
                decision = RETRY_POLICY.decideStatus(statusFailure.status, retriesRemaining, fallback.isPresent());
            } else {
                return Mono.error(error);
            }
            if (decision == AiProxyRetryPolicy.Decision.RETRY) {
                final int retryIndex = maxRetries - retriesRemaining;
                final Duration delay = retryProperties.retryDelay(prepared.stream(), retryIndex);
                return Mono.delay(delay).then(attempt(requestBody, prepared, fallback, maxRetries, retriesRemaining - 1,
                        consumer, emitted, observation, AiAttemptEvent.Kind.RETRY));
            }
            if (decision == AiProxyRetryPolicy.Decision.FALLBACK) {
                return Mono.defer(() -> attempt(requestBody, prepare(requestBody, fallback.get()), Optional.empty(), 0, 0,
                        consumer, emitted, observation, AiAttemptEvent.Kind.FALLBACK));
            }
            return Mono.error(error);
        });
    }

    private Mono<Void> exchange(final PreparedExchange prepared, final Optional<AiCommonConfig> fallback, final int retriesRemaining,
                                final Function<AiUpstreamResponse, Mono<Void>> consumer, final AtomicBoolean emitted, final AiRequestObservation.Attempt attempt) {
        return Mono.defer(() -> aiProxyTransport.execute(prepared.request()))
                .flatMap(response -> {
                    int status = response.getStatusCode();
                    attempt.status(status);
                    AiUpstreamResponse observed = new AiUpstreamUsageTap(attempt, response.getHeaders(), prepared.stream()).attach(response);
                    if (status >= 400) {
                        final AiProxyRetryPolicy.Decision decision =
                                RETRY_POLICY.decideStatus(status, retriesRemaining, fallback.isPresent());
                        if (decision == AiProxyRetryPolicy.Decision.FORWARD_RESPONSE) {
                            return consumeResponse(observed, consumer, emitted);
                        }
                        return observed.getBody().then(Mono.error(new UpstreamStatusFailure(status)));
                    }
                    AiUpstreamResponse adapted = prepared.responseAdapter().adapt(observed);
                    return consumeResponse(adapted, consumer, emitted);
                });
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
        AiProxyProvider provider = aiProxyProviderFactory.getProvider(
                Objects.isNull(config.getProvider()) || config.getProvider().isBlank() ? "openai" : config.getProvider());
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
            return new PreparedExchange(preparedRequest.request(), responseAdapter,
                    route.clientProtocol(), route.targetProtocol(), stream, provider.name(), selectedModel(preparedRequest.request()), decoded.getOperation().name());
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

    private record PreparedExchange(
            AiUpstreamRequest request,
            AiResponseAdapter responseAdapter,
            String clientProtocol,
            String targetProtocol,
            boolean stream,
            String provider,
            String target,
            String operation) {
    }

    private static final class UpstreamStatusFailure extends RuntimeException {

        private static final long serialVersionUID = 1L;

        private final int status;

        private UpstreamStatusFailure(final int status) {
            super("AI upstream returned HTTP " + status);
            this.status = status;
        }
    }
}
