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
import org.apache.shenyu.common.metrics.AiCallObserver;
import org.apache.shenyu.common.metrics.AiRequestObservation;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.proxy.enhanced.cache.AiExactResponseCache;
import org.apache.shenyu.plugin.ai.proxy.enhanced.conversion.AiProtocolConverterFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProviderFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.routing.AiProtocolRouteResolver;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiProxyTransport;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiTransportException;
import reactor.core.publisher.Mono;

import java.time.Duration;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;

/**
 * Entry point for planning and executing reactive AI proxy exchanges.
 */
public final class AiProxyEngine {

    private final AiExchangePlanner exchangePlanner;

    private final AiProxyRetryProperties retryProperties;

    private final AiCallObserver observer;

    private final AiExactResponseCache responseCache;

    private final AiExecutionCoordinator executionCoordinator;

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
        this(protocols, providers, routeResolver, transport, retryProperties, (AiProxyAttemptListener) event -> {
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
        this(protocols, providers, routeResolver, transport, retryProperties, observer, attemptListener, null);
    }

    /**
     * Create an engine with an optional, explicitly scoped exact response cache.
     *
     * @param protocols protocol registry
     * @param providers provider registry
     * @param routeResolver protocol routing resolver
     * @param transport upstream transport
     * @param retryProperties retry policy
     * @param observer optional logical call observer
     * @param attemptListener bounded attempt observer
     * @param responseCache optional exact response cache
     */
    public AiProxyEngine(final AiProxyProtocolFactory protocols, final AiProxyProviderFactory providers,
                         final AiProtocolRouteResolver routeResolver, final AiProxyTransport transport,
                         final AiProxyRetryProperties retryProperties, final AiCallObserver observer,
                         final AiProxyAttemptListener attemptListener, final AiExactResponseCache responseCache) {
        this.exchangePlanner = new AiExchangePlanner(protocols, providers, routeResolver);
        this.retryProperties = Objects.requireNonNull(retryProperties, "retryProperties must not be null");
        this.observer = observer;
        this.responseCache = responseCache;
        this.executionCoordinator = new AiExecutionCoordinator(
                exchangePlanner,
                transport,
                this.retryProperties,
                attemptListener);
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
                AiPreparedExchange primary = exchangePlanner.prepare(requestBody, new AiCommonConfig(primaryConfig));
                Optional<AiCommonConfig> fallback = fallbackConfig.map(config -> {
                    AiCommonConfig copy = new AiCommonConfig(config);
                    copy.setStream(primary.stream());
                    // Fallback changes the upstream, never the already selected client protocol.
                    copy.setProtocol(primary.clientProtocol());
                    return copy;
                });
                final int maxRetries = retryProperties.maxRetries(primary.stream());
                final boolean fallbackProviderDeclaredReplaySafe = fallback
                        .map(config -> exchangePlanner.providerDeclaredReplaySafe(config, primary.operation()))
                        .orElse(false);
                Function<AiUpstreamResponse, Mono<Void>> observedConsumer = response -> {
                    status.set(response.getStatusCode());
                    return responseConsumer.apply(response);
                };
                Function<Function<AiUpstreamResponse, Mono<Void>>, Mono<Void>> upstream = consumer -> {
                    AiExecutionState execution = new AiExecutionState(requestBody, consumer, new AtomicBoolean(), new AtomicInteger(),
                            invocationId, deadlineNanos, observation);
                    return executionCoordinator.execute(
                            execution,
                            primary,
                            fallback,
                            maxRetries,
                            fallbackProviderDeclaredReplaySafe);
                };
                if (Objects.nonNull(responseCache) && primary.cacheCompatible()) {
                    return responseCache.execute(requestBody, primary.request(), primary.provider(), primary.clientProtocol(),
                            primary.targetProtocol(), primary.stream(), fallback.isPresent(), observedConsumer, upstream)
                            .timeout(Duration.ofNanos(Math.max(1L, deadlineNanos - System.nanoTime())),
                                    Mono.error(new AiTransportException(AiTransportException.Kind.RESPONSE_HEADER_TIMEOUT,
                                            "AI proxy request deadline exceeded", null)));
                }
                return upstream.apply(observedConsumer);
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
        return exchangePlanner.prepare(requestBody, config).request();
    }

}
