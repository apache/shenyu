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

import org.apache.shenyu.common.metrics.AiCallObserver;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.proxy.enhanced.cache.AiExactResponseCache;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProviderFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.routing.AiProtocolRouteResolver;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.event.AiProxyAttemptListener;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiProxyTransport;
import reactor.core.publisher.Mono;

import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;

/**
 * Entry point for planning and executing reactive AI proxy exchanges.
 *
 * <p>Request flow: client protocol decoding, native-protocol routing or conversion,
 * target protocol preparation, provider adaptation, then transport execution.
 * Response flow: raw target-protocol inspection, provider adaptation, optional
 * reverse conversion, then client protocol encoding and response consumption.</p>
 *
 * <p>The execution pipeline owns observation, caching and primary/retry/fallback
 * lifetimes. Protocol and provider extensions register through their factories
 * without changing this flow. Start with OpenAiResponses and OpenAi for native
 * extension implementations, and OpenAiChat for stream framing.</p>
 */
public final class AiProxyEngine {

    private final AiExchangePlanner exchangePlanner;

    private final AiExecutionCoordinator executionCoordinator;

    private final AiCallObservationScope observationScope;

    /**
     * Create an AI proxy engine from its external extension points and policies.
     *
     * @param protocols protocol registry
     * @param providers provider registry
     * @param routeResolver protocol routing resolver
     * @param transport upstream transport
     * @param options optional policies and integrations
     */
    public AiProxyEngine(final AiProxyProtocolFactory protocols, final AiProxyProviderFactory providers,
                         final AiProtocolRouteResolver routeResolver, final AiProxyTransport transport,
                         final AiProxyEngineOptions options) {
        Objects.requireNonNull(options, "options must not be null");
        this.exchangePlanner = new AiExchangePlanner(protocols, providers, routeResolver);
        AiAttemptExecutor attemptExecutor = new AiAttemptExecutor(transport);
        AiAttemptOrchestrator attemptOrchestrator = new AiAttemptOrchestrator(
                exchangePlanner, attemptExecutor, options.retryProperties(), options.attemptListener());
        this.executionCoordinator = new AiExecutionCoordinator(
                attemptOrchestrator, options.retryProperties(), options.responseCache());
        this.observationScope = new AiCallObservationScope(options.observer());
    }

    /**
     * Create a builder from the required extension points.
     *
     * @param protocols protocol registry
     * @param providers provider registry
     * @param routeResolver protocol routing resolver
     * @param transport upstream transport
     * @return engine builder with default optional policies
     */
    public static Builder builder(final AiProxyProtocolFactory protocols, final AiProxyProviderFactory providers,
                                  final AiProtocolRouteResolver routeResolver, final AiProxyTransport transport) {
        return new Builder(protocols, providers, routeResolver, transport);
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
    public Mono<Void> execute(final String requestBody,
                              final AiCommonConfig primaryConfig,
                              final Optional<AiCommonConfig> fallbackConfig,
                              final Function<AiUpstreamResponse, Mono<Void>> responseConsumer) {
        return observationScope.observe(responseConsumer, observedConsumer -> Mono.defer(() -> {
            AiPreparedExchange primary = exchangePlanner.prepare(
                    requestBody, new AiCommonConfig(primaryConfig));
            return executionCoordinator.execute(requestBody, primary, fallbackConfig, observedConsumer);
        }));
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

    /**
     * Incrementally configures optional engine policies without exposing execution assembly types.
     */
    public static final class Builder {

        private final AiProxyProtocolFactory protocols;

        private final AiProxyProviderFactory providers;

        private final AiProtocolRouteResolver routeResolver;

        private final AiProxyTransport transport;

        private AiProxyRetryProperties retryProperties = new AiProxyRetryProperties();

        private AiCallObserver observer;

        private AiProxyAttemptListener attemptListener = event -> {
        };

        private AiExactResponseCache responseCache;

        private Builder(final AiProxyProtocolFactory protocols, final AiProxyProviderFactory providers,
                        final AiProtocolRouteResolver routeResolver, final AiProxyTransport transport) {
            this.protocols = Objects.requireNonNull(protocols, "protocols must not be null");
            this.providers = Objects.requireNonNull(providers, "providers must not be null");
            this.routeResolver = Objects.requireNonNull(routeResolver, "routeResolver must not be null");
            this.transport = Objects.requireNonNull(transport, "transport must not be null");
        }

        /**
         * Configure retry, replay and shared-deadline policies.
         *
         * @param retryProperties retry policies
         * @return this builder
         */
        public Builder retryProperties(final AiProxyRetryProperties retryProperties) {
            this.retryProperties = Objects.requireNonNull(retryProperties, "retryProperties must not be null");
            return this;
        }

        /**
         * Configure the optional logical-call observer.
         *
         * @param observer logical-call observer, or null to disable standalone observation export
         * @return this builder
         */
        public Builder observer(final AiCallObserver observer) {
            this.observer = observer;
            return this;
        }

        /**
         * Configure attempt lifecycle events.
         *
         * @param attemptListener attempt listener
         * @return this builder
         */
        public Builder attemptListener(final AiProxyAttemptListener attemptListener) {
            this.attemptListener = Objects.requireNonNull(attemptListener, "attemptListener must not be null");
            return this;
        }

        /**
         * Configure the optional exact response cache.
         *
         * @param responseCache exact response cache, or null to disable caching
         * @return this builder
         */
        public Builder responseCache(final AiExactResponseCache responseCache) {
            this.responseCache = responseCache;
            return this;
        }

        /**
         * Assemble an engine while keeping its execution graph private.
         *
         * @return configured engine
         */
        public AiProxyEngine build() {
            return new AiProxyEngine(protocols, providers, routeResolver, transport,
                    new AiProxyEngineOptions(retryProperties, observer, attemptListener, responseCache));
        }
    }

}
