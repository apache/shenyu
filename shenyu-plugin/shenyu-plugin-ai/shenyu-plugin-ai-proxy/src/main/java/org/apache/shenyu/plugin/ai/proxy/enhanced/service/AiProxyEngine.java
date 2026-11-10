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

import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProviderFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.routing.AiProtocolRouteResolver;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiProxyTransport;
import reactor.core.publisher.Mono;

import java.util.Optional;
import java.util.function.Function;

/**
 * Entry point for planning and executing reactive AI proxy exchanges.
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
        this(AiProxyEngineComponents.create(protocols, providers, routeResolver, transport, options));
    }

    private AiProxyEngine(final AiProxyEngineComponents components) {
        this.exchangePlanner = components.exchangePlanner();
        this.executionCoordinator = components.executionCoordinator();
        this.observationScope = components.observationScope();
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
        return observationScope.observe(responseConsumer, observedConsumer ->
                Mono.defer(() -> {
                    AiPreparedExchange primary = exchangePlanner.prepare(
                            requestBody, new AiCommonConfig(primaryConfig));
                    return executionCoordinator.execute(
                            requestBody,
                            primary,
                            fallbackConfig,
                            observedConsumer);
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

}
