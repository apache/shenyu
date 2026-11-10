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

import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProviderFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.routing.AiProtocolRouteResolver;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiProxyTransport;

import java.util.Objects;

/**
 * Internally assembled collaborators required by the AI proxy engine.
 *
 * @param exchangePlanner request exchange planner
 * @param observationScope logical-call observation scope
 * @param executionCoordinator attempt execution coordinator
 */
record AiProxyEngineComponents(
        AiExchangePlanner exchangePlanner,
        AiCallObservationScope observationScope,
        AiExecutionCoordinator executionCoordinator) {

    AiProxyEngineComponents {
        Objects.requireNonNull(exchangePlanner, "exchangePlanner must not be null");
        Objects.requireNonNull(observationScope, "observationScope must not be null");
        Objects.requireNonNull(executionCoordinator, "executionCoordinator must not be null");
    }

    static AiProxyEngineComponents create(
            final AiProxyProtocolFactory protocols,
            final AiProxyProviderFactory providers,
            final AiProtocolRouteResolver routeResolver,
            final AiProxyTransport transport,
            final AiProxyEngineOptions options) {
        Objects.requireNonNull(options, "options must not be null");
        AiExchangePlanner planner = new AiExchangePlanner(protocols, providers, routeResolver);
        return new AiProxyEngineComponents(
                planner,
                new AiCallObservationScope(options.observer()),
                new AiExecutionCoordinator(planner, transport, options.retryProperties(),
                        options.attemptListener(), options.responseCache()));
    }
}
