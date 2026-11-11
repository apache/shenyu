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

import org.apache.shenyu.common.metrics.AiRequestObservation;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.proxy.enhanced.cache.AiExactResponseCache;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiTransportException;
import reactor.core.publisher.Mono;

import java.time.Duration;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;

/**
 * Coordinates execution state and response caching for one logical AI request.
 */
final class AiExecutionCoordinator {

    private final AiAttemptOrchestrator attemptOrchestrator;

    private final AiProxyRetryProperties retryProperties;

    private final AiExactResponseCache responseCache;

    AiExecutionCoordinator(final AiAttemptOrchestrator attemptOrchestrator,
                           final AiProxyRetryProperties retryProperties,
                           final AiExactResponseCache responseCache) {
        this.attemptOrchestrator = Objects.requireNonNull(attemptOrchestrator, "attemptOrchestrator must not be null");
        this.retryProperties = Objects.requireNonNull(retryProperties, "retryProperties must not be null");
        this.responseCache = responseCache;
    }

    Mono<Void> execute(final String requestBody,
                       final AiPreparedExchange primary,
                       final Optional<AiCommonConfig> fallbackConfig,
                       final Function<AiUpstreamResponse, Mono<Void>> responseConsumer) {
        return Mono.deferContextual(view -> {
            AiRequestObservation observation = Objects.requireNonNull(
                    view.getOrDefault(AiRequestObservation.REACTOR_KEY, null),
                    "AI request observation must be established");
            long deadlineNanos = System.nanoTime()
                    + retryProperties.getRequestDeadline().toNanos();
            String invocationId = UUID.randomUUID().toString();

            Optional<AiCommonConfig> fallback = fallbackConfig.map(config -> {
                AiCommonConfig copy = new AiCommonConfig(config);
                copy.setStream(primary.stream());
                // Fallback changes the upstream, never the already selected client protocol.
                copy.setProtocol(primary.clientProtocol());
                return copy;
            });

            Function<Function<AiUpstreamResponse, Mono<Void>>, Mono<Void>> upstream = consumer -> {
                AiExecutionState execution = new AiExecutionState(
                        requestBody,
                        consumer,
                        new AtomicBoolean(),
                        new AtomicInteger(),
                        invocationId,
                        deadlineNanos,
                        observation);
                return attemptOrchestrator.execute(execution, primary, fallback);
            };

            if (Objects.nonNull(responseCache)) {
                return responseCache.execute(
                                requestBody,
                                primary.request(),
                                primary.providerName(),
                                primary.clientProtocol(),
                                primary.targetProtocol(),
                                primary.stream(),
                                fallback.isPresent(),
                                primary.cacheCompatible(),
                                responseConsumer,
                                upstream)
                        .timeout(
                                Duration.ofNanos(Math.max(
                                        1L, deadlineNanos - System.nanoTime())),
                                Mono.error(new AiTransportException(
                                        AiTransportException.Kind.RESPONSE_HEADER_TIMEOUT,
                                        "AI proxy request deadline exceeded",
                                        null)));
            }

            return upstream.apply(responseConsumer);
        });
    }
}
