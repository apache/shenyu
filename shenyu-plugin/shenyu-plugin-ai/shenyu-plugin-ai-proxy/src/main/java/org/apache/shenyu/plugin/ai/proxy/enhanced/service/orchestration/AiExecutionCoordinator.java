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

package org.apache.shenyu.plugin.ai.proxy.enhanced.service.orchestration;


import org.apache.shenyu.common.metrics.AiRequestObservation;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.proxy.enhanced.cache.AiExactResponseCache;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyRetryProperties;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiStructuredOutputPolicy;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiProxyException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.admission.AiConcurrencyAdmission;
import org.apache.shenyu.plugin.ai.proxy.enhanced.admission.AiConcurrencyAdmission.AdmissionOutcome;
import org.apache.shenyu.plugin.ai.proxy.enhanced.admission.AiConcurrencyAdmission.AdmissionResult;
import org.apache.shenyu.plugin.ai.proxy.enhanced.admission.AiConcurrencyAdmission.AdmissionTicket;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.execution.AiExecutionState;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.structured.AiStructuredOutputSession;
import reactor.core.publisher.Mono;

import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;

/**
 * Coordinates execution state and response caching for one logical AI request.
 */
public final class AiExecutionCoordinator {

    private final AiAttemptOrchestrator attemptOrchestrator;

    private final AiProxyRetryProperties retryProperties;

    private final AiExactResponseCache responseCache;

    private final AiConcurrencyAdmission admission;

    public AiExecutionCoordinator(final AiAttemptOrchestrator attemptOrchestrator,
                           final AiProxyRetryProperties retryProperties,
                           final AiExactResponseCache responseCache,
                           final AiConcurrencyAdmission admission) {
        this.attemptOrchestrator = Objects.requireNonNull(attemptOrchestrator, "attemptOrchestrator must not be null");
        this.retryProperties = Objects.requireNonNull(retryProperties, "retryProperties must not be null");
        this.responseCache = responseCache;
        this.admission = admission;
    }

    public Mono<Void> execute(final AiExecutionRequest request,
                              final Function<AiUpstreamResponse, Mono<Void>> responseConsumer) {
        return Mono.deferContextual(view -> {
            AiRequestObservation observation = Objects.requireNonNull(
                    view.getOrDefault(AiRequestObservation.REACTOR_KEY, null),
                    "AI request observation must be established");
            AiExecutionState.RequestDeadline requestDeadline = new AiExecutionState.RequestDeadline(
                    System.nanoTime() + retryProperties.getRequestDeadline().toNanos());
            AiStructuredOutputPolicy structuredOutputPolicy =
                    AiStructuredOutputPolicy.of(request.primaryConfig().getStructuredOutput());

            Optional<AiCommonConfig> fallback = request.fallbackConfig().map(config -> {
                AiCommonConfig copy = new AiCommonConfig(config);
                copy.setStream(request.primary().stream());
                // Fallback changes the upstream, never the already selected client protocol.
                copy.setProtocol(request.primary().clientProtocol());
                return copy;
            });

            // the logical permit is held across retries, fallbacks and repairs until the
            // terminal signal; the tenant is the trusted identity, never wire metadata (#8254)
            final AtomicReference<AdmissionTicket> logicalTicket = new AtomicReference<>();
            Mono<AdmissionResult> logicalAdmission = Objects.isNull(admission)
                    ? Mono.just(AdmissionResult.ADMITTED)
                    : admission.acquireTenant(observation.getContext().getTenant(),
                            requestDeadline.deadlineNanos());
            Function<Function<AiUpstreamResponse, Mono<Void>>, Mono<Void>> upstream = consumer -> {
                AiExecutionState execution = new AiExecutionState(
                        new AiExecutionState.RequestExecution(
                                request.payload(),
                                new AiExecutionState.ActiveUpstream(request.primaryConfig()),
                                new AiStructuredOutputSession(structuredOutputPolicy)),
                        new AiExecutionState.AttemptLifecycle(requestDeadline, observation),
                        new AiExecutionState.ResponseBoundary(consumer));
                return attemptOrchestrator.execute(execution, request.primary(), fallback);
            };

            Function<Function<AiUpstreamResponse, Mono<Void>>, Mono<Void>> gated = consumer -> {
                // the logical permit is acquired outside the cache/orchestration split and
                // released at the terminal signal, after retries, fallbacks, repairs and
                // cache reads are all done
                Mono<Void> inner;
                if (Objects.nonNull(responseCache)) {
                    inner = responseCache.execute(
                                    new AiExactResponseCache.CacheRequest(
                                            new AiExactResponseCache.CacheKeySource(
                                                    request.payload().body(), request.primary().request()),
                                            new AiExactResponseCache.CacheIdentity(
                                                    request.primary().providerName(), request.primary().clientProtocol(),
                                                    request.primary().targetProtocol()),
                                            !request.primary().stream() && fallback.isEmpty()
                                                    && request.primary().cacheCompatible()),
                                    responseConsumer,
                                    upstream)
                            .timeout(requestDeadline.remainingTimeout(),
                                    Mono.error(requestDeadline.timeoutException()));
                } else {
                    inner = upstream.apply(consumer)
                            .timeout(requestDeadline.remainingTimeout(),
                                    Mono.error(requestDeadline.timeoutException()));
                }
                Mono<Void> gatedChain = Objects.nonNull(admission)
                        ? inner.doFinally(ignored -> admission.release(logicalTicket.get())) : inner;
                return logicalAdmission.flatMap(result -> {
                    if (result.outcome() != AdmissionOutcome.ACQUIRED) {
                        return Mono.error(new AiProxyException(
                                AiProxyException.Kind.ADMISSION,
                                "AI request rejected by concurrency admission: " + result.outcome(), null));
                    }
                    logicalTicket.set(result.ticket());
                    return gatedChain;
                });
            };

            return gated.apply(responseConsumer);
        });
    }
}
