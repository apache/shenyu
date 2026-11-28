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

package org.apache.shenyu.plugin.ai.proxy.enhanced.service.execution;


import org.apache.shenyu.common.dto.AgentRequestContext;
import org.apache.shenyu.common.metrics.AiAttemptEvent;
import org.apache.shenyu.common.metrics.AiRequestObservation;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.event.AiProxyAttemptEvent;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.observation.AiObservationSupport;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.orchestration.AiAttemptOutcome;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiProxyException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.admission.AiConcurrencyAdmission;
import org.apache.shenyu.plugin.ai.proxy.enhanced.admission.AiConcurrencyAdmission.AdmissionOutcome;
import org.apache.shenyu.plugin.ai.proxy.enhanced.admission.AiConcurrencyAdmission.AdmissionResult;
import org.apache.shenyu.plugin.ai.proxy.enhanced.admission.AiConcurrencyAdmission.AdmissionTicket;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.planning.AiPreparedExchange;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiProxyTransport;
import reactor.core.publisher.Mono;
import reactor.core.publisher.SignalType;

import java.util.Objects;

/**
 * Executes one observed upstream attempt.
 */
public final class AiAttemptExecutor {

    private final AiProxyTransport transport;

    private final AiConcurrencyAdmission admission;

    public AiAttemptExecutor(final AiProxyTransport transport, final AiConcurrencyAdmission admission) {
        this.transport = Objects.requireNonNull(transport, "transport must not be null");
        this.admission = admission;
    }

    public Mono<AiAttemptOutcome> execute(final AiExecutionState.AttemptLifecycle lifecycle,
                                   final AiPreparedExchange prepared,
                                   final int attemptIndex, final AiProxyAttemptEvent.Phase phase,
                                   final AiAttemptResponseHandler responseHandler) {
        Objects.requireNonNull(responseHandler, "responseHandler must not be null");
        return Mono.defer(() -> {
            AgentRequestContext target = lifecycle.observation().getContext()
                    .withTarget(prepared.selectedModel(), prepared.operation().name(), null);
            AiRequestObservation.Attempt observation = lifecycle.observation().startAttempt(
                    target, attemptKind(phase, attemptIndex), prepared.providerName(),
                    prepared.clientProtocol(), prepared.stream());
            // the target permit covers exactly one attempt — for streaming that is body
            // completion — and backoff between attempts holds none (#8254)
            Mono<AdmissionResult> targetAdmission = Objects.isNull(admission)
                    ? Mono.just(AdmissionResult.ADMITTED)
                    : admission.acquireTarget(prepared.selectedModel(), lifecycle.requestDeadline().deadlineNanos());
            return targetAdmission.flatMap(result -> {
                if (result.outcome() != AdmissionOutcome.ACQUIRED) {
                    return Mono.error(new AiProxyException(AiProxyException.Kind.ADMISSION,
                            "AI attempt rejected by concurrency admission: " + result.outcome(), null));
                }
                AdmissionTicket targetTicket = result.ticket();
                return transport.execute(prepared.request())
                        .flatMap(response -> handleResponse(response, prepared, observation, responseHandler))
                        .timeout(lifecycle.requestDeadline().remainingTimeout(),
                                Mono.error(lifecycle.requestDeadline().timeoutException()))
                        .doFinally(signal -> {
                            if (java.util.Objects.nonNull(admission)) {
                                admission.release(targetTicket);
                            }
                        });
            })
                    .doOnSuccess(ignored -> observation.finish(SignalType.ON_COMPLETE, null))
                    .doOnError(error -> observation.finish(SignalType.ON_ERROR, AiObservationSupport.outcome(error)))
                    .doFinally(signal -> {
                        if (signal == SignalType.CANCEL) {
                            observation.finish(SignalType.CANCEL, null);
                        }
                    })
                    .contextWrite(context -> context.put(AgentRequestContext.REACTOR_KEY, target)
                            .put(AiRequestObservation.Attempt.REACTOR_KEY, observation));
        });
    }

    private Mono<AiAttemptOutcome> handleResponse(final AiUpstreamResponse response,
                                                  final AiPreparedExchange prepared,
                                                  final AiRequestObservation.Attempt observation,
                                                  final AiAttemptResponseHandler responseHandler) {
        observation.status(response.getStatusCode());
        AiUpstreamResponse inspected = prepared.upstreamResponseInspector()
                .inspect(response, observation, prepared.stream());
        return responseHandler.handle(inspected, observation);
    }

    private static AiAttemptEvent.Kind attemptKind(final AiProxyAttemptEvent.Phase phase, final int attemptIndex) {
        if (phase == AiProxyAttemptEvent.Phase.FALLBACK) {
            return AiAttemptEvent.Kind.FALLBACK;
        }
        if (phase == AiProxyAttemptEvent.Phase.REPAIR) {
            return AiAttemptEvent.Kind.REPAIR;
        }
        return attemptIndex == 0 ? AiAttemptEvent.Kind.PRIMARY : AiAttemptEvent.Kind.RETRY;
    }

}
