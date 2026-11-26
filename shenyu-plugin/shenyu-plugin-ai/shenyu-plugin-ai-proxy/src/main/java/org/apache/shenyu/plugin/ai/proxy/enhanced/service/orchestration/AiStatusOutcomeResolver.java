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

import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.event.AiProxyAttemptEvent;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.execution.AiResponseConsumer;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.failure.AiCredentialFailureDetector;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.failure.AiStatusFailure;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.failure.AiStatusFailureClassifier;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.failure.AiUpstreamStatusException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.orchestration.AiAttemptOutcome.Decision;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.Objects;
import java.util.Optional;

/**
 * Converts classified HTTP failures into forward, retry or fallback attempt outcomes.
 */
public final class AiStatusOutcomeResolver {

    private static final AiProxyRetryPolicy RETRY_POLICY = new AiProxyRetryPolicy();

    private final AiRetrySettings retrySettings;

    private final AiStatusFailureClassifier failureClassifier;

    private final AiAttemptEventEmitter eventEmitter;

    private final AiResponseConsumer responseConsumer = new AiResponseConsumer();

    public AiStatusOutcomeResolver(final AiRetrySettings retrySettings,
                                   final AiStatusFailureClassifier failureClassifier,
                                   final AiAttemptEventEmitter eventEmitter) {
        this.retrySettings = Objects.requireNonNull(retrySettings, "retrySettings must not be null");
        this.failureClassifier = Objects.requireNonNull(failureClassifier, "failureClassifier must not be null");
        this.eventEmitter = Objects.requireNonNull(eventEmitter, "eventEmitter must not be null");
    }

    public Mono<AiAttemptOutcome> resolve(final AiUpstreamResponse response, final AiAttemptPlan plan,
                                          final int currentAttempt) {
        if (plan.phase() == AiProxyAttemptEvent.Phase.REPAIR) {
            return response.getBody().then(Mono.error(
                    new AiUpstreamStatusException(response.getStatusCode())));
        }
        return failureClassifier.classify(response, credentialTarget(plan),
                failure -> decide(failure, plan, currentAttempt));
    }

    private Optional<AiCredentialFailureDetector.DetectionTarget> credentialTarget(final AiAttemptPlan plan) {
        if (plan.phase() != AiProxyAttemptEvent.Phase.PRIMARY
                || plan.execution().response().emitted().get() || plan.fallback().isEmpty()) {
            return Optional.empty();
        }
        return Optional.of(new AiCredentialFailureDetector.DetectionTarget(
                plan.prepared().request().getUri(), plan.prepared().targetProtocol()));
    }

    private Mono<AiAttemptOutcome> decide(final AiStatusFailure failure, final AiAttemptPlan plan,
                                          final int currentAttempt) {
        AiProxyRetryPolicy.FailureContext failureContext = failureContext(failure, plan);
        AiUpstreamResponse response = failure.credentialFailure()
                ? credentialFailureObserved(failure.response(), plan, currentAttempt) : failure.response();
        Decision decision = RETRY_POLICY.decideStatus(failureContext);
        if (decision == Decision.FORWARD_RESPONSE) {
            eventEmitter.emit(plan.execution().lifecycle().invocationId(), currentAttempt, plan.phase(),
                    AiProxyAttemptEvent.Reason.NON_RETRYABLE_RESPONSE, null);
            return responseConsumer.consume(
                            response, plan.execution().response().consumer(), plan.execution().response().emitted())
                    .thenReturn(new AiAttemptOutcome.Completed());
        }
        AiUpstreamStatusException statusFailure = new AiUpstreamStatusException(
                response.getStatusCode(), failure.credentialFailure());
        return response.getBody().then(Mono.just(new AiAttemptOutcome.Transition(
                decision, statusFailure, response.getHeaders())));
    }

    private AiProxyRetryPolicy.FailureContext failureContext(final AiStatusFailure failure,
                                                              final AiAttemptPlan plan) {
        AiUpstreamResponse response = failure.response();
        return new AiProxyRetryPolicy.FailureContext(
                null, response.getStatusCode(), response.getHeaders(), plan.retryBudget().remaining(),
                plan.fallback().isPresent(), plan.prepared().conservativeReplaySafe(), plan.prepared().operation(),
                retrySettings, plan.prepared().providerDeclaredReplaySafe(), fallbackProviderDeclaredReplaySafe(plan),
                failure.credentialFailure());
    }

    private AiUpstreamResponse credentialFailureObserved(final AiUpstreamResponse response,
                                                          final AiAttemptPlan plan,
                                                          final int currentAttempt) {
        Flux<byte[]> observedBody = response.getBody().doOnSubscribe(ignored ->
                eventEmitter.emit(plan.execution().lifecycle().invocationId(), currentAttempt, plan.phase(),
                        AiProxyAttemptEvent.Reason.CREDENTIAL_FAILURE, null));
        return new AiUpstreamResponse(response.getStatusCode(), response.getHeaders(), observedBody);
    }

    private static boolean fallbackProviderDeclaredReplaySafe(final AiAttemptPlan plan) {
        return plan.fallback().map(AiAttemptPlan.FallbackPlan::providerDeclaredReplaySafe).orElse(false);
    }
}
