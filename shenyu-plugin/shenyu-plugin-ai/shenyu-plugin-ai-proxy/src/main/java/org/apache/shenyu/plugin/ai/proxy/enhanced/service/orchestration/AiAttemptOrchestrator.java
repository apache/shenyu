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

import org.apache.shenyu.plugin.ai.proxy.enhanced.service.orchestration.AiAttemptOutcome.Decision;

import org.apache.shenyu.common.metrics.AiRequestObservation;
import org.apache.shenyu.common.metrics.AiStreamCompletion;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiContentCompliance;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiProxyException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.ShenyuAiRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.event.AiProxyAttemptEvent;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.execution.AiAttemptExecutor;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.execution.AiExecutionState;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.execution.AiResponseConsumer;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.failure.AiUpstreamStatusException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.planning.AiExchangePlanner;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.planning.AiPreparedExchange;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.structured.AiStructuredOutputAttempt;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.structured.AiStructuredOutputDecision;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.structured.AiStructuredOutputEvaluator;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiTransportException;
import reactor.core.publisher.Mono;

import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

/**
 * Orchestrates primary, retry, fallback and repair transitions for one logical AI request.
 */
public final class AiAttemptOrchestrator {

    private static final int MINIMUM_ERROR_STATUS = 400;

    private static final AiProxyRetryPolicy RETRY_POLICY = new AiProxyRetryPolicy();

    private final AiExchangePlanner exchangePlanner;

    private final AiAttemptExecutor attemptExecutor;

    private final AiRetrySettings retrySettings;

    private final AiAttemptEventEmitter eventEmitter;

    private final AiStructuredOutputEvaluator structuredOutputEvaluator = new AiStructuredOutputEvaluator();

    private final AiResponseConsumer responseConsumer = new AiResponseConsumer();

    private final AiStatusOutcomeResolver statusOutcomeResolver;

    public AiAttemptOrchestrator(final AiExchangePlanner exchangePlanner,
                          final AiAttemptExecutor attemptExecutor,
                          final AiRetrySettings retrySettings,
                          final AiAttemptEventEmitter eventEmitter,
                          final AiStatusOutcomeResolver statusOutcomeResolver) {
        this.exchangePlanner = Objects.requireNonNull(exchangePlanner, "exchangePlanner must not be null");
        this.attemptExecutor = Objects.requireNonNull(attemptExecutor, "attemptExecutor must not be null");
        this.retrySettings = Objects.requireNonNull(retrySettings, "retrySettings must not be null");
        this.eventEmitter = Objects.requireNonNull(eventEmitter, "eventEmitter must not be null");
        this.statusOutcomeResolver = Objects.requireNonNull(
                statusOutcomeResolver, "statusOutcomeResolver must not be null");
    }

    public Mono<Void> execute(final AiExecutionState execution, final AiPreparedExchange primary,
                       final Optional<AiCommonConfig> fallback) {
        int maxRetries = retrySettings.maxRetries(primary.stream());
        Optional<AiAttemptPlan.FallbackPlan> fallbackPlan = fallback.map(config -> new AiAttemptPlan.FallbackPlan(
                config, exchangePlanner.providerDeclaredReplaySafe(config, primary.operation())));
        return attempt(new AiAttemptPlan(execution, primary, fallbackPlan,
                new AiAttemptPlan.RetryBudget(maxRetries, maxRetries), AiProxyAttemptEvent.Phase.PRIMARY));
    }

    private Mono<Void> attempt(final AiAttemptPlan plan) {
        AiExecutionState execution = plan.execution();
        final int currentAttempt = execution.lifecycle().nextAttemptIndex();
        if (execution.lifecycle().requestDeadline().expired()) {
            eventEmitter.emit(execution.lifecycle().invocationId(), currentAttempt, plan.phase(),
                    AiProxyAttemptEvent.Reason.REQUEST_DEADLINE_EXCEEDED, null);
            return Mono.error(new AiProxyException(AiProxyException.Kind.TRANSPORT,
                    "AI proxy request deadline exceeded before attempt", null));
        }
        return attemptExecutor.execute(execution.lifecycle(), plan.prepared(), currentAttempt, plan.phase(),
                        (response, observation) -> handleResponse(response, plan, currentAttempt, observation))
                .onErrorResume(error -> classifyFailure(error, plan, currentAttempt))
                .flatMap(outcome -> applyOutcome(outcome, plan, currentAttempt));
    }

    private Mono<AiAttemptOutcome> handleResponse(final AiUpstreamResponse response, final AiAttemptPlan plan,
                                                  final int currentAttempt,
                                                  final AiRequestObservation.Attempt observation) {
        return Mono.defer(() -> {
            if (response.getStatusCode() >= MINIMUM_ERROR_STATUS) {
                return statusOutcomeResolver.resolve(response, plan, currentAttempt);
            }
            AiUpstreamResponse adapted = plan.prepared().clientResponseAdapter().adapt(response);
            return handleSuccessfulResponse(adapted, plan, currentAttempt)
                    .doOnSuccess(ignored -> emitCompletion(plan, currentAttempt, observation))
                    .thenReturn(new AiAttemptOutcome.Completed());
        });
    }

    private Mono<Void> handleSuccessfulResponse(final AiUpstreamResponse response,
                                                final AiAttemptPlan plan,
                                                final int currentAttempt) {
        AiExecutionState execution = plan.execution();
        if (!structuredOutputEnforced(plan)) {
            return consume(response, execution);
        }
        AiStructuredOutputAttempt.Phase phase = plan.phase() == AiProxyAttemptEvent.Phase.REPAIR
                ? AiStructuredOutputAttempt.Phase.REPAIR : AiStructuredOutputAttempt.Phase.INITIAL;
        AiStructuredOutputAttempt attempt = new AiStructuredOutputAttempt(
                execution.request().structuredOutput(), execution.request().payload().body(), phase);
        return structuredOutputEvaluator.evaluate(response, attempt)
                .flatMap(decision -> applyStructuredOutputDecision(decision, plan, currentAttempt));
    }

    private boolean structuredOutputEnforced(final AiAttemptPlan plan) {
        return plan.execution().request().structuredOutput().policy().isActive()
                && !plan.prepared().stream()
                && plan.prepared().operation() == ShenyuAiRequest.Operation.CHAT_COMPLETION;
    }

    private Mono<Void> applyStructuredOutputDecision(final AiStructuredOutputDecision decision,
                                                     final AiAttemptPlan plan,
                                                     final int currentAttempt) {
        if (decision instanceof AiStructuredOutputDecision.Accepted accepted) {
            if (accepted.compliance() == AiContentCompliance.VALID
                    && plan.phase() != AiProxyAttemptEvent.Phase.REPAIR) {
                eventEmitter.emit(plan.execution().lifecycle().invocationId(), currentAttempt, plan.phase(),
                        AiProxyAttemptEvent.Reason.CONTENT_VALIDATED, null);
            }
            return consume(accepted.response(), plan.execution());
        }
        if (decision instanceof AiStructuredOutputDecision.RepairRequired repair) {
            return startRepair(plan, repair.requestBody(), currentAttempt);
        }
        AiStructuredOutputDecision.Rejected rejected = (AiStructuredOutputDecision.Rejected) decision;
        AiProxyAttemptEvent.Reason reason = plan.phase() == AiProxyAttemptEvent.Phase.REPAIR
                ? AiProxyAttemptEvent.Reason.CONTENT_REPAIR_FAILED
                : AiProxyAttemptEvent.Reason.CONTENT_REJECTED;
        eventEmitter.emit(plan.execution().lifecycle().invocationId(), currentAttempt, plan.phase(), reason, null);
        return Mono.error(rejected.failure());
    }

    private Mono<Void> startRepair(final AiAttemptPlan plan, final String repairBody, final int currentAttempt) {
        AiExecutionState execution = plan.execution();
        if (execution.lifecycle().requestDeadline().expired()) {
            eventEmitter.emit(execution.lifecycle().invocationId(), currentAttempt, plan.phase(),
                    AiProxyAttemptEvent.Reason.REQUEST_DEADLINE_EXCEEDED, null);
            return Mono.error(new AiProxyException(AiProxyException.Kind.TRANSPORT,
                    "AI proxy request deadline exceeded before repair", null));
        }
        AiPreparedExchange repairPrepared = exchangePlanner.prepare(
                repairBody, execution.request().activeUpstream().current());
        eventEmitter.emit(execution.lifecycle().invocationId(), currentAttempt, plan.phase(),
                AiProxyAttemptEvent.Reason.CONTENT_REPAIR, null);
        return attempt(plan.forRepair(repairPrepared))
                .onErrorMap(error -> error instanceof AiTransportException || error instanceof AiUpstreamStatusException
                        ? new AiProxyException(AiProxyException.Kind.TRANSPORT, "AI repair generation failed", error)
                        : error);
    }

    private Mono<Void> consume(final AiUpstreamResponse response, final AiExecutionState execution) {
        return responseConsumer.consume(
                response, execution.response().consumer(), execution.response().emitted());
    }

    private void emitCompletion(final AiAttemptPlan plan, final int currentAttempt,
                                final AiRequestObservation.Attempt observation) {
        AiProxyAttemptEvent.Reason reason = plan.prepared().stream()
                && observation.streamCompletion().evidence() != AiStreamCompletion.Evidence.UPSTREAM_MARKER
                ? AiProxyAttemptEvent.Reason.STREAM_COMPLETION_UNCONFIRMED
                : AiProxyAttemptEvent.Reason.COMPLETED;
        eventEmitter.emit(plan.execution().lifecycle().invocationId(), currentAttempt, plan.phase(), reason, null);
    }

    private Mono<AiAttemptOutcome> classifyFailure(final Throwable error, final AiAttemptPlan plan,
                                                   final int currentAttempt) {
        return Mono.defer(() -> {
            if (plan.phase() == AiProxyAttemptEvent.Phase.REPAIR) {
                return Mono.error(error);
            }
            if (plan.execution().response().emitted().get()) {
                eventEmitter.emit(plan.execution().lifecycle().invocationId(), currentAttempt, plan.phase(),
                        AiProxyAttemptEvent.Reason.OUTPUT_COMMITTED, null);
                return Mono.just(new AiAttemptOutcome.Transition(
                        Decision.PROPAGATE_ERROR, error, Map.of()));
            }
            if (error instanceof AiTransportException transportException) {
                return classifyTransportFailure(transportException, plan, currentAttempt);
            }
            return Mono.error(error);
        });
    }

    private Mono<AiAttemptOutcome> classifyTransportFailure(final AiTransportException error,
                                                             final AiAttemptPlan plan,
                                                             final int currentAttempt) {
        Decision decision = RETRY_POLICY.decideTransport(
                new AiProxyRetryPolicy.FailureContext(
                        error.getKind(), null, Map.of(), plan.retryBudget().remaining(),
                        plan.fallback().isPresent(), plan.prepared().conservativeReplaySafe(),
                        plan.prepared().operation(), retrySettings,
                        plan.prepared().providerDeclaredReplaySafe(), fallbackProviderDeclaredReplaySafe(plan)));
        if (decision == Decision.PROPAGATE_ERROR) {
            eventEmitter.emit(plan.execution().lifecycle().invocationId(), currentAttempt, plan.phase(),
                    AiProxyAttemptEvent.Reason.NON_RETRYABLE_TRANSPORT, null);
        }
        return Mono.just(new AiAttemptOutcome.Transition(decision, error, Map.of()));
    }

    private Mono<Void> applyOutcome(final AiAttemptOutcome outcome, final AiAttemptPlan plan,
                                    final int currentAttempt) {
        if (outcome instanceof AiAttemptOutcome.Completed) {
            return Mono.empty();
        }
        AiAttemptOutcome.Transition transition = (AiAttemptOutcome.Transition) outcome;
        if (transition.decision() == Decision.RETRY) {
            return retry(transition.failure(), plan, currentAttempt, transition.responseHeaders());
        }
        if (transition.decision() == Decision.FALLBACK) {
            return fallback(transition.failure(), plan, currentAttempt, transition.responseHeaders());
        }
        return Mono.error(transition.failure());
    }

    private Mono<Void> retry(final Throwable error, final AiAttemptPlan plan, final int currentAttempt,
                             final Map<String, List<String>> responseHeaders) {
        return Mono.defer(() -> {
            final Duration retryAfter = AiProxyRetryPolicy.retryAfter(responseHeaders, retrySettings);
            if (AiProxyRetryPolicy.hasRetryAfter(responseHeaders)
                    && !retrySettings.acceptsRetryAfter(retryAfter)) {
                eventEmitter.emit(plan.execution().lifecycle().invocationId(), currentAttempt, plan.phase(),
                        AiProxyAttemptEvent.Reason.REPLAY_BLOCKED, null);
                return Mono.error(error);
            }
            Duration delay = retrySettings.retryDelay(
                    plan.prepared().stream(), plan.retryBudget().retryIndex(), System.nanoTime());
            if (Objects.nonNull(retryAfter) && retryAfter.compareTo(delay) > 0) {
                delay = retryAfter;
            }
            Duration remaining = plan.execution().lifecycle().requestDeadline().remaining();
            if (remaining.isNegative() || remaining.isZero() || delay.compareTo(remaining) >= 0) {
                eventEmitter.emit(plan.execution().lifecycle().invocationId(), currentAttempt, plan.phase(),
                        AiProxyAttemptEvent.Reason.REQUEST_DEADLINE_EXCEEDED, retryAfter);
                return Mono.error(new AiProxyException(AiProxyException.Kind.TRANSPORT,
                        "AI proxy request deadline exceeded before retry", error));
            }
            eventEmitter.emit(plan.execution().lifecycle().invocationId(), currentAttempt, plan.phase(),
                    error instanceof AiTransportException
                            ? AiProxyAttemptEvent.Reason.RETRY_TRANSPORT : AiProxyAttemptEvent.Reason.RETRY_STATUS,
                    retryAfter);
            return Mono.delay(delay).then(attempt(plan.nextRetry()));
        });
    }

    private Mono<Void> fallback(final Throwable error, final AiAttemptPlan plan, final int currentAttempt,
                                final Map<String, List<String>> responseHeaders) {
        return Mono.defer(() -> {
            Duration remaining = plan.execution().lifecycle().requestDeadline().remaining();
            if (remaining.isNegative() || remaining.isZero()) {
                eventEmitter.emit(plan.execution().lifecycle().invocationId(), currentAttempt, plan.phase(),
                        AiProxyAttemptEvent.Reason.REQUEST_DEADLINE_EXCEEDED,
                        AiProxyRetryPolicy.retryAfter(responseHeaders, retrySettings));
                return Mono.error(new AiProxyException(AiProxyException.Kind.TRANSPORT,
                        "AI proxy request deadline exceeded before fallback", error));
            }
            eventEmitter.emit(plan.execution().lifecycle().invocationId(), currentAttempt, plan.phase(), fallbackReason(error),
                    AiProxyRetryPolicy.retryAfter(responseHeaders, retrySettings));
            AiCommonConfig fallbackConfig = plan.fallback().orElseThrow().config();
            plan.execution().request().activeUpstream().select(fallbackConfig);
            AiExecutionState.RequestPayload payload = plan.execution().request().payload();
            AiPreparedExchange fallbackExchange = exchangePlanner.prepare(payload.body(), payload.parsed(), fallbackConfig);
            return attempt(plan.forFallback(fallbackExchange));
        });
    }

    private static AiProxyAttemptEvent.Reason fallbackReason(final Throwable error) {
        if (error instanceof AiTransportException) {
            return AiProxyAttemptEvent.Reason.FALLBACK_TRANSPORT;
        }
        if (error instanceof AiUpstreamStatusException statusFailure && statusFailure.credentialFailure()) {
            return AiProxyAttemptEvent.Reason.FALLBACK_CREDENTIAL;
        }
        return AiProxyAttemptEvent.Reason.FALLBACK_STATUS;
    }

    private static boolean fallbackProviderDeclaredReplaySafe(final AiAttemptPlan plan) {
        return plan.fallback().map(AiAttemptPlan.FallbackPlan::providerDeclaredReplaySafe).orElse(false);
    }
}
