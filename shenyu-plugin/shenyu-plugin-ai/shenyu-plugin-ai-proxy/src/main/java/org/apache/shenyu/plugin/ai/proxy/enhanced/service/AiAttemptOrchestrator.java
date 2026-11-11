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
import org.apache.shenyu.common.metrics.AiStreamCompletion;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiProxyException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.event.AiProxyAttemptEvent;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.event.AiProxyAttemptListener;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiTransportException;
import reactor.core.publisher.Mono;

import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

/**
 * Orchestrates primary, retry and fallback attempts for one logical AI request.
 */
final class AiAttemptOrchestrator {

    private static final AiProxyRetryPolicy RETRY_POLICY = new AiProxyRetryPolicy();

    private final AiExchangePlanner exchangePlanner;

    private final AiAttemptExecutor attemptExecutor;

    private final AiRetrySettings retrySettings;

    private final AiProxyAttemptListener attemptListener;

    private final AiResponseConsumer responseConsumer;

    AiAttemptOrchestrator(final AiExchangePlanner exchangePlanner,
                          final AiAttemptExecutor attemptExecutor,
                          final AiProxyRetryProperties retryProperties,
                          final AiProxyAttemptListener attemptListener) {
        this.exchangePlanner = Objects.requireNonNull(exchangePlanner, "exchangePlanner must not be null");
        this.attemptExecutor = Objects.requireNonNull(attemptExecutor, "attemptExecutor must not be null");
        this.retrySettings = new AiRetrySettings(retryProperties);
        this.attemptListener = Objects.requireNonNull(attemptListener, "attemptListener must not be null");
        this.responseConsumer = new AiResponseConsumer();
    }

    Mono<Void> execute(final AiExecutionState execution, final AiPreparedExchange primary,
                       final Optional<AiCommonConfig> fallback) {
        int maxRetries = retrySettings.maxRetries(primary.stream());
        Optional<AiAttemptPlan.FallbackPlan> fallbackPlan = fallback.map(config -> new AiAttemptPlan.FallbackPlan(
                config, exchangePlanner.providerDeclaredReplaySafe(config, primary.operation())));
        return attempt(new AiAttemptPlan(execution, primary, fallbackPlan,
                new AiAttemptPlan.RetryBudget(maxRetries, maxRetries), AiProxyAttemptEvent.Phase.PRIMARY));
    }

    private Mono<Void> attempt(final AiAttemptPlan plan) {
        AiExecutionState execution = plan.execution();
        final int currentAttempt = execution.attemptIndex().getAndIncrement();
        if (hasRequestDeadlineExpired(execution.deadlineNanos())) {
            emit(execution.invocationId(), currentAttempt, plan.phase(),
                    AiProxyAttemptEvent.Reason.REQUEST_DEADLINE_EXCEEDED, null);
            return Mono.error(new AiProxyException(AiProxyException.Kind.TRANSPORT,
                    "AI proxy request deadline exceeded before attempt", null));
        }
        return attemptExecutor.execute(execution, plan.prepared(), currentAttempt, plan.phase(),
                        (response, observation) -> handleResponse(response, plan, currentAttempt, observation))
                .onErrorResume(error -> resumeAttempt(error, plan, currentAttempt));
    }

    private Mono<Void> handleResponse(final AiUpstreamResponse response, final AiAttemptPlan plan,
                                      final int currentAttempt, final AiRequestObservation.Attempt observation) {
        return Mono.defer(() -> {
            int status = response.getStatusCode();
            if (status >= 400) {
                return handleStatusFailure(response, plan, currentAttempt);
            }
            AiUpstreamResponse adapted = plan.prepared().clientResponseAdapter().adapt(response);
            return responseConsumer.consume(adapted, plan.execution().responseConsumer(), plan.execution().emitted())
                    .doOnSuccess(ignored -> emit(plan.execution().invocationId(), currentAttempt, plan.phase(),
                            plan.prepared().stream() && observation.streamCompletion().evidence() != AiStreamCompletion.Evidence.UPSTREAM_MARKER
                                    ? AiProxyAttemptEvent.Reason.STREAM_COMPLETION_UNCONFIRMED : AiProxyAttemptEvent.Reason.COMPLETED, null));
        });
    }

    private Mono<Void> resumeAttempt(final Throwable error, final AiAttemptPlan plan, final int currentAttempt) {
        return Mono.defer(() -> {
            final AiProxyRetryPolicy.Decision decision;
            final Map<String, List<String>> responseHeaders;
            if (plan.execution().emitted().get()) {
                emit(plan.execution().invocationId(), currentAttempt, plan.phase(),
                        AiProxyAttemptEvent.Reason.OUTPUT_COMMITTED, null);
                return Mono.error(error);
            }
            if (error instanceof AiTransportException transportException) {
                responseHeaders = Map.of();
                decision = RETRY_POLICY.decideTransport(new AiProxyRetryPolicy.FailureContext(
                        transportException.getKind(), null, Map.of(), plan.retryBudget().remaining(),
                        plan.fallback().isPresent(), plan.prepared().conservativeReplaySafe(), plan.prepared().operation(),
                        retrySettings, plan.prepared().providerDeclaredReplaySafe(),
                        fallbackProviderDeclaredReplaySafe(plan)));
            } else if (error instanceof AiUpstreamStatusException statusFailure) {
                responseHeaders = statusFailure.headers();
                decision = RETRY_POLICY.decideStatus(new AiProxyRetryPolicy.FailureContext(
                        null, statusFailure.status(), statusFailure.headers(), plan.retryBudget().remaining(),
                        plan.fallback().isPresent(), plan.prepared().conservativeReplaySafe(), plan.prepared().operation(),
                        retrySettings, plan.prepared().providerDeclaredReplaySafe(),
                        fallbackProviderDeclaredReplaySafe(plan)));
            } else {
                return Mono.error(error);
            }
            if (decision == AiProxyRetryPolicy.Decision.RETRY) {
                return retry(error, plan, currentAttempt, responseHeaders);
            }
            if (decision == AiProxyRetryPolicy.Decision.FALLBACK) {
                return fallback(error, plan, currentAttempt, responseHeaders);
            }
            if (decision == AiProxyRetryPolicy.Decision.PROPAGATE_ERROR
                    && error instanceof AiTransportException) {
                emit(plan.execution().invocationId(), currentAttempt, plan.phase(),
                        AiProxyAttemptEvent.Reason.NON_RETRYABLE_TRANSPORT, null);
            }
            return Mono.error(error);
        });
    }

    private Mono<Void> retry(final Throwable error, final AiAttemptPlan plan, final int currentAttempt,
                             final Map<String, List<String>> responseHeaders) {
        return Mono.defer(() -> {
            final Duration retryAfter = AiProxyRetryPolicy.retryAfter(responseHeaders, retrySettings);
            if (AiProxyRetryPolicy.hasRetryAfter(responseHeaders)
                    && !retrySettings.acceptsRetryAfter(retryAfter)) {
                emit(plan.execution().invocationId(), currentAttempt, plan.phase(),
                        AiProxyAttemptEvent.Reason.REPLAY_BLOCKED, null);
                return Mono.error(error);
            }
            Duration delay = retrySettings.retryDelay(
                    plan.prepared().stream(), plan.retryBudget().retryIndex(), System.nanoTime());
            if (Objects.nonNull(retryAfter) && retryAfter.compareTo(delay) > 0) {
                delay = retryAfter;
            }
            Duration remaining = Duration.ofNanos(plan.execution().deadlineNanos() - System.nanoTime());
            if (remaining.isNegative() || remaining.isZero() || delay.compareTo(remaining) >= 0) {
                emit(plan.execution().invocationId(), currentAttempt, plan.phase(),
                        AiProxyAttemptEvent.Reason.REQUEST_DEADLINE_EXCEEDED, retryAfter);
                return Mono.error(new AiProxyException(AiProxyException.Kind.TRANSPORT,
                        "AI proxy request deadline exceeded before retry", error));
            }
            emit(plan.execution().invocationId(), currentAttempt, plan.phase(),
                    error instanceof AiTransportException
                            ? AiProxyAttemptEvent.Reason.RETRY_TRANSPORT : AiProxyAttemptEvent.Reason.RETRY_STATUS,
                    retryAfter);
            return Mono.delay(delay).then(attempt(plan.nextRetry()));
        });
    }

    private Mono<Void> fallback(final Throwable error, final AiAttemptPlan plan, final int currentAttempt,
                                final Map<String, List<String>> responseHeaders) {
        return Mono.defer(() -> {
            Duration remaining = Duration.ofNanos(plan.execution().deadlineNanos() - System.nanoTime());
            if (remaining.isNegative() || remaining.isZero()) {
                emit(plan.execution().invocationId(), currentAttempt, plan.phase(),
                        AiProxyAttemptEvent.Reason.REQUEST_DEADLINE_EXCEEDED,
                        AiProxyRetryPolicy.retryAfter(responseHeaders, retrySettings));
                return Mono.error(new AiProxyException(AiProxyException.Kind.TRANSPORT,
                        "AI proxy request deadline exceeded before fallback", error));
            }
            emit(plan.execution().invocationId(), currentAttempt, plan.phase(),
                    error instanceof AiTransportException
                            ? AiProxyAttemptEvent.Reason.FALLBACK_TRANSPORT : AiProxyAttemptEvent.Reason.FALLBACK_STATUS,
                    AiProxyRetryPolicy.retryAfter(responseHeaders, retrySettings));
            AiCommonConfig fallbackConfig = plan.fallback().orElseThrow().config();
            AiPreparedExchange fallbackExchange = exchangePlanner.prepare(
                    plan.execution().requestBody(), fallbackConfig);
            return attempt(plan.forFallback(fallbackExchange));
        });
    }

    private Mono<Void> handleStatusFailure(final AiUpstreamResponse response, final AiAttemptPlan plan,
                                           final int currentAttempt) {
        final AiProxyRetryPolicy.Decision decision =
                RETRY_POLICY.decideStatus(new AiProxyRetryPolicy.FailureContext(
                        null, response.getStatusCode(), response.getHeaders(), plan.retryBudget().remaining(),
                        plan.fallback().isPresent(), plan.prepared().conservativeReplaySafe(), plan.prepared().operation(),
                        retrySettings, plan.prepared().providerDeclaredReplaySafe(),
                        fallbackProviderDeclaredReplaySafe(plan)));
        if (decision == AiProxyRetryPolicy.Decision.FORWARD_RESPONSE) {
            emit(plan.execution().invocationId(), currentAttempt, plan.phase(),
                    AiProxyAttemptEvent.Reason.NON_RETRYABLE_RESPONSE, null);
            return responseConsumer.consume(
                    response, plan.execution().responseConsumer(), plan.execution().emitted());
        }
        return response.getBody().then(Mono.error(new AiUpstreamStatusException(
                response.getStatusCode(), response.getHeaders())));
    }

    private static boolean fallbackProviderDeclaredReplaySafe(final AiAttemptPlan plan) {
        return plan.fallback().map(AiAttemptPlan.FallbackPlan::providerDeclaredReplaySafe).orElse(false);
    }

    private void emit(final String invocationId, final int attemptIndex, final AiProxyAttemptEvent.Phase phase,
                      final AiProxyAttemptEvent.Reason reason, final Duration retryAfter) {
        try {
            attemptListener.onAttempt(new AiProxyAttemptEvent(invocationId, attemptIndex, phase, reason, retryAfter));
        } catch (RuntimeException ignored) {
            // Observation must never change the AI request outcome.
        }
    }

    private static boolean hasRequestDeadlineExpired(final long deadlineNanos) {
        return deadlineNanos - System.nanoTime() <= 0L;
    }
}
