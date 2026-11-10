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

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.shenyu.common.dto.AgentRequestContext;
import org.apache.shenyu.common.metrics.AiAttemptEvent;
import org.apache.shenyu.common.metrics.AiRequestObservation;
import org.apache.shenyu.common.metrics.AiStreamCompletion;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.proxy.enhanced.cache.AiExactResponseCache;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiProxyException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiProxyTransport;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiTransportException;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.SignalType;

import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;

/**
 * Coordinates primary, retry and fallback attempts for one logical AI request.
 */
final class AiExecutionCoordinator {

    private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper().enable(DeserializationFeature.FAIL_ON_TRAILING_TOKENS);

    private static final AiProxyRetryPolicy RETRY_POLICY = new AiProxyRetryPolicy();

    private final AiExchangePlanner exchangePlanner;

    private final AiProxyTransport transport;

    private final AiProxyRetryProperties retryProperties;

    private final AiProxyAttemptListener attemptListener;

    private final AiExactResponseCache responseCache;

    AiExecutionCoordinator(final AiExchangePlanner exchangePlanner,
                           final AiProxyTransport transport,
                           final AiProxyRetryProperties retryProperties,
                           final AiProxyAttemptListener attemptListener,
        final AiExactResponseCache responseCache) {
        this.exchangePlanner = Objects.requireNonNull(exchangePlanner, "exchangePlanner must not be null");
        this.transport = Objects.requireNonNull(transport, "transport must not be null");
        this.retryProperties = Objects.requireNonNull(retryProperties, "retryProperties must not be null");
        this.attemptListener = Objects.requireNonNull(attemptListener, "attemptListener must not be null");
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

            int maxRetries = retryProperties.maxRetries(primary.stream());
            boolean fallbackProviderDeclaredReplaySafe = fallback
                    .map(config -> exchangePlanner.providerDeclaredReplaySafe(
                            config, primary.operation()))
                    .orElse(false);

            Function<Function<AiUpstreamResponse, Mono<Void>>, Mono<Void>> upstream = consumer -> {
                AiExecutionState execution = new AiExecutionState(
                        requestBody,
                        consumer,
                        new AtomicBoolean(),
                        new AtomicInteger(),
                        invocationId,
                        deadlineNanos,
                        observation);
                return attempt(
                        execution,
                        primary,
                        fallback,
                        maxRetries,
                        maxRetries,
                        AiProxyAttemptEvent.Phase.PRIMARY,
                        fallbackProviderDeclaredReplaySafe);
            };

            if (Objects.nonNull(responseCache) && primary.cacheCompatible()) {
                return responseCache.execute(
                                requestBody,
                                primary.request(),
                                primary.provider(),
                                primary.clientProtocol(),
                                primary.targetProtocol(),
                                primary.stream(),
                                fallback.isPresent(),
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

    private Mono<Void> attempt(final AiExecutionState execution, final AiPreparedExchange prepared,
                               final Optional<AiCommonConfig> fallback, final int maxRetries, final int retriesRemaining,
                               final AiProxyAttemptEvent.Phase phase,
                               final boolean fallbackProviderDeclaredReplaySafe) {
        final int currentAttempt = execution.attemptIndex().getAndIncrement();
        long deadlineStartNanos = System.nanoTime();
        long remainingNanos = execution.deadlineNanos() - deadlineStartNanos;
        if (remainingNanos <= 0L) {
            emit(execution.invocationId(), currentAttempt, phase,
                    AiProxyAttemptEvent.Reason.REQUEST_DEADLINE_EXCEEDED, null);
            return Mono.error(new AiProxyException(AiProxyException.Kind.TRANSPORT,
                    "AI proxy request deadline exceeded before attempt", null));
        }
        return Mono.defer(() -> {
            // A configured protocol identifier is not a negotiated protocol version.
            AgentRequestContext target = execution.observation().getContext()
                    .withTarget(selectedModel(prepared.request()), prepared.operation().name(), null);
            AiAttemptEvent.Kind kind = phase == AiProxyAttemptEvent.Phase.FALLBACK
                    ? AiAttemptEvent.Kind.FALLBACK : currentAttempt == 0
                    ? AiAttemptEvent.Kind.PRIMARY : AiAttemptEvent.Kind.RETRY;
            AiRequestObservation.Attempt observedAttempt = execution.observation()
                    .startAttempt(target, kind, prepared.provider(), prepared.clientProtocol(), prepared.stream());
            return transport.execute(prepared.request())
                    .flatMap(response -> {
                        observedAttempt.status(response.getStatusCode());
                        AiUpstreamResponse observed = new AiUpstreamUsageTap(
                                observedAttempt, response.getHeaders(), prepared.stream(), "openai-chat".equals(prepared.targetProtocol())
                                && response.getStatusCode() >= 200 && response.getStatusCode() < 300).attach(response);
                        return handleResponse(observed, prepared, fallback, retriesRemaining,
                                execution, currentAttempt, phase, fallbackProviderDeclaredReplaySafe, observedAttempt);
                    })
                    .timeout(Duration.ofNanos(Math.max(1L, remainingNanos - elapsedSince(deadlineStartNanos))),
                            Mono.error(new AiTransportException(
                                    AiTransportException.Kind.RESPONSE_HEADER_TIMEOUT,
                                    "AI proxy request deadline exceeded", null)))
                    .doOnSuccess(ignored -> observedAttempt.finish(SignalType.ON_COMPLETE, null))
                    .doOnError(error -> observedAttempt.finish(SignalType.ON_ERROR,
                            error instanceof UpstreamStatusFailure ? null : AiObservationSupport.outcome(error)))
                    .doOnCancel(() -> observedAttempt.finish(SignalType.CANCEL, null))
                    .contextWrite(context -> context.put(AgentRequestContext.REACTOR_KEY, target).put(AiRequestObservation.Attempt.REACTOR_KEY, observedAttempt))
                    .onErrorResume(error -> resumeAttempt(error, prepared, fallback, maxRetries,
                            retriesRemaining, execution, currentAttempt, phase, fallbackProviderDeclaredReplaySafe));
        });
    }

    private Mono<Void> handleResponse(final AiUpstreamResponse response, final AiPreparedExchange prepared,
                                      final Optional<AiCommonConfig> fallback, final int retriesRemaining,
                                      final AiExecutionState execution, final int currentAttempt,
                                      final AiProxyAttemptEvent.Phase phase,
                                      final boolean fallbackProviderDeclaredReplaySafe, final AiRequestObservation.Attempt observation) {
        return Mono.defer(() -> {
            int status = response.getStatusCode();
            if (status >= 400) {
                return handleStatusFailure(response, prepared, fallback, retriesRemaining,
                        execution, currentAttempt, phase, fallbackProviderDeclaredReplaySafe);
            }
            AiUpstreamResponse adapted = prepared.responseAdapter().adapt(response);
            return consumeResponse(adapted, execution.responseConsumer(), execution.emitted())
                    .doOnSuccess(ignored -> emit(execution.invocationId(), currentAttempt, phase,
                            prepared.stream() && observation.streamCompletion().evidence() != AiStreamCompletion.Evidence.UPSTREAM_MARKER
                                    ? AiProxyAttemptEvent.Reason.STREAM_COMPLETION_UNCONFIRMED : AiProxyAttemptEvent.Reason.COMPLETED, null));
        });
    }

    private Mono<Void> resumeAttempt(final Throwable error, final AiPreparedExchange prepared,
                                     final Optional<AiCommonConfig> fallback, final int maxRetries,
                                     final int retriesRemaining,
                                     final AiExecutionState execution,
                                     final int currentAttempt, final AiProxyAttemptEvent.Phase phase,
                                     final boolean fallbackProviderDeclaredReplaySafe) {
        return Mono.defer(() -> {
            final AiProxyRetryPolicy.Decision decision;
            final Map<String, List<String>> responseHeaders;
            if (execution.emitted().get()) {
                emit(execution.invocationId(), currentAttempt, phase,
                        AiProxyAttemptEvent.Reason.OUTPUT_COMMITTED, null);
                return Mono.error(error);
            }
            if (error instanceof AiTransportException transportException) {
                responseHeaders = Map.of();
                decision = RETRY_POLICY.decideTransport(new AiProxyRetryPolicy.FailureContext(
                        transportException.getKind(), null, Map.of(), retriesRemaining,
                        fallback.isPresent(), prepared.conservativeReplaySafe(), prepared.operation(),
                        retryProperties.getReplay(), prepared.providerDeclaredReplaySafe(),
                        fallbackProviderDeclaredReplaySafe));
            } else if (error instanceof UpstreamStatusFailure statusFailure) {
                responseHeaders = statusFailure.headers;
                decision = RETRY_POLICY.decideStatus(new AiProxyRetryPolicy.FailureContext(
                        null, statusFailure.status, statusFailure.headers, retriesRemaining,
                        fallback.isPresent(), prepared.conservativeReplaySafe(), prepared.operation(),
                        retryProperties.getReplay(), prepared.providerDeclaredReplaySafe(),
                        fallbackProviderDeclaredReplaySafe));
            } else {
                return Mono.error(error);
            }
            if (decision == AiProxyRetryPolicy.Decision.RETRY) {
                return retry(error, prepared, fallback, maxRetries, retriesRemaining,
                        currentAttempt, execution, phase,
                        fallbackProviderDeclaredReplaySafe, responseHeaders);
            }
            if (decision == AiProxyRetryPolicy.Decision.FALLBACK) {
                return fallback(error, prepared, fallback, currentAttempt, execution, phase, responseHeaders);
            }
            if (decision == AiProxyRetryPolicy.Decision.PROPAGATE_ERROR
                    && error instanceof AiTransportException) {
                emit(execution.invocationId(), currentAttempt, phase,
                        AiProxyAttemptEvent.Reason.NON_RETRYABLE_TRANSPORT, null);
            }
            return Mono.error(error);
        });
    }

    private Mono<Void> retry(final Throwable error, final AiPreparedExchange prepared,
                             final Optional<AiCommonConfig> fallback, final int maxRetries,
                             final int retriesRemaining, final int currentAttempt,
                             final AiExecutionState execution,
                             final AiProxyAttemptEvent.Phase phase, final boolean fallbackProviderDeclaredReplaySafe,
                             final Map<String, List<String>> responseHeaders) {
        return Mono.defer(() -> {
            final int retryIndex = maxRetries - retriesRemaining;
            final Duration retryAfter = AiProxyRetryPolicy.retryAfter(responseHeaders, retryProperties);
            if (AiProxyRetryPolicy.hasRetryAfter(responseHeaders)
                    && !retryProperties.acceptsRetryAfter(retryAfter)) {
                emit(execution.invocationId(), currentAttempt, phase,
                        AiProxyAttemptEvent.Reason.REPLAY_BLOCKED, null);
                return Mono.error(error);
            }
            Duration delay = retryProperties.retryDelay(prepared.stream(), retryIndex, System.nanoTime());
            if (Objects.nonNull(retryAfter) && retryAfter.compareTo(delay) > 0) {
                delay = retryAfter;
            }
            Duration remaining = Duration.ofNanos(execution.deadlineNanos() - System.nanoTime());
            if (remaining.isNegative() || remaining.isZero() || delay.compareTo(remaining) >= 0) {
                emit(execution.invocationId(), currentAttempt, phase,
                        AiProxyAttemptEvent.Reason.REQUEST_DEADLINE_EXCEEDED, retryAfter);
                return Mono.error(new AiProxyException(AiProxyException.Kind.TRANSPORT,
                        "AI proxy request deadline exceeded before retry", error));
            }
            emit(execution.invocationId(), currentAttempt, phase,
                    error instanceof AiTransportException
                            ? AiProxyAttemptEvent.Reason.RETRY_TRANSPORT : AiProxyAttemptEvent.Reason.RETRY_STATUS,
                    retryAfter);
            return Mono.delay(delay).then(attempt(execution, prepared, fallback, maxRetries,
                    retriesRemaining - 1, phase, fallbackProviderDeclaredReplaySafe));
        });
    }

    private Mono<Void> fallback(final Throwable error, final AiPreparedExchange prepared,
                                final Optional<AiCommonConfig> fallback,
                                final int currentAttempt, final AiExecutionState execution,
                                final AiProxyAttemptEvent.Phase phase,
                                final Map<String, List<String>> responseHeaders) {
        return Mono.defer(() -> {
            Duration remaining = Duration.ofNanos(execution.deadlineNanos() - System.nanoTime());
            if (remaining.isNegative() || remaining.isZero()) {
                emit(execution.invocationId(), currentAttempt, phase,
                        AiProxyAttemptEvent.Reason.REQUEST_DEADLINE_EXCEEDED,
                        AiProxyRetryPolicy.retryAfter(responseHeaders, retryProperties));
                return Mono.error(new AiProxyException(AiProxyException.Kind.TRANSPORT,
                        "AI proxy request deadline exceeded before fallback", error));
            }
            emit(execution.invocationId(), currentAttempt, phase,
                    error instanceof AiTransportException
                            ? AiProxyAttemptEvent.Reason.FALLBACK_TRANSPORT : AiProxyAttemptEvent.Reason.FALLBACK_STATUS,
                    AiProxyRetryPolicy.retryAfter(responseHeaders, retryProperties));
            return attempt(execution, exchangePlanner.prepare(execution.requestBody(), fallback.get()),
                    Optional.empty(), 0, 0, AiProxyAttemptEvent.Phase.FALLBACK, false);
        });
    }

    private Mono<Void> handleStatusFailure(final AiUpstreamResponse response, final AiPreparedExchange prepared,
                                           final Optional<AiCommonConfig> fallback, final int retriesRemaining,
                                           final AiExecutionState execution,
                                           final int currentAttempt,
                                           final AiProxyAttemptEvent.Phase phase,
                                           final boolean fallbackProviderDeclaredReplaySafe) {
        final AiProxyRetryPolicy.Decision decision =
                RETRY_POLICY.decideStatus(new AiProxyRetryPolicy.FailureContext(
                        null, response.getStatusCode(), response.getHeaders(), retriesRemaining,
                        fallback.isPresent(), prepared.conservativeReplaySafe(), prepared.operation(),
                        retryProperties.getReplay(), prepared.providerDeclaredReplaySafe(),
                        fallbackProviderDeclaredReplaySafe));
        if (decision == AiProxyRetryPolicy.Decision.FORWARD_RESPONSE) {
            emit(execution.invocationId(), currentAttempt, phase,
                    AiProxyAttemptEvent.Reason.NON_RETRYABLE_RESPONSE, null);
            return consumeResponse(response, execution.responseConsumer(), execution.emitted());
        }
        return response.getBody().then(Mono.error(new UpstreamStatusFailure(
                response.getStatusCode(), response.getHeaders())));
    }

    private void emit(final String invocationId, final int attemptIndex, final AiProxyAttemptEvent.Phase phase,
                      final AiProxyAttemptEvent.Reason reason, final Duration retryAfter) {
        try {
            attemptListener.onAttempt(new AiProxyAttemptEvent(invocationId, attemptIndex, phase, reason, retryAfter));
        } catch (RuntimeException ignored) {
            // Observation must never change the AI request outcome.
        }
    }

    private Mono<Void> consumeResponse(
            final AiUpstreamResponse response,
            final Function<AiUpstreamResponse, Mono<Void>> consumer,
            final AtomicBoolean emitted) {
        return response.getBody()
                .switchOnFirst((signal, body) -> {
                    if (signal.isOnError()) {
                        return Flux.error(
                                Objects.requireNonNull(
                                        signal.getThrowable()));
                    }

                    final AiUpstreamResponse ready =
                            new AiUpstreamResponse(
                                    response.getStatusCode(),
                                    response.getHeaders(),
                                    body.doOnNext(
                                            bytes -> emitted.set(true)));

                    return Mono.defer(
                            () -> consumer.apply(ready)).flux();
                })
                .then();
    }

    private String selectedModel(final AiUpstreamRequest request) {
        // Only inspect the already constructed upstream request, not a second subscription to either HTTP body.
        // A custom provider may use a non-JSON body; no target is fabricated in that case.
        try {
            JsonNode payload = OBJECT_MAPPER.readTree(request.getBody());
            JsonNode model = Objects.isNull(payload) ? null : payload.get("model");
            return Objects.nonNull(model) && model.isTextual() ? model.textValue() : null;
        } catch (Exception ignored) {
            return null;
        }
    }

    private long elapsedSince(final long startNanos) {
        return System.nanoTime() - startNanos;
    }

    private static final class UpstreamStatusFailure extends RuntimeException {

        private static final long serialVersionUID = 1L;

        private final int status;

        private final Map<String, List<String>> headers;

        private UpstreamStatusFailure(final int status, final Map<String, List<String>> headers) {
            super("AI upstream returned HTTP " + status);
            this.status = status;
            this.headers = Objects.requireNonNull(headers, "headers must not be null");
        }
    }

}
