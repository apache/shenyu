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
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.event.AiProxyAttemptEvent;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.observation.AiObservationSupport;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiProxyTransport;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiTransportException;
import reactor.core.publisher.Mono;
import reactor.core.publisher.SignalType;

import java.time.Duration;
import java.util.Objects;

/**
 * Executes one observed upstream attempt.
 */
final class AiAttemptExecutor {

    private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper().enable(DeserializationFeature.FAIL_ON_TRAILING_TOKENS);

    private final AiProxyTransport transport;

    AiAttemptExecutor(final AiProxyTransport transport) {
        this.transport = Objects.requireNonNull(transport, "transport must not be null");
    }

    Mono<Void> execute(final AiExecutionState execution, final AiPreparedExchange prepared,
                       final int attemptIndex, final AiProxyAttemptEvent.Phase phase,
                       final AiAttemptResponseHandler responseHandler) {
        Objects.requireNonNull(responseHandler, "responseHandler must not be null");
        return Mono.defer(() -> {
            AgentRequestContext target = execution.observation().getContext()
                    .withTarget(selectedModel(prepared.request()), prepared.operation().name(), null);
            AiRequestObservation.Attempt observation = execution.observation().startAttempt(
                    target, attemptKind(phase, attemptIndex), prepared.providerName(),
                    prepared.clientProtocol(), prepared.stream());
            return transport.execute(prepared.request())
                    .flatMap(response -> handleResponse(response, prepared, observation, responseHandler))
                    .timeout(remainingRequestTimeout(execution.deadlineNanos()),
                            Mono.error(new AiTransportException(
                                    AiTransportException.Kind.RESPONSE_HEADER_TIMEOUT,
                                    "AI proxy request deadline exceeded", null)))
                    .doOnSuccess(ignored -> observation.finish(SignalType.ON_COMPLETE, null))
                    .doOnError(error -> observation.finish(SignalType.ON_ERROR,
                            error instanceof AiUpstreamStatusException ? null : AiObservationSupport.outcome(error)))
                    .doOnCancel(() -> observation.finish(SignalType.CANCEL, null))
                    .contextWrite(context -> context.put(AgentRequestContext.REACTOR_KEY, target)
                            .put(AiRequestObservation.Attempt.REACTOR_KEY, observation));
        });
    }

    private Mono<Void> handleResponse(final AiUpstreamResponse response, final AiPreparedExchange prepared,
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
        return attemptIndex == 0 ? AiAttemptEvent.Kind.PRIMARY : AiAttemptEvent.Kind.RETRY;
    }

    private static Duration remainingRequestTimeout(final long deadlineNanos) {
        return Duration.ofNanos(Math.max(1L, deadlineNanos - System.nanoTime()));
    }

    private static String selectedModel(final AiUpstreamRequest request) {
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
}
