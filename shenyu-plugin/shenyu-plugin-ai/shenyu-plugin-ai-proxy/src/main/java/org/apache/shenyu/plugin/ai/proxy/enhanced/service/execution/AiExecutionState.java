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


import com.fasterxml.jackson.databind.node.ObjectNode;
import org.apache.shenyu.common.metrics.AiRequestObservation;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.structured.AiStructuredOutputSession;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiTransportException;
import reactor.core.publisher.Mono;

import java.time.Duration;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;

/**
 * Request-scoped execution state shared by primary, retry and fallback attempts.
 *
 * @param request immutable request facts and active upstream selection
 * @param lifecycle attempt sequencing, deadline and observation lifecycle
 * @param response downstream response ownership boundary
 */
public record AiExecutionState(
        RequestExecution request,
        AttemptLifecycle lifecycle,
        ResponseBoundary response) {

    /**
     * Original request and policies that may select a different active upstream.
     *
     * @param payload original request payload
     * @param activeUpstream currently selected upstream configuration
     * @param structuredOutput structured output validation and repair session
     */
    public record RequestExecution(
            RequestPayload payload,
            ActiveUpstream activeUpstream,
        AiStructuredOutputSession structuredOutput) {
    }

    /**
     * Original wire payload retained for fallback and repair preparation.
     *
     * @param body original client request body
     * @param parsed original parsed object shared by primary and fallback planning
     */
    public record RequestPayload(String body, ObjectNode parsed) {

        public RequestPayload {
            Objects.requireNonNull(body, "body must not be null");
            Objects.requireNonNull(parsed, "parsed must not be null");
        }
    }

    /**
     * Mutable upstream selection owned by the orchestration flow.
     */
    public static final class ActiveUpstream {

        private final AtomicReference<AiCommonConfig> config;

        public ActiveUpstream(final AiCommonConfig initialConfig) {
            config = new AtomicReference<>(Objects.requireNonNull(initialConfig, "initialConfig must not be null"));
        }

        public AiCommonConfig current() {
            return config.get();
        }

        public void select(final AiCommonConfig selectedConfig) {
            config.set(Objects.requireNonNull(selectedConfig, "selectedConfig must not be null"));
        }
    }

    /**
     * Attempt sequencing, shared deadline and subscription-local observation.
     */
    public static final class AttemptLifecycle {

        private final AtomicInteger attemptIndex = new AtomicInteger();

        private final String invocationId = UUID.randomUUID().toString();

        private final RequestDeadline requestDeadline;

        private final AiRequestObservation observation;

        public AttemptLifecycle(final RequestDeadline requestDeadline, final AiRequestObservation observation) {
            this.requestDeadline = Objects.requireNonNull(requestDeadline, "requestDeadline must not be null");
            this.observation = Objects.requireNonNull(observation, "observation must not be null");
        }

        public int nextAttemptIndex() {
            return attemptIndex.getAndIncrement();
        }

        public String invocationId() {
            return invocationId;
        }

        public RequestDeadline requestDeadline() {
            return requestDeadline;
        }

        public AiRequestObservation observation() {
            return observation;
        }
    }

    /**
     * Owns the single downstream response consumer and commit state.
     */
    public static final class ResponseBoundary {

        private final Function<AiUpstreamResponse, Mono<Void>> consumer;

        private final AtomicBoolean emitted = new AtomicBoolean();

        public ResponseBoundary(final Function<AiUpstreamResponse, Mono<Void>> consumer) {
            this.consumer = Objects.requireNonNull(consumer, "consumer must not be null");
        }

        public Function<AiUpstreamResponse, Mono<Void>> consumer() {
            return consumer;
        }

        public AtomicBoolean emitted() {
            return emitted;
        }
    }

    /**
     * Shared request deadline and its terminal transport failure.
     *
     * @param deadlineNanos absolute deadline from the monotonic clock
     */
    public record RequestDeadline(long deadlineNanos) {

        public Duration remaining() {
            return Duration.ofNanos(remainingNanos());
        }

        public Duration remainingTimeout() {
            return Duration.ofNanos(Math.max(1L, remainingNanos()));
        }

        public boolean expired() {
            return remainingNanos() <= 0L;
        }

        public AiTransportException timeoutException() {
            return new AiTransportException(AiTransportException.Kind.RESPONSE_HEADER_TIMEOUT,
                    "AI proxy request deadline exceeded", null);
        }

        private long remainingNanos() {
            return deadlineNanos - System.nanoTime();
        }
    }
}
