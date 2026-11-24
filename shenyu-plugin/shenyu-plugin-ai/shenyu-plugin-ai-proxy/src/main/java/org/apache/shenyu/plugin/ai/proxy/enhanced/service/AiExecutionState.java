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
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiTransportException;
import reactor.core.publisher.Mono;

import java.time.Duration;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;

/**
 * Request-scoped mutable execution state shared by primary, retry and fallback attempts.
 *
 * @param requestBody original client request body
 * @param responseConsumer client response boundary
 * @param emitted whether response bytes have reached the client
 * @param attemptIndex attempt sequence shared by primary and fallback
 * @param invocationId opaque logical invocation identifier
 * @param requestDeadline absolute shared request deadline
 * @param observation subscription-local terminal observation
 * @param structuredOutputPolicy request-level structured output policy
 * @param repairsRemaining bounded structured output regeneration budget
 * @param activeConfig configuration of the upstream that produced the current response
 */
record AiExecutionState(
        String requestBody,
        Function<AiUpstreamResponse, Mono<Void>> responseConsumer,
        AtomicBoolean emitted,
        AtomicInteger attemptIndex,
        String invocationId,
        RequestDeadline requestDeadline,
        AiRequestObservation observation,
        AiStructuredOutputPolicy structuredOutputPolicy,
        AtomicInteger repairsRemaining,
        AtomicReference<AiCommonConfig> activeConfig) {

    /**
     * Shared request deadline and its terminal transport failure.
     *
     * @param deadlineNanos absolute deadline from the monotonic clock
     */
    record RequestDeadline(long deadlineNanos) {

        Duration remaining() {
            return Duration.ofNanos(remainingNanos());
        }

        Duration remainingTimeout() {
            return Duration.ofNanos(Math.max(1L, remainingNanos()));
        }

        boolean expired() {
            return remainingNanos() <= 0L;
        }

        AiTransportException timeoutException() {
            return new AiTransportException(AiTransportException.Kind.RESPONSE_HEADER_TIMEOUT,
                    "AI proxy request deadline exceeded", null);
        }

        private long remainingNanos() {
            return deadlineNanos - System.nanoTime();
        }
    }
}
