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

import java.time.Duration;
import java.util.Objects;
import java.util.Optional;

/**
 * Invocation-scoped attempt identity and retry reason. Values intentionally contain
 * no prompt, response, credential, endpoint, or provider content so downstream
 * metering cannot accidentally persist sensitive request data.
 *
 * @param invocationId opaque identifier shared by every attempt in one logical request
 * @param attemptIndex zero-based attempt index within the logical execution
 * @param phase whether this attempt is the primary or fallback phase
 * @param reason closed reason describing why the attempt ended
 * @param retryAfter bounded upstream delay hint, when one was supplied
 */
public record AiProxyAttemptEvent(
        String invocationId,
        int attemptIndex,
        Phase phase,
        Reason reason,
        Duration retryAfter) {

    /**
     * Create an event with a closed reason vocabulary.
     *
     * @param invocationId identifier shared by one logical request
     * @param attemptIndex zero-based attempt index
     * @param phase primary or fallback phase
     * @param reason closed ending reason
     * @param retryAfter bounded upstream delay hint, or null
     */
    public AiProxyAttemptEvent {
        Objects.requireNonNull(invocationId, "invocationId must not be null");
        if (invocationId.isBlank()) {
            throw new IllegalArgumentException("invocationId must not be blank");
        }
        if (attemptIndex < 0) {
            throw new IllegalArgumentException("attemptIndex must not be negative");
        }
        Objects.requireNonNull(phase, "phase must not be null");
        Objects.requireNonNull(reason, "reason must not be null");
    }

    /**
     * Get an optional retry delay hint supplied by the upstream.
     *
     * @return bounded Retry-After delay, or empty
     */
    public Optional<Duration> boundedRetryAfter() {
        return Optional.ofNullable(retryAfter);
    }

    /**
     * Logical execution phase.
     */
    public enum Phase {
        PRIMARY,
        FALLBACK
    }

    /**
     * Closed vocabulary for attempt endings used by retry policy and metering.
     */
    public enum Reason {
        COMPLETED,
        STREAM_COMPLETION_UNCONFIRMED,
        RETRY_STATUS,
        RETRY_TRANSPORT,
        FALLBACK_STATUS,
        FALLBACK_TRANSPORT,
        REPLAY_BLOCKED,
        REQUEST_DEADLINE_EXCEEDED,
        OUTPUT_COMMITTED,
        NON_RETRYABLE_RESPONSE,
        NON_RETRYABLE_TRANSPORT
    }
}
