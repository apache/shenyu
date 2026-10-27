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

import org.apache.shenyu.plugin.ai.proxy.enhanced.model.ShenyuAiRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiTransportException;
import org.springframework.http.HttpHeaders;

import java.time.Clock;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * Decides whether an AI upstream failure may be replayed, switched to fallback,
 * forwarded to the client, or propagated.
 */
final class AiProxyRetryPolicy {

    private static final Set<AiTransportException.Kind> AMBIGUOUS_FAILURES =
            Set.of(AiTransportException.Kind.RESPONSE_HEADER_TIMEOUT,
                    AiTransportException.Kind.CONNECTION_RESET,
                    AiTransportException.Kind.IO_FAILURE);

    Decision decideStatus(final FailureContext context) {
        if (!retryableStatus(context.status())) {
            return Decision.FORWARD_RESPONSE;
        }
        return decideReplayableFailure(context, context.fallbackAvailable());
    }

    Decision decideTransport(final FailureContext context) {
        if (!retryableTransportFailure(context.kind())) {
            if (context.fallbackAvailable() && fallbackEligibleTransportFailure(context.kind())) {
                return Decision.FALLBACK;
            }
            return Decision.PROPAGATE_ERROR;
        }
        return decideReplayableFailure(context, fallbackEligibleTransportFailure(context.kind()));
    }

    private Decision decideReplayableFailure(final FailureContext context, final boolean fallbackEligible) {
        AiProxyRetryProperties.ReplayMode mode = context.replayPolicy()
                .mode(context.operation(), context.executionAmbiguous(), !context.hasStatus());
        if (context.retriesRemaining() > 0
                && replayAllowed(mode, context.providerDeclaredReplaySafe(), context)) {
            return Decision.RETRY;
        }
        if (context.fallbackAvailable() && fallbackEligible
                && replayAllowed(mode, context.fallbackProviderDeclaredReplaySafe(), context)) {
            return Decision.FALLBACK;
        }
        return context.hasStatus() ? Decision.FORWARD_RESPONSE : Decision.PROPAGATE_ERROR;
    }

    private boolean replayAllowed(final AiProxyRetryProperties.ReplayMode mode,
                                  final boolean providerDeclaredReplaySafe,
                                  final FailureContext context) {
        return switch (mode) {
            case NEVER -> false;
            case PROVIDER_DECLARED -> providerDeclaredReplaySafe && context.conservativeReplaySafe();
            case CONSERVATIVE -> !context.executionAmbiguous() || context.conservativeReplaySafe();
        };
    }

    private boolean retryableStatus(final int status) {
        return status == 429 || status >= 500 && status < 600;
    }

    private boolean retryableTransportFailure(final AiTransportException.Kind kind) {
        return switch (kind) {
            case CONNECT_TIMEOUT, RESPONSE_HEADER_TIMEOUT, STREAM_IDLE_TIMEOUT,
                    CONNECTION_RESET, IO_FAILURE -> true;
            case STREAM_DURATION_TIMEOUT, RESPONSE_TOO_LARGE, DNS_FAILURE,
                    TLS_FAILURE, UNKNOWN -> false;
        };
    }

    private boolean fallbackEligibleTransportFailure(final AiTransportException.Kind kind) {
        return switch (kind) {
            case CONNECT_TIMEOUT, RESPONSE_HEADER_TIMEOUT, RESPONSE_TOO_LARGE,
                    CONNECTION_RESET, IO_FAILURE -> true;
            case STREAM_IDLE_TIMEOUT, STREAM_DURATION_TIMEOUT, DNS_FAILURE,
                    TLS_FAILURE, UNKNOWN -> false;
        };
    }

    /**
     * Whether the request can be replayed under ShenYu's conservative default.
     * Provider-specific idempotency must be declared by provider capabilities;
     * the gateway does not infer application idempotency from payloads.
     *
     * @param request final decoded request
     * @return true when the request has no tool-bearing fields
     */
    static boolean replaySafe(final ShenyuAiRequest request) {
        return request.getRawPayload().path("tools").isEmpty()
                && request.getRawPayload().path("functions").isEmpty()
                && !"required".equalsIgnoreCase(request.getRawPayload().path("tool_choice").asText())
                && !"auto".equalsIgnoreCase(request.getRawPayload().path("tool_choice").asText())
                && !"required".equalsIgnoreCase(request.getRawPayload().path("function_call").asText())
                && !"auto".equalsIgnoreCase(request.getRawPayload().path("function_call").asText());
    }

    static Duration retryAfter(final Map<String, ?> headers, final AiProxyRetryProperties properties) {
        Object value = null;
        for (Map.Entry<String, ?> entry : headers.entrySet()) {
            if (HttpHeaders.RETRY_AFTER.equalsIgnoreCase(entry.getKey())) {
                value = entry.getValue();
                break;
            }
        }
        String text;
        if (value instanceof List<?> values && !values.isEmpty()) {
            text = String.valueOf(values.get(0));
        } else if (value instanceof String direct) {
            text = direct;
        } else {
            return null;
        }
        long seconds = properties.parseRetryAfter(text, Clock.systemUTC());
        return seconds < 0 ? null : properties.clampRetryAfter(Duration.ofSeconds(seconds));
    }

    static boolean hasRetryAfter(final Map<String, ?> headers) {
        for (Map.Entry<String, ?> entry : headers.entrySet()) {
            if (HttpHeaders.RETRY_AFTER.equalsIgnoreCase(entry.getKey())) {
                return Objects.nonNull(entry.getValue())
                        && (!(entry.getValue() instanceof List<?> values) || !values.isEmpty());
            }
        }
        return false;
    }

    /**
     * Failure facts needed to determine whether replay is safe.
     *
     * @param kind classified transport failure, or null for an HTTP status failure
     * @param status upstream HTTP status, or null for transport failure
     * @param headers upstream response headers
     * @param retriesRemaining retry allowance shared by this execution phase
     * @param fallbackAvailable whether a fallback upstream is configured
     * @param conservativeReplaySafe whether the final payload has no tool-bearing fields
     * @param operation final AI operation
     * @param replayPolicy configured replay policy
     * @param providerDeclaredReplaySafe whether the current provider explicitly declared replay safety
     * @param fallbackProviderDeclaredReplaySafe whether the fallback provider explicitly declared replay safety
     */
    record FailureContext(
            AiTransportException.Kind kind,
            Integer status,
            Map<String, ?> headers,
            int retriesRemaining,
            boolean fallbackAvailable,
            boolean conservativeReplaySafe,
            ShenyuAiRequest.Operation operation,
            AiProxyRetryProperties.ReplayProperties replayPolicy,
            boolean providerDeclaredReplaySafe,
            boolean fallbackProviderDeclaredReplaySafe) {

        FailureContext {
            Objects.requireNonNull(headers, "headers must not be null");
            Objects.requireNonNull(operation, "operation must not be null");
            Objects.requireNonNull(replayPolicy, "replayPolicy must not be null");
            if ((Objects.isNull(kind)) == (Objects.isNull(status))) {
                throw new IllegalArgumentException("exactly one transport kind or status is required");
            }
        }

        boolean hasStatus() {
            return Objects.nonNull(status);
        }

        boolean executionAmbiguous() {
            return Objects.nonNull(kind) && AMBIGUOUS_FAILURES.contains(kind);
        }
    }

    enum Decision {
        RETRY,
        FALLBACK,
        FORWARD_RESPONSE,
        PROPAGATE_ERROR
    }
}
