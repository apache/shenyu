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

import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiTransportException;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Test cases for {@link AiProxyRetryPolicy}.
 */
class AiProxyRetryPolicyTest {

    private final AiProxyRetryPolicy policy = new AiProxyRetryPolicy();

    @ParameterizedTest
    @CsvSource({
        "400, 3, true, FORWARD_RESPONSE",
        "401, 3, true, FORWARD_RESPONSE",
        "408, 3, true, FORWARD_RESPONSE",
        "422, 3, true, FORWARD_RESPONSE",
        "429, 1, true, RETRY",
        "429, 0, true, FALLBACK",
        "429, 0, false, FORWARD_RESPONSE",
        "500, 1, true, RETRY",
        "503, 0, true, FALLBACK",
        "599, 0, false, FORWARD_RESPONSE",
        "600, 3, true, FORWARD_RESPONSE"
    })
    void shouldDecideStatusFailure(final int status,
                                   final int retriesRemaining,
                                   final boolean fallbackAvailable,
                                   final AiProxyRetryPolicy.Decision expected) {
        assertEquals(expected, policy.decideStatus(status, retriesRemaining, fallbackAvailable));
    }

    @ParameterizedTest
    @CsvSource({
        "CONNECT_TIMEOUT, RETRY",
        "RESPONSE_HEADER_TIMEOUT, RETRY",
        "STREAM_IDLE_TIMEOUT, RETRY",
        "STREAM_DURATION_TIMEOUT, PROPAGATE_ERROR",
        "CONNECTION_POOL_TIMEOUT, PROPAGATE_ERROR",
        "REQUEST_TOO_LARGE, PROPAGATE_ERROR",
        "RESPONSE_TOO_LARGE, FALLBACK",
        "DNS_FAILURE, PROPAGATE_ERROR",
        "TLS_FAILURE, PROPAGATE_ERROR",
        "CONNECTION_RESET, RETRY",
        "IO_FAILURE, RETRY",
        "UNKNOWN, PROPAGATE_ERROR"
    })
    void shouldRetryBeforeUsingFallback(final AiTransportException.Kind kind,
                                        final AiProxyRetryPolicy.Decision expected) {
        assertEquals(expected, policy.decideTransport(kind, 1, true));
    }

    @ParameterizedTest
    @CsvSource({
        "CONNECT_TIMEOUT, FALLBACK",
        "RESPONSE_HEADER_TIMEOUT, FALLBACK",
        "STREAM_IDLE_TIMEOUT, PROPAGATE_ERROR",
        "STREAM_DURATION_TIMEOUT, PROPAGATE_ERROR",
        "CONNECTION_POOL_TIMEOUT, PROPAGATE_ERROR",
        "REQUEST_TOO_LARGE, PROPAGATE_ERROR",
        "RESPONSE_TOO_LARGE, FALLBACK",
        "DNS_FAILURE, PROPAGATE_ERROR",
        "TLS_FAILURE, PROPAGATE_ERROR",
        "CONNECTION_RESET, FALLBACK",
        "IO_FAILURE, FALLBACK",
        "UNKNOWN, PROPAGATE_ERROR"
    })
    void shouldApplyFallbackWhitelistAfterRetriesAreExhausted(final AiTransportException.Kind kind,
                                                              final AiProxyRetryPolicy.Decision expected) {
        assertEquals(expected, policy.decideTransport(kind, 0, true));
    }

    @ParameterizedTest
    @CsvSource({
        "CONNECT_TIMEOUT",
        "RESPONSE_HEADER_TIMEOUT",
        "STREAM_IDLE_TIMEOUT",
        "STREAM_DURATION_TIMEOUT",
        "CONNECTION_POOL_TIMEOUT",
        "REQUEST_TOO_LARGE",
        "RESPONSE_TOO_LARGE",
        "DNS_FAILURE",
        "TLS_FAILURE",
        "CONNECTION_RESET",
        "IO_FAILURE",
        "UNKNOWN"
    })
    void shouldPropagateTransportFailureWithoutRetriesOrFallback(final AiTransportException.Kind kind) {
        assertEquals(AiProxyRetryPolicy.Decision.PROPAGATE_ERROR,
                policy.decideTransport(kind, 0, false));
    }
}
