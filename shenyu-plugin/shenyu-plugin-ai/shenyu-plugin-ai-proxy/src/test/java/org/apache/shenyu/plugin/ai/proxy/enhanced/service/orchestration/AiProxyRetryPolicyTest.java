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

import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiTransportException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.ShenyuAiRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyRetryProperties;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.orchestration.AiAttemptOutcome.Decision;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for {@link AiProxyRetryPolicy}.
 */
class AiProxyRetryPolicyTest {

    private final AiProxyRetryPolicy policy = new AiProxyRetryPolicy();

    @ParameterizedTest
    @CsvSource({"401,3,true,FALLBACK", "403,3,true,FALLBACK", "401,3,false,FORWARD_RESPONSE",
        "403,0,false,FORWARD_RESPONSE", "429,3,true,RETRY", "503,3,true,RETRY", "400,3,true,FORWARD_RESPONSE"})
    void classifiedCredentialsSkipRetriesWithoutExpandingOtherStatuses(final int status, final int retries,
                                                                      final boolean fallback, final Decision expected) {
        assertEquals(expected, policy.decideStatus(status(status, retries, fallback, true).withCredentialFailure(true)));
    }

    @ParameterizedTest
    @CsvSource({"NEVER,true,true,FORWARD_RESPONSE", "CONSERVATIVE,false,true,FALLBACK",
        "PROVIDER_DECLARED,false,true,FORWARD_RESPONSE", "PROVIDER_DECLARED,true,false,FORWARD_RESPONSE",
        "PROVIDER_DECLARED,true,true,FALLBACK"})
    void classifiedCredentialsStillHonorFallbackReplayPolicy(final AiProxyRetryProperties.ReplayMode mode,
                                                              final boolean fallbackDeclared, final boolean replaySafe,
                                                              final Decision expected) {
        final AiProxyRetryProperties properties = new AiProxyRetryProperties();
        properties.getReplay().setResponseStatus(mode);
        final AiProxyRetryPolicy.FailureContext context = new AiProxyRetryPolicy.FailureContext(null, 401, Map.of(),
                3, true, replaySafe, ShenyuAiRequest.Operation.CHAT_COMPLETION,
                new AiRetrySettings(properties), true, fallbackDeclared, true);
        assertEquals(expected, policy.decideStatus(context));
    }

    @ParameterizedTest
    @CsvSource({
        "400, FORWARD_RESPONSE",
        "401, FORWARD_RESPONSE",
        "408, FORWARD_RESPONSE",
        "422, FORWARD_RESPONSE",
        "503, FALLBACK",
        "599, FALLBACK",
        "600, FORWARD_RESPONSE"
    })
    void shouldDecideStatusFailure(final int status, final Decision expected) {
        assertEquals(expected, policy.decideStatus(status(status, 0, true, true)));
    }

    @ParameterizedTest
    @CsvSource({"429", "500"})
    void shouldRetryStatusFailuresWhileAllowanceRemains(final int status) {
        assertEquals(Decision.RETRY, policy.decideStatus(status(status, 1, true, true)));
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
                                        final Decision expected) {
        assertEquals(expected, policy.decideTransport(transport(kind, 1, true, true)));
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
                                                              final Decision expected) {
        assertEquals(expected, policy.decideTransport(transport(kind, 0, true, true)));
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
        assertEquals(Decision.PROPAGATE_ERROR,
                policy.decideTransport(transport(kind, 0, false, true)));
    }

    @Test
    void shouldBlockAmbiguousReplayWhenRequestBearsTools() {
        for (AiTransportException.Kind kind : new AiTransportException.Kind[] {
                AiTransportException.Kind.RESPONSE_HEADER_TIMEOUT,
                AiTransportException.Kind.STREAM_IDLE_TIMEOUT,
                AiTransportException.Kind.RESPONSE_TOO_LARGE,
                AiTransportException.Kind.CONNECTION_RESET,
                AiTransportException.Kind.IO_FAILURE}) {
            assertEquals(Decision.PROPAGATE_ERROR,
                    policy.decideTransport(transport(kind, 2, true, false)));
        }
        assertEquals(Decision.RETRY,
                policy.decideTransport(transport(AiTransportException.Kind.CONNECT_TIMEOUT, 2, true, false)));
    }

    @Test
    void shouldApplyNeverPolicyToFallbackEligibleTransportFailure() {
        AiProxyRetryProperties properties = new AiProxyRetryProperties();
        properties.getReplay().setAmbiguousTransport(AiProxyRetryProperties.ReplayMode.NEVER);
        assertEquals(Decision.PROPAGATE_ERROR, policy.decideTransport(
                fallbackContext(AiTransportException.Kind.RESPONSE_TOO_LARGE, properties)));

        properties.getReplay().setDefiniteTransport(AiProxyRetryProperties.ReplayMode.NEVER);
        assertEquals(Decision.PROPAGATE_ERROR, policy.decideTransport(
                fallbackContext(AiTransportException.Kind.CONNECT_TIMEOUT, properties)));
    }

    @Test
    void shouldApplyConfiguredReplayPoliciesByStageOperationAndProviderDeclaration() {
        AiProxyRetryProperties properties = new AiProxyRetryProperties();
        properties.getReplay().setAmbiguousTransport(AiProxyRetryProperties.ReplayMode.PROVIDER_DECLARED);
        assertEquals(AiProxyRetryProperties.ReplayMode.PROVIDER_DECLARED, new AiRetrySettings(properties)
                .replayMode(ShenyuAiRequest.Operation.CHAT_COMPLETION, true, true));
        AiProxyRetryPolicy.FailureContext providerDeclared = context(
                AiTransportException.Kind.RESPONSE_HEADER_TIMEOUT, properties, true);
        AiProxyRetryPolicy.FailureContext providerNotDeclared = context(
                AiTransportException.Kind.RESPONSE_HEADER_TIMEOUT, properties, false);
        assertTrue(providerDeclared.providerDeclaredReplaySafe());
        assertFalse(providerNotDeclared.providerDeclaredReplaySafe());
        assertEquals(Decision.RETRY, policy.decideTransport(providerDeclared),
                () -> "provider declared: " + providerDeclared);
        assertEquals(Decision.PROPAGATE_ERROR, policy.decideTransport(providerNotDeclared),
                () -> "provider not declared: " + providerNotDeclared);

        AiProxyRetryProperties toolProperties = new AiProxyRetryProperties();
        toolProperties.getReplay().setAmbiguousTransport(AiProxyRetryProperties.ReplayMode.PROVIDER_DECLARED);
        assertEquals(Decision.PROPAGATE_ERROR, policy.decideTransport(
                new AiProxyRetryPolicy.FailureContext(AiTransportException.Kind.IO_FAILURE, null, Map.of(),
                        2, false, false, ShenyuAiRequest.Operation.CHAT_COMPLETION,
                        new AiRetrySettings(toolProperties), true, false)),
                "provider declaration must not override tool-bearing replay risk");

        properties.getReplay().setOperations(Map.of(
                ShenyuAiRequest.Operation.CHAT_COMPLETION.name(), AiProxyRetryProperties.ReplayMode.NEVER));
        assertEquals(Decision.PROPAGATE_ERROR, policy.decideTransport(context(
                AiTransportException.Kind.IO_FAILURE, properties, true)));
        properties.getReplay().setOperations(Map.of());
        properties.getReplay().setDefiniteTransport(AiProxyRetryProperties.ReplayMode.NEVER);
        assertEquals(Decision.PROPAGATE_ERROR, policy.decideTransport(context(
                AiTransportException.Kind.CONNECT_TIMEOUT, properties, true)));
        properties.getReplay().setDefiniteTransport(AiProxyRetryProperties.ReplayMode.CONSERVATIVE);
        properties.getReplay().setResponseStatus(AiProxyRetryProperties.ReplayMode.NEVER);
        assertEquals(Decision.FORWARD_RESPONSE, policy.decideStatus(contextStatus(
                429, properties, true)));
    }

    @Test
    void shouldParseBoundedRetryAfterSeconds() {
        AiProxyRetryProperties properties = new AiProxyRetryProperties(3, 1,
                java.time.Duration.ZERO, java.time.Duration.ZERO, 0d,
                java.time.Duration.ofSeconds(2), java.time.Duration.ofMinutes(1));

        assertEquals(java.time.Duration.ofSeconds(1),
                AiProxyRetryPolicy.retryAfter(Map.of("Retry-After", "1"), new AiRetrySettings(properties)));
        assertEquals(java.time.Duration.ofSeconds(1),
                AiProxyRetryPolicy.retryAfter(Map.of("Retry-After", List.of("1")), new AiRetrySettings(properties)));
        assertNull(AiProxyRetryPolicy.retryAfter(Map.of("retry-after", "999"), new AiRetrySettings(properties)));
        assertEquals(java.time.Duration.ofSeconds(1),
                AiProxyRetryPolicy.retryAfter(Map.of("RETRY-AFTER", "1"), new AiRetrySettings(properties)));
        assertNull(AiProxyRetryPolicy.retryAfter(Map.of("Retry-After", "-1"), new AiRetrySettings(properties)));
        assertNull(AiProxyRetryPolicy.retryAfter(Map.of("Retry-After", "invalid"), new AiRetrySettings(properties)));
    }

    private AiProxyRetryPolicy.FailureContext status(final int status, final int retries,
                                                     final boolean fallback, final boolean replaySafe) {
        return new AiProxyRetryPolicy.FailureContext(null, status, Map.of(), retries, fallback,
                replaySafe, ShenyuAiRequest.Operation.CHAT_COMPLETION,
                new AiRetrySettings(new AiProxyRetryProperties()), false, false);
    }

    private AiProxyRetryPolicy.FailureContext transport(final AiTransportException.Kind kind, final int retries,
                                                        final boolean fallback, final boolean replaySafe) {
        return new AiProxyRetryPolicy.FailureContext(kind, null, Map.of(), retries, fallback,
                replaySafe, ShenyuAiRequest.Operation.CHAT_COMPLETION,
                new AiRetrySettings(new AiProxyRetryProperties()), false, false);
    }

    private AiProxyRetryPolicy.FailureContext context(final AiTransportException.Kind kind,
                                                      final AiProxyRetryProperties properties,
                                                      final boolean providerDeclared) {
        return new AiProxyRetryPolicy.FailureContext(kind, null, Map.of(), 1, false, true,
                ShenyuAiRequest.Operation.CHAT_COMPLETION, new AiRetrySettings(properties), providerDeclared, false);
    }

    private AiProxyRetryPolicy.FailureContext fallbackContext(final AiTransportException.Kind kind,
                                                              final AiProxyRetryProperties properties) {
        return new AiProxyRetryPolicy.FailureContext(kind, null, Map.of(), 1, true, true,
                ShenyuAiRequest.Operation.CHAT_COMPLETION, new AiRetrySettings(properties), true, true);
    }

    private AiProxyRetryPolicy.FailureContext contextStatus(final int status,
                                                            final AiProxyRetryProperties properties,
                                                            final boolean providerDeclared) {
        return new AiProxyRetryPolicy.FailureContext(null, status, Map.of(), 1, false, true,
                ShenyuAiRequest.Operation.CHAT_COMPLETION, new AiRetrySettings(properties), providerDeclared, false);
    }
}
