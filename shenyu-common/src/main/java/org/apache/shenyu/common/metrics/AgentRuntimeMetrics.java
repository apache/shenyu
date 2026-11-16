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

package org.apache.shenyu.common.metrics;

/**
 * Optional request-scoped metrics port. Only closed enums cross this boundary;
 * identities, tool names, payloads and credentials are never metric labels.
 */
public interface AgentRuntimeMetrics {

    String KEY = AgentRuntimeMetrics.class.getName();

    AgentRuntimeMetrics NOOP = new AgentRuntimeMetrics() { };

    /** Count one subscription owned by the MCP or AI logical-request boundary.
     * @param traffic bounded traffic type
     */
    default void started(final Traffic traffic) {
    }

    /** Release one subscription and report its terminal outcome.
     * @param traffic bounded traffic type
     * @param outcome terminal outcome
     * @param reason bounded failure category
     * @param millis monotonic duration
     */
    default void finished(final Traffic traffic, final Outcome outcome, final Reason reason, final long millis) {
    }

    /** Count an actually completed AI upstream attempt, separately from the logical request.
     * @param kind primary, retry or fallback
     * @param outcome attempt outcome
     * @param millis attempt duration
     */
    default void attempt(final AiAttemptEvent.Kind kind, final Outcome outcome, final long millis) {
    }

    /** Record catalog availability at lease acquisition, not an always-current health gauge.
     * @param available whether the request acquired a catalog snapshot
     */
    default void catalog(final boolean available) {
    }

    /** Observe an authentication plugin rejection before Agent routing is known.
     * @param source fixed authentication adapter
     * @param reason credential rejection or invalid server configuration
     */
    default void authenticationRejected(final AuthenticationSource source, final AuthenticationReason reason) {
    }

    /** Isolate metrics failures from forwarding, including cancellation cleanup.
     * @param callback bounded metrics operation
     */
    static void safely(final Runnable callback) {
        try {
            callback.run();
        } catch (RuntimeException ignored) {
            // Never log callback messages: a custom reporter may include sensitive data.
        }
    }

    enum Traffic {
        MCP, LLM
    }

    enum Outcome {
        SUCCESS, TOOL_ERROR, ERROR, TIMEOUT, CANCELLED, REJECTED, RATE_LIMITED
    }

    enum Reason {
        NONE, AUTHENTICATION, AUTHORIZATION, RESOURCE_LIMIT, CATALOG_UNAVAILABLE, PROTOCOL, OTHER
    }

    enum AuthenticationSource {
        KEY_AUTH
    }

    enum AuthenticationReason {
        CREDENTIAL_REJECTED, INVALID_CONFIGURATION
    }
}
