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

import org.apache.shenyu.common.dto.AgentRequestContext;

/**
 * Body-free terminal fact for one upstream attempt. Status is assigned, not proof of delivery.
 * @param context immutable trusted request snapshot
 * @param attemptNumber one-based sequence within a logical request
 * @param kind primary, retry or fallback
 * @param route actual client/target protocol and model attribution
 * @param streaming selected response mode
 * @param outcome terminal classification
 * @param httpStatus received upstream status, null before headers
 * @param elapsedMillis monotonic attempt duration
 * @param usage last explicit usage snapshot for this attempt
 * @param completion independent stream evidence and framing; SUCCESS is transport/HTTP outcome only
 */
public record AiAttemptEvent(AgentRequestContext context, int attemptNumber, Kind kind, AiAttemptRoute route, boolean streaming,
                             AiCallOutcome outcome, Integer httpStatus, long elapsedMillis, AiUsage usage, AiStreamCompletion completion) {

    /**
     * Preserve the existing event constructor with explicitly uninspected stream evidence.
     * @param context trusted context
     * @param attemptNumber actual attempt sequence
     * @param kind selection reason
     * @param provider provider
     * @param protocol client protocol
     * @param streaming response mode
     * @param outcome transport/HTTP outcome
     * @param httpStatus upstream status
     * @param elapsedMillis duration
     * @param usage explicit usage
     */
    public AiAttemptEvent(final AgentRequestContext context, final int attemptNumber, final Kind kind, final String provider, final String protocol,
                          final boolean streaming, final AiCallOutcome outcome, final Integer httpStatus, final long elapsedMillis, final AiUsage usage) {
        this(context, attemptNumber, kind, new AiAttemptRoute(provider, protocol, null, null, null),
                streaming, outcome, httpStatus, elapsedMillis, usage, AiStreamCompletion.NOT_INSPECTED);
    }

    /**
     * Preserve the original attempt contract without inventing its unknown native protocol.
     * @param context trusted request
     * @param attemptNumber attempt sequence
     * @param kind selection phase
     * @param provider actual provider
     * @param protocol client protocol
     * @param streaming selected mode
     * @param outcome outcome
     * @param httpStatus received status
     * @param elapsedMillis elapsed time
     * @param usage observed usage
     * @param completion independent stream completion evidence
     */
    public AiAttemptEvent(final AgentRequestContext context, final int attemptNumber, final Kind kind,
                          final String provider, final String protocol, final boolean streaming,
                          final AiCallOutcome outcome, final Integer httpStatus, final long elapsedMillis, final AiUsage usage,
                          final AiStreamCompletion completion) {
        this(context, attemptNumber, kind, new AiAttemptRoute(provider, protocol, null, null, null),
                streaming, outcome, httpStatus, elapsedMillis, usage, completion);
    }

    /**
     * Preserve route-aware callers without assuming stream completion evidence.
     * @param context trusted request
     * @param attemptNumber attempt sequence
     * @param kind selection phase
     * @param route resolved route
     * @param streaming selected mode
     * @param outcome outcome
     * @param httpStatus received status
     * @param elapsedMillis elapsed time
     * @param usage observed usage
     */
    public AiAttemptEvent(final AgentRequestContext context, final int attemptNumber, final Kind kind,
                          final AiAttemptRoute route, final boolean streaming,
                          final AiCallOutcome outcome, final Integer httpStatus, final long elapsedMillis, final AiUsage usage) {
        this(context, attemptNumber, kind, route, streaming, outcome, httpStatus, elapsedMillis, usage, AiStreamCompletion.NOT_INSPECTED);
    }

    /**
     * Preserve the original client-protocol accessor.
     * @return client protocol, not the provider-native route
     */
    public String protocol() {
        return route.clientProtocol();
    }

    public String provider() {
        return route.provider();
    }

    public String clientProtocol() {
        return route.clientProtocol();
    }

    public String targetProtocol() {
        return route.targetProtocol();
    }

    public String requestedModel() {
        return route.requestedModel();
    }

    public String resolvedModel() {
        return route.resolvedModel();
    }

    /**
     * How this actual network attempt was selected.
     */
    public enum Kind {
        PRIMARY, RETRY, FALLBACK
    }
}
