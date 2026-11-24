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

import org.apache.shenyu.common.dto.AgentRequestContext;
import org.apache.shenyu.common.metrics.AiCallOutcome;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiProxyException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiTransportException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.buffer.DataBufferLimitException;
import reactor.util.context.ContextView;

import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.TimeoutException;

/**
 * Body-free context resolution and bounded failure classification for AI layers.
 */
public final class AiObservationSupport {

    private static final Logger LOG = LoggerFactory.getLogger(AiObservationSupport.class);

    private AiObservationSupport() {
    }

    /**
     * Resolve only subscription context; never recover identities from exchange attributes or headers.
     * When the observation owner supplies matched selector/rule ids, refresh those fields while
     * preserving the existing logical request id.
     *
     * @param view Reactor context
     * @param selectorId matched AI selector for standalone requests
     * @param ruleId matched AI rule for standalone requests
     * @return existing authoritative context or an anonymous standalone request
     */
    public static AgentRequestContext context(final ContextView view, final String selectorId, final String ruleId) {
        Object value = view.getOrDefault(AgentRequestContext.REACTOR_KEY, null);
        if (value instanceof AgentRequestContext context) {
            if (!AgentRequestContext.isReusableRequestId(context.getRequestId())) {
                // Never log the rejected value; it may contain characters unsafe for log output.
                LOG.warn("Replacing an invalid existing AI request id in AI Proxy");
                final AgentRequestContext replaced = context.withRequestId(UUID.randomUUID().toString());
                return Objects.isNull(selectorId) && Objects.isNull(ruleId)
                        ? replaced
                        : replaced.withSelectorAndRule(selectorId, ruleId);
            }
            return Objects.isNull(selectorId) && Objects.isNull(ruleId)
                    ? context
                    : context.withSelectorAndRule(selectorId, ruleId);
        }
        return new AgentRequestContext(UUID.randomUUID().toString(), "llm", selectorId, ruleId);
    }

    /**
     * Convert implementation errors without storing messages or causes in events.
     * @param error actual pipeline failure
     * @return bounded classification
     */
    public static AiCallOutcome outcome(final Throwable error) {
        Throwable cause = error;
        for (int depth = 0; depth < 16 && Objects.nonNull(cause); depth++) {
            if (cause instanceof TimeoutException) {
                return AiCallOutcome.TIMEOUT;
            }
            cause = cause.getCause();
        }
        if (error instanceof AiTransportException transport) {
            return switch (transport.getKind()) {
                case CONNECT_TIMEOUT, RESPONSE_HEADER_TIMEOUT, STREAM_IDLE_TIMEOUT, STREAM_DURATION_TIMEOUT -> AiCallOutcome.TIMEOUT;
                default -> AiCallOutcome.ERROR;
            };
        }
        if (error instanceof DataBufferLimitException
                || error instanceof AiProxyException proxy && (proxy.getKind() == AiProxyException.Kind.REQUEST
                || proxy.getKind() == AiProxyException.Kind.CONTENT)) {
            return AiCallOutcome.REJECTED;
        }
        return AiCallOutcome.ERROR;
    }
}
