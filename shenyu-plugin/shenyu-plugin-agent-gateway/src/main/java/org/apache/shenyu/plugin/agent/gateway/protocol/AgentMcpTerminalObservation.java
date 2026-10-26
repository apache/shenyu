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

package org.apache.shenyu.plugin.agent.gateway.protocol;

import org.apache.shenyu.common.metrics.AgentMcpCallObserver.Outcome;
import org.apache.shenyu.common.metrics.AgentMcpTerminalEvent;
import org.apache.shenyu.common.metrics.AgentMcpTerminalObserver;
import org.apache.shenyu.plugin.agent.gateway.AgentTrafficContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.buffer.DataBufferLimitException;
import reactor.core.publisher.SignalType;

import java.util.Objects;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * Per-subscription state frozen atomically on termination, never keyed by RPC id.
 */
final class AgentMcpTerminalObservation {

    private static final Logger LOG = LoggerFactory.getLogger(AgentMcpTerminalObservation.class);

    private final AgentMcpTerminalObserver observer;

    private final AgentTrafficContext traffic;

    private final long configurationVersion;

    private final long started = System.nanoTime();

    private String subject;

    private String tenant;

    private String method;

    private String requestedTool;

    private String protocolVersion;

    private Outcome outcome = Outcome.SERVER_ERROR;

    private boolean failureClassified;

    private boolean completed;

    AgentMcpTerminalObservation(final AgentMcpTerminalObserver observer, final AgentTrafficContext traffic, final long configurationVersion) {
        this.observer = observer;
        this.traffic = traffic;
        this.configurationVersion = configurationVersion;
    }

    synchronized void identified(final String value) {
        if (!completed) {
            subject = value;
        }
    }

    synchronized void identified(final String trustedTenant, final String trustedSubject) {
        if (!completed) {
            tenant = trustedTenant;
            subject = trustedSubject;
        }
    }

    synchronized void parsed(final AgentMcpRequest request) {
        if (!completed) {
            method = request.getMethod();
            requestedTool = "tools/call".equals(method) ? request.getParams().path("name").textValue() : null;
            protocolVersion = AgentMcpRequestParser.VERSION;
        }
    }

    synchronized void response(final boolean toolError) {
        if (!completed) {
            outcome = toolError ? Outcome.TOOL_ERROR : Outcome.SUCCESS;
        }
    }

    synchronized void failure(final Throwable error, final Integer transportStatus) {
        if (completed || failureClassified) {
            return;
        }
        failureClassified = true;
        Integer status = transportStatus;
        if (error instanceof AgentMcpProtocolException) {
            status = ((AgentMcpProtocolException) error).getHttpStatus();
        }
        if (error instanceof TimeoutException || Objects.equals(status, 504)) {
            outcome = Outcome.TIMEOUT;
        } else if (error instanceof DataBufferLimitException || Objects.nonNull(status) && status < 500) {
            outcome = Outcome.REJECTED;
        } else {
            outcome = Outcome.SERVER_ERROR;
        }
    }

    void finish(final SignalType signal, final Integer status) {
        AgentMcpTerminalEvent event;
        synchronized (this) {
            if (completed) {
                return;
            }
            completed = true;
            event = new AgentMcpTerminalEvent(traffic.toRequestContext(configurationVersion).withIdentity(tenant, subject)
                    .withTarget(requestedTool, method, protocolVersion), signal == SignalType.CANCEL ? Outcome.CANCELLED : outcome,
                    TimeUnit.NANOSECONDS.toMillis(Math.max(0, System.nanoTime() - started)), status);
        }
        try {
            observer.record(event);
        } catch (RuntimeException error) {
            // Callback messages may contain sensitive application data.
            LOG.debug("MCP terminal event callback failed");
        }
    }
}
