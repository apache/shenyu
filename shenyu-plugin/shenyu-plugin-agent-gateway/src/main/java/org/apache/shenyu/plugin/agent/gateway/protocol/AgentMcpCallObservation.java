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

import com.fasterxml.jackson.databind.node.ObjectNode;
import org.apache.shenyu.common.metrics.AgentMcpCallObserver;
import org.apache.shenyu.common.metrics.AgentMcpCallObserver.Outcome;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import reactor.core.publisher.SignalType;

import java.util.Objects;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * One subscription's bounded observation state, independent of RPC ids and sessions.
 */
final class AgentMcpCallObservation {

    private static final Logger LOG = LoggerFactory.getLogger(AgentMcpCallObservation.class);

    private final AgentMcpCallObserver observer;

    private final AtomicBoolean recorded = new AtomicBoolean();

    private volatile long started;

    private volatile boolean call;

    private volatile Outcome outcome = Outcome.SERVER_ERROR;

    AgentMcpCallObservation(final Object observer) {
        this.observer = observer instanceof AgentMcpCallObserver ? (AgentMcpCallObserver) observer : null;
    }

    void parsed(final AgentMcpRequest request) {
        if (Objects.nonNull(observer) && "tools/call".equals(request.getMethod())) {
            started = System.nanoTime();
            call = true;
        }
    }

    void response(final ObjectNode response) {
        outcome = response.path("result").path("isError").asBoolean(false) ? Outcome.TOOL_ERROR : Outcome.SUCCESS;
    }

    void failure(final Throwable error) {
        if (error instanceof TimeoutException) {
            outcome = Outcome.TIMEOUT;
        } else if (error instanceof AgentMcpProtocolException && ((AgentMcpProtocolException) error).getHttpStatus() < 500) {
            outcome = Outcome.REJECTED;
        } else {
            outcome = Outcome.SERVER_ERROR;
        }
    }

    void finish(final SignalType signal) {
        if (!call || !recorded.compareAndSet(false, true)) {
            return;
        }
        Outcome terminal = signal == SignalType.CANCEL ? Outcome.CANCELLED : outcome;
        long millis = TimeUnit.NANOSECONDS.toMillis(Math.max(0, System.nanoTime() - started));
        try {
            observer.record(terminal, millis);
        } catch (RuntimeException error) {
            // Observability must not fail a call or log potentially sensitive callback errors.
            LOG.debug("MCP call statistics callback failed");
        }
    }
}
