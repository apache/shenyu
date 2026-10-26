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
import reactor.util.context.Context;
import reactor.util.context.ContextView;

import java.util.Objects;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

/**
 * One subscription's bounded observation state, independent of RPC ids and sessions.
 */
final class AgentMcpCallObservation {

    private static final Logger LOG = LoggerFactory.getLogger(AgentMcpCallObservation.class);

    private final AgentMcpCallObserver observer;

    private final AgentMcpCallTracer tracer;

    private final ContextView parentContext;

    private volatile AgentMcpCallTracer.Trace trace;

    private final AtomicReference<Outcome> completed = new AtomicReference<>();

    private final AtomicBoolean traceRecorded = new AtomicBoolean();

    private volatile long started;

    private volatile boolean call;

    private volatile Outcome outcome = Outcome.SERVER_ERROR;

    AgentMcpCallObservation(final AgentMcpCallObserver observer) {
        this(observer, null, Context.empty());
    }

    AgentMcpCallObservation(final AgentMcpCallObserver observer, final AgentMcpCallTracer tracer, final ContextView parentContext) {
        this.observer = observer;
        this.tracer = tracer;
        this.parentContext = parentContext;
    }

    void parsed(final AgentMcpRequest request) {
        if (Objects.nonNull(completed.get())) {
            return;
        }
        if ((Objects.nonNull(observer) || Objects.nonNull(tracer)) && "tools/call".equals(request.getMethod())) {
            started = System.nanoTime();
            call = true;
            if (Objects.nonNull(tracer)) {
                try {
                    trace = tracer.start(parentContext);
                    // Cancellation may finish the request while the application creates its span.
                    finishTrace();
                } catch (RuntimeException error) {
                    LOG.debug("MCP call tracing callback failed");
                }
            }
        }
    }

    Context traceContext(final Context context) {
        if (Objects.nonNull(trace)) {
            try {
                return Objects.requireNonNull(trace.attach(context), "trace context");
            } catch (RuntimeException error) {
                LOG.debug("MCP call tracing callback failed");
            }
        }
        return context;
    }

    void response(final ObjectNode response) {
        outcome = response.path("result").path("isError").asBoolean(false) ? Outcome.TOOL_ERROR : Outcome.SUCCESS;
    }

    /** HTTP 504 is intentionally folded into {@link Outcome#TIMEOUT}, not a server error. */
    void failure(final Throwable error) {
        if (error instanceof TimeoutException
                || error instanceof AgentMcpProtocolException && ((AgentMcpProtocolException) error).getHttpStatus() == 504) {
            outcome = Outcome.TIMEOUT;
        } else if (error instanceof AgentMcpProtocolException && ((AgentMcpProtocolException) error).getHttpStatus() < 500) {
            outcome = Outcome.REJECTED;
        } else {
            outcome = Outcome.SERVER_ERROR;
        }
    }

    void finish(final SignalType signal) {
        Outcome terminal = signal == SignalType.CANCEL ? Outcome.CANCELLED : outcome;
        if (!completed.compareAndSet(null, terminal) || !call) {
            return;
        }
        long millis = TimeUnit.NANOSECONDS.toMillis(Math.max(0, System.nanoTime() - started));
        if (Objects.nonNull(observer)) {
            try {
                observer.record(terminal, millis);
            } catch (RuntimeException error) {
                // Observability must not fail a call or log potentially sensitive callback errors.
                LOG.debug("MCP call statistics callback failed");
            }
        }
        finishTrace();
    }

    private void finishTrace() {
        AgentMcpCallTracer.Trace current = trace;
        Outcome terminal = completed.get();
        if (Objects.nonNull(current) && Objects.nonNull(terminal) && traceRecorded.compareAndSet(false, true)) {
            try {
                current.finish(terminal);
            } catch (RuntimeException error) {
                LOG.debug("MCP call tracing callback failed");
            }
        }
    }
}
