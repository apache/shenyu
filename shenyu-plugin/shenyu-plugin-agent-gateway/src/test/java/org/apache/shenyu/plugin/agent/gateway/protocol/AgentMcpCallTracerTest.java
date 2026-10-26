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

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.node.TextNode;
import org.apache.shenyu.common.metrics.AgentMcpCallObserver;
import org.apache.shenyu.common.metrics.AgentMcpCallObserver.Outcome;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import reactor.core.publisher.SignalType;
import reactor.util.context.Context;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class AgentMcpCallTracerTest {

    @Test
    void parsingAfterTerminationCannotStartANewTrace() {
        AtomicInteger starts = new AtomicInteger();
        AtomicInteger metrics = new AtomicInteger();
        AgentMcpCallObservation observation = new AgentMcpCallObservation((outcome, millis) -> metrics.incrementAndGet(), ignored -> {
            starts.incrementAndGet();
            return mock(AgentMcpCallTracer.Trace.class);
        }, Context.empty());
        observation.finish(SignalType.CANCEL);
        observation.parsed(request("tools/call"));
        observation.finish(SignalType.ON_COMPLETE);
        assertEquals(0, starts.get());
        assertEquals(0, metrics.get());
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void aSpanCreatedAfterRequestTerminationIsStillFinishedOnce(final boolean cancel) throws Exception {
        CountDownLatch entered = new CountDownLatch(1);
        CountDownLatch release = new CountDownLatch(1);
        var executor = Executors.newSingleThreadExecutor();
        AgentMcpCallTracer.Trace trace = mock(AgentMcpCallTracer.Trace.class);
        AtomicInteger metrics = new AtomicInteger();
        AgentMcpCallTracer tracer = ignored -> {
            entered.countDown();
            try {
                if (!release.await(5, TimeUnit.SECONDS)) {
                    throw new IllegalStateException("Fixture release timed out");
                }
            } catch (InterruptedException error) {
                Thread.currentThread().interrupt();
                throw new IllegalStateException("Fixture interrupted", error);
            }
            return trace;
        };
        AgentMcpCallObservation observation = new AgentMcpCallObservation((outcome, millis) -> metrics.incrementAndGet(), tracer, Context.empty());
        try {
            final var parsed = executor.submit(() -> observation.parsed(request("tools/call")));
            assertTrue(entered.await(5, TimeUnit.SECONDS));
            observation.failure(new TimeoutException("private-timeout"));
            observation.finish(cancel ? SignalType.CANCEL : SignalType.ON_COMPLETE);
            release.countDown();
            parsed.get(5, TimeUnit.SECONDS);
            observation.finish(SignalType.ON_ERROR);
            verify(trace).finish(cancel ? Outcome.CANCELLED : Outcome.TIMEOUT);
            assertEquals(1, metrics.get());
        } finally {
            release.countDown();
            executor.shutdownNow();
        }
    }

    @Test
    void tracingWithoutMetricsStillEndsExactlyOnce() {
        AgentMcpCallTracer.Trace trace = mock(AgentMcpCallTracer.Trace.class);
        AgentMcpCallObservation observation = new AgentMcpCallObservation(null, ignored -> trace, Context.empty());
        observation.parsed(request("tools/call"));
        observation.finish(SignalType.CANCEL);
        observation.finish(SignalType.ON_COMPLETE);
        verify(trace).finish(Outcome.CANCELLED);
    }

    @ParameterizedTest
    @ValueSource(ints = {0, 1, 2})
    void tracingFailuresDoNotPreventMetricsOrChangeContext(final int stage) {
        AtomicInteger metrics = new AtomicInteger();
        AgentMcpCallTracer.Trace trace = new AgentMcpCallTracer.Trace() {
            @Override
            public Context attach(final Context context) {
                if (stage == 1) {
                    throw new IllegalStateException("private-attach-error");
                }
                return context;
            }

            @Override
            public void finish(final Outcome outcome) {
                if (stage == 2) {
                    throw new IllegalStateException("private-finish-error");
                }
            }
        };
        AgentMcpCallTracer tracer = ignored -> {
            if (stage == 0) {
                throw new IllegalStateException("private-start-error");
            }
            return trace;
        };
        AgentMcpCallObservation observation = new AgentMcpCallObservation((outcome, millis) -> metrics.incrementAndGet(), tracer, Context.empty());
        observation.parsed(request("tools/call"));
        Context context = Context.of("private-identity", "private-value");
        assertSame(context, observation.traceContext(context));
        observation.finish(SignalType.ON_COMPLETE);
        assertEquals(1, metrics.get());
    }

    @Test
    void metricsFailureDoesNotPreventTraceCompletion() {
        AgentMcpCallTracer.Trace trace = mock(AgentMcpCallTracer.Trace.class);
        AgentMcpCallObserver observer = (outcome, millis) -> {
            throw new IllegalStateException("private-observer-error");
        };
        AgentMcpCallObservation observation = new AgentMcpCallObservation(observer, ignored -> trace, Context.empty());
        observation.parsed(request("tools/call"));
        observation.finish(SignalType.ON_COMPLETE);
        verify(trace).finish(Outcome.SERVER_ERROR);
    }

    @ParameterizedTest
    @ValueSource(strings = {"tools/list", "server/discover"})
    void discoveryDoesNotStartACallTrace(final String method) {
        AtomicInteger starts = new AtomicInteger();
        AgentMcpCallObservation observation = new AgentMcpCallObservation(null, ignored -> {
            starts.incrementAndGet();
            return mock(AgentMcpCallTracer.Trace.class);
        }, Context.empty());
        observation.parsed(request(method));
        observation.finish(SignalType.ON_COMPLETE);
        assertEquals(0, starts.get());
    }

    @Test
    void invalidTraceContextCannotReplaceExecutionContextWithNull() {
        AgentMcpCallTracer.Trace trace = mock(AgentMcpCallTracer.Trace.class);
        when(trace.attach(any())).thenReturn(null);
        AgentMcpCallObservation observation = new AgentMcpCallObservation(null, ignored -> trace, Context.empty());
        observation.parsed(request("tools/call"));
        Context context = Context.empty();
        assertSame(context, observation.traceContext(context));
        observation.finish(SignalType.CANCEL);
        verify(trace).finish(Outcome.CANCELLED);
    }

    private AgentMcpRequest request(final String method) {
        ObjectNode params = new ObjectMapper().createObjectNode();
        params.putObject("_meta").putObject("io.modelcontextprotocol/clientCapabilities");
        return new AgentMcpRequest(TextNode.valueOf("same-id"), method, params);
    }
}
