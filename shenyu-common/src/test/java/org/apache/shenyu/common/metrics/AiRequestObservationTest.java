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
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import reactor.core.publisher.SignalType;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;

class AiRequestObservationTest {

    @Test
    void recordsEachAttemptAndOnlyTheLastUsageWithoutSummation() {
        final Sink sink = new Sink();
        final AgentRequestContext base = new AgentRequestContext("request", "llm", "selector", "rule");
        final AiRequestObservation observation = new AiRequestObservation(sink, base);
        final AiRequestObservation.Attempt first = observation.startAttempt(base.withTarget("primary", "chat", null),
                AiAttemptEvent.Kind.PRIMARY, "openai", "openai-chat", false);
        first.status(503);
        first.usage(new AiUsage(5L, 2L, null, AiUsage.Status.REPORTED));
        first.finish(SignalType.ON_COMPLETE, null);
        first.finish(SignalType.CANCEL, null);
        final AiRequestObservation.Attempt second = observation.startAttempt(base.withTarget("fallback", "chat", null),
                AiAttemptEvent.Kind.FALLBACK, "openai", "openai-chat", false);
        second.status(200);
        second.finish(SignalType.ON_COMPLETE, null);
        observation.finish(SignalType.ON_COMPLETE, 200);
        observation.finish(SignalType.CANCEL, null);
        assertEquals(2, sink.attempts.size());
        assertEquals(AiCallOutcome.ERROR, sink.attempts.get(0).outcome());
        assertNull(sink.attempts.get(0).usage().totalTokens());
        assertEquals(1, sink.events.size());
        assertEquals(2, sink.events.get(0).attemptCount());
        assertSame(AiUsage.UNKNOWN, sink.events.get(0).usage());
        assertEquals("fallback", sink.events.get(0).context().getTarget());
    }

    @Test
    void cancelSealsActiveAttemptBeforeLogicalEventAndIgnoresLateFacts() {
        final Sink sink = new Sink();
        final AgentRequestContext base = new AgentRequestContext("request", "llm", null, null);
        final AiRequestObservation observation = new AiRequestObservation(sink, base);
        final AiRequestObservation.Attempt attempt = observation.startAttempt(base, AiAttemptEvent.Kind.PRIMARY, "openai", "openai-chat", true);
        attempt.status(200);
        observation.finish(SignalType.CANCEL, 200);
        attempt.usage(new AiUsage(1L, 1L, 2L, AiUsage.Status.REPORTED));
        attempt.finish(SignalType.ON_COMPLETE, null);
        observation.updateContext(base.withIdentity("late", "late"));
        assertEquals(List.of("attempt", "terminal"), sink.order);
        assertEquals(AiCallOutcome.CANCELLED, sink.attempts.get(0).outcome());
        assertEquals(AiCallOutcome.CANCELLED, sink.events.get(0).outcome());
        assertSame(AiUsage.UNKNOWN, sink.events.get(0).usage());
        assertNull(observation.getContext().getTenant());
    }

    @Test
    void acceptsOnlySameRequestIdentityUpdatesAndRecordsPreExecutionRejection() {
        final Sink sink = new Sink();
        final AgentRequestContext base = new AgentRequestContext("request", "llm", null, null);
        final AiRequestObservation observation = new AiRequestObservation(sink, base);
        observation.updateContext(base.withIdentity("tenant", "subject"));
        observation.updateContext(new AgentRequestContext("different", "llm", null, null));
        observation.finish(SignalType.ON_COMPLETE, 429);
        assertEquals("tenant", sink.events.get(0).context().getTenant());
        assertEquals(0, sink.events.get(0).attemptCount());
        assertEquals(AiCallOutcome.RATE_LIMITED, sink.events.get(0).outcome());
    }

    @Test
    void observerReentrancyAndFailureCannotDuplicateOrFailTerminalDelivery() {
        final AtomicReference<AiRequestObservation> owner = new AtomicReference<>();
        final List<AiTerminalEvent> events = new ArrayList<>();
        final AiCallObserver observer = new AiCallObserver() {
            @Override
            public void record(final AiTerminalEvent event) {
                events.add(event);
                owner.get().finish(SignalType.CANCEL, null);
                throw new IllegalStateException("private callback text");
            }

            @Override
            public void recordAttempt(final AiAttemptEvent event) {
                owner.get().finish(SignalType.CANCEL, null);
                throw new IllegalStateException("private callback text");
            }
        };
        final AgentRequestContext base = new AgentRequestContext("request", "llm", null, null);
        owner.set(new AiRequestObservation(observer, base));
        owner.get().startAttempt(base, AiAttemptEvent.Kind.PRIMARY, "openai", "openai-chat", false);
        assertDoesNotThrow(() -> owner.get().finish(SignalType.CANCEL, null));
        assertEquals(1, events.size());
    }

    @Test
    void classifiesResponseWriteFailureAndTimeoutWithoutRetainingException() {
        final Sink sink = new Sink();
        final AiRequestObservation observation = new AiRequestObservation(sink, new AgentRequestContext("request", "llm", null, null));
        observation.error(new TimeoutException("sensitive text"));
        observation.finish(SignalType.ON_ERROR, 200);
        assertEquals(AiCallOutcome.TIMEOUT, sink.events.get(0).outcome());
        assertEquals(AiCallOutcome.ERROR, AiRequestObservation.terminalOutcome(200, AiCallOutcome.ERROR));
        assertEquals(AiCallOutcome.TIMEOUT, AiRequestObservation.terminalOutcome(504, null));
    }

    @ParameterizedTest
    @ValueSource(ints = {200, 400, 401, 429, 500, 504})
    void writeFailureAndCancellationOverrideAssignedStatus(final int status) {
        final AgentRequestContext base = new AgentRequestContext("request", "llm", null, null);
        final Sink sink = new Sink();
        final AiRequestObservation observation = new AiRequestObservation(sink, base);
        observation.failure(AiCallOutcome.TIMEOUT);
        observation.error(new IllegalStateException("response write failed"));
        observation.finish(SignalType.ON_ERROR, status);
        assertEquals(AiCallOutcome.ERROR, sink.events.get(0).outcome());
        final AiRequestObservation cancelled = new AiRequestObservation(sink, base);
        cancelled.error(new IllegalStateException("response write failed"));
        cancelled.finish(SignalType.CANCEL, status);
        assertEquals(AiCallOutcome.CANCELLED, sink.events.get(1).outcome());
        final AiRequestObservation unhandled = new AiRequestObservation(sink, base);
        unhandled.finish(SignalType.ON_ERROR, status);
        assertEquals(AiCallOutcome.ERROR, sink.events.get(2).outcome());
    }

    private static final class Sink implements AiCallObserver {

        private final List<AiTerminalEvent> events = new ArrayList<>();

        private final List<AiAttemptEvent> attempts = new ArrayList<>();

        private final List<String> order = new ArrayList<>();

        @Override
        public void record(final AiTerminalEvent event) {
            events.add(event);
            order.add("terminal");
        }

        @Override
        public void recordAttempt(final AiAttemptEvent event) {
            attempts.add(event);
            order.add("attempt");
        }
    }
}
