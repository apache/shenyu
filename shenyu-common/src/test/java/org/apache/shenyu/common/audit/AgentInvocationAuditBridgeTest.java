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

package org.apache.shenyu.common.audit;

import org.apache.shenyu.common.dto.AgentRequestContext;
import org.apache.shenyu.common.metrics.AiAttemptEvent;
import org.apache.shenyu.common.metrics.AiCallObserver;
import org.apache.shenyu.common.metrics.AiCallOutcome;
import org.apache.shenyu.common.metrics.AiRequestObservation;
import org.junit.jupiter.api.Test;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.SignalType;
import reactor.core.scheduler.Schedulers;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AgentInvocationAuditBridgeTest {

    @Test
    void reusesTerminalIdempotenceAndKeepsAttemptIdentity() {
        List<AgentInvocationAuditEvent> records = new CopyOnWriteArrayList<>();
        AiRequestObservation observation = new AiRequestObservation(AgentInvocationAuditBridge.ai(null, records::add), context("one"));
        AiRequestObservation.Attempt primary = observation.startAttempt(observation.getContext().withTarget("primary", "chat", null),
                AiAttemptEvent.Kind.PRIMARY, "openai", "chat", false);
        primary.status(503);
        primary.finish(SignalType.ON_ERROR, AiCallOutcome.ERROR);
        primary.finish(SignalType.ON_ERROR, AiCallOutcome.ERROR);
        AiRequestObservation.Attempt fallback = observation.startAttempt(observation.getContext().withTarget("fallback", "chat", null),
                AiAttemptEvent.Kind.FALLBACK, "openai", "chat", false);
        fallback.status(200);
        fallback.finish(SignalType.ON_COMPLETE, null);
        observation.finish(SignalType.ON_COMPLETE, 200);
        observation.finish(SignalType.CANCEL, null);
        assertEquals(3, records.size());
        assertEquals(List.of(1, 2), records.stream().filter(event -> event.getAttempt().isPresent()).map(event -> event.getAttempt().orElseThrow()).toList());
        assertEquals(1, records.stream().filter(event -> event.getRecordType() == AgentInvocationAuditEvent.RecordType.LOGICAL).count());
        assertTrue(records.stream().allMatch(event -> event.getApiKeyId().orElseThrow().equals("key-id-one")));
        assertEquals("fallback", records.get(2).getTarget().orElseThrow());
    }

    @Test
    void independentReactorSubscriptionsDoNotShareIdentity() {
        List<AgentInvocationAuditEvent> records = new CopyOnWriteArrayList<>();
        AiCallObserver bridge = AgentInvocationAuditBridge.ai(null, records::add);
        List<Integer> completed = Flux.range(0, 64).flatMap(index -> Mono.deferContextual(view -> {
            AiRequestObservation observation = new AiRequestObservation(bridge, view.get(AgentRequestContext.REACTOR_KEY));
            observation.finish(SignalType.ON_COMPLETE, index % 2 == 0 ? 200 : 429);
            return Mono.just(index);
        }).subscribeOn(Schedulers.parallel()).contextWrite(view -> view.put(AgentRequestContext.REACTOR_KEY, context(Integer.toString(index)))))
                .collectList().block(java.time.Duration.ofSeconds(10));
        assertEquals(64, completed.size());
        assertEquals(64, records.size());
        for (AgentInvocationAuditEvent event : records) {
            assertEquals("tenant-" + event.getRequestId(), event.getTenant().orElseThrow());
            assertEquals("subject-" + event.getRequestId(), event.getSubject().orElseThrow());
            assertEquals("key-id-" + event.getRequestId(), event.getApiKeyId().orElseThrow());
            if (event.getHttpStatus().orElseThrow() == 429) {
                assertEquals(AgentInvocationAuditEvent.Decision.REJECTED, event.getDecision());
            }
        }
    }

    @Test
    void auditFailureDoesNotSuppressExistingObserverAndDisabledPathIsUnchanged() {
        AtomicInteger delivered = new AtomicInteger();
        AiCallObserver existing = event -> delivered.incrementAndGet();
        assertSame(existing, AgentInvocationAuditBridge.ai(existing, null));
        AiRequestObservation observation = new AiRequestObservation(AgentInvocationAuditBridge.ai(existing, event -> {
            throw new IllegalStateException("private backend data");
        }), context("one"));
        observation.finish(SignalType.ON_COMPLETE, 200);
        assertEquals(1, delivered.get());
    }

    private AgentRequestContext context(final String id) {
        return new AgentRequestContext(id, "llm", "selector", "rule").withIdentity("tenant-" + id, "subject-" + id)
                .withApiKeyId("key-id-" + id).withConfigurationVersion(7);
    }

    @Test
    void invalidOptionalTargetsCannotSuppressLogicalOrAttemptAudits() {
        for (String target : List.of("x".repeat(257), " padded", "padded ", "control\nname", Character.toString(127), "", " ")) {
            List<AgentInvocationAuditEvent> records = new CopyOnWriteArrayList<>();
            AiRequestObservation observation = new AiRequestObservation(AgentInvocationAuditBridge.ai(null, records::add),
                    context("rejected").withTarget(target, "chat", null));
            AiRequestObservation.Attempt attempt = observation.startAttempt(observation.getContext(), AiAttemptEvent.Kind.PRIMARY, "openai", "chat", false);
            attempt.status(403);
            attempt.finish(SignalType.ON_COMPLETE, null);
            observation.finish(SignalType.ON_COMPLETE, 403);
            assertEquals(2, records.size());
            assertTrue(records.stream().allMatch(event -> event.getTarget().isEmpty()
                    && event.getDecision() == AgentInvocationAuditEvent.Decision.REJECTED
                    && event.getHttpStatus().orElseThrow() == 403));
        }
    }

    @Test
    void validBoundaryTargetIsPreserved() {
        List<AgentInvocationAuditEvent> records = new CopyOnWriteArrayList<>();
        String target = "x".repeat(256);
        AiRequestObservation observation = new AiRequestObservation(AgentInvocationAuditBridge.ai(null, records::add),
                context("one").withTarget(target, "chat", null));
        observation.finish(SignalType.ON_COMPLETE, 200);
        assertEquals(target, records.get(0).getTarget().orElseThrow());
    }

}
