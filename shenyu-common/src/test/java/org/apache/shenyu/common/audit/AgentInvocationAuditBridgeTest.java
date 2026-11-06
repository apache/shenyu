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
import org.apache.shenyu.common.metrics.AiAttemptRoute;
import org.apache.shenyu.common.metrics.AiUsage;
import org.junit.jupiter.api.Test;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.SignalType;
import reactor.core.scheduler.Schedulers;

import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class AgentInvocationAuditBridgeTest {

    @Test
    void reusesTerminalIdempotenceAndKeepsAttemptIdentity() {
        List<AgentInvocationAuditEvent> records = new CopyOnWriteArrayList<>();
        AiRequestObservation observation = new AiRequestObservation(AgentInvocationAuditBridge.ai(null, records::add), context("one"));
        AiRequestObservation.Attempt primary = observation.startAttempt(observation.getContext().withTarget("primary", "chat", null),
                AiAttemptEvent.Kind.PRIMARY, new AiAttemptRoute("openai", "openai-responses", "openai-chat", "client-model", "upstream-model"), false);
        primary.status(503);
        primary.usage(new AiUsage(3L, null, null, null, Map.of(), AiUsage.Status.REPORTED));
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
        assertEquals("openai", records.get(0).getProvider().orElseThrow());
        assertEquals("openai-responses", records.get(0).getClientProtocol().orElseThrow());
        assertEquals("openai-chat", records.get(0).getTargetProtocol().orElseThrow());
        assertEquals("client-model", records.get(0).getRequestedModel().orElseThrow());
        assertEquals("upstream-model", records.get(0).getResolvedModel().orElseThrow());
        assertEquals(3L, records.get(0).getUsage().orElseThrow().promptTokens());
        assertEquals(1, records.get(2).getUsageTotals().orElseThrow().reportedAttemptCount());
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
        final long failuresBefore = AgentInvocationAuditBridge.deliveryFailureCount();
        AtomicInteger delivered = new AtomicInteger();
        AiCallObserver existing = event -> delivered.incrementAndGet();
        assertSame(existing, AgentInvocationAuditBridge.ai(existing, null));
        AiRequestObservation observation = new AiRequestObservation(AgentInvocationAuditBridge.ai(existing, event -> {
            throw new IllegalStateException("private backend data");
        }), context("one"));
        observation.finish(SignalType.ON_COMPLETE, 200);
        assertEquals(1, delivered.get());
        assertTrue(AgentInvocationAuditBridge.deliveryFailureCount() > failuresBefore);
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

    @Test
    void invalidOptionalIdentityAndRouteFieldsDoNotDiscardRecords() {
        for (String invalid : List.of("x".repeat(257), " padded", "padded ", "control\nname", Character.toString(127), "", " ")) {
            List<AgentInvocationAuditEvent> records = new CopyOnWriteArrayList<>();
            final long omittedBefore = AgentInvocationAuditBridge.omittedMetadataCount();
            AgentRequestContext context = new AgentRequestContext("preserved", "llm", invalid, invalid)
                    .withIdentity(invalid, invalid).withApiKeyId(invalid);
            final AiCallObserver observer = AgentInvocationAuditBridge.ai(null, records::add);
            AiAttemptEvent attempt = mock(AiAttemptEvent.class);
            when(attempt.context()).thenReturn(context);
            when(attempt.attemptNumber()).thenReturn(1);
            when(attempt.outcome()).thenReturn(AiCallOutcome.ERROR);
            when(attempt.provider()).thenReturn(invalid);
            when(attempt.clientProtocol()).thenReturn(invalid);
            when(attempt.targetProtocol()).thenReturn(invalid);
            when(attempt.requestedModel()).thenReturn(invalid);
            when(attempt.resolvedModel()).thenReturn(invalid);
            observer.recordAttempt(attempt);
            new AiRequestObservation(observer, context).finish(SignalType.ON_COMPLETE, 403);
            assertEquals(2, records.size());
            assertTrue(records.stream().allMatch(event -> event.getTenant().isEmpty() && event.getSubject().isEmpty()
                    && event.getApiKeyId().isEmpty() && event.getSelectorId().isEmpty() && event.getRuleId().isEmpty()));
            assertTrue(records.get(0).getProvider().isEmpty());
            assertTrue(records.get(0).getClientProtocol().isEmpty());
            assertTrue(records.get(0).getTargetProtocol().isEmpty());
            assertTrue(records.get(0).getRequestedModel().isEmpty());
            assertTrue(records.get(0).getResolvedModel().isEmpty());
            assertTrue(AgentInvocationAuditBridge.omittedMetadataCount() >= omittedBefore + 15);
        }
    }

    @Test
    void validBoundaryIdentityAndModelsRemainUnchanged() {
        String boundary = "x".repeat(256);
        List<AgentInvocationAuditEvent> records = new CopyOnWriteArrayList<>();
        AgentRequestContext context = new AgentRequestContext("boundary", "llm", boundary, boundary)
                .withIdentity(boundary, boundary).withApiKeyId(boundary);
        AiRequestObservation observation = new AiRequestObservation(AgentInvocationAuditBridge.ai(null, records::add), context);
        observation.startAttempt(context, AiAttemptEvent.Kind.PRIMARY,
                new AiAttemptRoute("provider", "chat", "chat", boundary, boundary), false).finish(SignalType.ON_COMPLETE, null);
        observation.finish(SignalType.ON_COMPLETE, 200);
        assertEquals(2, records.size());
        assertTrue(records.stream().allMatch(event -> event.getTenant().orElseThrow().equals(boundary)
                && event.getSubject().orElseThrow().equals(boundary) && event.getApiKeyId().orElseThrow().equals(boundary)
                && event.getSelectorId().orElseThrow().equals(boundary) && event.getRuleId().orElseThrow().equals(boundary)));
        assertEquals(boundary, records.get(0).getRequestedModel().orElseThrow());
        assertEquals(boundary, records.get(0).getResolvedModel().orElseThrow());
    }

}
