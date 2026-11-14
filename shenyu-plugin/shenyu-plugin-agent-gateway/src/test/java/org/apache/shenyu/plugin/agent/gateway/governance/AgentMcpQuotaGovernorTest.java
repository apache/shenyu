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

package org.apache.shenyu.plugin.agent.gateway.governance;

import org.apache.shenyu.common.governance.AgentGovernanceDecision;
import org.apache.shenyu.common.audit.AgentInvocationAuditEvent;
import org.apache.shenyu.common.dto.AgentRequestContext;
import org.apache.shenyu.common.metrics.AgentRuntimeMetrics;
import org.junit.jupiter.api.Test;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

final class AgentMcpQuotaGovernorTest {

    private final List<AgentGovernanceDecision> decisions = new CopyOnWriteArrayList<>();

    private final List<AgentInvocationAuditEvent> audits = new CopyOnWriteArrayList<>();

    @Test
    void enforceDenyStopsExecutionWithoutRelease() {
        RecordingStore store = new RecordingStore(new AgentMcpQuotaStore.Admission(false, true, false, ""));
        AgentMcpQuotaGovernor governor = governor(AgentMcpQuotaConfig.Mode.ENFORCE, store, false);
        StepVerifier.create(governor.withQuota("tenant", "subject", "request", 1, Mono.just("value")))
                .expectError(AgentMcpQuotaException.class)
                .verify(Duration.ofSeconds(5));
        assertEquals(1, store.admissions);
        assertEquals(1, store.releaseCount.getCount());
        assertEquals(AgentGovernanceDecision.Reason.MCP_INVOCATION_LIMIT, decisions.get(0).getReason());
    }

    @Test
    void shadowDenyStillExecutesAndReleasesShadowLease() {
        RecordingStore store = new RecordingStore(new AgentMcpQuotaStore.Admission(false, true, false, ""));
        AgentMcpQuotaGovernor governor = governor(AgentMcpQuotaConfig.Mode.SHADOW, store, false, false);
        StepVerifier.create(governor.withQuota("tenant", "subject", "request", 1, Mono.just("value")))
                .expectNext("value")
                .verifyComplete();
        assertEquals(1, store.admissions);
        assertTrue(awaitRelease(store.releaseCount));
        assertEquals(AgentGovernanceDecision.Result.WOULD_DENY, decisions.get(0).getResult());
        assertEquals(1, governor.shadowSnapshot().observedDenials());
    }

    @Test
    void shadowWouldDenyUsesRequestAuditAndRuntimeMetricsWithoutDuplicatingAllows() {
        RecordingStore store = new RecordingStore(new AgentMcpQuotaStore.Admission(false, true, false, ""));
        AgentMcpQuotaGovernor governor = governor(AgentMcpQuotaConfig.Mode.SHADOW, store, false, true);
        StepVerifier.create(governor.withQuota("tenant", "subject", "request-1", 1, Mono.just("value"))
                .contextWrite(context -> context.put(AgentRequestContext.REACTOR_KEY,
                        new AgentRequestContext("request-1", "mcp", "selector", "rule")
                                .withIdentity("tenant", "subject"))))
                .expectNext("value")
                .verifyComplete();
        assertEquals(1, audits.size());
        assertEquals(AgentInvocationAuditEvent.Decision.ALLOWED, audits.get(0).getDecision());
        assertEquals("SHADOW_MCP_INVOCATION_LIMIT", audits.get(0).getReason());
    }

    @Test
    void failOpenPolicyErrorAllowsTrafficWithoutPretendingToOwnLease() {
        RecordingStore store = new RecordingStore(null);
        store.fail = true;
        AgentMcpQuotaGovernor governor = governor(AgentMcpQuotaConfig.Mode.ENFORCE, store, true);
        StepVerifier.create(governor.withQuota("tenant", "subject", "request", 1, Mono.just("value")))
                .expectNext("value")
                .verifyComplete();
        assertEquals(1, store.admissions);
        assertEquals(1, store.releaseCount.getCount());
        assertEquals(AgentGovernanceDecision.Reason.POLICY_ERROR, decisions.get(0).getReason());
        assertEquals(AgentGovernanceDecision.Result.ALLOW, decisions.get(0).getResult());
    }

    @Test
    void missingIdentityRejectsBeforeRedisEvenWhenFailOpenOrShadow() {
        RecordingStore store = new RecordingStore(new AgentMcpQuotaStore.Admission(true, false, false, "lease"));
        AgentMcpQuotaGovernor governor = governor(AgentMcpQuotaConfig.Mode.SHADOW, store, true);
        for (String identity : java.util.Arrays.asList(null, "", " padded", "bad\nidentity")) {
            StepVerifier.create(governor.withQuota(identity, "subject", "request", 1, Mono.just("value")))
                    .expectErrorSatisfies(error -> assertEquals(AgentGovernanceDecision.Reason.AUTHENTICATION_REJECTED,
                            ((AgentMcpQuotaException) error).getDecision().getReason())).verify();
            StepVerifier.create(governor.withQuota("tenant", identity, "request", 1, Mono.just("value")))
                    .expectError(AgentMcpQuotaException.class).verify();
        }
        assertEquals(0, store.admissions);
    }

    @Test
    void deadlineCannotOutliveItsConcurrencyLeaseEvenWhenFailOpen() {
        RecordingStore store = new RecordingStore(new AgentMcpQuotaStore.Admission(true, false, false, "lease"));
        AgentMcpQuotaGovernor governor = governor(AgentMcpQuotaConfig.Mode.ENFORCE, store, true);
        StepVerifier.create(governor.withQuota("tenant", "subject", "request", 1, Instant.now().plusSeconds(3600), Mono.just("value")))
                .expectErrorSatisfies(error -> assertEquals(AgentGovernanceDecision.Reason.POLICY_ERROR,
                        ((AgentMcpQuotaException) error).getDecision().getReason())).verify();
        assertEquals(0, store.admissions);
    }

    @Test
    void failClosedPolicyErrorRejectsTraffic() {
        RecordingStore store = new RecordingStore(null);
        store.fail = true;
        AgentMcpQuotaGovernor governor = governor(AgentMcpQuotaConfig.Mode.ENFORCE, store, false);
        StepVerifier.create(governor.withQuota("tenant", "subject", "request", 1, Mono.just("value")))
                .expectErrorSatisfies(error -> assertTrue(error instanceof AgentMcpQuotaException
                        && ((AgentMcpQuotaException) error).getDecision().getReason() == AgentGovernanceDecision.Reason.POLICY_ERROR))
                .verify(Duration.ofSeconds(5));
        assertEquals(1, store.releaseCount.getCount());
        assertEquals(1, decisions.size());
        assertEquals(AgentGovernanceDecision.Result.DENY, decisions.get(0).getResult());
        assertEquals(AgentGovernanceDecision.Reason.POLICY_ERROR, decisions.get(0).getReason());
    }

    @Test
    void failClosedPolicyErrorRecordsAuditWithRequestContext() {
        RecordingStore store = new RecordingStore(null);
        store.fail = true;
        AgentMcpQuotaGovernor governor = governor(AgentMcpQuotaConfig.Mode.ENFORCE, store, false, true);
        StepVerifier.create(governor.withQuota("tenant", "subject", "request-1", 1, Mono.just("value"))
                .contextWrite(context -> context.put(AgentRequestContext.REACTOR_KEY,
                        new AgentRequestContext("request-1", "mcp", "selector", "rule")
                                .withIdentity("tenant", "subject"))))
                .expectError(AgentMcpQuotaException.class)
                .verify(Duration.ofSeconds(5));
        assertEquals(1, audits.size());
        assertEquals(AgentInvocationAuditEvent.Decision.REJECTED, audits.get(0).getDecision());
        assertEquals("POLICY_ERROR", audits.get(0).getReason());
    }

    @Test
    void cancellationReleasesConcurrencyButDoesNotRefundInvocation() {
        RecordingStore store = new RecordingStore(new AgentMcpQuotaStore.Admission(true, false, false, "lease"));
        AgentMcpQuotaGovernor governor = governor(AgentMcpQuotaConfig.Mode.ENFORCE, store, false);
        CountDownLatch subscribed = new CountDownLatch(1);
        reactor.core.Disposable subscription = governor.withQuota("tenant", "subject", "request", 1,
                Mono.defer(() -> {
                    subscribed.countDown();
                    return Mono.never();
                })).subscribe();
        assertTrue(awaitRelease(subscribed));
        subscription.dispose();
        assertTrue(awaitRelease(store.releaseCount));
    }

    @Test
    void metricsFailureDoesNotAffectAdmission() {
        RecordingStore store = new RecordingStore(new AgentMcpQuotaStore.Admission(true, false, false, "lease"));
        AgentRuntimeMetrics broken = new AgentRuntimeMetrics() {
            @Override
            public void governanceDecision(final AgentGovernanceDecision decision) {
                throw new IllegalStateException("expected");
            }
        };
        AgentMcpQuotaGovernor governor = new AgentMcpQuotaGovernor(
                AgentMcpQuotaConfig.of(AgentMcpQuotaConfig.Mode.ENFORCE, 2, Duration.ofSeconds(1), 2,
                        Duration.ofSeconds(1), false), store, broken, "test");
        StepVerifier.create(governor.withQuota("tenant", "subject", "request", 1, Mono.just("value")))
                .expectNext("value")
                .verifyComplete();
    }

    @Test
    void executionErrorStillReleasesLease() {
        RecordingStore store = new RecordingStore(new AgentMcpQuotaStore.Admission(true, false, false, "lease"));
        AgentMcpQuotaGovernor governor = governor(AgentMcpQuotaConfig.Mode.ENFORCE, store, false);
        StepVerifier.create(governor.withQuota("tenant", "subject", "request", 1,
                        Mono.error(new IllegalStateException("tool failed"))))
                .expectError(IllegalStateException.class)
                .verify(Duration.ofSeconds(5));
        assertTrue(awaitRelease(store.releaseCount));
    }

    @Test
    void releaseFailureDoesNotChangeSuccessfulToolResult() {
        RecordingStore store = new RecordingStore(new AgentMcpQuotaStore.Admission(true, false, false, "lease"));
        store.failRelease = true;
        AgentMcpQuotaGovernor governor = governor(AgentMcpQuotaConfig.Mode.ENFORCE, store, false);
        StepVerifier.create(governor.withQuota("tenant", "subject", "request", 1, Mono.just("value")))
                .expectNext("value")
                .verifyComplete();
        assertEquals(2, store.releases);
    }

    private AgentMcpQuotaGovernor governor(final AgentMcpQuotaConfig.Mode mode,
                                           final AgentMcpQuotaStore store, final boolean failOpen, final boolean audited) {
        AgentRuntimeMetrics metrics = new AgentRuntimeMetrics() {
            @Override
            public void governanceDecision(final AgentGovernanceDecision decision) {
                decisions.add(decision);
            }
        };
        return new AgentMcpQuotaGovernor(AgentMcpQuotaConfig.of(mode, 1, Duration.ofSeconds(60),
                1, Duration.ofSeconds(60), failOpen), store, metrics, "test", audited ? audits::add : null);
    }

    private AgentMcpQuotaGovernor governor(final AgentMcpQuotaConfig.Mode mode,
                                           final AgentMcpQuotaStore store, final boolean failOpen) {
        return governor(mode, store, failOpen, false);
    }

    private static boolean awaitRelease(final CountDownLatch latch) {
        try {
            return latch.await(1, TimeUnit.SECONDS);
        } catch (InterruptedException error) {
            Thread.currentThread().interrupt();
            return false;
        }
    }

    private static final class RecordingStore implements AgentMcpQuotaStore {
        private final AgentMcpQuotaStore.Admission admission;

        private boolean fail;

        private boolean failRelease;

        private int releases;

        private int admissions;

        private final CountDownLatch releaseCount = new CountDownLatch(1);

        private RecordingStore(final AgentMcpQuotaStore.Admission admission) {
            this.admission = admission;
        }

        @Override
        public Mono<Admission> admit(final AgentMcpQuotaConfig policy, final String tenant,
                                     final String subject, final String correlationId, final Instant now) {
            admissions++;
            return fail ? Mono.error(new IllegalStateException("redis unavailable")) : Mono.just(admission);
        }

        @Override
        public Mono<Boolean> release(final AgentMcpQuotaConfig policy, final String tenant,
                                     final String subject, final String correlationId, final String lease) {
            releaseCount.countDown();
            releases++;
            return failRelease ? Mono.error(new IllegalStateException("release unavailable")) : Mono.just(true);
        }
    }
}
