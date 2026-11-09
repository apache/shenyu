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

package org.apache.shenyu.plugin.ai.proxy.enhanced.admission;

import org.apache.shenyu.plugin.ai.proxy.enhanced.admission.AiConcurrencyAdmission.AdmissionOutcome;
import org.apache.shenyu.plugin.ai.proxy.enhanced.admission.AiConcurrencyAdmission.AdmissionResult;
import org.apache.shenyu.plugin.ai.proxy.enhanced.admission.AiConcurrencyAdmission.AdmissionTicket;
import org.junit.jupiter.api.Test;
import reactor.test.StepVerifier;

import java.util.Objects;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Deterministic pool-semantics tests for the concurrency admission: immediate
 * rejections, bounded waiting, exactly-once release, anonymous sharing, pool caps
 * and fail-fast configuration.
 */
class AiConcurrencyAdmissionTest {

    private static final long FAR_DEADLINE = System.nanoTime() + 60_000_000_000L;

    private static final long EXPIRED_DEADLINE = System.nanoTime() - 1L;

    private AiConcurrencyAdmissionProperties properties(final int tenantInflight, final int tenantWaiting) {
        AiConcurrencyAdmissionProperties properties = new AiConcurrencyAdmissionProperties();
        properties.setTenantMaxInflight(tenantInflight);
        properties.setTenantMaxWaiting(tenantWaiting);
        properties.setMaxWaitMillis(200L);
        return properties;
    }

    @Test
    void acquireAndReleaseCycleRestoresThePool() {
        AiConcurrencyAdmission admission = new AiConcurrencyAdmission(properties(1, 0));

        final AdmissionResult first = admission.acquireTenant("t1", FAR_DEADLINE).block();
        assertNotNull(first);
        assertEquals(AdmissionOutcome.ACQUIRED, first.outcome());
        assertEquals(1, admission.inflightOf("tenant:t1"));

        admission.release(first.ticket());
        assertEquals(0, admission.inflightOf("tenant:t1"));
    }

    @Test
    void aSecondArrivalAtAFullPoolWithoutWaitingBoundIsRejectedImmediately() {
        AiConcurrencyAdmission admission = new AiConcurrencyAdmission(properties(1, 0));
        final AdmissionResult first = admission.acquireTenant("t1", FAR_DEADLINE).block();

        StepVerifier.create(admission.acquireTenant("t1", FAR_DEADLINE))
                .expectNextMatches(result -> result.outcome() == AdmissionOutcome.OVERLOAD)
                .verifyComplete();
        assertEquals(1, admission.inflightOf("tenant:t1"));
        admission.release(first.ticket());
    }

    @Test
    void aFullWaitingBoundRejectsImmediately() {
        AiConcurrencyAdmission admission = new AiConcurrencyAdmission(properties(1, 1));
        final AdmissionResult holder = admission.acquireTenant("t1", FAR_DEADLINE).block();
        // one waiter occupies the waiting bound
        AtomicReference<AdmissionResult> waiter = new AtomicReference<>();
        admission.acquireTenant("t1", FAR_DEADLINE).subscribe(waiter::set);

        await().atMost(2, TimeUnit.SECONDS).until(() -> admission.waitingOf("tenant:t1") == 1);
        StepVerifier.create(admission.acquireTenant("t1", FAR_DEADLINE))
                .expectNextMatches(result -> result.outcome() == AdmissionOutcome.OVERLOAD)
                .verifyComplete();
        assertEquals(1, admission.inflightOf("tenant:t1"), "the holder keeps its permit; only the overflow was rejected");
        admission.release(holder.ticket());
        await().atMost(2, TimeUnit.SECONDS).until(() ->
                Objects.nonNull(waiter.get()) && waiter.get().outcome() == AdmissionOutcome.ACQUIRED);
        admission.release(waiter.get().ticket());
    }

    @Test
    void aWaiterIsAdmittedWhenCapacityIsReleased() {
        AiConcurrencyAdmission admission = new AiConcurrencyAdmission(properties(1, 4));
        final AdmissionResult holder = admission.acquireTenant("t1", FAR_DEADLINE).block();
        AtomicReference<AdmissionResult> waiter = new AtomicReference<>();
        admission.acquireTenant("t1", FAR_DEADLINE).subscribe(waiter::set);

        await().atMost(2, TimeUnit.SECONDS).until(() -> admission.waitingOf("tenant:t1") == 1);
        admission.release(holder.ticket());

        await().atMost(2, TimeUnit.SECONDS).until(() ->
                Objects.nonNull(waiter.get()) && waiter.get().outcome() == AdmissionOutcome.ACQUIRED);
        assertEquals(1, admission.inflightOf("tenant:t1"));
        admission.release(waiter.get().ticket());
        assertEquals(0, admission.inflightOf("tenant:t1"));
    }

    @Test
    void anExpiredDeadlineRejectsBeforeAnyWaiting() {
        AiConcurrencyAdmission admission = new AiConcurrencyAdmission(properties(1, 4));
        StepVerifier.create(admission.acquireTenant("t1", EXPIRED_DEADLINE))
                .expectNextMatches(result -> result.outcome() == AdmissionOutcome.OVERLOAD)
                .verifyComplete();
        assertEquals(-1, admission.inflightOf("tenant:t1"));
    }

    @Test
    void anonymousTenantsShareOnePool() {
        AiConcurrencyAdmission admission = new AiConcurrencyAdmission(properties(1, 0));
        final AdmissionResult first = admission.acquireTenant(null, FAR_DEADLINE).block();

        StepVerifier.create(admission.acquireTenant(null, FAR_DEADLINE))
                .expectNextMatches(result -> result.outcome() == AdmissionOutcome.OVERLOAD)
                .verifyComplete();
        assertEquals(1, admission.inflightOf("tenant:anonymous"));
        admission.release(first.ticket());
    }

    @Test
    void distinctTenantsDoNotSharePools() {
        AiConcurrencyAdmission admission = new AiConcurrencyAdmission(properties(1, 0));
        final AdmissionResult first = admission.acquireTenant("t1", FAR_DEADLINE).block();

        AdmissionResult second = admission.acquireTenant("t2", FAR_DEADLINE).block();
        assertNotNull(second);
        assertEquals(AdmissionOutcome.ACQUIRED, second.outcome());
        assertEquals(1, admission.inflightOf("tenant:t1"));
        assertEquals(1, admission.inflightOf("tenant:t2"));
        admission.release(first.ticket());
        admission.release(second.ticket());
    }

    @Test
    void tenantAndModelPoolsAreIndependent() {
        AiConcurrencyAdmission admission = new AiConcurrencyAdmission(properties(1, 0));
        final AdmissionResult tenant = admission.acquireTenant("t1", FAR_DEADLINE).block();

        AdmissionResult model = admission.acquireTarget("main-model", FAR_DEADLINE).block();
        assertNotNull(model);
        assertEquals(AdmissionOutcome.ACQUIRED, model.outcome());
        assertEquals(1, admission.inflightOf("tenant:t1"));
        assertEquals(1, admission.inflightOf("model:main-model"));
        admission.release(tenant.ticket());
        admission.release(model.ticket());
    }

    @Test
    void thePoolMapIsHardCapped() {
        AiConcurrencyAdmissionProperties properties = properties(1, 0);
        properties.setMaxPools(1);
        AiConcurrencyAdmission admission = new AiConcurrencyAdmission(properties);
        admission.acquireTenant("t1", FAR_DEADLINE).block();

        StepVerifier.create(admission.acquireTenant("t2", FAR_DEADLINE))
                .expectNextMatches(result -> result.outcome() == AdmissionOutcome.OVERLOAD)
                .verifyComplete();
    }

    @Test
    void releasingAnUnknownPoolIsANoOp() {
        AiConcurrencyAdmission admission = new AiConcurrencyAdmission(properties(1, 0));
        admission.release(new AdmissionTicket("tenant:unknown"));
        admission.release(null);
        assertEquals(-1, admission.inflightOf("tenant:unknown"));
    }

    @Test
    void misconfiguredBoundsFailFast() {
        AiConcurrencyAdmissionProperties zeroInflight = properties(0, 0);
        assertThrows(IllegalStateException.class, () -> new AiConcurrencyAdmission(zeroInflight));

        AiConcurrencyAdmissionProperties negativeWaiting = properties(1, -1);
        assertThrows(IllegalStateException.class, () -> new AiConcurrencyAdmission(negativeWaiting));
    }
}
