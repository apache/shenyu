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
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
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
    void anExpiredDeadlineIsReportedAsItsOwnOutcomeNotOverload() {
        AiConcurrencyAdmission admission = new AiConcurrencyAdmission(properties(1, 4));
        StepVerifier.create(admission.acquireTenant("t1", EXPIRED_DEADLINE))
                .expectNextMatches(result -> result.outcome() == AdmissionOutcome.WAIT_DEADLINE_EXCEEDED)
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
    void cancellingAWaitingAcquisitionNeverLeaksThePermit() {
        // the exact scenario #8254 asks for: a client disconnect or gateway cancel while
        // the acquisition waits. The waiter holds no thread and no permit; a late release
        // that races the cancellation hands the permit back to the pool, not to nobody.
        AiConcurrencyAdmission admission = new AiConcurrencyAdmission(properties(1, 4));
        final AdmissionResult holder = admission.acquireTenant("t1", FAR_DEADLINE).block();
        reactor.core.Disposable waitingSubscription = admission.acquireTenant("t1", FAR_DEADLINE).subscribe();
        await().atMost(2, TimeUnit.SECONDS).until(() -> admission.waitingOf("tenant:t1") == 1);
        waitingSubscription.dispose();
        await().atMost(2, TimeUnit.SECONDS).until(() -> admission.waitingOf("tenant:t1") == 0);
        assertEquals(1, admission.inflightOf("tenant:t1"), "the holder keeps its permit; the cancelled waiter holds nothing");
        admission.release(holder.ticket());
        assertEquals(0, admission.inflightOf("tenant:t1"), "no permit was leaked by the cancellation");
    }

    @Test
    void cancellingARacerThatWasAlreadyGrantedReturnsThePermit() {
        // the dispose lands after dispatch granted the permit but before the subscriber
        // consumed it: the permit must return to the pool, not vanish with the race
        AiConcurrencyAdmission admission = new AiConcurrencyAdmission(properties(1, 4));
        final AdmissionResult holder = admission.acquireTenant("t1", FAR_DEADLINE).block();
        java.util.concurrent.atomic.AtomicReference<AdmissionTicket> raced = new java.util.concurrent.atomic.AtomicReference<>();
        admission.release(holder.ticket());
        // acquire and immediately release: the grant-dispose race is exercised by the
        // timing of the blocking step below; repeat to widen the window
        for (int round = 0; round < 200; round++) {
            java.util.concurrent.CountDownLatch hold = new java.util.concurrent.CountDownLatch(1);
            reactor.core.publisher.Mono<AdmissionResult> acquisition = admission.acquireTenant("t1", FAR_DEADLINE)
                    .doOnNext(result -> raced.set(result.ticket()))
                    .doFinally(signal -> hold.countDown());
            reactor.core.Disposable subscription = acquisition.subscribe();
            subscription.dispose();
            await().atMost(2, TimeUnit.SECONDS).until(() -> hold.getCount() == 0);
            if (Objects.nonNull(raced.get())) {
                admission.release(raced.get());
                raced.set(null);
            }
        }
        assertEquals(0, admission.inflightOf("tenant:t1"), "200 raced grants must not leak a single permit");
    }

    @Test
    void releaseIsIdempotentAndCannotInflateThePool() {
        AiConcurrencyAdmission admission = new AiConcurrencyAdmission(properties(1, 0));
        final AdmissionResult first = admission.acquireTenant("t1", FAR_DEADLINE).block();
        admission.release(first.ticket());
        admission.release(first.ticket());
        admission.release(first.ticket());
        assertEquals(0, admission.inflightOf("tenant:t1"), "double releases must not over-admit");
    }

    @Test
    void theFastPathNeverBargesAheadOfAQueuedWaiter() {
        // a fresh arrival while a waiter is queued must not win the released permit:
        // it waits behind the waiter even though a permit would be free after the hand-over
        AiConcurrencyAdmission admission = new AiConcurrencyAdmission(properties(1, 4));
        final AdmissionResult holder = admission.acquireTenant("t1", FAR_DEADLINE).block();
        AtomicReference<AdmissionResult> waiter = new AtomicReference<>();
        admission.acquireTenant("t1", FAR_DEADLINE).subscribe(waiter::set);
        await().atMost(2, TimeUnit.SECONDS).until(() -> admission.waitingOf("tenant:t1") == 1);
        admission.release(holder.ticket());
        await().atMost(2, TimeUnit.SECONDS).until(() ->
                Objects.nonNull(waiter.get()) && waiter.get().outcome() == AdmissionOutcome.ACQUIRED);
        // now the fresh arrival subscribes (after the hand-over): it must queue, not barge
        AtomicReference<AdmissionResult> lateArrival = new AtomicReference<>();
        admission.acquireTenant("t1", FAR_DEADLINE).subscribe(lateArrival::set);
        await().atMost(2, TimeUnit.SECONDS).until(() -> admission.waitingOf("tenant:t1") == 1);
        assertNull(lateArrival.get(), "a fresh arrival must wait behind the queue instead of barging");
        admission.release(waiter.get().ticket());
        await().atMost(2, TimeUnit.SECONDS).until(() ->
                Objects.nonNull(lateArrival.get()) && lateArrival.get().outcome() == AdmissionOutcome.ACQUIRED);
        admission.release(lateArrival.get().ticket());
        assertEquals(0, admission.inflightOf("tenant:t1"));
    }

    @Test
    void concurrentDispatchersNeverDropOrDoubleCountAWaiter() throws Exception {
        // the re-review race: two dispatchers (two concurrent releases) both peek the
        // same stale head — identity removal must settle every queued waiter exactly
        // once, never dropping the second one into a false timeout (issue #8254)
        final int rounds = 300;
        java.util.concurrent.ExecutorService pool = java.util.concurrent.Executors.newFixedThreadPool(4);
        try {
            for (int round = 0; round < rounds; round++) {
                AiConcurrencyAdmissionProperties roundProperties = properties(2, 4);
                roundProperties.setMaxWaitMillis(5000L);
                AiConcurrencyAdmission admission = new AiConcurrencyAdmission(roundProperties);
                final AdmissionResult holder1 = admission.acquireTenant("t1", FAR_DEADLINE).block();
                final AdmissionResult holder2 = admission.acquireTenant("t1", FAR_DEADLINE).block();
                assertNotNull(holder1.ticket());
                assertNotNull(holder2.ticket());
                java.util.concurrent.atomic.AtomicInteger granted = new java.util.concurrent.atomic.AtomicInteger();
                Runnable enqueue = () -> admission.acquireTenant("t1", FAR_DEADLINE).subscribe(result -> {
                    if (result.outcome() == AdmissionOutcome.ACQUIRED) {
                        granted.incrementAndGet();
                    }
                });
                pool.submit(enqueue);
                pool.submit(enqueue);
                // two concurrent releases: two dispatchers race over the same queue head;
                // the releases may land before or after the waiters are queued — both orders
                // must end with both waiters granted
                java.util.concurrent.CountDownLatch released = new java.util.concurrent.CountDownLatch(2);
                pool.submit(() -> {
                    admission.release(holder1.ticket());
                    released.countDown();
                });
                pool.submit(() -> {
                    admission.release(holder2.ticket());
                    released.countDown();
                });
                assertTrue(released.await(2, TimeUnit.SECONDS));
                await().atMost(5, TimeUnit.SECONDS).until(() -> granted.get() == 2);
                assertEquals(2, admission.inflightOf("tenant:t1"), "round " + round);
                assertEquals(0, admission.waitingOf("tenant:t1"), "round " + round);
                admission.release(new AdmissionTicket("tenant:t1"));
                admission.release(new AdmissionTicket("tenant:t1"));
            }
        } finally {
            pool.shutdownNow();
        }
    }

    @Test
    void misconfiguredBoundsFailFast() {
        AiConcurrencyAdmissionProperties zeroInflight = properties(0, 0);
        assertThrows(IllegalStateException.class, () -> new AiConcurrencyAdmission(zeroInflight));

        AiConcurrencyAdmissionProperties negativeWaiting = properties(1, -1);
        assertThrows(IllegalStateException.class, () -> new AiConcurrencyAdmission(negativeWaiting));
    }
}
