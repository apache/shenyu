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

package org.apache.shenyu.plugin.base.circuitbreaker;

import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.junit.jupiter.api.Test;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;
import reactor.test.StepVerifier;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test for {@link UpstreamCircuitBreaker}.
 */
public final class UpstreamCircuitBreakerTest {

    private static final String KEY = "selector:http://127.0.0.1:9090";

    @Test
    public void testRequestsAllowedByDefault() {
        UpstreamCircuitBreaker breaker = UpstreamCircuitBreaker.create(3, 100L);
        assertTrue(breaker.isRequestAllowed(KEY));
        assertFalse(breaker.isBlocking(KEY));
        assertFalse(breaker.tryAcquireHalfOpenProbe(KEY));
    }

    @Test
    public void testOpensAfterConsecutiveFailures() {
        UpstreamCircuitBreaker breaker = UpstreamCircuitBreaker.create(3, 100L);
        breaker.recordFailure(KEY);
        breaker.recordFailure(KEY);
        assertFalse(breaker.isBlocking(KEY));
        breaker.recordFailure(KEY);
        assertTrue(breaker.isBlocking(KEY));
        assertFalse(breaker.isRequestAllowed(KEY));
    }

    @Test
    public void testSuccessResetsConsecutiveFailures() {
        UpstreamCircuitBreaker breaker = UpstreamCircuitBreaker.create(3, 100L);
        breaker.recordFailure(KEY);
        breaker.recordFailure(KEY);
        breaker.recordSuccess(KEY);
        breaker.recordFailure(KEY);
        breaker.recordFailure(KEY);
        assertFalse(breaker.isBlocking(KEY));
        breaker.recordFailure(KEY);
        assertTrue(breaker.isBlocking(KEY));
    }

    @Test
    public void testSuccessClosesOpenBreaker() {
        UpstreamCircuitBreaker breaker = UpstreamCircuitBreaker.create(1, 100000L);
        breaker.recordFailure(KEY);
        assertTrue(breaker.isBlocking(KEY));
        breaker.recordSuccess(KEY);
        assertFalse(breaker.isBlocking(KEY));
        assertTrue(breaker.isRequestAllowed(KEY));
    }

    @Test
    public void testHalfOpenProbeAllowedAfterWaitWindow() throws InterruptedException {
        UpstreamCircuitBreaker breaker = UpstreamCircuitBreaker.create(1, 100L);
        breaker.recordFailure(KEY);
        assertTrue(breaker.isBlocking(KEY));
        // within the wait window no probe is granted
        assertFalse(breaker.tryAcquireHalfOpenProbe(KEY));
        TimeUnit.MILLISECONDS.sleep(150L);
        assertFalse(breaker.isRequestAllowed(KEY));
        // the first caller is granted the single probe
        assertTrue(breaker.tryAcquireHalfOpenProbe(KEY));
        assertFalse(breaker.isRequestAllowed(KEY));
        // no second probe while the first one is in flight
        assertFalse(breaker.tryAcquireHalfOpenProbe(KEY));
        // a failed probe re-opens the breaker
        breaker.recordFailure(KEY);
        assertTrue(breaker.isBlocking(KEY));
    }

    @Test
    public void testSuccessfulProbeClosesBreaker() throws InterruptedException {
        UpstreamCircuitBreaker breaker = UpstreamCircuitBreaker.create(2, 100L);
        breaker.recordFailure(KEY);
        breaker.recordFailure(KEY);
        assertTrue(breaker.isBlocking(KEY));
        TimeUnit.MILLISECONDS.sleep(150L);
        assertTrue(breaker.tryAcquireHalfOpenProbe(KEY));
        breaker.recordSuccess(KEY);
        assertFalse(breaker.isBlocking(KEY));
        // breaker fully recovered, a single new failure stays below the threshold
        breaker.recordFailure(KEY);
        assertFalse(breaker.isBlocking(KEY));
    }

    @Test
    public void testCancelledHalfOpenProbeReopensBreaker() throws Exception {
        UpstreamCircuitBreaker breaker = UpstreamCircuitBreaker.getInstance();
        breaker.reset(KEY);
        breaker.recordFailure(KEY);
        breaker.recordFailure(KEY);
        breaker.recordFailure(KEY);
        elapseWaitWindow(breaker);
        assertTrue(breaker.tryAcquireHalfOpenProbe(KEY));

        StepVerifier.create(UpstreamCircuitBreaker.recordOutcome(Mono.never(), KEY))
                .thenCancel()
                .verify();
        assertFalse(breaker.tryAcquireHalfOpenProbe(KEY));

        StepVerifier.create(UpstreamCircuitBreaker.recordOutcome(Mono.never(), KEY, true))
                .thenCancel()
                .verify();

        assertFalse(breaker.tryAcquireHalfOpenProbe(KEY));
        elapseWaitWindow(breaker);
        assertTrue(breaker.tryAcquireHalfOpenProbe(KEY));
        breaker.reset(KEY);
    }

    @Test
    public void testDefaultOutcomeRecordingIgnoresCancellation() {
        UpstreamCircuitBreaker breaker = UpstreamCircuitBreaker.getInstance();
        breaker.reset(KEY);
        breaker.recordFailure(KEY);

        StepVerifier.create(UpstreamCircuitBreaker.recordOutcome(Mono.never(), KEY))
                .thenCancel()
                .verify();
        breaker.recordFailure(KEY);

        assertTrue(breaker.isRequestAllowed(KEY));
        breaker.reset(KEY);
    }

    @Test
    public void testGrantedProbeSuccessAndFailureAreRecorded() throws Exception {
        UpstreamCircuitBreaker breaker = UpstreamCircuitBreaker.getInstance();
        breaker.reset(KEY);
        try {
            for (int index = 0; index < 3; index++) {
                breaker.recordFailure(KEY);
            }
            elapseWaitWindow(breaker);
            assertTrue(breaker.tryAcquireHalfOpenProbe(KEY));
            StepVerifier.create(UpstreamCircuitBreaker.recordOutcome(Mono.error(new IllegalStateException("probe failed")), KEY, true))
                    .expectError(IllegalStateException.class).verify();
            assertFalse(breaker.tryAcquireHalfOpenProbe(KEY));
            elapseWaitWindow(breaker);
            assertTrue(breaker.tryAcquireHalfOpenProbe(KEY));
            StepVerifier.create(UpstreamCircuitBreaker.recordOutcome(Mono.empty(), KEY, true)).verifyComplete();
            assertTrue(breaker.isRequestAllowed(KEY));
        } finally {
            breaker.reset(KEY);
        }
    }

    @Test
    public void testGrantedProbeOutcomeCannotResolveReplacementProbe() throws Exception {
        UpstreamCircuitBreaker breaker = UpstreamCircuitBreaker.getInstance();
        breaker.reset(KEY);
        try {
            for (int index = 0; index < 3; index++) {
                breaker.recordFailure(KEY);
            }
            elapseWaitWindow(breaker);
            assertTrue(breaker.tryAcquireHalfOpenProbe(KEY));
            final Mono<Object> old = UpstreamCircuitBreaker.recordOutcome(Mono.empty(), KEY, true);
            breaker.recordFailure(KEY);
            elapseWaitWindow(breaker);
            assertTrue(breaker.tryAcquireHalfOpenProbe(KEY));
            final Mono<Object> current = UpstreamCircuitBreaker.recordOutcome(Mono.empty(), KEY, true);
            StepVerifier.create(old).verifyComplete();
            assertFalse(breaker.isRequestAllowed(KEY));
            StepVerifier.create(current).verifyComplete();
            assertTrue(breaker.isRequestAllowed(KEY));
        } finally {
            breaker.reset(KEY);
        }
    }

    private void elapseWaitWindow(final UpstreamCircuitBreaker breaker) throws Exception {
        Field breakersField = UpstreamCircuitBreaker.class.getDeclaredField("breakers");
        breakersField.setAccessible(true);
        ConcurrentMap<String, ?> breakers = (ConcurrentMap<String, ?>) breakersField.get(breaker);
        Object state = breakers.get(KEY);
        Field openedAtField = state.getClass().getDeclaredField("openedAtMillis");
        openedAtField.setAccessible(true);
        openedAtField.setLong(state, System.currentTimeMillis() - 60000L);
    }

    @Test
    public void testConcurrentFailuresOpenBreaker() throws InterruptedException {
        final int threads = 32;
        UpstreamCircuitBreaker breaker = UpstreamCircuitBreaker.create(3, 100_000L);
        ExecutorService executor = Executors.newFixedThreadPool(threads);
        CountDownLatch start = new CountDownLatch(1);
        CountDownLatch done = new CountDownLatch(threads);
        for (int i = 0; i < threads; i++) {
            executor.execute(() -> {
                try {
                    start.await();
                    breaker.recordFailure(KEY);
                } catch (InterruptedException ignored) {
                    Thread.currentThread().interrupt();
                } finally {
                    done.countDown();
                }
            });
        }
        start.countDown();
        assertTrue(done.await(10, TimeUnit.SECONDS));
        executor.shutdownNow();
        assertTrue(breaker.isBlocking(KEY));
    }

    @Test
    public void testOnlyOneConcurrentHalfOpenProbeGranted() throws InterruptedException {
        final int threads = 16;
        UpstreamCircuitBreaker breaker = UpstreamCircuitBreaker.create(1, 50L);
        breaker.recordFailure(KEY);
        TimeUnit.MILLISECONDS.sleep(100L);
        ExecutorService executor = Executors.newFixedThreadPool(threads);
        CountDownLatch start = new CountDownLatch(1);
        CountDownLatch done = new CountDownLatch(threads);
        AtomicInteger granted = new AtomicInteger();
        for (int i = 0; i < threads; i++) {
            executor.execute(() -> {
                try {
                    start.await();
                    if (breaker.tryAcquireHalfOpenProbe(KEY)) {
                        granted.incrementAndGet();
                    }
                } catch (InterruptedException ignored) {
                    Thread.currentThread().interrupt();
                } finally {
                    done.countDown();
                }
            });
        }
        start.countDown();
        assertTrue(done.await(10, TimeUnit.SECONDS));
        executor.shutdownNow();
        assertEquals(1, granted.get());
    }

    @Test
    public void testRecordOutcomeRecordsTerminalSignals() {
        UpstreamCircuitBreaker breaker = UpstreamCircuitBreaker.getInstance();
        breaker.reset(KEY);
        StepVerifier.create(UpstreamCircuitBreaker.recordOutcome(Mono.error(new IllegalStateException("upstream down")), KEY))
                .expectError(IllegalStateException.class)
                .verify();
        StepVerifier.create(UpstreamCircuitBreaker.recordOutcome(Mono.just("ok"), KEY))
                .expectNext("ok")
                .verifyComplete();
        // one failure recorded and then reset by the success, so one more failure keeps the breaker closed
        breaker.recordFailure(KEY);
        breaker.recordFailure(KEY);
        assertFalse(breaker.isBlocking(KEY));
        breaker.reset(KEY);
    }

    @Test
    public void testProbeCancellationReopensAndCanRecover() {
        UpstreamCircuitBreaker breaker = UpstreamCircuitBreaker.create(1, 0);
        breaker.recordFailure(KEY);
        UpstreamCircuitBreaker.HalfOpenProbe probe = breaker.acquireHalfOpenProbe(KEY);
        assertNotNull(probe);
        assertNull(breaker.acquireHalfOpenProbe(KEY));
        StepVerifier.create(probe.recordOutcome(Mono.never())).thenCancel().verify();
        assertTrue(breaker.isBlocking(KEY));
        UpstreamCircuitBreaker.HalfOpenProbe next = breaker.acquireHalfOpenProbe(KEY);
        assertNotNull(next);
        StepVerifier.create(next.recordOutcome(Mono.empty())).verifyComplete();
        assertTrue(breaker.isRequestAllowed(KEY));
    }

    @Test
    public void testProbeErrorAndSynchronousFailureReopen() {
        UpstreamCircuitBreaker breaker = UpstreamCircuitBreaker.create(1, 0);
        breaker.recordFailure(KEY);
        UpstreamCircuitBreaker.HalfOpenProbe probe = breaker.acquireHalfOpenProbe(KEY);
        StepVerifier.create(probe.recordOutcome(Mono.defer(() -> {
            throw new IllegalStateException("synchronous failure");
        }))).expectError(IllegalStateException.class).verify();
        assertNotNull(breaker.acquireHalfOpenProbe(KEY));
    }

    @Test
    public void testRemovedProbeCannotResurrectOrChangeReplacementState() {
        UpstreamCircuitBreaker breaker = UpstreamCircuitBreaker.create(1, 0);
        breaker.recordFailure(KEY);
        UpstreamCircuitBreaker.HalfOpenProbe removed = breaker.acquireHalfOpenProbe(KEY);
        breaker.reset(KEY);
        StepVerifier.create(removed.recordOutcome(Mono.error(new IllegalStateException()))).expectError().verify();
        assertTrue(breaker.isRequestAllowed(KEY));
        breaker.recordFailure(KEY);
        final UpstreamCircuitBreaker.HalfOpenProbe replacement = breaker.acquireHalfOpenProbe(KEY);
        StepVerifier.create(removed.recordOutcome(Mono.empty())).verifyComplete();
        assertFalse(breaker.isRequestAllowed(KEY));
        assertNull(breaker.acquireHalfOpenProbe(KEY));
        StepVerifier.create(replacement.recordOutcome(Mono.empty())).verifyComplete();
        assertTrue(breaker.isRequestAllowed(KEY));
    }

    @Test
    public void testStaleProbeCannotCompleteANewerProbe() {
        UpstreamCircuitBreaker breaker = UpstreamCircuitBreaker.create(1, 0);
        breaker.recordFailure(KEY);
        UpstreamCircuitBreaker.HalfOpenProbe old = breaker.acquireHalfOpenProbe(KEY);
        StepVerifier.create(old.recordOutcome(Mono.never())).thenCancel().verify();
        final UpstreamCircuitBreaker.HalfOpenProbe current = breaker.acquireHalfOpenProbe(KEY);
        StepVerifier.create(old.recordOutcome(Mono.empty())).verifyComplete();
        assertFalse(breaker.isRequestAllowed(KEY));
        assertNull(breaker.acquireHalfOpenProbe(KEY));
        StepVerifier.create(current.recordOutcome(Mono.empty())).verifyComplete();
        assertTrue(breaker.isRequestAllowed(KEY));
    }

    @Test
    public void testLateOrdinaryOutcomesCannotResolveProbeOrDamageRecovery() throws Exception {
        UpstreamCircuitBreaker breaker = UpstreamCircuitBreaker.getInstance();
        breaker.reset(KEY);
        Sinks.Empty<Void> success = Sinks.empty();
        Sinks.Empty<Void> failure = Sinks.empty();
        UpstreamCircuitBreaker.recordOutcome(success.asMono(), KEY).subscribe();
        UpstreamCircuitBreaker.recordOutcome(failure.asMono(), KEY).subscribe(ignored -> { }, ignored -> { });
        try {
            for (int index = 0; index < UpstreamCircuitBreaker.DEFAULT_FAILURE_THRESHOLD; index++) {
                breaker.recordFailure(KEY);
            }
            Field statesField = UpstreamCircuitBreaker.class.getDeclaredField("breakers");
            statesField.setAccessible(true);
            Object state = ((Map<?, ?>) statesField.get(breaker)).get(KEY);
            Field openedAt = state.getClass().getDeclaredField("openedAtMillis");
            openedAt.setAccessible(true);
            openedAt.setLong(state, System.currentTimeMillis() - 60000L);
            UpstreamCircuitBreaker.HalfOpenProbe probe = breaker.acquireHalfOpenProbe(KEY);
            assertNotNull(probe);
            success.tryEmitEmpty();
            assertFalse(breaker.isRequestAllowed(KEY));
            assertNull(breaker.acquireHalfOpenProbe(KEY));
            StepVerifier.create(probe.recordOutcome(Mono.empty())).verifyComplete();
            failure.tryEmitError(new IllegalStateException("old request"));
            breaker.recordFailure(KEY);
            breaker.recordFailure(KEY);
            assertTrue(breaker.isRequestAllowed(KEY));
        } finally {
            breaker.reset(KEY);
        }
    }

    @Test
    public void testBuildKeyUsesSelectorIdAndDomain() {
        Upstream upstream = Upstream.builder().protocol("http://").url("127.0.0.1:9090").build();
        assertEquals("selector:http://127.0.0.1:9090", UpstreamCircuitBreaker.buildKey("selector", upstream));
    }

    @Test
    public void testResetClearsState() {
        UpstreamCircuitBreaker breaker = UpstreamCircuitBreaker.create(1, 100000L);
        breaker.recordFailure(KEY);
        assertTrue(breaker.isBlocking(KEY));
        breaker.reset(KEY);
        assertFalse(breaker.isBlocking(KEY));
    }

    @Test
    public void testMultipleKeysAreIndependent() {
        UpstreamCircuitBreaker breaker = UpstreamCircuitBreaker.create(1, 100000L);
        List<String> keys = List.of("selector-a:http://a:8080", "selector-b:http://b:8080");
        breaker.recordFailure(keys.get(0));
        assertTrue(breaker.isBlocking(keys.get(0)));
        assertFalse(breaker.isBlocking(keys.get(1)));
    }
}
