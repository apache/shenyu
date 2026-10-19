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

package org.apache.shenyu.plugin.divide;

import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.plugin.base.circuitbreaker.UpstreamCircuitBreaker;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.function.Supplier;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;

/**
 * Tests request-local key reuse and candidate selection.
 */
class CircuitBreakerCandidatesTest {

    private static final String SELECTOR = "candidate-selector";

    private final List<String> keys = new ArrayList<>();

    @AfterEach
    void tearDown() {
        keys.forEach(UpstreamCircuitBreaker.getInstance()::reset);
    }

    @Test
    void preservesOrderWeightsAndIdentityForEveryBlockedPosition() {
        List<Upstream> configured = List.of(upstream("first", 1), upstream("second", 7), upstream("third", 20));
        for (int blocked = 0; blocked < configured.size(); blocked++) {
            String key = key(SELECTOR, configured.get(blocked));
            open(key);
            List<Upstream> expected = new ArrayList<>(configured);
            expected.remove(blocked);
            List<Upstream> snapshot = new ArrayList<>(configured);
            CircuitBreakerCandidates candidates = CircuitBreakerCandidates.filter(SELECTOR, snapshot);
            assertSame(snapshot, candidates.getUpstreams());
            assertEquals(expected, candidates.getUpstreams());
            for (int index = 0; index < expected.size(); index++) {
                assertSame(expected.get(index), candidates.getUpstreams().get(index));
                assertEquals(key(SELECTOR, expected.get(index)), candidates.getBreakerKey(expected.get(index)));
            }
            assertEquals(3, configured.size());
            UpstreamCircuitBreaker.getInstance().reset(key);
        }
    }

    @Test
    void reusesKeysForExplicitTargetCopiesAndAllBlockedFallback() {
        Upstream first = upstream("first", 1);
        Upstream second = upstream("second", 7);
        try (MockedStatic<UpstreamCircuitBreaker> breaker = mockStatic(UpstreamCircuitBreaker.class, CALLS_REAL_METHODS)) {
            CircuitBreakerCandidates candidates = CircuitBreakerCandidates.filter(SELECTOR, new ArrayList<>(List.of(first, second)));
            assertEquals(SELECTOR + ":http://second", candidates.getBreakerKey(upstream("second", 7)));
            breaker.verify(() -> UpstreamCircuitBreaker.buildKey(SELECTOR, first), times(1));
            breaker.verify(() -> UpstreamCircuitBreaker.buildKey(SELECTOR, second), times(1));
            breaker.clearInvocations();
            open(key(SELECTOR, first));
            open(key(SELECTOR, second));
            breaker.clearInvocations();
            assertTrue(CircuitBreakerCandidates.filter(SELECTOR, new ArrayList<>(List.of(first, second))).getUpstreams().isEmpty());
            breaker.verify(() -> UpstreamCircuitBreaker.buildKey(SELECTOR, first), times(1));
            breaker.verify(() -> UpstreamCircuitBreaker.buildKey(SELECTOR, second), times(1));
        }
    }

    @Test
    void isolatesSelectorsAndRefreshesChangedTargetIdentity() {
        Upstream target = upstream("original", 1);
        String originalKey = key(SELECTOR, target);
        open(originalKey);
        assertTrue(CircuitBreakerCandidates.filter(SELECTOR, new ArrayList<>(List.of(target))).getUpstreams().isEmpty());
        CircuitBreakerCandidates otherSelector = CircuitBreakerCandidates.filter("other", new ArrayList<>(List.of(target)));
        assertEquals("other:http://original", otherSelector.getBreakerKey(target));
        target.setUrl("replacement");
        CircuitBreakerCandidates replacement = CircuitBreakerCandidates.filter(SELECTOR, new ArrayList<>(List.of(target)));
        assertEquals(SELECTOR + ":http://replacement", replacement.getBreakerKey(target));
        Upstream secure = Upstream.builder().protocol("https://").url("original").build();
        CircuitBreakerCandidates protocolChange = CircuitBreakerCandidates.filter(SELECTOR, new ArrayList<>(List.of(secure)));
        assertEquals(SELECTOR + ":https://original", protocolChange.getBreakerKey(secure));
        assertTrue(UpstreamCircuitBreaker.getInstance().isBlocking(originalKey));
    }

    @Test
    void handlesEmptyAndSingleBlockedSnapshots() {
        assertTrue(CircuitBreakerCandidates.filter(SELECTOR, new ArrayList<>()).getUpstreams().isEmpty());
        Upstream target = upstream("single", 1);
        open(key(SELECTOR, target));
        assertTrue(CircuitBreakerCandidates.filter(SELECTOR, new ArrayList<>(List.of(target))).getUpstreams().isEmpty());
    }

    @Test
    void configurationRemovalDoesNotInvalidateAnInFlightSnapshot() throws Exception {
        Upstream target = Upstream.builder().protocol("http://").url("removed").healthCheckEnabled(false).build();
        UpstreamCacheManager cache = UpstreamCacheManager.getInstance();
        try {
            cache.submit(SELECTOR, List.of(target));
            CircuitBreakerCandidates candidates = CircuitBreakerCandidates.filter(SELECTOR, cache.findUpstreamListBySelectorId(SELECTOR));
            CompletableFuture.runAsync(() -> cache.removeByKey(SELECTOR)).get(5, TimeUnit.SECONDS);
            assertNull(cache.findUpstreamListBySelectorId(SELECTOR));
            assertSame(target, candidates.getUpstreams().get(0));
            assertEquals(SELECTOR + ":http://removed", candidates.getBreakerKey(target));
            Upstream replacement = Upstream.builder().protocol("https://").url("new").healthCheckEnabled(false).build();
            cache.submit(SELECTOR, List.of(replacement));
            CircuitBreakerCandidates updated = CircuitBreakerCandidates.filter(SELECTOR, cache.findUpstreamListBySelectorId(SELECTOR));
            assertEquals(List.of(replacement), updated.getUpstreams());
            assertEquals(SELECTOR + ":https://new", updated.getBreakerKey(replacement));
        } finally {
            cache.removeByKey(SELECTOR);
        }
    }

    @Test
    void selectsOnlyTheAcquiredProbeAndPreservesItsKey() {
        Upstream first = upstream("first", 1);
        Upstream second = upstream("second", 7);
        UpstreamCircuitBreaker breaker = strictBreaker();
        breaker.recordFailure(key(SELECTOR, second));
        try (MockedStatic<UpstreamCircuitBreaker> mocked = mockStatic(UpstreamCircuitBreaker.class, CALLS_REAL_METHODS)) {
            mocked.when(UpstreamCircuitBreaker::getInstance).thenReturn(breaker);
            CircuitBreakerCandidates candidates = CircuitBreakerCandidates.filter(SELECTOR, new ArrayList<>(List.of(first, second)));
            assertEquals(List.of(second), candidates.getUpstreams());
            assertEquals(SELECTOR + ":http://second", candidates.getBreakerKey(second));
            assertTrue(CircuitBreakerCandidates.filter(SELECTOR, new ArrayList<>(List.of(first, second))).getUpstreams().isEmpty());
        }
    }

    @Test
    void concurrentFallbackDoesNotGrantMultipleProbes() throws Exception {
        Upstream target = upstream("probe", 1);
        UpstreamCircuitBreaker breaker = strictBreaker();
        String key = key(SELECTOR, target);
        breaker.recordFailure(key);
        ExecutorService executor = Executors.newFixedThreadPool(8);
        CountDownLatch start = new CountDownLatch(1);
        List<Future<Integer>> admissions = new ArrayList<>();
        try {
            for (int index = 0; index < 8; index++) {
                admissions.add(executor.submit(() -> {
                    assertTrue(start.await(5, TimeUnit.SECONDS));
                    try (MockedStatic<UpstreamCircuitBreaker> mocked = mockStatic(UpstreamCircuitBreaker.class, CALLS_REAL_METHODS)) {
                        mocked.when(UpstreamCircuitBreaker::getInstance).thenReturn(breaker);
                        return CircuitBreakerCandidates.filter(SELECTOR, new ArrayList<>(List.of(target))).getUpstreams().size();
                    }
                }));
            }
            start.countDown();
            int admitted = 0;
            for (Future<Integer> admission : admissions) {
                admitted += admission.get(10, TimeUnit.SECONDS);
            }
            assertEquals(1, admitted);
            assertFalse(breaker.tryAcquireHalfOpenProbe(key));
        } finally {
            executor.shutdownNow();
        }
    }

    @Test
    void concurrentAdmissionMatchesExistingBreakerStateMachine() throws Exception {
        Upstream target = upstream("compatibility", 1);
        String key = key(SELECTOR, target);
        for (boolean halfOpen : List.of(false, true)) {
            expireBreaker(key);
            if (halfOpen) {
                assertTrue(UpstreamCircuitBreaker.getInstance().tryAcquireHalfOpenProbe(key));
            }
            int before = concurrentAdmissions(() -> originalFilter(target));
            expireBreaker(key);
            if (halfOpen) {
                assertTrue(UpstreamCircuitBreaker.getInstance().tryAcquireHalfOpenProbe(key));
            }
            int after = concurrentAdmissions(() -> CircuitBreakerCandidates.filter(SELECTOR, new ArrayList<>(List.of(target))).getUpstreams());
            // This is a compatibility check, not proof of single-probe admission:
            // the real breaker still has #7760 (both versions admit all callers).
            assertEquals(before, after);
        }
    }

    private int concurrentAdmissions(final Supplier<List<Upstream>> filter) throws Exception {
        ExecutorService executor = Executors.newFixedThreadPool(8);
        CountDownLatch start = new CountDownLatch(1);
        List<Future<Integer>> results = new ArrayList<>();
        try {
            for (int index = 0; index < 8; index++) {
                results.add(executor.submit(() -> {
                    assertTrue(start.await(5, TimeUnit.SECONDS));
                    return filter.get().size();
                }));
            }
            start.countDown();
            int admitted = 0;
            for (Future<Integer> result : results) {
                admitted += result.get(10, TimeUnit.SECONDS);
            }
            return admitted;
        } finally {
            executor.shutdownNow();
        }
    }

    private List<Upstream> originalFilter(final Upstream upstream) {
        if (UpstreamCircuitBreaker.getInstance().isRequestAllowed(UpstreamCircuitBreaker.buildKey(SELECTOR, upstream))) {
            return Collections.singletonList(upstream);
        }
        if (UpstreamCircuitBreaker.getInstance().tryAcquireHalfOpenProbe(UpstreamCircuitBreaker.buildKey(SELECTOR, upstream))) {
            return Collections.singletonList(upstream);
        }
        return Collections.emptyList();
    }

    private void expireBreaker(final String key) throws Exception {
        UpstreamCircuitBreaker.getInstance().reset(key);
        open(key);
        Field breakersField = UpstreamCircuitBreaker.class.getDeclaredField("breakers");
        breakersField.setAccessible(true);
        Object state = ((Map<?, ?>) breakersField.get(UpstreamCircuitBreaker.getInstance())).get(key);
        Field openedAt = state.getClass().getDeclaredField("openedAtMillis");
        openedAt.setAccessible(true);
        openedAt.setLong(state, System.currentTimeMillis() - 60000L);
    }

    private UpstreamCircuitBreaker strictBreaker() {
        // #7760 owns ordinary eligibility for expired OPEN / HALF_OPEN states.
        // Exercise the fallback against the intended eligibility contract while
        // retaining the real atomic state transition, rather than masking that bug.
        UpstreamCircuitBreaker breaker = spy(UpstreamCircuitBreaker.create(1, 0));
        doReturn(false).when(breaker).isRequestAllowed(anyString());
        return breaker;
    }

    private Upstream upstream(final String url, final int weight) {
        return Upstream.builder().protocol("http://").url(url).weight(weight).build();
    }

    private String key(final String selector, final Upstream target) {
        String key = UpstreamCircuitBreaker.buildKey(selector, target);
        keys.add(key);
        return key;
    }

    private void open(final String key) {
        for (int index = 0; index < UpstreamCircuitBreaker.DEFAULT_FAILURE_THRESHOLD; index++) {
            UpstreamCircuitBreaker.getInstance().recordFailure(key);
        }
    }
}
