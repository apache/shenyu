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

package org.apache.shenyu.admin.service.support;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.util.ReflectionTestUtils;

import java.time.Duration;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.awaitility.Awaitility.await;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasEntry;
import static org.hamcrest.Matchers.hasKey;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertTimeoutPreemptively;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test cases for bounded AI proxy key resolution and caching.
 */
@ExtendWith(MockitoExtension.class)
public final class AiProxyRealKeyResolverTest {
    
    private static final String SELECTOR_ID_1 = "selector-1";
    
    private static final String SELECTOR_ID_2 = "selector-2";
    
    private static final String SELECTOR_ID_NOT_EXIST = "selector-not-exist";
    
    private static final String API_KEY_1 = "api-key-1";
    
    private static final String API_KEY_2 = "api-key-2";
    
    private static final String HANDLE_JSON_1 = "{\"apiKey\":\"" + API_KEY_1 + "\"}";
    
    private static final String HANDLE_JSON_2 = "{\"apiKey\":\"" + API_KEY_2 + "\"}";

    @Mock
    private SelectorMapper selectorMapper;

    private ObjectMapper objectMapper;

    private AiProxyRealKeyResolver resolver;

    private AtomicLong ticker;

    @BeforeEach
    public void setUp() {
        // Use real ObjectMapper for JSON parsing
        objectMapper = new ObjectMapper();
        resolver = new AiProxyRealKeyResolver(selectorMapper, objectMapper);
        ticker = new AtomicLong();
        final Cache<?, ?> failures = getCache("failures");
        final Cache<String, Boolean> timedFailures = Caffeine.newBuilder()
                .maximumSize(failures.policy().eviction().orElseThrow().getMaximum())
                .expireAfterWrite(failures.policy().expireAfterWrite().orElseThrow().getExpiresAfter(TimeUnit.NANOSECONDS), TimeUnit.NANOSECONDS)
                .ticker(ticker::get).build();
        ReflectionTestUtils.setField(resolver, "failures", timedFailures);
    }

    @AfterEach
    public void tearDown() {
        resolver.destroy();
    }

    @Test
    public void testResolveRealKeysWithNullInput() {
        Map<String, String> result = resolver.resolveRealKeys(null);
        assertThat(result, is(Collections.emptyMap()));
        verify(selectorMapper, never()).selectByIdSet(any());
    }

    @Test
    public void testResolveRealKeysWithEmptyInput() {
        Map<String, String> result = resolver.resolveRealKeys(Collections.emptySet());
        assertThat(result, is(Collections.emptyMap()));
        verify(selectorMapper, never()).selectByIdSet(any());
    }

    @Test
    public void testResolveRealKeysWithCacheHit() {
        // Pre-populate cache
        SelectorDO selector1 = buildSelectorDO(SELECTOR_ID_1, HANDLE_JSON_1);
        when(selectorMapper.selectById(SELECTOR_ID_1)).thenReturn(selector1);
        resolver.resolveRealKey(SELECTOR_ID_1);

        // Now test batch resolve - should hit cache
        Set<String> selectorIds = new HashSet<>();
        selectorIds.add(SELECTOR_ID_1);

        Map<String, String> result = resolver.resolveRealKeys(selectorIds);

        assertThat(result, hasKey(SELECTOR_ID_1));
        assertThat(result, hasEntry(SELECTOR_ID_1, API_KEY_1));
        // Should not query database for cached entries
        verify(selectorMapper, never()).selectByIdSet(any());
    }

    @Test
    public void testResolveRealKeysWithCacheMiss() {
        // Test batch database query for cache misses
        SelectorDO selector1 = buildSelectorDO(SELECTOR_ID_1, HANDLE_JSON_1);
        SelectorDO selector2 = buildSelectorDO(SELECTOR_ID_2, HANDLE_JSON_2);

        Set<String> selectorIds = new HashSet<>();
        selectorIds.add(SELECTOR_ID_1);
        selectorIds.add(SELECTOR_ID_2);

        when(selectorMapper.selectByIdSet(selectorIds)).thenReturn(List.of(selector1, selector2));

        Map<String, String> result = resolver.resolveRealKeys(selectorIds);

        assertThat(result.size(), is(2));
        assertThat(result, hasEntry(SELECTOR_ID_1, API_KEY_1));
        assertThat(result, hasEntry(SELECTOR_ID_2, API_KEY_2));
        verify(selectorMapper, times(1)).selectByIdSet(selectorIds);
    }

    @Test
    public void testResolveRealKeysWithMixedCacheHitAndMiss() {
        // Pre-populate cache for selector1
        SelectorDO selector1 = buildSelectorDO(SELECTOR_ID_1, HANDLE_JSON_1);
        when(selectorMapper.selectById(SELECTOR_ID_1)).thenReturn(selector1);
        resolver.resolveRealKey(SELECTOR_ID_1);

        // selector2 is not in cache
        SelectorDO selector2 = buildSelectorDO(SELECTOR_ID_2, HANDLE_JSON_2);
        Set<String> selectorIds = new HashSet<>();
        selectorIds.add(SELECTOR_ID_1);
        selectorIds.add(SELECTOR_ID_2);

        when(selectorMapper.selectByIdSet(Set.of(SELECTOR_ID_2))).thenReturn(List.of(selector2));

        Map<String, String> result = resolver.resolveRealKeys(selectorIds);

        assertThat(result.size(), is(2));
        assertThat(result, hasEntry(SELECTOR_ID_1, API_KEY_1));
        assertThat(result, hasEntry(SELECTOR_ID_2, API_KEY_2));
        // Should only query for selector2
        verify(selectorMapper, times(1)).selectByIdSet(Set.of(SELECTOR_ID_2));
    }

    @Test
    public void testResolveRealKeysWithNonExistentSelector() {
        // Test null caching for non-existent selectors
        Set<String> selectorIds = new HashSet<>();
        selectorIds.add(SELECTOR_ID_NOT_EXIST);

        when(selectorMapper.selectByIdSet(selectorIds)).thenReturn(Collections.emptyList());

        Map<String, String> result = resolver.resolveRealKeys(selectorIds);

        assertThat(result.size(), is(1));
        assertThat(result, hasKey(SELECTOR_ID_NOT_EXIST));
        assertThat(result.get(SELECTOR_ID_NOT_EXIST), is(nullValue()));
        verify(selectorMapper, times(1)).selectByIdSet(selectorIds);

        // Verify null is cached - second call should not query database
        Map<String, String> result2 = resolver.resolveRealKeys(selectorIds);
        assertThat(result2, hasKey(SELECTOR_ID_NOT_EXIST));
        assertThat(result2.get(SELECTOR_ID_NOT_EXIST), is(nullValue()));
        verify(selectorMapper, times(1)).selectByIdSet(selectorIds);
    }

    @Test
    public void testResolveRealKeysWithSelectorHavingNullHandle() {
        // Test selector with null handle
        SelectorDO selector = buildSelectorDO(SELECTOR_ID_1, null);
        Set<String> selectorIds = new HashSet<>();
        selectorIds.add(SELECTOR_ID_1);

        when(selectorMapper.selectByIdSet(selectorIds)).thenReturn(List.of(selector));

        Map<String, String> result = resolver.resolveRealKeys(selectorIds);

        assertThat(result.size(), is(1));
        assertThat(result, hasKey(SELECTOR_ID_1));
        assertThat(result.get(SELECTOR_ID_1), is(nullValue()));
    }

    @Test
    public void testResolveRealKeysWithSelectorHavingInvalidHandle() {
        // Test selector with invalid JSON handle
        SelectorDO selector = buildSelectorDO(SELECTOR_ID_1, "invalid-json");
        Set<String> selectorIds = new HashSet<>();
        selectorIds.add(SELECTOR_ID_1);

        when(selectorMapper.selectByIdSet(selectorIds)).thenReturn(List.of(selector));

        Map<String, String> result = resolver.resolveRealKeys(selectorIds);

        assertThat(result.size(), is(1));
        assertThat(result, hasKey(SELECTOR_ID_1));
        assertThat(result.get(SELECTOR_ID_1), is(nullValue()));
    }

    @Test
    public void testResolveRealKeysWithMultipleNonExistentSelectors() {
        // Test multiple non-existent selectors
        Set<String> selectorIds = new HashSet<>();
        selectorIds.add(SELECTOR_ID_NOT_EXIST);
        selectorIds.add("another-not-exist");

        when(selectorMapper.selectByIdSet(selectorIds)).thenReturn(Collections.emptyList());

        Map<String, String> result = resolver.resolveRealKeys(selectorIds);

        assertThat(result.size(), is(2));
        assertThat(result, hasKey(SELECTOR_ID_NOT_EXIST));
        assertThat(result, hasKey("another-not-exist"));
        assertThat(result.get(SELECTOR_ID_NOT_EXIST), is(nullValue()));
        assertThat(result.get("another-not-exist"), is(nullValue()));
    }

    @Test
    public void testResolveRealKeysWithNullReturnFromMapper() {
        // Test when mapper returns null
        Set<String> selectorIds = new HashSet<>();
        selectorIds.add(SELECTOR_ID_1);

        when(selectorMapper.selectByIdSet(selectorIds)).thenReturn(null);

        Map<String, String> result = resolver.resolveRealKeys(selectorIds);

        assertThat(result.size(), is(1));
        assertThat(result, hasKey(SELECTOR_ID_1));
        assertThat(result.get(SELECTOR_ID_1), is(nullValue()));
    }

    @Test
    public void testResolveRealKeyFailureCooldownAndRecovery() {
        when(selectorMapper.selectById(SELECTOR_ID_1)).thenThrow(new IllegalStateException("database unavailable"))
                .thenReturn(buildSelectorDO(SELECTOR_ID_1, HANDLE_JSON_1));

        assertThat(resolver.resolveRealKey(SELECTOR_ID_1), is(Optional.empty()));
        assertThat(resolver.resolveRealKey(SELECTOR_ID_1), is(Optional.empty()));
        assertThat(resolver.resolveRealKeys(Set.of(SELECTOR_ID_1)).get(SELECTOR_ID_1), is(nullValue()));
        verify(selectorMapper, times(1)).selectById(SELECTOR_ID_1);
        verify(selectorMapper, never()).selectByIdSet(any());

        ticker.addAndGet(TimeUnit.SECONDS.toNanos(5));
        assertThat(resolver.resolveRealKey(SELECTOR_ID_1), is(Optional.of(API_KEY_1)));
        verify(selectorMapper, times(2)).selectById(SELECTOR_ID_1);
        assertThat(getInFlight(), is(Collections.emptyMap()));
    }

    @Test
    public void testResolveRealKeyEmptyResultCooldownExpires() {
        when(selectorMapper.selectById(SELECTOR_ID_1)).thenReturn(null, buildSelectorDO(SELECTOR_ID_1, HANDLE_JSON_1));

        assertThat(resolver.resolveRealKey(SELECTOR_ID_1), is(Optional.empty()));
        assertThat(resolver.resolveRealKey(SELECTOR_ID_1), is(Optional.empty()));
        verify(selectorMapper, times(1)).selectById(SELECTOR_ID_1);

        ticker.addAndGet(TimeUnit.SECONDS.toNanos(5));
        assertThat(resolver.resolveRealKey(SELECTOR_ID_1), is(Optional.of(API_KEY_1)));
        verify(selectorMapper, times(2)).selectById(SELECTOR_ID_1);
    }

    @Test
    public void testResolveRealKeysEmptyResultCooldownAndRecovery() {
        final Set<String> selectorIds = Set.of(SELECTOR_ID_1, SELECTOR_ID_2);
        when(selectorMapper.selectByIdSet(selectorIds)).thenReturn(Collections.emptyList())
                .thenReturn(List.of(buildSelectorDO(SELECTOR_ID_1, HANDLE_JSON_1), buildSelectorDO(SELECTOR_ID_2, HANDLE_JSON_2)));

        assertThat(resolver.resolveRealKeys(selectorIds).get(SELECTOR_ID_1), is(nullValue()));
        assertThat(resolver.resolveRealKeys(selectorIds).get(SELECTOR_ID_2), is(nullValue()));
        assertThat(resolver.resolveRealKey(SELECTOR_ID_1), is(Optional.empty()));
        verify(selectorMapper, times(1)).selectByIdSet(selectorIds);
        verify(selectorMapper, never()).selectById(any());

        ticker.addAndGet(TimeUnit.SECONDS.toNanos(5));
        assertThat(resolver.resolveRealKeys(selectorIds), hasEntry(SELECTOR_ID_1, API_KEY_1));
        verify(selectorMapper, times(2)).selectByIdSet(selectorIds);
    }

    @Test
    public void testInvalidateClearsFailureCooldown() {
        when(selectorMapper.selectById(SELECTOR_ID_1)).thenThrow(new IllegalStateException("database unavailable"))
                .thenReturn(buildSelectorDO(SELECTOR_ID_1, HANDLE_JSON_1));

        assertThat(resolver.resolveRealKey(SELECTOR_ID_1), is(Optional.empty()));
        resolver.invalidate(SELECTOR_ID_1);
        assertThat(resolver.resolveRealKey(SELECTOR_ID_1), is(Optional.of(API_KEY_1)));
        verify(selectorMapper, times(2)).selectById(SELECTOR_ID_1);
    }

    @Test
    public void testRefreshReplacesCachedKey() {
        when(selectorMapper.selectById(SELECTOR_ID_1)).thenReturn(buildSelectorDO(SELECTOR_ID_1, HANDLE_JSON_1),
                buildSelectorDO(SELECTOR_ID_1, HANDLE_JSON_2));

        assertThat(resolver.resolveRealKey(SELECTOR_ID_1), is(Optional.of(API_KEY_1)));
        ticker.addAndGet(TimeUnit.DAYS.toNanos(1));
        assertThat(resolver.resolveRealKey(SELECTOR_ID_1), is(Optional.of(API_KEY_1)));
        assertThat(resolver.refresh(SELECTOR_ID_1), is(Optional.of(API_KEY_2)));
        verify(selectorMapper, times(2)).selectById(SELECTOR_ID_1);
    }

    @Test
    public void testCachesStayBoundedUnderSelectorChurn() {
        final Set<String> selectorIds = IntStream.range(0, 10010).mapToObj(id -> "selector-" + id).collect(Collectors.toSet());
        final List<SelectorDO> selectors = selectorIds.stream().map(id -> buildSelectorDO(id, HANDLE_JSON_1)).collect(Collectors.toList());
        when(selectorMapper.selectByIdSet(selectorIds)).thenReturn(selectors);

        assertThat(resolver.resolveRealKeys(selectorIds).size(), is(selectorIds.size()));
        final Cache<?, ?> keys = getCache("cache");
        keys.cleanUp();
        assertTrue(keys.estimatedSize() <= 10000L);

        final Set<String> missingIds = selectorIds.stream().map(id -> "missing-" + id).collect(Collectors.toSet());
        when(selectorMapper.selectByIdSet(missingIds)).thenReturn(Collections.emptyList());
        assertThat(resolver.resolveRealKeys(missingIds).size(), is(missingIds.size()));
        final Cache<?, ?> failures = getCache("failures");
        failures.cleanUp();
        assertTrue(failures.estimatedSize() <= 10000L);
    }

    @Test
    public void testQueueSaturationFailsFastWithoutLeakingFlight() throws InterruptedException {
        final ThreadPoolExecutor executor = (ThreadPoolExecutor) ReflectionTestUtils.getField(resolver, "executorService");
        final CountDownLatch started = new CountDownLatch(10);
        final CountDownLatch release = new CountDownLatch(1);
        try {
            for (int index = 0; index < 10; index++) {
                executor.execute(() -> {
                    started.countDown();
                    awaitRelease(release);
                });
            }
            assertTrue(started.await(1, TimeUnit.SECONDS));
            for (int index = 0; index < 1024; index++) {
                executor.execute(() -> { });
            }

            assertTimeoutPreemptively(Duration.ofSeconds(1), () -> assertThat(resolver.resolveRealKey(SELECTOR_ID_1), is(Optional.empty())));
            assertThat(resolver.resolveRealKey(SELECTOR_ID_1), is(Optional.empty()));
            assertThat(executor.getQueue().size(), is(1024));
            assertThat(getInFlight(), is(Collections.emptyMap()));
            verify(selectorMapper, never()).selectById(any());
        } finally {
            release.countDown();
        }
    }

    @Test
    public void testImmediateCompletionRemovesFlight() {
        final ExecutorService executor = mock(ExecutorService.class);
        doAnswer(invocation -> {
            invocation.getArgument(0, Runnable.class).run();
            return null;
        }).when(executor).execute(any(Runnable.class));
        replaceExecutor(executor);
        when(selectorMapper.selectById(SELECTOR_ID_1)).thenReturn(buildSelectorDO(SELECTOR_ID_1, HANDLE_JSON_1));

        assertThat(resolver.resolveRealKey(SELECTOR_ID_1), is(Optional.of(API_KEY_1)));
        assertThat(getInFlight(), is(Collections.emptyMap()));
        assertThat(resolver.resolveRealKey(SELECTOR_ID_1), is(Optional.of(API_KEY_1)));
        verify(selectorMapper, times(1)).selectById(SELECTOR_ID_1);
    }

    @Test
    public void testConcurrentCallsShareOneFlight() throws Exception {
        final CountDownLatch release = new CountDownLatch(1);
        final List<Thread> callerThreads = new CopyOnWriteArrayList<>();
        final ExecutorService callers = Executors.newFixedThreadPool(4, runnable -> {
            final Thread thread = new Thread(runnable);
            callerThreads.add(thread);
            return thread;
        });
        when(selectorMapper.selectById(SELECTOR_ID_1)).thenAnswer(invocation -> {
            awaitRelease(release);
            return buildSelectorDO(SELECTOR_ID_1, HANDLE_JSON_1);
        });
        try {
            final List<Future<Optional<String>>> results = new ArrayList<>();
            for (int index = 0; index < 4; index++) {
                results.add(callers.submit(() -> resolver.resolveRealKey(SELECTOR_ID_1)));
            }
            await().atMost(Duration.ofSeconds(1)).until(() -> callerThreads.size() == 4
                    && callerThreads.stream().allMatch(thread -> thread.getState() == Thread.State.TIMED_WAITING));
            release.countDown();
            for (Future<Optional<String>> result : results) {
                assertThat(result.get(1, TimeUnit.SECONDS), is(Optional.of(API_KEY_1)));
            }
            verify(selectorMapper, times(1)).selectById(SELECTOR_ID_1);
            assertThat(getInFlight(), is(Collections.emptyMap()));
        } finally {
            release.countDown();
            callers.shutdownNow();
        }
    }

    @Test
    public void testInvalidatedTaskCannotOverwriteCacheOrRemoveNewFlight() throws Exception {
        final ThreadPoolExecutor executor = new ThreadPoolExecutor(1, 1, 0L, TimeUnit.MILLISECONDS, new ArrayBlockingQueue<>(2));
        replaceExecutor(executor);
        final ExecutorService callers = Executors.newFixedThreadPool(2);
        final CountDownLatch oldStarted = new CountDownLatch(1);
        final CountDownLatch oldRelease = new CountDownLatch(1);
        final CountDownLatch newStarted = new CountDownLatch(1);
        final CountDownLatch newRelease = new CountDownLatch(1);
        when(selectorMapper.selectById(SELECTOR_ID_1)).thenAnswer(invocation -> {
            oldStarted.countDown();
            awaitRelease(oldRelease);
            return buildSelectorDO(SELECTOR_ID_1, HANDLE_JSON_1);
        }).thenAnswer(invocation -> {
            newStarted.countDown();
            awaitRelease(newRelease);
            return buildSelectorDO(SELECTOR_ID_1, HANDLE_JSON_2);
        });
        try {
            final Future<Optional<String>> oldResult = callers.submit(() -> resolver.resolveRealKey(SELECTOR_ID_1));
            assertTrue(oldStarted.await(1, TimeUnit.SECONDS));
            resolver.invalidate(SELECTOR_ID_1);
            assertThat(oldResult.get(1, TimeUnit.SECONDS), is(Optional.empty()));
            final Future<Optional<String>> newResult = callers.submit(() -> resolver.resolveRealKey(SELECTOR_ID_1));
            await().atMost(Duration.ofSeconds(1)).until(() -> executor.getQueue().size() == 1);
            oldRelease.countDown();
            assertTrue(newStarted.await(1, TimeUnit.SECONDS));
            assertThat(getInFlight().size(), is(1));
            final Future<Optional<String>> sharedResult = callers.submit(() -> resolver.resolveRealKey(SELECTOR_ID_1));
            newRelease.countDown();
            assertThat(newResult.get(1, TimeUnit.SECONDS), is(Optional.of(API_KEY_2)));
            assertThat(sharedResult.get(1, TimeUnit.SECONDS), is(Optional.of(API_KEY_2)));
            assertThat(resolver.resolveRealKey(SELECTOR_ID_1), is(Optional.of(API_KEY_2)));
            verify(selectorMapper, times(2)).selectById(SELECTOR_ID_1);
            assertThat(getInFlight(), is(Collections.emptyMap()));
        } finally {
            oldRelease.countDown();
            newRelease.countDown();
            callers.shutdownNow();
        }
    }

    private Cache<?, ?> getCache(final String field) {
        return (Cache<?, ?>) ReflectionTestUtils.getField(resolver, field);
    }

    private Map<?, ?> getInFlight() {
        return (Map<?, ?>) ReflectionTestUtils.getField(resolver, "inFlight");
    }

    private void replaceExecutor(final ExecutorService executor) {
        final ExecutorService original = (ExecutorService) ReflectionTestUtils.getField(resolver, "executorService");
        original.shutdownNow();
        ReflectionTestUtils.setField(resolver, "executorService", executor);
    }

    private void awaitRelease(final CountDownLatch release) {
        try {
            assertTrue(release.await(5, TimeUnit.SECONDS));
        } catch (InterruptedException ex) {
            Thread.currentThread().interrupt();
            throw new IllegalStateException(ex);
        }
    }

    private SelectorDO buildSelectorDO(final String id, final String handle) {
        SelectorDO selector = new SelectorDO();
        selector.setId(id);
        selector.setHandle(handle);
        return selector;
    }
}
