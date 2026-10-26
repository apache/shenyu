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

package org.apache.shenyu.plugin.cache;

import org.apache.shenyu.common.dto.convert.selector.CacheUpstream;
import org.apache.shenyu.plugin.cache.cache.ApplicationConfigCache;
import org.apache.shenyu.spi.ExtensionLoader;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.MockedStatic;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.CyclicBarrier;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Tests ownership of selector backends during concurrent initialization and invalidation.
 */
class ApplicationConfigCacheConcurrencyTest {

    private static final String SELECTOR_ID = "concurrent-cache-selector";

    private final ApplicationConfigCache cache = ApplicationConfigCache.getInstance();

    private final ICacheBuilder builder = mock(ICacheBuilder.class);

    private ExtensionLoader<ICacheBuilder> loader;

    private ExecutorService executor;

    @BeforeEach
    @SuppressWarnings("unchecked")
    void setUp() {
        cache.invalidateAll();
        loader = mock(ExtensionLoader.class);
        when(loader.getJoin("memory")).thenReturn(builder);
        executor = Executors.newFixedThreadPool(2);
    }

    @AfterEach
    void tearDown() throws InterruptedException {
        executor.shutdownNow();
        assertTrue(executor.awaitTermination(10, TimeUnit.SECONDS));
        cache.invalidateAll();
    }

    @Test
    void testConcurrentFirstRequestsShareOneBackend() throws Exception {
        List<ICache> created = new CopyOnWriteArrayList<>();
        when(builder.builderCache(anyString())).thenAnswer(invocation -> {
            ICache backend = mock(ICache.class);
            created.add(backend);
            return backend;
        });
        ApplicationConfigCache observedCache = spy(cache);
        CyclicBarrier emptyLookups = new CyclicBarrier(2);
        doAnswer(invocation -> {
            Object backend = invocation.callRealMethod();
            // Force both real request helpers to observe the absent selector before either initializes it.
            emptyLookups.await(10, TimeUnit.SECONDS);
            return backend;
        }).when(observedCache).get(SELECTOR_ID);
        CacheUpstream upstream = upstream("localhost:6379");

        Future<ICache> first = executor.submit(() -> requestCache(observedCache, upstream));
        Future<ICache> second = executor.submit(() -> requestCache(observedCache, upstream));
        ICache firstBackend = first.get(10, TimeUnit.SECONDS);
        ICache secondBackend = second.get(10, TimeUnit.SECONDS);

        assertEquals(1, created.size(), "Concurrent first requests must not create an unowned backend");
        assertSame(firstBackend, secondBackend);
        assertSame(firstBackend, cache.get(SELECTOR_ID));
        assertSame(upstream, cache.getUpstream(SELECTOR_ID));
        verify(firstBackend, never()).close();
        cache.invalidateCache(SELECTOR_ID);
        verify(firstBackend).close();
        assertNull(cache.get(SELECTOR_ID));
        assertNull(cache.getUpstream(SELECTOR_ID));
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void testInvalidationDoesNotRemoveReplacement(final boolean invalidateAll) throws Exception {
        ICache previous = mock(ICache.class);
        ICache replacement = mock(ICache.class);
        when(builder.builderCache(anyString())).thenReturn(previous, replacement);
        initCache(SELECTOR_ID, upstream("old:6379"));
        CountDownLatch closing = new CountDownLatch(1);
        CountDownLatch finishClose = new CountDownLatch(1);
        doAnswer(invocation -> {
            closing.countDown();
            assertTrue(finishClose.await(10, TimeUnit.SECONDS));
            return null;
        }).when(previous).close();

        Future<?> invalidation = executor.submit(() -> invalidate(invalidateAll));
        try {
            assertTrue(closing.await(10, TimeUnit.SECONDS));
            CacheUpstream nextUpstream = upstream("new:6379");
            assertSame(replacement, initCache(SELECTOR_ID, nextUpstream));
            finishClose.countDown();
            invalidation.get(10, TimeUnit.SECONDS);

            assertSame(replacement, cache.get(SELECTOR_ID));
            assertSame(nextUpstream, cache.getUpstream(SELECTOR_ID));
            verify(previous).close();
            verify(replacement, never()).close();
            cache.invalidateCache(SELECTOR_ID);
            verify(replacement).close();
        } finally {
            finishClose.countDown();
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void testConcurrentInvalidationsCloseBackendOnce(final boolean invalidateAll) throws Exception {
        ICache backend = mock(ICache.class);
        when(builder.builderCache(anyString())).thenReturn(backend);
        initCache(SELECTOR_ID, upstream("localhost:6379"));
        CountDownLatch closing = new CountDownLatch(1);
        CountDownLatch finishClose = new CountDownLatch(1);
        AtomicInteger closes = new AtomicInteger();
        doAnswer(invocation -> {
            if (closes.incrementAndGet() == 1) {
                closing.countDown();
                assertTrue(finishClose.await(10, TimeUnit.SECONDS));
            }
            return null;
        }).when(backend).close();

        Future<?> firstInvalidation = executor.submit(() -> invalidate(invalidateAll));
        try {
            assertTrue(closing.await(10, TimeUnit.SECONDS));
            invalidate(invalidateAll);
            finishClose.countDown();
            firstInvalidation.get(10, TimeUnit.SECONDS);

            verify(backend).close();
            assertNull(cache.get(SELECTOR_ID));
            assertNull(cache.getUpstream(SELECTOR_ID));
        } finally {
            finishClose.countDown();
        }
    }

    @Test
    void testFailedInitializationCanBeRetried() {
        ICache backend = mock(ICache.class);
        when(builder.builderCache(anyString())).thenThrow(new IllegalStateException("build failed")).thenReturn(backend);
        CacheUpstream upstream = upstream("localhost:6379");

        assertThrows(IllegalStateException.class, () -> initCache(SELECTOR_ID, upstream));
        assertNull(cache.get(SELECTOR_ID));
        assertNull(cache.getUpstream(SELECTOR_ID));
        assertSame(backend, initCache(SELECTOR_ID, upstream));
        assertSame(upstream, cache.getUpstream(SELECTOR_ID));
        verify(builder, times(2)).builderCache(anyString());
    }

    @Test
    void testCloseFailureDoesNotLeavePublishedBackend() {
        ICache backend = mock(ICache.class);
        when(builder.builderCache(anyString())).thenReturn(backend);
        initCache(SELECTOR_ID, upstream("localhost:6379"));
        doThrow(new IllegalStateException("close failed")).when(backend).close();

        assertThrows(IllegalStateException.class, () -> cache.invalidateCache(SELECTOR_ID));
        assertNull(cache.get(SELECTOR_ID));
        assertNull(cache.getUpstream(SELECTOR_ID));
    }

    @Test
    void testSelectorsHaveIndependentBackends() {
        ICache first = mock(ICache.class);
        ICache second = mock(ICache.class);
        when(builder.builderCache(anyString())).thenReturn(first, second);
        CacheUpstream firstUpstream = upstream("first:6379");
        CacheUpstream secondUpstream = upstream("second:6379");

        assertSame(first, initCache(SELECTOR_ID, firstUpstream));
        assertSame(second, initCache("another-selector", secondUpstream));
        assertNotSame(first, second);
        assertSame(firstUpstream, cache.getUpstream(SELECTOR_ID));
        assertSame(secondUpstream, cache.getUpstream("another-selector"));
        cache.invalidateAll();
        verify(first).close();
        verify(second).close();
    }

    private ICache requestCache(final ApplicationConfigCache observedCache, final CacheUpstream upstream) {
        // Static mocks are scoped to each request thread; the production lookup and init methods still run.
        try (MockedStatic<ExtensionLoader> extensions = mockStatic(ExtensionLoader.class);
                MockedStatic<ApplicationConfigCache> instances = mockStatic(ApplicationConfigCache.class)) {
            extensions.when(() -> ExtensionLoader.getExtensionLoader(ICacheBuilder.class)).thenReturn(loader);
            instances.when(ApplicationConfigCache::getInstance).thenReturn(observedCache);
            return ReflectionTestUtils.invokeMethod(new CachePlugin(), "getCache", SELECTOR_ID, upstream);
        }
    }

    private ICache initCache(final String selectorId, final CacheUpstream upstream) {
        try (MockedStatic<ExtensionLoader> extensions = mockStatic(ExtensionLoader.class)) {
            extensions.when(() -> ExtensionLoader.getExtensionLoader(ICacheBuilder.class)).thenReturn(loader);
            return cache.init(selectorId, upstream);
        }
    }

    private void invalidate(final boolean invalidateAll) {
        if (invalidateAll) {
            cache.invalidateAll();
        } else {
            cache.invalidateCache(SELECTOR_ID);
        }
    }

    private CacheUpstream upstream(final String url) {
        return CacheUpstream.builder().cacheType("memory").url(url).build();
    }
}
