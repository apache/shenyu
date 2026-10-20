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

package org.apache.shenyu.plugin.cache.memory;

import com.github.benmanes.caffeine.cache.Cache;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Scheduler;
import reactor.core.scheduler.Schedulers;

import java.nio.charset.StandardCharsets;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.awaitility.Awaitility.await;

/**
 * MemoryCacheTest.
 */
public class MemoryCacheTest {

    private static final String CALLER_THREAD_NAME = "caller-thread";

    private final MemoryCache memoryCache = new MemoryCache();

    @AfterEach
    public void tearDown() {
        memoryCache.close();
    }

    @Test
    public void testMemoryCache() {
        final String key = "data";
        final byte[] bytes = "data".getBytes(StandardCharsets.UTF_8);
        assertEquals(Boolean.FALSE, memoryCache.isExist(key).block());
        assertEquals(Boolean.TRUE, memoryCache.cacheData(key, bytes, 10).block());
        assertEquals(Boolean.TRUE, memoryCache.isExist(key).block());
        assertArrayEquals(bytes, memoryCache.getData(key).block());
        memoryCache.close();
        assertEquals(Boolean.FALSE, memoryCache.isExist(key).block());
        assertNull(memoryCache.getData(key).block());
    }

    @Test
    public void testExpiredEntriesAreRemovedWithoutReads() {
        for (int i = 0; i < 100; i++) {
            memoryCache.cacheData("key-" + i, new byte[]{1}, 1).block();
        }
        Cache<?, ?> cache = (Cache<?, ?>) ReflectionTestUtils.getField(memoryCache, "mainCache");
        // estimatedSize observes retained entries without triggering reads or maintenance.
        await().atMost(10, TimeUnit.SECONDS).untilAsserted(() -> assertEquals(0L, cache.estimatedSize()));
    }

    @Test
    public void testReadsRunOnTheSubscribingThread() {
        final String key = "data";
        final byte[] bytes = "data".getBytes(StandardCharsets.UTF_8);
        final Scheduler caller = Schedulers.newSingle(CALLER_THREAD_NAME);
        try {
            memoryCache.cacheData(key, bytes, 10).block();
            assertSubscribesOnCallerThread(caller, memoryCache.isExist(key), Boolean.TRUE);
            assertSubscribesOnCallerThread(caller, memoryCache.getData(key), bytes);
        } finally {
            caller.dispose();
        }
    }

    @Test
    public void testReadsAreDeferredUntilSubscription() {
        final String key = "data";
        final byte[] bytes = "data".getBytes(StandardCharsets.UTF_8);
        final Mono<byte[]> deferred = memoryCache.getData(key);
        // Assembling the publisher must not observe the cache; only subscribing may.
        memoryCache.cacheData(key, bytes, 10).block();
        assertArrayEquals(bytes, deferred.block());
    }

    @Test
    public void testPerEntryExpirationAndReplacement() {
        final byte[] bytes = "data".getBytes(StandardCharsets.UTF_8);
        memoryCache.cacheData("short", bytes, 1).block();
        memoryCache.cacheData("long", bytes, 60).block();
        memoryCache.cacheData("extended", bytes, 1).block();
        memoryCache.cacheData("extended", bytes, 60).block();
        memoryCache.cacheData("shortened", bytes, 60).block();
        memoryCache.cacheData("shortened", bytes, 1).block();

        // Polling the short entry must not extend its write TTL.
        await().pollInterval(50, TimeUnit.MILLISECONDS).atMost(10, TimeUnit.SECONDS).untilAsserted(() -> {
            assertNull(memoryCache.getData("short").block());
            assertNull(memoryCache.getData("shortened").block());
        });
        assertArrayEquals(bytes, memoryCache.getData("long").block());
        assertArrayEquals(bytes, memoryCache.getData("extended").block());
    }

    private <T> void assertSubscribesOnCallerThread(final Scheduler caller, final Mono<T> publisher, final T expected) {
        final AtomicReference<Thread> lookupThread = new AtomicReference<>();
        final T actual = publisher
                // Captures the thread that evaluates the deferred lookup, which is the subscribing thread.
                .doOnNext(ignored -> lookupThread.set(Thread.currentThread()))
                .subscribeOn(caller)
                .block();
        assertEquals(expected, actual);
        // The lookup must stay on the caller-supplied scheduler instead of hopping to bounded-elastic.
        assertTrue(lookupThread.get().getName().startsWith(CALLER_THREAD_NAME), lookupThread.get().getName());
    }

}
