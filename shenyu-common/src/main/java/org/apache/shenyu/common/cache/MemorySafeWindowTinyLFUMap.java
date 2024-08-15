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

package org.apache.shenyu.common.cache;

import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import org.apache.shenyu.common.concurrent.MemoryLimitCalculator;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.constant.Constants;
import org.checkerframework.checker.nullness.qual.NonNull;

import javax.annotation.concurrent.ThreadSafe;
import java.io.Serializable;
import java.lang.ref.WeakReference;
import java.util.AbstractMap;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * The differences between this class and {@link MemorySafeLRUMap}
 * is that this class is thread safe, and it use WindowTinyLFU algorithm
 * provided by caffeine which is better than LRU algorithm.
 */
@ThreadSafe
public class MemorySafeWindowTinyLFUMap<K, V> extends AbstractMap<K, V> implements Serializable {

    private static final long serialVersionUID = -3288161459386389022L;

    private static final AtomicBoolean GLOBAL = new AtomicBoolean(false);

    private static final Set<WeakReference<MemorySafeWindowTinyLFUMap<?, ?>>> ALL = new CopyOnWriteArraySet<>();

    private final int maxFreeMemory;

    private final Cache<K, V> cache;

    public MemorySafeWindowTinyLFUMap(final int maxFreeMemory,
                                      final int initialSize) {
        this(maxFreeMemory, initialSize, Long.MAX_VALUE, Constants.LRU_MAP_MAXSIZE);
    }

    public MemorySafeWindowTinyLFUMap(final int maxFreeMemory,
                                      final int initialSize,
                                      final long expireAfterWrite,
                                      final long maximumSize) {
        this.maxFreeMemory = maxFreeMemory;
        //see https://github.com/ben-manes/caffeine/issues/776
        this.cache = Caffeine.newBuilder()
                .expireAfterWrite(expireAfterWrite, TimeUnit.MILLISECONDS)
                .maximumSize(maximumSize)
                .initialCapacity(initialSize)
                .build();
    }

    @Override
    public V get(final Object key) {
        return cache.getIfPresent(key);
    }

    @Override
    public V put(final K key, final V value) {
        checkAndScheduleRefresh(this);
        final V previous = cache.getIfPresent(key);
        cache.put(key, value);
        return previous;
    }

    @Override
    public V remove(final Object key) {
        final V previous = cache.getIfPresent(key);
        cache.invalidate(key);
        cache.cleanUp();
        return previous;
    }

    @Override
    public Set<Entry<K, V>> entrySet() {
        return cache.asMap().entrySet();
    }

    /**
     * clean invalidated cache now.
     */
    public void cleanUp() {
        while (isFull()) {
            invalidate();
        }
    }

    /**
     * invalidate coldest cache now.
     */
    public void invalidate() {
        cache.policy().eviction().ifPresent(eviction -> {
            final Map<@NonNull K, @NonNull V> coldest = eviction.coldest(1);
            if (coldest.size() == 0) {
                return;
            }
            Optional.ofNullable(coldest.entrySet().iterator().next())
                    .ifPresent(entry -> cache.invalidate(entry.getKey()));
        });
    }

    /**
     * whether to full.
     *
     * @return true if it's full
     */
    public boolean isFull() {
        // when free memory less than certain value, consider it's full
        return cache.estimatedSize() > 0 && MemoryLimitCalculator.maxAvailable() < maxFreeMemory;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof MemorySafeWindowTinyLFUMap)) {
            return false;
        }
        if (!super.equals(o)) {
            return false;
        }
        MemorySafeWindowTinyLFUMap<?, ?> that = (MemorySafeWindowTinyLFUMap<?, ?>) o;
        return maxFreeMemory == that.maxFreeMemory && Objects.equals(cache, that.cache);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), maxFreeMemory, cache);
    }

    private static void checkAndScheduleRefresh(final MemorySafeWindowTinyLFUMap<?, ?> map) {
        ALL.add(new WeakReference<>(map));
        if (!GLOBAL.get()) {
            refresh();
            if (GLOBAL.compareAndSet(false, true)) {
                ScheduledExecutorService scheduledExecutorService =
                        new ScheduledThreadPoolExecutor(1, ShenyuThreadFactory.create("Shenyu-Memory-Safe-Lru-Map", false));
                // check every 50 ms to improve performance
                scheduledExecutorService.scheduleWithFixedDelay(MemorySafeWindowTinyLFUMap::refresh, 50, 50, TimeUnit.MILLISECONDS);
                Runtime.getRuntime().addShutdownHook(new Thread(() -> {
                    GLOBAL.set(false);
                    scheduledExecutorService.shutdown();
                }));
            }
        }
    }

    private static void refresh() {
        // try to clear weak reference
        for (WeakReference<MemorySafeWindowTinyLFUMap<?, ?>> weakReference : ALL) {
            MemorySafeWindowTinyLFUMap<?, ?> cacheMap = weakReference.get();
            if (cacheMap == null) {
                ALL.remove(weakReference);
            }
        }
        // if jvm memory is full, try to release memory from caffine
        boolean anyFull = ALL.stream().map(WeakReference::get).filter(Objects::nonNull)
                .anyMatch(MemorySafeWindowTinyLFUMap::isFull);
        while (anyFull) {
            ALL.stream().map(WeakReference::get).filter(Objects::nonNull).forEach(MemorySafeWindowTinyLFUMap::invalidate);
            anyFull = ALL.stream().map(WeakReference::get).filter(Objects::nonNull)
                    .anyMatch(MemorySafeWindowTinyLFUMap::isFull);
        }
    }
}
