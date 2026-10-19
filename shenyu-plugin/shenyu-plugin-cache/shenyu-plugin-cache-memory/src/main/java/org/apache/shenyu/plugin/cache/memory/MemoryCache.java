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
import com.github.benmanes.caffeine.cache.Caffeine;
import com.github.benmanes.caffeine.cache.Expiry;
import com.github.benmanes.caffeine.cache.Scheduler;
import org.apache.shenyu.plugin.cache.ICache;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;

import java.util.Objects;
import java.util.concurrent.TimeUnit;

/**
 * MemoryCache.
 */
public final class MemoryCache implements ICache {

    private final Cache<String, byte[]> mainCache;

    public MemoryCache() {
        // Entries must be written through VarExpiration.put(...) to receive their per-entry TTL.
        this.mainCache = Caffeine.newBuilder()
                .scheduler(Scheduler.systemScheduler())
                .expireAfter(new Expiry<String, byte[]>() {
                    @Override
                    public long expireAfterCreate(final String key, final byte[] value, final long currentTime) {
                        return 0;
                    }

                    @Override
                    public long expireAfterUpdate(final String key, final byte[] value, final long currentTime, final long currentDuration) {
                        return 0;
                    }

                    @Override
                    public long expireAfterRead(final String key, final byte[] value, final long currentTime, final long currentDuration) {
                        return currentDuration;
                    }
                }).build();
    }

    /**
     * Cache the data with the key.
     *
     * @param key            the cache key
     * @param bytes          the data
     * @param timeoutSeconds the timeout seconds
     * @return success or not
     */
    @Override
    public Mono<Boolean> cacheData(final String key, final byte[] bytes, final long timeoutSeconds) {
        // Write the value and its TTL together, overriding the default expiry callbacks.
        this.mainCache.policy().expireVariably()
                .orElseThrow(() -> new IllegalStateException("Variable expiration is not configured for the memory cache"))
                .put(key, bytes, timeoutSeconds, TimeUnit.SECONDS);
        return Mono.fromCallable(() -> Boolean.TRUE).subscribeOn(Schedulers.boundedElastic());
    }

    /**
     * Check the cache is exist or not.
     *
     * @param key the cache key
     * @return true exist
     */
    @Override
    public Mono<Boolean> isExist(final String key) {
        return Mono.fromCallable(() -> Objects.nonNull(this.mainCache.getIfPresent(key))).subscribeOn(Schedulers.boundedElastic());
    }

    /**
     * Get data with the key.
     *
     * @param key the cache key
     * @return the data
     */
    @Override
    public Mono<byte[]> getData(final String key) {
        return Mono.fromCallable(() -> this.mainCache.getIfPresent(key)).subscribeOn(Schedulers.boundedElastic());
    }

    /**
     * close the cache.
     */
    @Override
    public void close() {
        this.mainCache.invalidateAll();
        this.mainCache.cleanUp();
    }
}
