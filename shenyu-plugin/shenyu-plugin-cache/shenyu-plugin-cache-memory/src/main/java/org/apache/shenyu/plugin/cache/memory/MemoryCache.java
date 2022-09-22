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

import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import org.apache.shenyu.plugin.cache.ICache;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;

import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;

/**
 * MemoryCache.
 */
public final class MemoryCache implements ICache {

    private final Map<String, Cache<String, byte[]>> mainCache;

    public MemoryCache() {
        this.mainCache = new ConcurrentHashMap<>();
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
        final Cache<String, byte[]> cache = CacheBuilder.newBuilder().expireAfterWrite(timeoutSeconds, TimeUnit.SECONDS).build();
        cache.put(key, bytes);
        this.mainCache.put(key, cache);
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
        final Cache<String, byte[]> cache = this.mainCache.get(key);
        if (Objects.isNull(cache) || !cache.asMap().containsKey(key)) {
            // remove from main cache
            this.mainCache.remove(key);
            return Mono.fromCallable(() -> Boolean.FALSE).subscribeOn(Schedulers.boundedElastic());
        }
        return Mono.fromCallable(() -> Boolean.TRUE).subscribeOn(Schedulers.boundedElastic());
    }

    /**
     * Get data with the key.
     *
     * @param key the cache key
     * @return the data
     */
    @Override
    public Mono<byte[]> getData(final String key) {
        return isExist(key).mapNotNull(exist -> {
            if (exist) {
                return this.mainCache.get(key).asMap().get(key);
            }
            return null;
        });
    }

    /**
     * close the cache.
     */
    @Override
    public void close() {
        this.mainCache.clear();
    }
}
