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

package org.apache.shenyu.plugin.cache.redis;

import org.apache.shenyu.plugin.cache.ICache;
import org.apache.shenyu.plugin.cache.redis.serializer.ShenyuRedisSerializationContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.redis.connection.ReactiveRedisConnection;
import org.springframework.data.redis.connection.ReactiveRedisConnectionFactory;
import org.springframework.data.redis.core.ReactiveRedisTemplate;
import reactor.core.publisher.Mono;

import java.time.Duration;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * RedisCache.
 */
public final class RedisCache implements ICache {

    private static final Logger LOGGER = LoggerFactory.getLogger(RedisCache.class);

    private static final long REDIS_DEFAULT_TIMEOUT = 3L;

    private final ReactiveRedisTemplate<String, byte[]> redisTemplate;

    public RedisCache(final RedisConfigProperties redisConfigProperties) {
        this.redisTemplate = new ReactiveRedisTemplate<>(new RedisConnectionFactory(redisConfigProperties).getLettuceConnectionFactory(),
                ShenyuRedisSerializationContext.bytesSerializationContext());
    }

    /**
     * Cache the data with the key.
     * @param key the cache key
     * @param bytes the data
     * @param timeoutSeconds value valid time
     * @return success or not
     */
    @Override
    public boolean cacheData(final String key, final byte[] bytes, final long timeoutSeconds) {
        this.redisTemplate.opsForValue().set(key, bytes, Duration.ofSeconds(timeoutSeconds)).subscribe();
        return true;
    }

    /**
     * Check the cache is existed or not.
     * @param key the cache key
     * @return true exist
     */
    @Override
    public boolean isExist(final String key) {
        CompletableFuture<Boolean> f = CompletableFuture.supplyAsync(() -> Mono.from(this.redisTemplate.hasKey(key)).block());
        Boolean result = null;
        try {
            result = f.get(REDIS_DEFAULT_TIMEOUT, TimeUnit.SECONDS);
        } catch (ExecutionException | TimeoutException | InterruptedException e) {
            LOGGER.error("isExist error: {}", e.getMessage());
        }
        if (Objects.isNull(result)) {
            return Boolean.FALSE;
        }
        return Boolean.TRUE.equals(result);
    }

    /**
     * Get data with the key.
     * @param key the cache key
     * @return the data
     */
    @Override
    public byte[] getData(final String key) {
        CompletableFuture<byte[]> f = CompletableFuture.supplyAsync(() -> Mono.from(this.redisTemplate.opsForValue().get(key)).block());
        byte[] result = null;
        try {
            // can't get result in 3 seconds, this handle will fail
            result = f.get(REDIS_DEFAULT_TIMEOUT, TimeUnit.SECONDS);
        } catch (ExecutionException | TimeoutException | InterruptedException e) {
            LOGGER.error("getData error: {}", e.getMessage());
        }
        return result;
    }

    /**
     * close the redis cache.
     */
    @Override
    public void close() {
        if (Objects.isNull(this.redisTemplate)) {
            return;
        }
        final ReactiveRedisConnectionFactory connectionFactory = this.redisTemplate.getConnectionFactory();
        try {
            ReactiveRedisConnection connection = connectionFactory.getReactiveConnection();
            connection.close();
            connection = connectionFactory.getReactiveClusterConnection();
            connection.close();
        } catch (Exception ignored) {
        }
    }
}
