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
import org.springframework.data.redis.connection.ReactiveRedisConnection;
import org.springframework.data.redis.connection.ReactiveRedisConnectionFactory;
import org.springframework.data.redis.core.ReactiveRedisTemplate;
import reactor.core.publisher.Mono;

import java.time.Duration;
import java.util.Objects;

/**
 * RedisCache.
 */
public final class RedisCache implements ICache {

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
    public Mono<Boolean> cacheData(final String key, final byte[] bytes, final long timeoutSeconds) {
        return this.redisTemplate.opsForValue().set(key, bytes, Duration.ofSeconds(timeoutSeconds));
    }

    /**
     * Check the cache is existed or not.
     * @param key the cache key
     * @return true exist
     */
    @Override
    public Mono<Boolean> isExist(final String key) {
        return this.redisTemplate.hasKey(key);
    }

    /**
     * Get data with the key.
     * @param key the cache key
     * @return the data
     */
    @Override
    public Mono<byte[]> getData(final String key) {
        return this.redisTemplate.opsForValue().get(key);
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
