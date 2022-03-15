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

package org.apache.shenyu.plugin.cache.base.redis;

import org.apache.shenyu.plugin.cache.base.ICache;
import org.apache.shenyu.plugin.cache.base.redis.serializer.ShenyuSerializationContext;
import org.springframework.data.redis.connection.ReactiveRedisConnectionFactory;
import org.springframework.data.redis.core.ReactiveRedisTemplate;

/**
 * ShenyuCacheReactiveRedisTemplate.
 */
public class ShenyuCacheReactiveRedisTemplate extends ReactiveRedisTemplate<String, byte[]> implements ICache {

    public ShenyuCacheReactiveRedisTemplate(final ReactiveRedisConnectionFactory connectionFactory) {
        super(connectionFactory, ShenyuSerializationContext.bytesSerializationContext());
    }

    /**
     * Cache the data with the key.
     * @param key the cache key
     * @param bytes the data
     * @param timeoutSeconds the timeout seconds
     * @return success or not
     */
    @Override
    public boolean cacheData(final String key, final byte[] bytes, final long timeoutSeconds) {
        return true;
    }

    /**
     * Check the cache is exist or not.
     * @param key the cache key
     * @return true exist
     */
    @Override
    public boolean isExist(final String key) {
        return false;
    }

    /**
     * Get data with the key.
     * @param key the cache key
     * @return the data
     */
    @Override
    public byte[] getData(final String key) {
        return new byte[0];
    }
}
