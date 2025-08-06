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

import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.infra.redis.RedisConfigProperties;
import org.apache.shenyu.plugin.cache.ICache;
import org.apache.shenyu.plugin.cache.ICacheBuilder;
import org.apache.shenyu.spi.Join;

/**
 * RedisCacheBuilder.
 */
@Join
public class RedisCacheBuilder implements ICacheBuilder {

    /**
     * builder the cache with config.
     *
     * @param cacheConfig the cache config
     * @return cache instance
     */
    @Override
    public ICache builderCache(final String cacheConfig) {
        RedisConfigProperties redisConfigProperties = GsonUtils.getInstance().fromJson(cacheConfig, RedisConfigProperties.class);
        return new RedisCache(redisConfigProperties);
    }
}
