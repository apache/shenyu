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

package org.apache.shenyu.plugin.cache.base.utils;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.cache.base.ICache;
import org.apache.shenyu.plugin.cache.base.config.CacheConfig;
import org.apache.shenyu.plugin.cache.base.enums.CacheEnum;
import org.apache.shenyu.plugin.cache.base.memory.MemoryCache;
import org.apache.shenyu.plugin.cache.base.redis.ShenyuCacheReactiveRedisTemplate;
import org.springframework.web.server.ServerWebExchange;

/**
 * CacheUtils.
 */
public final class CacheUtils {

    private static final String CONTENT_TYPEKEY_SUFFIX = "-contentType";

    private CacheUtils() {
    }

    /**
     * the cache data key.
     *
     * @param exchange the exchange.
     * @return data key
     */
    public static String dataKey(final ServerWebExchange exchange) {
        // TODO: 2022/3/16 暂定为path路径，后面可以通过页面配置中设置key
        ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        assert shenyuContext != null;
        return shenyuContext.getPath();
    }

    /**
     * the cache data content type key.
     *
     * @param exchange the exchange
     * @return the content type key
     */
    public static String contentTypeKey(final ServerWebExchange exchange) {
        return dataKey(exchange) + CONTENT_TYPEKEY_SUFFIX;
    }

    /**
     * get the cache.
     *
     * @return cache
     */
    public static ICache getCache() {
        final CacheConfig cacheConfig = Singleton.INST.get(CacheConfig.class);
        assert cacheConfig != null;
        ICache cache = null;
        if (CacheEnum.MEMORY.getName().equals(cacheConfig.getCacheType())) {
            cache = Singleton.INST.get(MemoryCache.class);
        } else if (CacheEnum.REDIS.getName().equals(cacheConfig.getCacheType())) {
            cache = Singleton.INST.get(ShenyuCacheReactiveRedisTemplate.class);
        }
        return cache;
    }
}
