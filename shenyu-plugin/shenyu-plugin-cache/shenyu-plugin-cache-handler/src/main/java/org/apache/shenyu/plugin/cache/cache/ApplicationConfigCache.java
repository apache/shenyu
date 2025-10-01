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

package org.apache.shenyu.plugin.cache.cache;

import com.google.common.collect.Maps;
import org.apache.shenyu.common.dto.convert.selector.CacheUpstream;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.cache.ICache;
import org.apache.shenyu.plugin.cache.ICacheBuilder;
import org.apache.shenyu.spi.ExtensionLoader;


import java.util.Map;
import java.util.Objects;

import static org.apache.shenyu.plugin.api.ShenyuPlugin.LOG;

/**
 * ApplicationConfigCache.
 */
public class ApplicationConfigCache {

    private static final Map<String, CacheUpstream> UPSTREAM_CACHE_MAP = Maps.newConcurrentMap();

    private static final Map<String, ICache> CONFIG_CACHE_MAP = Maps.newConcurrentMap();

    public ICache init(final String selectorId, final CacheUpstream cacheUpstream) {
        String config = GsonUtils.getInstance().toJson(cacheUpstream);
        final ICacheBuilder cacheBuilder = ExtensionLoader.getExtensionLoader(ICacheBuilder.class)
                .getJoin(cacheUpstream.getCacheType());
        ICache cache = cacheBuilder.builderCache(config);
        CONFIG_CACHE_MAP.put(selectorId, cache);
        UPSTREAM_CACHE_MAP.put(selectorId, cacheUpstream);
        return cache;
    }

    /**
     * Get ICache .
     *
     * @param path the path
     * @return the ICache
     */
    public ICache get(final String path) {
        return CONFIG_CACHE_MAP.get(path);
    }

    /**
     * Get CacheUpstream .
     *
     * @param path the path
     * @return the cacheUpstream
     */
    public CacheUpstream getUpstream(final String path) {
        return UPSTREAM_CACHE_MAP.get(path);
    }

    /**
     * Remove ICache .
     *
     * @param path the path
     */
    public void invalidateCache(final String path) {
        ICache cache = CONFIG_CACHE_MAP.get(path);
        if (!Objects.isNull(cache)) {
            cache.close();
            CONFIG_CACHE_MAP.remove(path);
            UPSTREAM_CACHE_MAP.remove(path);
        }
    }

    /**
     * Invalidate all.
     */
    public void invalidateAll() {
        if (CONFIG_CACHE_MAP.isEmpty()) {
            return;
        }

        CONFIG_CACHE_MAP.forEach((key, cache) -> {
            try {
                if (!Objects.isNull(cache)) {
                    cache.close();
                }
            } catch (Exception e) {
                LOG.error("Failed to close upstream {}", key, e);
            }
        });

        UPSTREAM_CACHE_MAP.clear();
        CONFIG_CACHE_MAP.clear();
    }

    /**
     * Gets instance.
     *
     * @return the instance
     */
    public static ApplicationConfigCache getInstance() {
        return ApplicationConfigCacheInstance.INSTANCE;
    }

    /**
     * The type Application config cache instance.
     */
    static final class ApplicationConfigCacheInstance {
        /**
         * The Instance.
         */
        static final ApplicationConfigCache INSTANCE = new ApplicationConfigCache();

        private ApplicationConfigCacheInstance() {

        }
    }
}
