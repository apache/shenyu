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

import java.util.HashMap;
import java.util.Objects;
import java.util.concurrent.ConcurrentMap;

import static org.apache.shenyu.plugin.api.ShenyuPlugin.LOG;

/**
 * ApplicationConfigCache.
 */
public class ApplicationConfigCache {

    private static final ConcurrentMap<String, CacheEntry> CONFIG_CACHE_MAP = Maps.newConcurrentMap();

    /**
     * Initialize a selector backend once, publishing its configuration with it.
     *
     * @param selectorId the selector id
     * @param cacheUpstream the backend configuration
     * @return the existing or newly initialized backend
     */
    public ICache init(final String selectorId, final CacheUpstream cacheUpstream) {
        return CONFIG_CACHE_MAP.computeIfAbsent(selectorId, key -> {
            String config = GsonUtils.getInstance().toJson(cacheUpstream);
            final ICacheBuilder cacheBuilder = ExtensionLoader.getExtensionLoader(ICacheBuilder.class)
                    .getJoin(cacheUpstream.getCacheType());
            return new CacheEntry(Objects.requireNonNull(cacheBuilder.builderCache(config)), cacheUpstream);
        }).cache;
    }

    /**
     * Get ICache .
     *
     * @param path the path
     * @return the ICache
     */
    public ICache get(final String path) {
        CacheEntry entry = CONFIG_CACHE_MAP.get(path);
        return Objects.isNull(entry) ? null : entry.cache;
    }

    /**
     * Get CacheUpstream .
     *
     * @param path the path
     * @return the cacheUpstream
     */
    public CacheUpstream getUpstream(final String path) {
        CacheEntry entry = CONFIG_CACHE_MAP.get(path);
        return Objects.isNull(entry) ? null : entry.upstream;
    }

    /**
     * Remove ICache .
     *
     * @param path the path
     */
    public void invalidateCache(final String path) {
        // Detach before closing so a concurrent initialization cannot be removed by this invalidation.
        CacheEntry entry = CONFIG_CACHE_MAP.remove(path);
        if (Objects.nonNull(entry)) {
            entry.cache.close();
        }
    }

    /**
     * Invalidate all.
     */
    public void invalidateAll() {
        new HashMap<>(CONFIG_CACHE_MAP).forEach((key, entry) -> {
            // Claim only the captured entry; a replacement belongs to a later lifecycle.
            if (CONFIG_CACHE_MAP.remove(key, entry)) {
                try {
                    entry.cache.close();
                } catch (Exception e) {
                    LOG.error("Failed to close upstream {}", key, e);
                }
            }
        });
    }

    /**
     * Gets instance.
     *
     * @return the instance
     */
    public static ApplicationConfigCache getInstance() {
        return ApplicationConfigCacheInstance.INSTANCE;
    }

    private static final class CacheEntry {

        private final ICache cache;

        private final CacheUpstream upstream;

        private CacheEntry(final ICache cache, final CacheUpstream upstream) {
            this.cache = cache;
            this.upstream = upstream;
        }
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
