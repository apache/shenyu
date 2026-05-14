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

package org.apache.shenyu.plugin.ai.proxy.enhanced.cache;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.ai.openai.api.OpenAiApi;

import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Supplier;

/**
 * This is OpenAiApi cache.
 * Caches OpenAiApi instances by config key to avoid recreating on every request.
 */
public final class OpenAiApiCache {

    private static final Logger LOG = LoggerFactory.getLogger(OpenAiApiCache.class);

    private static final OpenAiApiCache INSTANCE = new OpenAiApiCache();

    private static final int MAX_CACHE_SIZE = getCacheSize();

    private final Map<String, OpenAiApi> openAiApiMap = new ConcurrentHashMap<>();

    private final AtomicBoolean evictionInProgress = new AtomicBoolean(false);

    /**
     * Instantiates a new OpenAiApi cache.
     */
    public OpenAiApiCache() {
    }

    /**
     * Gets instance.
     *
     * @return singleton
     */
    public static OpenAiApiCache getInstance() {
        return INSTANCE;
    }

    private static int getCacheSize() {
        String value = System.getProperty("shenyu.plugin.ai.proxy.enhanced.cache.maxSize",
                System.getenv("SHENYU_PLUGIN_AI_PROXY_ENHANCED_CACHE_MAXSIZE"));
        if (Objects.nonNull(value)) {
            try {
                return Integer.parseInt(value);
            } catch (NumberFormatException e) {
                LOG.warn("[OpenAiApiCache] Invalid cache size '{}', using default 500.", value);
            }
        }
        return 500;
    }

    /**
     * Gets OpenAiApi or compute if absent.
     *
     * @param key the cache key (typically selectorId|configHash)
     * @param openAiApiSupplier the supplier to create OpenAiApi if absent
     * @return the cached or newly created OpenAiApi
     */
    public OpenAiApi computeIfAbsent(final String key, final Supplier<OpenAiApi> openAiApiSupplier) {
        final int currentSize = openAiApiMap.size();
        if (currentSize > MAX_CACHE_SIZE) {
            if (evictionInProgress.compareAndSet(false, true)) {
                try {
                    synchronized (openAiApiMap) {
                        if (openAiApiMap.size() > MAX_CACHE_SIZE) {
                            evictOldestEntries();
                        }
                    }
                } finally {
                    evictionInProgress.set(false);
                }
            }
        }
        return openAiApiMap.computeIfAbsent(key, k -> openAiApiSupplier.get());
    }

    /**
     * Evict oldest entries when cache size exceeds limit.
     * Removes approximately 25% of entries to avoid thundering herd problem.
     */
    private void evictOldestEntries() {
        final int currentSize = openAiApiMap.size();
        if (currentSize <= MAX_CACHE_SIZE) {
            return;
        }

        final int evictCount = Math.max(10, currentSize / 4);
        LOG.warn("[OpenAiApiCache] Cache size {} exceeded limit {}, evicting {} oldest entries",
                currentSize, MAX_CACHE_SIZE, evictCount);

        int removed = 0;
        for (final String key : openAiApiMap.keySet()) {
            if (removed >= evictCount) {
                break;
            }
            openAiApiMap.remove(key);
            removed++;
        }

        LOG.info("[OpenAiApiCache] Evicted {} entries, cache size now: {}", removed, openAiApiMap.size());
    }

    /**
     * Removes all cached OpenAiApi instances associated with a selector ID
     * (by prefix matching "selectorId|").
     *
     * @param selectorId the selector id
     */
    public void remove(final String selectorId) {
        if (Objects.isNull(selectorId)) {
            return;
        }
        final String prefix = selectorId + "|";
        openAiApiMap.keySet().removeIf(k -> k.equals(selectorId) || k.startsWith(prefix));
        LOG.info("[OpenAiApiCache] invalidate selectorId={} (by prefix)", selectorId);
    }

    /**
     * Clear all cached OpenAiApi instances.
     */
    public void clearAll() {
        openAiApiMap.clear();
        LOG.info("[OpenAiApiCache] cleared all cached OpenAiApi instances");
    }
}
