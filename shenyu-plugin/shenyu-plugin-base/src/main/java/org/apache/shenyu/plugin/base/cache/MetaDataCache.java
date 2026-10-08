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

package org.apache.shenyu.plugin.base.cache;

import com.google.common.collect.Maps;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.cache.WindowTinyLFUMap;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.plugin.base.utils.PathMatchUtils;
import org.springframework.util.StringUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ConcurrentMap;

/**
 * The type Meta data cache.
 */
public final class MetaDataCache {

    private static final String DIVIDE_CACHE_KEY = "";

    private static final MetaData NULL = new MetaData();

    private static final MetaDataCache INSTANCE = new MetaDataCache();

    /**
     * id -> MetaData.
     */
    private static final ConcurrentMap<String, MetaData> META_DATA_MAP = Maps.newConcurrentMap();

    private static final WindowTinyLFUMap<String, MetaData> CACHE =
            new WindowTinyLFUMap<>(Constants.CACHE_MAX_COUNT, Constants.CACHE_MAX_COUNT, Boolean.FALSE);

    private volatile Map<String, List<MetaData>> pathIndex = Collections.emptyMap();

    // Index state is read and written only while holding this cache's monitor.
    private boolean indexDirty;

    private volatile boolean negativeCacheDirty;

    private MetaDataCache() {
    }

    /**
     * Gets instance.
     *
     * @return the instance
     */
    public static MetaDataCache getInstance() {
        return INSTANCE;
    }

    /**
     * Cache auth data.
     *
     * @param data the data
     */
    public synchronized void cache(final MetaData data) {
        // clean old path data
        Optional.ofNullable(META_DATA_MAP.get(data.getId())).ifPresent(oldMetaData -> {
            // the update is also need to clean, but there is
            // no way to distinguish between crate and update,
            // so it is always clean
            clean(oldMetaData.getPath());
        });
        META_DATA_MAP.put(data.getId(), data);
        indexDirty = true;
        negativeCacheDirty = true;
        final String path = data.getPath();
        clean(path);
        if (!path.contains("*")) {
            // only in this condition, we need to init cache
            initCache(path, data, path);
        }
    }

    /**
     * Remove auth data.
     *
     * @param data the data
     */
    public synchronized void remove(final MetaData data) {
        META_DATA_MAP.remove(data.getId());
        indexDirty = true;
        negativeCacheDirty = true;
        clean(data.getPath());
    }

    private void rebuildPathIndex() {
        Map<String, List<MetaData>> index = new HashMap<>();
        META_DATA_MAP.values().forEach(data -> index.computeIfAbsent(firstSegment(data.getPath()), key -> new ArrayList<>()).add(data));
        index.replaceAll((key, value) -> List.copyOf(value));
        pathIndex = Map.copyOf(index);
    }

    private String firstSegment(final String path) {
        // AntPathMatcher ignores repeated separators, so use the same tokenization.
        String[] segments = StringUtils.tokenizeToStringArray(path, "/", false, true);
        if (segments.length == 0) {
            return "";
        }
        String segment = segments[0];
        if (segment.indexOf('*') >= 0 || segment.indexOf('?') >= 0 || segment.indexOf('{') >= 0) {
            return "";
        }
        return (path.startsWith("/") ? "/" : "") + segment;
    }

    private MetaData matchIndexedPath(final String path) {
        // Called under the same monitor as mutations and cold-cache publication.
        if (indexDirty) {
            rebuildPathIndex();
            indexDirty = false;
        }
        Map<String, List<MetaData>> index = pathIndex;
        String segment = firstSegment(path);
        MetaData match = matchCandidates(index.getOrDefault(segment, Collections.emptyList()), path);
        MetaData fallback = segment.isEmpty() ? null : matchCandidates(index.getOrDefault("", Collections.emptyList()), path);
        if (Objects.isNull(match)) {
            return fallback;
        }
        return Objects.isNull(fallback) || PathMatchUtils.compare(match.getPath(), fallback.getPath(), path) <= 0 ? match : fallback;
    }

    private MetaData matchCandidates(final List<MetaData> candidates, final String path) {
        return candidates.stream()
                .filter(data -> data.getEnabled() && PathMatchUtils.match(data.getPath(), path))
                .min((left, right) -> PathMatchUtils.compare(left.getPath(), right.getPath(), path))
                .orElse(null);
    }

    private void clean(final String key) {
        if (key.contains("*")) {
            CACHE.clear();
            return;
        }
        CACHE.entrySet().removeIf(entry -> Objects.equals(key, NULL.equals(entry.getValue())
                ? DIVIDE_CACHE_KEY : entry.getValue().getPath()));
    }

    /**
     * clean cache for divide plugin.
     */
    public synchronized void clean() {
        clean(DIVIDE_CACHE_KEY);
        negativeCacheDirty = true;
        cleanNegativeCache();
    }

    private synchronized void cleanNegativeCache() {
        if (negativeCacheDirty) {
            // URI variables (/{tenant}/new) and normalized separators can turn earlier misses into hits.
            // Sweep the bounded result cache once per mutation burst, never an unbounded reverse index.
            CACHE.entrySet().removeIf(entry -> NULL.equals(entry.getValue()));
            negativeCacheDirty = false;
        }
    }

    /**
     * Obtain auth data meta data.
     *
     * @param path the path
     * @return the meta data
     */
    public MetaData obtain(final String path) {
        if (negativeCacheDirty) {
            cleanNegativeCache();
        }
        MetaData cached = CACHE.get(path);
        final MetaData metaData = Objects.nonNull(cached) ? cached : loadPath(path);
        return NULL.equals(metaData) ? null : metaData;
    }

    private synchronized MetaData loadPath(final String path) {
        MetaData cached = CACHE.get(path);
        if (Objects.nonNull(cached)) {
            return cached;
        }
        MetaData value = matchIndexedPath(path);
        String metaPath = Objects.isNull(value) ? DIVIDE_CACHE_KEY : value.getPath();
        initCache(path, value, metaPath);
        return value;
    }

    /**
     * cacheMap.
     *
     * @param path     the path
     * @param value    the MetaData
     * @param metaPath the metaPath
     */
    public synchronized void initCache(final String path, final MetaData value, final String metaPath) {
        // The extreme case will lead to OOM, that's why use LRU
        CACHE.put(path, Optional.ofNullable(value).orElse(NULL));
    }
    
    /**
     * get metaDataMap.
     *
     * @return metaDataMap
     */
    public Map<String, MetaData> getMetaDataMap() {
        return META_DATA_MAP;
    }
    
    /**
     * get metadata cache.
     *
     * @return cache map
     */
    public Map<String, MetaData> getMetaDataCache() {
        return CACHE;
    }
}
