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
import java.util.Set;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ConcurrentSkipListSet;

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

    private static final WindowTinyLFUMap<String, MetaData> CACHE = new WindowTinyLFUMap<>(1 << 16, Integer.MAX_VALUE, Boolean.FALSE);

    /**
     * pathPattern -> path.
     */
    private static final ConcurrentMap<String, Set<String>> MAPPING = Maps.newConcurrentMap();

    private volatile Map<String, List<MetaData>> pathIndex = Collections.emptyMap();

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
        rebuildPathIndex();
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
        rebuildPathIndex();
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
        Map<String, List<MetaData>> index = pathIndex;
        String segment = firstSegment(path);
        MetaData match = matchCandidates(index.getOrDefault(segment, Collections.emptyList()), path);
        return Objects.nonNull(match) || segment.isEmpty() ? match : matchCandidates(index.getOrDefault("", Collections.emptyList()), path);
    }

    private MetaData matchCandidates(final List<MetaData> candidates, final String path) {
        return candidates.stream()
                .filter(data -> data.getEnabled() && PathMatchUtils.match(data.getPath(), path))
                .findFirst()
                .orElse(null);
    }

    private void clean(final String key) {
        // springCloud and divide are needs to be cleaned
        Optional.ofNullable(MAPPING.get(key))
                .ifPresent(paths -> {
                    for (String path : paths) {
                        CACHE.remove(path);
                    }
                });
    }

    /**
     * clean cache for divide plugin.
     */
    public void clean() {
        clean(DIVIDE_CACHE_KEY);
    }

    /**
     * Obtain auth data meta data.
     *
     * @param path the path
     * @return the meta data
     */
    public MetaData obtain(final String path) {
        final MetaData metaData = Optional.ofNullable(CACHE.get(path))
                .orElseGet(() -> {
                    final MetaData value = matchIndexedPath(path);
                    final String metaPath = Optional.ofNullable(value)
                            .map(MetaData::getPath)
                            .orElse(DIVIDE_CACHE_KEY);
                    // init cache
                    initCache(path, value, metaPath);
                    return value;
                });
        return NULL.equals(metaData) ? null : metaData;
    }

    /**
     * cacheMap.
     *
     * @param path     the path
     * @param value    the MetaData
     * @param metaPath the metaPath
     */
    public void initCache(final String path, final MetaData value, final String metaPath) {
        // The extreme case will lead to OOM, that's why use LRU
        CACHE.put(path, Optional.ofNullable(value).orElse(NULL));
        // spring/** -> Collections 'spring/A', 'spring/B'
        Set<String> paths = MAPPING.get(metaPath);
        if (Objects.isNull(paths)) {
            MAPPING.putIfAbsent(metaPath, new ConcurrentSkipListSet<>());
            paths = MAPPING.get(metaPath);
        }
        paths.add(path);
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
