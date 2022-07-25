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
import org.apache.shenyu.common.cache.MemorySafeLRUMap;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.utils.PathMatchUtils;

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

    private static final MemorySafeLRUMap<String, MetaData> CACHE = new MemorySafeLRUMap<>(Constants.THE_256_MB, 1 << 16);

    /**
     * pathPattern -> path.
     */
    private static final ConcurrentMap<String, Set<String>> MAPPING = Maps.newConcurrentMap();

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
    public void cache(final MetaData data) {
        // clean old path data
        if (META_DATA_MAP.containsKey(data.getId())) {
            // the update is also need to clean, but there is
            // no way to distinguish between crate and update,
            // so it is always clean
            clean(META_DATA_MAP.get(data.getId()).getPath());
        }
        META_DATA_MAP.put(data.getId(), data);
        clean(data.getPath());
        clean(DIVIDE_CACHE_KEY);
    }

    /**
     * Remove auth data.
     *
     * @param data the data
     */
    public void remove(final MetaData data) {
        META_DATA_MAP.remove(data.getId());
        clean(data.getPath());
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
                    final MetaData value = META_DATA_MAP.values()
                            .stream()
                            .filter(data -> PathMatchUtils.match(data.getPath(), path))
                            .findFirst()
                            .orElse(null);
                    final String metaPath = Objects.isNull(value) ? DIVIDE_CACHE_KEY : value.getPath();

                    // The extreme case will lead to OOM, that's why use LRU
                    CACHE.put(path, Objects.isNull(value) ? NULL : value);
                    // spring/** -> Collections 'spring/A', 'spring/B'
                    Set<String> paths = MAPPING.get(metaPath);
                    if (Objects.isNull(paths)) {
                        MAPPING.putIfAbsent(metaPath, new ConcurrentSkipListSet<>());
                        paths = MAPPING.get(metaPath);
                    }
                    paths.add(path);

                    return value;
                });
        return NULL.equals(metaData) ? null : metaData;
    }
}
