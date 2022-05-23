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
     * path -> MetaData.
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
        META_DATA_MAP.put(data.getPath(), data);
        // the update is also need to clean, but there is
        // no way to distinguish between crate and update,
        // so it is always clean
        clean(data.getPath());
    }

    /**
     * Remove auth data.
     *
     * @param data the data
     */
    public void remove(final MetaData data) {
        META_DATA_MAP.remove(data.getPath());
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
        final MetaData metaData = Optional.ofNullable(META_DATA_MAP.get(path))
                .orElseGet(() -> {
                    final MetaData exist = CACHE.get(path);
                    if (Objects.nonNull(exist)) {
                        return exist;
                    }
                    final String key = META_DATA_MAP.keySet()
                            .stream()
                            .filter(k -> PathMatchUtils.match(k, path))
                            .findFirst()
                            .orElse(DIVIDE_CACHE_KEY);
                    final MetaData value = META_DATA_MAP.get(key);
                    // The extreme case will lead to OOM, that's why use LRU
                    CACHE.put(path, Objects.isNull(value) ? NULL : value);

                    Set<String> paths = MAPPING.get(key);
                    if (Objects.isNull(paths)) {
                        MAPPING.putIfAbsent(key, new ConcurrentSkipListSet<>());
                        paths = MAPPING.get(key);
                    }
                    paths.add(path);

                    return value;
                });
        return NULL.equals(metaData) ? null : metaData;
    }
}
