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
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.map.LRUMap;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.cache.MemorySafeLRUMap;
import org.apache.shenyu.common.dto.SelectorData;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ConcurrentSkipListSet;


/**
 * The match data cache.
 */
public final class MatchDataCache {

    private static final MatchDataCache INSTANCE = new MatchDataCache();

    /**
     * pluginName -> LRUMap.
     */
    private static final ConcurrentMap<String, MemorySafeLRUMap<String, List<SelectorData>>> SELECTOR_DATA_MAP = Maps.newConcurrentMap();

    /**
     * pluginName ->the empty collection path.
     */
    private static final ConcurrentMap<String, Set<String>> EMPTY_COLLECTION_PATH_MAP = Maps.newConcurrentMap();

    /**
     * selectorId -> path.
     */
    private static final ConcurrentMap<String, Set<String>> SELECTOR_MAPPING = Maps.newConcurrentMap();


    private MatchDataCache() {
    }

    /**
     * Gets instance.
     *
     * @return the instance
     */
    public static MatchDataCache getInstance() {
        return INSTANCE;
    }

    /**
     * Remove selector data.
     *
     * @param selectorData the selector data
     */
    public void removeSelectorData(final SelectorData selectorData) {
        final String pluginName = selectorData.getPluginName();
        final LRUMap<String, List<SelectorData>> lruMap = SELECTOR_DATA_MAP.get(pluginName);
        final String selectorDataId = selectorData.getId();
        if (Objects.nonNull(lruMap)) {
            // If the selector has been deleted or changed, need to delete the cache.
            final Set<String> paths = SELECTOR_MAPPING.get(selectorDataId);
            if (CollectionUtils.isNotEmpty(paths)) {
                SELECTOR_MAPPING.remove(selectorDataId);
                paths.forEach(path -> {
                    final List<SelectorData> selectorDataList = lruMap.get(path);
                    Optional.ofNullable(selectorDataList).ifPresent(list -> list.removeIf(e -> e.getId().equals(selectorDataId)));
                    if (CollectionUtils.isEmpty(selectorDataList)) {
                        lruMap.remove(path);
                    }
                });
            }
            // Delete the empty collection if any of the selectors in the plugin have been changed
            final Set<String> emptyCollectionPaths = EMPTY_COLLECTION_PATH_MAP.get(pluginName);
            if (CollectionUtils.isNotEmpty(emptyCollectionPaths)) {
                emptyCollectionPaths.forEach(lruMap::remove);
            }
        }
    }

    /**
     * Remove selector data by plugin name.
     *
     * @param pluginName the plugin name
     */
    public void removeSelectorDataByPluginName(final String pluginName) {
        SELECTOR_DATA_MAP.remove(pluginName);
        EMPTY_COLLECTION_PATH_MAP.remove(pluginName);
        SELECTOR_MAPPING.clear();
    }


    /**
     * Clean selector data.
     */
    public void cleanSelectorData() {
        SELECTOR_DATA_MAP.clear();
        SELECTOR_MAPPING.clear();
        EMPTY_COLLECTION_PATH_MAP.clear();
    }

    /**
     * Cache selector data.
     *
     * @param path         the path
     * @param selectorData the selector data
     * @param maxMemory    the max memory
     */
    public void cacheSelectorData(final String path, final SelectorData selectorData, final Integer maxMemory) {
        final LRUMap<String, List<SelectorData>> lruMap = SELECTOR_DATA_MAP.computeIfAbsent(selectorData.getPluginName(),
            map -> new MemorySafeLRUMap<>(maxMemory, 1 << 16));
        List<SelectorData> selectorDataList = lruMap.computeIfAbsent(path, list -> Collections.synchronizedList(new ArrayList<>()));
        if (StringUtils.isNoneBlank(selectorData.getId())) {
            selectorDataList.add(selectorData);
            SELECTOR_MAPPING.computeIfAbsent(selectorData.getId(), set -> new ConcurrentSkipListSet<>()).add(path);
        } else {
            EMPTY_COLLECTION_PATH_MAP.computeIfAbsent(selectorData.getPluginName(), set -> new ConcurrentSkipListSet<>()).add(path);
        }
    }

    /**
     * Obtain selector data.
     *
     * @param pluginName the pluginName
     * @param path       the path
     * @return the selector data
     */
    public List<SelectorData> obtainSelectorData(final String pluginName, final String path) {
        List<SelectorData> selectorDataList = null;
        final LRUMap<String, List<SelectorData>> lruMap = SELECTOR_DATA_MAP.get(pluginName);
        if (Objects.nonNull(lruMap)) {
            selectorDataList = lruMap.get(path);
        }
        return selectorDataList;
    }
}
