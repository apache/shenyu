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

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import org.apache.commons.collections4.map.LRUMap;
import org.apache.shenyu.common.cache.MemorySafeLRUMap;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;

import java.util.*;
import java.util.concurrent.ConcurrentMap;


/**
 * The match data cache.
 */
public final class MatchDataCache {

    private static final MatchDataCache INSTANCE = new MatchDataCache();

    /**
     * pluginName -> Map -> path -> list<SelectorData>.
     */
    private static final ConcurrentMap<String, MemorySafeLRUMap<String, List<SelectorData>>> SELECTOR_DATA_MAP = Maps.newConcurrentMap();

    /**
     * pluginName -> Map -> path -> list<RuleData>.
     */
    private static final ConcurrentMap<String, MemorySafeLRUMap<String, List<RuleData>>> RULE_DATA_MAP = Maps.newConcurrentMap();

    /**
     * selectorId -> path.
     */
    private static final MemorySafeLRUMap<String, String> SELECTOR_MAPPING = new MemorySafeLRUMap<>(Constants.THE_256_MB, 1 << 16);

    /**
     * ruleId -> path.
     */
    private static final MemorySafeLRUMap<String, String> RULE_MAPPING = new MemorySafeLRUMap<>(Constants.THE_256_MB, 1 << 16);

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
     * Cache selector data.
     *
     * @param path the path
     * @param data the selector data
     */
    public void cacheSelectorData(final String path, final SelectorData data) {
        if (!SELECTOR_MAPPING.containsKey(data.getId())) {
            LRUMap<String, List<SelectorData>> selectDataMap = SELECTOR_DATA_MAP.computeIfAbsent(data.getPluginName(),
                    map -> new MemorySafeLRUMap<>(Constants.THE_256_MB, 1 << 16));
            List<SelectorData> selectorDataList = selectDataMap.computeIfAbsent(path, list -> new ArrayList<>());
            selectorDataList.add(data);
            SELECTOR_MAPPING.putIfAbsent(data.getId(), path);
        }
    }

    /**
     * Obtain selector data.
     *
     * @param path the path
     * @return the selector data
     */
    public Collection<SelectorData> obtainSelectorData(final String pluginName, final String path) {
        MemorySafeLRUMap<String, List<SelectorData>> selectDataMap = SELECTOR_DATA_MAP.get(pluginName);
        if (Objects.nonNull(selectDataMap)) {
            return Optional.ofNullable(selectDataMap.get(path)).orElse(Lists.newArrayList());
        }
        return Lists.newArrayList();
    }

}
