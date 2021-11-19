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

package org.apache.shenyu.plugin.global.cache;

import com.google.common.collect.Maps;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.utils.PathMatchUtils;

import java.util.Optional;
import java.util.concurrent.ConcurrentMap;

/**
 * The type Meta data cache.
 */
public final class MetaDataCache {
    
    private static final MetaDataCache INSTANCE = new MetaDataCache();
    
    /**
     * path -> MetaData.
     */
    private static final ConcurrentMap<String, MetaData> META_DATA_MAP = Maps.newConcurrentMap();
    
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
    }
    
    /**
     * Remove auth data.
     *
     * @param data the data
     */
    public void remove(final MetaData data) {
        META_DATA_MAP.remove(data.getPath());
    }
    
    /**
     * Obtain auth data meta data.
     *
     * @param path the path
     * @return the meta data
     */
    public MetaData obtain(final String path) {
        return Optional.ofNullable(META_DATA_MAP.get(path))
                .orElseGet(() -> META_DATA_MAP.get(META_DATA_MAP.keySet()
                        .stream()
                        .filter(k -> PathMatchUtils.match(k, path))
                        .findFirst()
                        .orElse("")));
    }
}
