/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.soul.cache.local.impl;

import com.google.common.collect.Maps;
import org.dromara.soul.cache.api.data.PluginData;
import org.dromara.soul.cache.api.data.SelectorData;
import org.dromara.soul.cache.api.service.CacheService;

import java.util.List;
import java.util.concurrent.ConcurrentMap;

/**
 * @author xiaoyu(Myth)
 */
public abstract class AbstractLocalService implements CacheService {

    /**
     * pluginName -> PluginData.
     */
    static final ConcurrentMap<String, PluginData> PLUGIN_MAP = Maps.newConcurrentMap();

    /**
     * pluginName -> SelectorData.
     */
    static final ConcurrentMap<String, List<SelectorData>> SELECTOR_MAP = Maps.newConcurrentMap();

}
