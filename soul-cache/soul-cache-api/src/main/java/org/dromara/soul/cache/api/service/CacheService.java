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

package org.dromara.soul.cache.api.service;


import org.dromara.soul.cache.api.data.AppAuthData;
import org.dromara.soul.cache.api.data.PluginData;
import org.dromara.soul.cache.api.data.SelectorData;
import org.dromara.soul.common.extension.SPI;

import java.util.List;

/**
 * The interface Cache service.
 *
 * @author xiaoyu
 */
@SPI("local")
public interface CacheService {

    /**
     * Find plugin by name plugin data.
     *
     * @param pluginName the plugin name
     * @return the plugin data
     */
    PluginData findPluginByName(String pluginName);

    /**
     * Find selector by plugin name list.
     *
     * @param pluginName the plugin name
     * @return the list
     */
    List<SelectorData> findSelectorByPluginName(String pluginName);

    /**
     * Find by access key app auth data.
     *
     * @param accessKey the access key
     * @return the app auth data
     */
    AppAuthData findByAccessKey(String accessKey);
}
