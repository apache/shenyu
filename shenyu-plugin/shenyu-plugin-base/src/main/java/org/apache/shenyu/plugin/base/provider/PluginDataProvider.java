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

package org.apache.shenyu.plugin.base.provider;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;

import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 * A concrete implementation of the data provider pattern for retrieving plugin data
 * in the ShenYu gateway system.
 */
public class PluginDataProvider implements DataProvider<PluginData> {

    /**
     * Retrieves plugin data from the base data cache for the specified plugin name.
     *
     * @param pluginName the name of the plugin to retrieve. This parameter should not be null,
     *                   though the current implementation may return an empty list rather than
     *                   throwing an exception for null values.
     * @return an immutable list containing a single {@link PluginData} object if the plugin
     *         is found, otherwise an empty immutable list. The return value will never be null.
     */
    @Override
    public List<PluginData> getData(final String pluginName) {
        PluginData data = BaseDataCache.getInstance().obtainPluginData(pluginName);
        return Objects.nonNull(data) ? Collections.singletonList(data) : Collections.emptyList();
    }
}