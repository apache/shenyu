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

package org.apache.shenyu.sync.data.http.refresh;

import com.google.gson.JsonObject;
import org.apache.shenyu.common.dto.ConfigData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;

import java.util.EnumMap;
import java.util.List;
import java.util.stream.Collectors;

/**
 * The type Data refresh factory.
 */
public final class DataRefreshFactory {

    private static final EnumMap<ConfigGroupEnum, DataRefresh> ENUM_MAP = new EnumMap<>(ConfigGroupEnum.class);

    /**
     * Instantiates a new Data refresh factory.
     *
     * @param pluginDataSubscriber the plugin data subscriber
     * @param metaDataSubscribers  the meta data subscribers
     * @param authDataSubscribers  the auth data subscribers
     */
    public DataRefreshFactory(final PluginDataSubscriber pluginDataSubscriber,
                              final List<MetaDataSubscriber> metaDataSubscribers,
                              final List<AuthDataSubscriber> authDataSubscribers) {
        ENUM_MAP.put(ConfigGroupEnum.PLUGIN, new PluginDataRefresh(pluginDataSubscriber));
        ENUM_MAP.put(ConfigGroupEnum.SELECTOR, new SelectorDataRefresh(pluginDataSubscriber));
        ENUM_MAP.put(ConfigGroupEnum.RULE, new RuleDataRefresh(pluginDataSubscriber));
        ENUM_MAP.put(ConfigGroupEnum.APP_AUTH, new AppAuthDataRefresh(authDataSubscribers));
        ENUM_MAP.put(ConfigGroupEnum.META_DATA, new MetaDataRefresh(metaDataSubscribers));
    }

    /**
     * Executor.
     *
     * @param data the data
     * @return the boolean
     */
    public boolean executor(final JsonObject data) {
        List<Boolean> result = ENUM_MAP.values().stream()
                .map(dataRefresh -> dataRefresh.refresh(data))
                .collect(Collectors.toList());
        return result.stream().anyMatch(Boolean.TRUE::equals);
    }

    /**
     * Cache config data.
     *
     * @param group the group
     * @return the config data
     */
    public ConfigData<?> cacheConfigData(final ConfigGroupEnum group) {
        return ENUM_MAP.get(group).cacheConfigData();
    }
}
