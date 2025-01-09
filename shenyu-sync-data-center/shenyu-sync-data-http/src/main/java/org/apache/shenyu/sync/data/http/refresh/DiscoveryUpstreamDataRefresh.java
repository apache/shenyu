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
import com.google.gson.reflect.TypeToken;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.common.dto.ConfigData;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamDataSubscriber;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

public class DiscoveryUpstreamDataRefresh extends AbstractDataRefresh<DiscoverySyncData> {

    private static final Logger LOG = LoggerFactory.getLogger(DiscoveryUpstreamDataRefresh.class);

    private final List<DiscoveryUpstreamDataSubscriber> discoveryUpstreamDataSubscribers;

    public DiscoveryUpstreamDataRefresh(final List<DiscoveryUpstreamDataSubscriber> discoveryUpstreamDataSubscribers) {
        this.discoveryUpstreamDataSubscribers = discoveryUpstreamDataSubscribers;
    }

    @Override
    protected JsonObject convert(final JsonObject data) {
        return data.getAsJsonObject(ConfigGroupEnum.DISCOVER_UPSTREAM.name());
    }

    @Override
    protected ConfigData<DiscoverySyncData> fromJson(final JsonObject data) {
        return GsonUtils.getGson().fromJson(data, new TypeToken<ConfigData<DiscoverySyncData>>() {
        }.getType());
    }

    @Override
    protected void refresh(final List<DiscoverySyncData> data) {
        if (CollectionUtils.isEmpty(data)) {
            LOG.info("clear all plugin data cache");
            return;
        }
        data.forEach(d -> discoveryUpstreamDataSubscribers.forEach(dus -> dus.onSubscribe(d)));
    }

    @Override
    protected boolean updateCacheIfNeed(final ConfigData<DiscoverySyncData> result) {
        return updateCacheIfNeed(result, ConfigGroupEnum.DISCOVER_UPSTREAM);
    }

    @Override
    public ConfigData<?> cacheConfigData() {
        return GROUP_CACHE.get(ConfigGroupEnum.DISCOVER_UPSTREAM);
    }

}
