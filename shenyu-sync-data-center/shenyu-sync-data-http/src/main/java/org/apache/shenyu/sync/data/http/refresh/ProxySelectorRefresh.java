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
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.sync.data.api.ProxySelectorDataSubscriber;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class ProxySelectorRefresh extends AbstractDataRefresh<ProxySelectorData> {

    private static final Logger LOG = LoggerFactory.getLogger(ProxySelectorRefresh.class);

    private final List<ProxySelectorDataSubscriber> proxySelectorDataSubscribers;

    private final Map<SelectorKey, ProxySelectorData> previousSnapshot = new HashMap<>();

    public ProxySelectorRefresh(final List<ProxySelectorDataSubscriber> proxySelectorDataSubscribers) {
        this.proxySelectorDataSubscribers = proxySelectorDataSubscribers;
    }

    @Override
    protected JsonObject convert(final JsonObject data) {
        return data.getAsJsonObject(ConfigGroupEnum.PROXY_SELECTOR.name());
    }

    @Override
    protected ConfigData<ProxySelectorData> fromJson(final JsonObject data) {
        return GsonUtils.getGson().fromJson(data, new TypeToken<ConfigData<ProxySelectorData>>() {
        }.getType());
    }

    @Override
    public synchronized Boolean refresh(final JsonObject data) {
        // Version selection and the subscriber callbacks must complete in the same order.
        return super.refresh(data);
    }

    @Override
    protected void refresh(final List<ProxySelectorData> data) {
        if (CollectionUtils.isEmpty(data)) {
            LOG.info("clear all ProxySelector data cache");
            proxySelectorDataSubscribers.forEach(ProxySelectorDataSubscriber::refresh);
            previousSnapshot.clear();
            return;
        }
        Map<SelectorKey, ProxySelectorData> currentSnapshot = new HashMap<>();
        for (ProxySelectorData selector : data) {
            if (Objects.nonNull(selector.getId()) && Objects.nonNull(selector.getPluginName())) {
                currentSnapshot.put(new SelectorKey(selector.getPluginName(), selector.getId()), selector);
            }
        }
        previousSnapshot.forEach((key, selector) -> {
            if (!currentSnapshot.containsKey(key)) {
                proxySelectorDataSubscribers.forEach(subscriber -> subscriber.unSubscribe(selector));
            }
        });
        data.forEach(d -> proxySelectorDataSubscribers.forEach(pss -> pss.onSubscribe(d)));
        previousSnapshot.clear();
        previousSnapshot.putAll(currentSnapshot);
    }

    @Override
    protected boolean updateCacheIfNeed(final ConfigData<ProxySelectorData> result) {
        return updateCacheIfNeed(result, ConfigGroupEnum.PROXY_SELECTOR);
    }

    @Override
    public ConfigData<?> cacheConfigData() {
        return GROUP_CACHE.get(ConfigGroupEnum.PROXY_SELECTOR);
    }

    private record SelectorKey(String pluginName, String id) {
    }
}
