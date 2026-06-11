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
import java.util.List;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.common.dto.ConfigData;
import org.apache.shenyu.common.dto.ProxyApiKeyData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.sync.data.api.AiProxyApiKeyDataSubscriber;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * HTTP data refresh for AI Proxy ApiKey mapping.
 */
public class AiProxyApiKeyDataRefresh extends AbstractDataRefresh<ProxyApiKeyData> {

    private static final Logger LOG = LoggerFactory.getLogger(AiProxyApiKeyDataRefresh.class);

    private final List<AiProxyApiKeyDataSubscriber> subscribers;

    public AiProxyApiKeyDataRefresh(final List<AiProxyApiKeyDataSubscriber> subscribers) {
        this.subscribers = subscribers;
    }

    @Override
    protected JsonObject convert(final JsonObject data) {
        return data.getAsJsonObject(ConfigGroupEnum.AI_PROXY_API_KEY.name());
    }

    @Override
    protected ConfigData<ProxyApiKeyData> fromJson(final JsonObject data) {
        return GsonUtils.getGson().fromJson(data, new TypeToken<ConfigData<ProxyApiKeyData>>() { }.getType());
    }

    @Override
    protected boolean updateCacheIfNeed(final ConfigData<ProxyApiKeyData> result) {
        return updateCacheIfNeed(result, ConfigGroupEnum.AI_PROXY_API_KEY);
    }

    @Override
    public ConfigData<?> cacheConfigData() {
        return GROUP_CACHE.get(ConfigGroupEnum.AI_PROXY_API_KEY);
    }

    @Override
    protected void refresh(final List<ProxyApiKeyData> data) {
        if (CollectionUtils.isEmpty(subscribers)) {
            LOG.info("[AiProxySync][HTTP] no subscribers, skip refresh");
            return;
        }
        // Clear and then update to ensure full consistency
        subscribers.forEach(AiProxyApiKeyDataSubscriber::refresh);
        if (CollectionUtils.isEmpty(data)) {
            LOG.info("[AiProxySync][HTTP] received empty list, only refresh called");
            return;
        }
        for (ProxyApiKeyData item : data) {
            for (AiProxyApiKeyDataSubscriber s : subscribers) {
                s.onSubscribe(item);
            }
        }
    }
} 