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
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.ConfigData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

/**
 * The type app auth data refresh.
 */
public class AppAuthDataRefresh extends AbstractDataRefresh<AppAuthData> {

    /**
     * logger.
     */
    private static final Logger LOG = LoggerFactory.getLogger(AppAuthDataRefresh.class);

    private final List<AuthDataSubscriber> authDataSubscribers;

    public AppAuthDataRefresh(final List<AuthDataSubscriber> authDataSubscribers) {
        this.authDataSubscribers = authDataSubscribers;
    }

    @Override
    protected JsonObject convert(final JsonObject data) {
        return data.getAsJsonObject(ConfigGroupEnum.APP_AUTH.name());
    }

    @Override
    protected ConfigData<AppAuthData> fromJson(final JsonObject data) {
        return GsonUtils.getGson().fromJson(data, new TypeToken<ConfigData<AppAuthData>>() {
        }.getType());
    }

    @Override
    protected boolean updateCacheIfNeed(final ConfigData<AppAuthData> result) {
        return updateCacheIfNeed(result, ConfigGroupEnum.APP_AUTH);
    }

    @Override
    public ConfigData<?> cacheConfigData() {
        return GROUP_CACHE.get(ConfigGroupEnum.APP_AUTH);
    }

    @Override
    protected void refresh(final List<AppAuthData> data) {
        if (CollectionUtils.isEmpty(data)) {
            LOG.info("clear all appAuth data cache");
            authDataSubscribers.forEach(AuthDataSubscriber::refresh);
        } else {
            data.forEach(authData -> authDataSubscribers.forEach(subscriber -> subscriber.onSubscribe(authData)));
        }
    }
}
