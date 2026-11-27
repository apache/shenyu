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
import org.apache.commons.collections4.ListUtils;
import org.apache.shenyu.common.dto.ConfigData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * The type meta data refresh.
 */
public class MetaDataRefresh extends AbstractDataRefresh<MetaData> {
    /**
     * logger.
     */
    private static final Logger LOG = LoggerFactory.getLogger(MetaDataRefresh.class);

    private final List<MetaDataSubscriber> metaDataSubscribers;

    // A subscriber can install a row and then throw. Keep it until all subscribers have removed it.
    private final Map<String, MetaData> subscribedMetadata = new LinkedHashMap<>();

    // Long-polling threads read this without waiting for a subscriber callback.
    private volatile ConfigData<MetaData> appliedConfig;

    private boolean incomplete;

    public MetaDataRefresh(final List<MetaDataSubscriber> metaDataSubscribers) {
        this.metaDataSubscribers = metaDataSubscribers;
    }

    @Override
    protected JsonObject convert(final JsonObject data) {
        return data.getAsJsonObject(ConfigGroupEnum.META_DATA.name());
    }

    @Override
    protected ConfigData<MetaData> fromJson(final JsonObject data) {
        return GsonUtils.getGson().fromJson(data, new TypeToken<ConfigData<MetaData>>() {
        }.getType());
    }

    @Override
    protected synchronized boolean updateCacheIfNeed(final ConfigData<MetaData> result) {
        if (Objects.nonNull(appliedConfig) && appliedConfig.getLastModifyTime() >= result.getLastModifyTime()) {
            return false;
        }
        if (!incomplete && Objects.nonNull(appliedConfig) && Objects.equals(appliedConfig.getMd5(), result.getMd5())) {
            return false;
        }
        if (!reconcile(result.getData())) {
            return false;
        }
        appliedConfig = result;
        return true;
    }

    @Override
    public ConfigData<?> cacheConfigData() {
        return appliedConfig;
    }

    @Override
    public Boolean refresh(final JsonObject data) {
        JsonObject metadata = convert(data);
        return Objects.nonNull(metadata) && updateCacheIfNeed(fromJson(metadata));
    }

    @Override
    protected synchronized void refresh(final List<MetaData> data) {
        reconcile(data);
    }

    private boolean reconcile(final List<MetaData> data) {
        List<MetaData> rows = ListUtils.emptyIfNull(data);
        List<MetaData> snapshot = rows.stream().filter(Objects::nonNull).toList();
        if (!rows.isEmpty() && snapshot.isEmpty()) {
            LOG.warn("ignore HTTP metadata snapshot containing only null rows");
            return false;
        }
        if (snapshot.size() != rows.size()) {
            LOG.warn("ignore null item in HTTP metadata snapshot");
        }
        incomplete = true;
        unsubscribeMissingMetadata(snapshot);
        if (snapshot.isEmpty()) {
            LOG.info("clear all metaData cache");
            metaDataSubscribers.forEach(MetaDataSubscriber::refresh);
        } else {
            snapshot.forEach(metaData -> {
                subscribedMetadata.put(metaData.getId(), metaData);
                metaDataSubscribers.forEach(subscriber -> subscriber.onSubscribe(metaData));
            });
        }
        incomplete = false;
        return true;
    }

    private void unsubscribeMissingMetadata(final List<MetaData> snapshot) {
        Set<String> currentIds = snapshot.stream().map(MetaData::getId).collect(Collectors.toSet());
        subscribedMetadata.values().removeIf(metaData -> {
            if (currentIds.contains(metaData.getId())) {
                return false;
            }
            metaDataSubscribers.forEach(subscriber -> subscriber.unSubscribe(metaData));
            return true;
        });
    }
}
