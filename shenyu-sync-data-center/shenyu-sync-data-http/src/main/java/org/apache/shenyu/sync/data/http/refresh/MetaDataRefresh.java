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

import java.util.List;
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

    // The refresh instance's monitor guards the last snapshot applied to the subscribers.
    private List<MetaData> appliedMetadata = List.of();

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
    protected boolean updateCacheIfNeed(final ConfigData<MetaData> result) {
        List<MetaData> rows = ListUtils.emptyIfNull(result.getData());
        if (!rows.isEmpty() && rows.stream().allMatch(Objects::isNull)) {
            LOG.warn("ignore HTTP metadata snapshot containing only null rows");
            return false;
        }
        return updateCacheIfNeed(result, ConfigGroupEnum.META_DATA);
    }

    @Override
    public ConfigData<?> cacheConfigData() {
        return GROUP_CACHE.get(ConfigGroupEnum.META_DATA);
    }

    @Override
    public synchronized Boolean refresh(final JsonObject data) {
        // Multiple Admin connections can deliver snapshots. Keep the version check and subscriber updates ordered together.
        return super.refresh(data);
    }

    @Override
    protected synchronized void refresh(final List<MetaData> data) {
        List<MetaData> rows = ListUtils.emptyIfNull(data);
        List<MetaData> snapshot = rows.stream().filter(Objects::nonNull).toList();
        if (snapshot.size() != rows.size()) {
            LOG.warn("ignore null item in HTTP metadata snapshot");
            // A malformed frame with no usable rows must not clear an applied snapshot.
            if (snapshot.isEmpty()) {
                return;
            }
        }
        unsubscribeMissingMetadata(snapshot);
        if (snapshot.isEmpty()) {
            LOG.info("clear all metaData cache");
            metaDataSubscribers.forEach(MetaDataSubscriber::refresh);
        } else {
            snapshot.forEach(metaData -> metaDataSubscribers.forEach(subscriber -> subscriber.onSubscribe(metaData)));
        }
        appliedMetadata = snapshot;
    }

    private void unsubscribeMissingMetadata(final List<MetaData> snapshot) {
        if (appliedMetadata.isEmpty()) {
            return;
        }
        Set<String> currentIds = snapshot.stream().map(MetaData::getId).collect(Collectors.toSet());
        appliedMetadata.stream().filter(metaData -> !currentIds.contains(metaData.getId()))
                .forEach(metaData -> metaDataSubscribers.forEach(subscriber -> subscriber.unSubscribe(metaData)));
    }
}
