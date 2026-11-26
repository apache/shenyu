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
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.ConfigData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

/**
 * The type Abstract data refresh.
 *
 * @param <T> the type parameter
 */
public abstract class AbstractDataRefresh<T> implements DataRefresh {

    /**
     * The Group cache.
     */
    protected static final ConcurrentMap<ConfigGroupEnum, ConfigData<?>> GROUP_CACHE = new ConcurrentHashMap<>();

    /**
     * logger.
     */
    private static final Logger LOG = LoggerFactory.getLogger(AbstractDataRefresh.class);

    /**
     * Convert json object.
     *
     * @param data the data
     * @return the json object
     */
    protected abstract JsonObject convert(JsonObject data);

    /**
     * From json config data.
     *
     * @param data the data
     * @return the config data
     */
    protected abstract ConfigData<T> fromJson(JsonObject data);

    /**
     * Refresh.
     *
     * @param data the data
     */
    protected abstract void refresh(List<T> data);

    @Override
    public Boolean refresh(final JsonObject data) {
        JsonObject jsonObject = convert(data);
        if (Objects.isNull(jsonObject)) {
            return false;
        }

        return applyConfig(fromJson(jsonObject));
    }

    /**
     * Apply a configuration snapshot if it is newer than the cached snapshot.
     *
     * @param result the parsed snapshot
     * @return whether a new snapshot was applied
     */
    protected boolean applyConfig(final ConfigData<T> result) {
        if (!this.updateCacheIfNeed(result)) {
            return false;
        }
        refresh(result.getData());
        return true;
    }

    /**
     * Update cache if need boolean.
     *
     * @param result the result
     * @return the boolean
     */
    protected abstract boolean updateCacheIfNeed(ConfigData<T> result);

    /**
     * If the MD5 values are different and the last update time of the old data is less than
     * the last update time of the new data, the configuration cache is considered to have been changed.
     *
     * @param newVal    the lasted config
     * @param groupEnum the group enum
     * @return true : if need update
     */
    protected boolean updateCacheIfNeed(final ConfigData<T> newVal, final ConfigGroupEnum groupEnum) {
        // first init cache
        if (Objects.isNull(GROUP_CACHE.putIfAbsent(groupEnum, newVal))) {
            return true;
        }
        ResultHolder holder = new ResultHolder(false);
        GROUP_CACHE.merge(groupEnum, newVal, (oldVal, value) -> {
            if (!isNewerConfig(oldVal, newVal, groupEnum)) {
                return oldVal;
            }
            LOG.info("update {} config: {}", groupEnum, newVal);
            holder.result = true;
            return newVal;
        });
        return holder.result;
    }

    /**
     * Check whether the candidate has a different digest and a later version.
     *
     * @param oldVal the cached snapshot, if any
     * @param newVal the candidate snapshot
     * @param groupEnum the configuration group
     * @return whether the candidate should be applied
     */
    protected final boolean isNewerConfig(final ConfigData<?> oldVal, final ConfigData<T> newVal, final ConfigGroupEnum groupEnum) {
        if (Objects.isNull(oldVal)) {
            return true;
        }
        if (StringUtils.equals(oldVal.getMd5(), newVal.getMd5())) {
            LOG.info("Get the same config, the [{}] config cache will not be updated, md5:{}", groupEnum, oldVal.getMd5());
            return false;
        }
        if (oldVal.getLastModifyTime() >= newVal.getLastModifyTime()) {
            LOG.info("Last update time earlier than the current configuration, the [{}] config cache will not be updated", groupEnum);
            return false;
        }
        return true;
    }

    private static final class ResultHolder {

        private boolean result;

        /**
         * Instantiates a new Result holder.
         *
         * @param result the result
         */
        ResultHolder(final boolean result) {
            this.result = result;
        }
    }
}
