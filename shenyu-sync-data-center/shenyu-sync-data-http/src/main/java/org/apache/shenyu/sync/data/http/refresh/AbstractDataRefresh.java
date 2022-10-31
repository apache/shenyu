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

        boolean updated = false;
        ConfigData<T> result = fromJson(jsonObject);
        if (this.updateCacheIfNeed(result)) {
            updated = true;
            refresh(result.getData());
        }

        return updated;
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
        if (GROUP_CACHE.putIfAbsent(groupEnum, newVal) == null) {
            return true;
        }
        ResultHolder holder = new ResultHolder(false);
        GROUP_CACHE.merge(groupEnum, newVal, (oldVal, value) -> {
            if (StringUtils.equals(oldVal.getMd5(), newVal.getMd5())) {
                LOG.info("Get the same config, the [{}] config cache will not be updated, md5:{}", groupEnum, oldVal.getMd5());
                return oldVal;
            }
            // must compare the last update time
            if (oldVal.getLastModifyTime() >= newVal.getLastModifyTime()) {
                LOG.info("Last update time earlier than the current configuration, the [{}] config cache will not be updated", groupEnum);
                return oldVal;
            }
            LOG.info("update {} config: {}", groupEnum, newVal);
            holder.result = true;
            return newVal;
        });
        return holder.result;
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
