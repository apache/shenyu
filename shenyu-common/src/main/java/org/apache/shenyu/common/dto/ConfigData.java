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

package org.apache.shenyu.common.dto;

import org.apache.shenyu.common.utils.GsonUtils;

import java.util.List;
import java.util.Objects;

/**
 * Data set, including {@link AppAuthData}、{@link ConditionData}、{@link PluginData}、{@link RuleData}、{@link SelectorData}.
 *
 * @since 2.0.0
 */
public class ConfigData<T> {

    private int hashValue;

    private long lastModifyTime;

    private List<T> data;

    /**
     * no args constructor.
     */
    public ConfigData() {
    }

    /**
     * all args constructor.
     *
     * @param hashValue      hashValue
     * @param lastModifyTime lastModifyTime
     * @param data           data
     */
    public ConfigData(final int hashValue, final long lastModifyTime, final List<T> data) {
        this.hashValue = hashValue;
        this.lastModifyTime = lastModifyTime;
        this.data = data;
    }

    /**
     * get hashValue.
     *
     * @return hashValue
     */
    public int getHashValue() {
        return hashValue;
    }

    /**
     * set hashValue.
     *
     * @param hashValue hashValue
     * @return this
     */
    public ConfigData<T> setHashValue(final int hashValue) {
        this.hashValue = hashValue;
        return this;
    }

    /**
     * get lastModifyTime.
     *
     * @return lastModifyTime
     */
    public long getLastModifyTime() {
        return lastModifyTime;
    }

    /**
     * set lastModifyTime.
     *
     * @param lastModifyTime lastModifyTime
     * @return this
     */
    public ConfigData<T> setLastModifyTime(final long lastModifyTime) {
        this.lastModifyTime = lastModifyTime;
        return this;
    }

    /**
     * get data.
     *
     * @return data
     */
    public List<T> getData() {
        return data;
    }

    /**
     * set data.
     *
     * @param data data
     * @return this
     */
    public ConfigData<T> setData(final List<T> data) {
        this.data = data;
        return this;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        ConfigData<?> that = (ConfigData<?>) o;
        return lastModifyTime == that.lastModifyTime && Objects.equals(hashValue, that.hashValue) && Objects.equals(data, that.data);
    }

    @Override
    public int hashCode() {
        return Objects.hash(hashValue, lastModifyTime, data);
    }

    @Override
    public String toString() {
        return GsonUtils.getInstance().toJson(this);
    }
}
