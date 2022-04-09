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

package org.apache.shenyu.plugin.cache.config;

import java.util.Objects;

/**
 * CacheConfig.
 */
public class CacheConfig {

    /**
     * the type of config cache, default memory.
     */
    private String cacheType = "memory";

    /**
     * the origin config.
     */
    private String config;

    /**
     * Get cache type.
     * @return the cache type
     */
    public String getCacheType() {
        return cacheType;
    }

    /**
     * Set cache type.
     * @param cacheType the cache type
     */
    public void setCacheType(final String cacheType) {
        this.cacheType = cacheType;
    }

    /**
     * Get config.
     * @return the config
     */
    public String getConfig() {
        return config;
    }

    /**
     * Set the config.
     * @param config the config
     */
    public void setConfig(final String config) {
        this.config = config;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        final CacheConfig that = (CacheConfig) o;
        return cacheType.equals(that.cacheType) && config.equals(that.config);
    }

    @Override
    public int hashCode() {
        return Objects.hash(cacheType, config);
    }
}
