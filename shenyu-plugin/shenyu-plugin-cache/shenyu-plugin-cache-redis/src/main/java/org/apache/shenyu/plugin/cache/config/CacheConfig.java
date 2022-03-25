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

import org.apache.shenyu.plugin.cache.enums.CacheEnum;
import org.apache.shenyu.plugin.cache.redis.RedisConfigProperties;

import java.util.Objects;

/**
 * CacheConfig.
 */
public class CacheConfig extends RedisConfigProperties {

    /**
     * the type of config cache, default memory {@linkplain CacheEnum}.
     */
    private String cacheType = CacheEnum.MEMORY.getName();

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

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        if (!super.equals(o)) {
            return false;
        }
        final CacheConfig that = (CacheConfig) o;
        return Objects.equals(cacheType, that.cacheType);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), cacheType);
    }
}
