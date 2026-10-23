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
     * whether concurrent misses on one key share a single upstream fill, off by default.
     */
    private boolean singleFlightEnabled;

    /**
     * the maximum number of keys filling at once on this gateway.
     */
    private int singleFlightMaxKeys = 1024;

    /**
     * the maximum number of requests waiting on one filling key.
     */
    private int singleFlightMaxWaiters = 64;

    /**
     * how long a request waits for another request's fill, in milliseconds.
     */
    private long singleFlightWaitTimeoutMillis = 1000L;

    /**
     * how long a key stays owned when its leader never completes, in milliseconds.
     */
    private long singleFlightFillTimeoutMillis = 30000L;

    /**
     * the largest response shared with waiting requests, in bytes.
     */
    private long singleFlightMaxResponseBytes = 1048576L;

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

    /**
     * Whether concurrent misses on one key share a single upstream fill.
     *
     * @return true when coalescing is enabled
     */
    public boolean isSingleFlightEnabled() {
        return singleFlightEnabled;
    }

    /**
     * Set whether concurrent misses on one key share a single upstream fill.
     *
     * @param singleFlightEnabled true to enable coalescing
     */
    public void setSingleFlightEnabled(final boolean singleFlightEnabled) {
        this.singleFlightEnabled = singleFlightEnabled;
    }

    /**
     * Get the maximum number of keys filling at once.
     *
     * @return the maximum number of filling keys
     */
    public int getSingleFlightMaxKeys() {
        return singleFlightMaxKeys;
    }

    /**
     * Set the maximum number of keys filling at once.
     *
     * @param singleFlightMaxKeys the maximum number of filling keys
     */
    public void setSingleFlightMaxKeys(final int singleFlightMaxKeys) {
        this.singleFlightMaxKeys = singleFlightMaxKeys;
    }

    /**
     * Get the maximum number of requests waiting on one filling key.
     *
     * @return the maximum number of waiters
     */
    public int getSingleFlightMaxWaiters() {
        return singleFlightMaxWaiters;
    }

    /**
     * Set the maximum number of requests waiting on one filling key.
     *
     * @param singleFlightMaxWaiters the maximum number of waiters
     */
    public void setSingleFlightMaxWaiters(final int singleFlightMaxWaiters) {
        this.singleFlightMaxWaiters = singleFlightMaxWaiters;
    }

    /**
     * Get how long a request waits for another request's fill.
     *
     * @return the wait timeout in milliseconds
     */
    public long getSingleFlightWaitTimeoutMillis() {
        return singleFlightWaitTimeoutMillis;
    }

    /**
     * Set how long a request waits for another request's fill.
     *
     * @param singleFlightWaitTimeoutMillis the wait timeout in milliseconds
     */
    public void setSingleFlightWaitTimeoutMillis(final long singleFlightWaitTimeoutMillis) {
        this.singleFlightWaitTimeoutMillis = singleFlightWaitTimeoutMillis;
    }

    /**
     * Get how long a key stays owned when its leader never completes.
     *
     * @return the fill timeout in milliseconds
     */
    public long getSingleFlightFillTimeoutMillis() {
        return singleFlightFillTimeoutMillis;
    }

    /**
     * Set how long a key stays owned when its leader never completes.
     *
     * @param singleFlightFillTimeoutMillis the fill timeout in milliseconds
     */
    public void setSingleFlightFillTimeoutMillis(final long singleFlightFillTimeoutMillis) {
        this.singleFlightFillTimeoutMillis = singleFlightFillTimeoutMillis;
    }

    /**
     * Get the largest response shared with waiting requests.
     *
     * @return the maximum shared response size in bytes
     */
    public long getSingleFlightMaxResponseBytes() {
        return singleFlightMaxResponseBytes;
    }

    /**
     * Set the largest response shared with waiting requests.
     *
     * @param singleFlightMaxResponseBytes the maximum shared response size in bytes
     */
    public void setSingleFlightMaxResponseBytes(final long singleFlightMaxResponseBytes) {
        this.singleFlightMaxResponseBytes = singleFlightMaxResponseBytes;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
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
