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

package org.apache.shenyu.plugin.ai.proxy.enhanced.cache;

/**
 * Startup-only limits for the opt-in, local exact response cache.
 */
public final class AiResponseCacheProperties {

    private boolean enabled;

    private long ttlSeconds = 60;

    private int maxRequestBytes = 65536;

    private int maxEntryBytes = 1048576;

    private long maxWeightBytes = 16777216;

    private int maxConcurrentFills = 32;

    /**
     * Whether exact response caching is enabled.
     * @return enabled, false by default
     */
    public boolean isEnabled() {
        return enabled;
    }

    /**
     * Enable caching only with an application-owned scope resolver.
     * @param enabled whether to enable caching
     */
    public void setEnabled(final boolean enabled) {
        this.enabled = enabled;
    }

    /**
     * Get time to live after publication.
     * @return seconds
     */
    public long getTtlSeconds() {
        return ttlSeconds;
    }

    /**
     * Set time to live after publication.
     * @param ttlSeconds positive seconds, at most one day
     */
    public void setTtlSeconds(final long ttlSeconds) {
        if (ttlSeconds <= 0 || ttlSeconds > 86400) {
            throw new IllegalArgumentException("AI response cache TTL must be between 1 and 86400 seconds");
        }
        this.ttlSeconds = ttlSeconds;
    }

    /**
     * Get the maximum original or effective request size considered for caching.
     * @return bytes
     */
    public int getMaxRequestBytes() {
        return maxRequestBytes;
    }

    /**
     * Set the request inspection limit, not the proxy request limit.
     * @param maxRequestBytes positive limit, at most 5 MiB
     */
    public void setMaxRequestBytes(final int maxRequestBytes) {
        if (maxRequestBytes <= 0 || maxRequestBytes > 5 * 1024 * 1024) {
            throw new IllegalArgumentException("AI response cache request limit must be between 1 byte and 5 MiB");
        }
        this.maxRequestBytes = maxRequestBytes;
    }

    /**
     * Get the per-entry capture limit.
     * @return bytes
     */
    public int getMaxEntryBytes() {
        return maxEntryBytes;
    }

    /**
     * Set the per-entry capture limit.
     * @param maxEntryBytes positive limit, at most 16 MiB
     */
    public void setMaxEntryBytes(final int maxEntryBytes) {
        if (maxEntryBytes <= 0 || maxEntryBytes > 16 * 1024 * 1024) {
            throw new IllegalArgumentException("AI response cache entry limit must be between 1 byte and 16 MiB");
        }
        this.maxEntryBytes = maxEntryBytes;
    }

    /**
     * Get the maximum aggregate entry weight.
     * @return bytes, including retained headers and key accounting
     */
    public long getMaxWeightBytes() {
        return maxWeightBytes;
    }

    /**
     * Set maximum aggregate entry weight.
     * @param maxWeightBytes positive byte budget, at most 1 GiB
     */
    public void setMaxWeightBytes(final long maxWeightBytes) {
        if (maxWeightBytes <= 0 || maxWeightBytes > 1024L * 1024 * 1024) {
            throw new IllegalArgumentException("AI response cache weight must be between 1 byte and 1 GiB");
        }
        this.maxWeightBytes = maxWeightBytes;
    }

    /**
     * Get the maximum concurrent response captures.
     * @return capture slots
     */
    public int getMaxConcurrentFills() {
        return maxConcurrentFills;
    }

    /**
     * Set bounded concurrent captures; excess misses bypass capture without waiting.
     * @param maxConcurrentFills positive slots, at most 1024
     */
    public void setMaxConcurrentFills(final int maxConcurrentFills) {
        if (maxConcurrentFills <= 0 || maxConcurrentFills > 1024) {
            throw new IllegalArgumentException("AI response cache fill slots must be between 1 and 1024");
        }
        this.maxConcurrentFills = maxConcurrentFills;
    }
}
