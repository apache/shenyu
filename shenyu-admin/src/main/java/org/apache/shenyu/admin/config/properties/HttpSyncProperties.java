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

package org.apache.shenyu.admin.config.properties;

import org.springframework.boot.context.properties.ConfigurationProperties;

import java.time.Duration;
import java.util.Objects;

/**
 * the http sync strategy properties.
 */
@ConfigurationProperties(prefix = "shenyu.sync.http")
public class HttpSyncProperties {

    /**
     * Whether enabled http sync strategy, default: true.
     */
    private boolean enabled = true;

    /**
     * Periodically refresh the config data interval from the database, default: 5 minutes.
     */
    private Duration refreshInterval = Duration.ofMinutes(5);

    /**
     * Notify client batch size.
     */
    private int notifyBatchSize = 100;

    /**
     * Fixed cache rebuild coalescing window. Zero preserves synchronous event processing.
     */
    private Duration cacheRebuildWindow = Duration.ZERO;

    /**
     * Maximum pending namespace/group keys. New keys rebuild on the caller when full.
     */
    private int cacheRebuildMaxPendingKeys = 1024;

    /**
     * Worker threads rebuilding coalesced keys. Distinct namespace/group keys rebuild in parallel;
     * one key never rebuilds concurrently.
     */
    private int cacheRebuildThreads = 1;

    /**
     * The heartbeat interval in milliseconds for liveness refresh during long-polling holds, default: 10000ms.
     */
    private long heartbeatInterval = 10000L;

    /**
     * Get the fixed coalescing window.
     *
     * @return cache rebuild window
     */
    public Duration getCacheRebuildWindow() {
        return cacheRebuildWindow;
    }

    /**
     * Set the fixed coalescing window.
     *
     * @param cacheRebuildWindow zero to disable, or a window from 1 to 1000 milliseconds
     */
    public void setCacheRebuildWindow(final Duration cacheRebuildWindow) {
        if (Objects.isNull(cacheRebuildWindow) || cacheRebuildWindow.isNegative()
                || cacheRebuildWindow.compareTo(Duration.ofSeconds(1)) > 0
                || (!cacheRebuildWindow.isZero() && cacheRebuildWindow.toMillis() == 0)) {
            throw new IllegalArgumentException("cacheRebuildWindow must be zero or between 1ms and 1s");
        }
        this.cacheRebuildWindow = cacheRebuildWindow;
    }

    /**
     * Get the pending key capacity.
     *
     * @return maximum pending keys
     */
    public int getCacheRebuildMaxPendingKeys() {
        return cacheRebuildMaxPendingKeys;
    }

    /**
     * Set the pending key capacity.
     *
     * @param cacheRebuildMaxPendingKeys positive maximum pending keys
     */
    public void setCacheRebuildMaxPendingKeys(final int cacheRebuildMaxPendingKeys) {
        if (cacheRebuildMaxPendingKeys <= 0) {
            throw new IllegalArgumentException("cacheRebuildMaxPendingKeys must be positive");
        }
        this.cacheRebuildMaxPendingKeys = cacheRebuildMaxPendingKeys;
    }

    /**
     * Get the number of cache rebuild worker threads.
     *
     * @return cache rebuild worker threads
     */
    public int getCacheRebuildThreads() {
        return cacheRebuildThreads;
    }

    /**
     * Set the number of cache rebuild worker threads.
     *
     * @param cacheRebuildThreads worker threads from 1 to 64
     */
    public void setCacheRebuildThreads(final int cacheRebuildThreads) {
        if (cacheRebuildThreads <= 0 || cacheRebuildThreads > 64) {
            throw new IllegalArgumentException("cacheRebuildThreads must be between 1 and 64");
        }
        this.cacheRebuildThreads = cacheRebuildThreads;
    }

    /**
     * Gets the value of enabled.
     *
     * @return the value of enabled
     */
    public boolean isEnabled() {
        return enabled;
    }

    /**
     * Sets the enabled.
     *
     * @param enabled enabled
     */
    public void setEnabled(final boolean enabled) {
        this.enabled = enabled;
    }

    /**
     * Gets the value of refreshInterval.
     *
     * @return the value of refreshInterval
     */
    public Duration getRefreshInterval() {
        return refreshInterval;
    }

    /**
     * Sets the refreshInterval.
     *
     * @param refreshInterval refreshInterval
     */
    public void setRefreshInterval(final Duration refreshInterval) {
        this.refreshInterval = refreshInterval;
    }

    /**
     * Gets the value of notifyBatchSize.
     *
     * @return the value of notifyBatchSize
     */
    public int getNotifyBatchSize() {
        return notifyBatchSize;
    }

    /**
     * Sets the notifyBatchSize.
     *
     * @param notifyBatchSize notifyBatchSize
     */
    public void setNotifyBatchSize(final int notifyBatchSize) {
        this.notifyBatchSize = notifyBatchSize;
    }

    /**
     * Gets the value of heartbeatInterval.
     *
     * @return the value of heartbeatInterval
     */
    public long getHeartbeatInterval() {
        return heartbeatInterval;
    }

    /**
     * Sets the heartbeatInterval.
     *
     * @param heartbeatInterval heartbeatInterval
     */
    public void setHeartbeatInterval(final long heartbeatInterval) {
        this.heartbeatInterval = heartbeatInterval;
    }
}
