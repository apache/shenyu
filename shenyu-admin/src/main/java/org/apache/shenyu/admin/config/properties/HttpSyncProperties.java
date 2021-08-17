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
}
