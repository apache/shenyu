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
 * Properties of the opt-in server-issued registration lease
 * ({@code registration-lease-v1}). When disabled, the {@code /instance/beat}
 * endpoint keeps its exact legacy v1 behavior.
 */
@ConfigurationProperties(prefix = "shenyu.register.lease")
public class RegistrationLeaseProperties {

    /**
     * Default heartbeat interval when the client does not propose one.
     */
    private static final Duration DEFAULT_INTERVAL = Duration.ofSeconds(5);

    /**
     * Default multiple of the heartbeat interval used as the offline deadline.
     */
    private static final int DEFAULT_EXPIRY_FACTOR = 3;

    /**
     * Whether the registration lease is enabled. When false, no capability is
     * advertised and beats keep the legacy v1 semantics.
     */
    private boolean enabled;

    /**
     * Heartbeat interval issued to clients that do not propose one, and the fallback
     * for proposals outside the accepted bounds.
     */
    private Duration interval = DEFAULT_INTERVAL;

    /**
     * Smallest heartbeat interval the server is willing to negotiate.
     */
    private Duration minInterval = Duration.ofSeconds(2);

    /**
     * Largest heartbeat interval the server is willing to negotiate.
     */
    private Duration maxInterval = Duration.ofSeconds(60);

    /**
     * Multiple of the negotiated interval used as the offline deadline: an instance
     * whose lease is not renewed within {@code expiryFactor * interval} is marked
     * offline.
     */
    private int expiryFactor = DEFAULT_EXPIRY_FACTOR;

    /**
     * Whether the lease is enabled.
     *
     * @return whether enabled
     */
    public boolean isEnabled() {
        return enabled;
    }

    /**
     * Set whether the lease is enabled.
     *
     * @param enabled whether enabled
     */
    public void setEnabled(final boolean enabled) {
        this.enabled = enabled;
    }

    /**
     * Get the default heartbeat interval.
     *
     * @return the default interval
     */
    public Duration getInterval() {
        return interval;
    }

    /**
     * Set the default heartbeat interval.
     *
     * @param interval the default interval
     */
    public void setInterval(final Duration interval) {
        this.interval = interval;
    }

    /**
     * Get the smallest negotiable heartbeat interval.
     *
     * @return the minimum interval
     */
    public Duration getMinInterval() {
        return minInterval;
    }

    /**
     * Set the smallest negotiable heartbeat interval.
     *
     * @param minInterval the minimum interval
     */
    public void setMinInterval(final Duration minInterval) {
        this.minInterval = minInterval;
    }

    /**
     * Get the largest negotiable heartbeat interval.
     *
     * @return the maximum interval
     */
    public Duration getMaxInterval() {
        return maxInterval;
    }

    /**
     * Set the largest negotiable heartbeat interval.
     *
     * @param maxInterval the maximum interval
     */
    public void setMaxInterval(final Duration maxInterval) {
        this.maxInterval = maxInterval;
    }

    /**
     * Get the expiry factor applied to the negotiated interval.
     *
     * @return the expiry factor
     */
    public int getExpiryFactor() {
        return expiryFactor;
    }

    /**
     * Set the expiry factor applied to the negotiated interval.
     *
     * @param expiryFactor the expiry factor
     */
    public void setExpiryFactor(final int expiryFactor) {
        this.expiryFactor = expiryFactor;
    }
}
