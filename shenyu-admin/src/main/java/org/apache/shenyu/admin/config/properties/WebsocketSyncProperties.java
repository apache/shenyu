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
 * the websocket sync strategy properties.
 */
@ConfigurationProperties(prefix = "shenyu.sync.websocket")
public class WebsocketSyncProperties {

    /**
     * default: true.
     */
    private boolean enabled = true;

    /**
     * default is 8192.
     */
    private int messageMaxSize;

    /**
     * allowOrigins.
     */
    private String allowOrigins;

    /**
     * WebSocket sync token.
     */
    private String token;

    private final Reconciliation reconciliation = new Reconciliation();

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
     * get messageMaxSize.
     *
     * @return messageMaxSize
     */
    public int getMessageMaxSize() {
        return messageMaxSize;
    }

    /**
     * set messageMaxSize.
     *
     * @param messageMaxSize messageMaxSize
     */
    public void setMessageMaxSize(final int messageMaxSize) {
        this.messageMaxSize = messageMaxSize;
    }

    /**
     * get allowOrigins.
     *
     * @return allowOrigins
     */
    public String getAllowOrigins() {
        return allowOrigins;
    }

    /**
     * set allowOrigins.
     * @param allowOrigins allowOrigins
     */
    public void setAllowOrigins(final String allowOrigins) {
        this.allowOrigins = allowOrigins;
    }

    /**
     * get token.
     *
     * @return token
     */
    public String getToken() {
        return token;
    }

    /**
     * set token.
     *
     * @param token token
     */
    public void setToken(final String token) {
        this.token = token;
    }

    /**
     * get reconciliation settings.
     *
     * @return reconciliation
     */
    public Reconciliation getReconciliation() {
        return reconciliation;
    }

    /**
     * Reconciliation settings for deployments where several standalone admin nodes
     * share one database: each node periodically compares a digest of plugin, selector and rule
     * configuration groups with the database state and pushes a full refresh of the
     * changed groups to the gateway sessions connected to it, so gateways converge
     * even when the change was written by another admin node.
     */
    public static class Reconciliation {

        /**
         * Whether reconciliation is enabled, default: false.
         */
        private boolean enabled;

        /**
         * Fixed delay between reconciliation cycles, default: 60s.
         * Larger values reduce the database polling cost but increase
         * the worst-case consistency delay for cross-admin changes.
         */
        private Duration interval = Duration.ofSeconds(60);

        /**
         * Whether reconciliation is enabled.
         *
         * @return enabled
         */
        public boolean isEnabled() {
            return enabled;
        }

        /**
         * Set enabled.
         *
         * @param enabled enabled
         */
        public void setEnabled(final boolean enabled) {
            this.enabled = enabled;
        }

        /**
         * Gets the fixed delay between reconciliation cycles.
         *
         * @return interval
         */
        public Duration getInterval() {
            return interval;
        }

        /**
         * Sets the fixed delay between reconciliation cycles.
         *
         * @param interval interval
         */
        public void setInterval(final Duration interval) {
            this.interval = interval;
        }
    }
}
