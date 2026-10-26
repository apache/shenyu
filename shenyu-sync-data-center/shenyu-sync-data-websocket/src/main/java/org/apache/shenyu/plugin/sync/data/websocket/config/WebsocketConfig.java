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

package org.apache.shenyu.plugin.sync.data.websocket.config;

import java.util.List;
import java.util.Objects;

public class WebsocketConfig {
    
    /**
     * if have more shenyu admin url,please config like this.
     * 127.0.0.1:8888,127.0.0.1:8889
     */
    private List<String> urls;

    /**
     * allowOrigin.
     */
    private String allowOrigin;

    /**
     * WebSocket sync token.
     */
    private String token;

    private boolean initialSyncReadiness;

    /**
     * Accept the compact-v1 Discovery wire projection on this gateway connection.
     */
    private boolean compactDiscoveryPayload;

    /**
     * Master check interval in seconds.
     */
    private int masterCheckInterval = 60;

    /**
     * Whether this gateway accepts compact Discovery payloads.
     *
     * @return whether compact payloads are accepted
     */
    public boolean isCompactDiscoveryPayload() {
        return compactDiscoveryPayload;
    }

    /**
     * Advertise support after auditing this gateway's Discovery subscribers.
     *
     * @param compactDiscoveryPayload whether compact payloads are accepted
     */
    public void setCompactDiscoveryPayload(final boolean compactDiscoveryPayload) {
        this.compactDiscoveryPayload = compactDiscoveryPayload;
    }

    /**
     * Whether the opt-in initial synchronization protocol is required.
     * @return enabled
     */
    public boolean isInitialSyncReadiness() {
        return initialSyncReadiness;
    }

    /**
     * Enable initial synchronization readiness.
     * @param initialSyncReadiness enabled
     */
    public void setInitialSyncReadiness(final boolean initialSyncReadiness) {
        this.initialSyncReadiness = initialSyncReadiness;
    }

    /**
     * get urls.
     *
     * @return urls
     */
    public List<String> getUrls() {
        return urls;
    }

    /**
     * set urls.
     *
     * @param urls urls
     */
    public void setUrls(final List<String> urls) {
        this.urls = urls;
    }

    /**
     * get allowOrigin.
     * @return allowOrigin
     */
    public String getAllowOrigin() {
        return allowOrigin;
    }

    /**
     * set allowOrigin.
     * @param allowOrigin allowOrigin
     */
    public void setAllowOrigin(final String allowOrigin) {
        this.allowOrigin = allowOrigin;
    }

    /**
     * get token.
     * @return token
     */
    public String getToken() {
        return token;
    }

    /**
     * set token.
     * @param token token
     */
    public void setToken(final String token) {
        this.token = token;
    }

    /**
     * get master check interval in seconds.
     *
     * @return master check interval
     */
    public int getMasterCheckInterval() {
        return masterCheckInterval;
    }

    /**
     * set master check interval in seconds.
     *
     * @param masterCheckInterval master check interval
     */
    public void setMasterCheckInterval(final int masterCheckInterval) {
        this.masterCheckInterval = masterCheckInterval;
    }
    
    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        WebsocketConfig that = (WebsocketConfig) o;
        return Objects.equals(urls, that.urls)
                && Objects.equals(allowOrigin, that.allowOrigin)
                && Objects.equals(token, that.token)
                && initialSyncReadiness == that.initialSyncReadiness
                && compactDiscoveryPayload == that.compactDiscoveryPayload
                && masterCheckInterval == that.masterCheckInterval;
    }

    @Override
    public int hashCode() {
        return Objects.hash(urls, allowOrigin, token, initialSyncReadiness, compactDiscoveryPayload, masterCheckInterval);
    }

    @Override
    public String toString() {
        return "WebsocketConfig{"
                + "urls='"
                + urls
                + '\''
                + ", allowOrigin='"
                + allowOrigin
                + '\''
                + ", token='******'"
                + ", masterCheckInterval="
                + masterCheckInterval
                + '}';
    }
}
