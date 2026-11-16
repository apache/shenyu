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

import java.util.ArrayList;
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
     * Additional advertised master origins trusted besides the configured seed origins.
     * Entries are {@code host}, {@code host:port} or a {@code ws(s)://} URI; an entry
     * without a port trusts that host on any port.
     */
    private List<String> trustedMasterHosts = new ArrayList<>();

    /**
     * Explicitly allow an advertised plaintext {@code ws} master after a secure {@code wss}
     * connection. Off by default so a server advertisement cannot silently downgrade transport.
     */
    private boolean allowInsecureMasterRedirect;

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
     * get additionally trusted advertised master origins.
     *
     * @return trusted master origin entries
     */
    public List<String> getTrustedMasterHosts() {
        return trustedMasterHosts;
    }

    /**
     * set additionally trusted advertised master origins.
     *
     * @param trustedMasterHosts trusted master origin entries
     */
    public void setTrustedMasterHosts(final List<String> trustedMasterHosts) {
        this.trustedMasterHosts = trustedMasterHosts;
    }

    /**
     * Whether a wss connection may follow a plaintext ws advertised master.
     *
     * @return whether insecure master redirect is explicitly allowed
     */
    public boolean isAllowInsecureMasterRedirect() {
        return allowInsecureMasterRedirect;
    }

    /**
     * set whether a wss connection may follow a plaintext ws advertised master.
     *
     * @param allowInsecureMasterRedirect whether insecure redirect is allowed
     */
    public void setAllowInsecureMasterRedirect(final boolean allowInsecureMasterRedirect) {
        this.allowInsecureMasterRedirect = allowInsecureMasterRedirect;
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
                && masterCheckInterval == that.masterCheckInterval
                && allowInsecureMasterRedirect == that.allowInsecureMasterRedirect
                && Objects.equals(trustedMasterHosts, that.trustedMasterHosts);
    }

    @Override
    public int hashCode() {
        return Objects.hash(urls, allowOrigin, token, initialSyncReadiness, compactDiscoveryPayload, masterCheckInterval,
                trustedMasterHosts, allowInsecureMasterRedirect);
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
                + ", trustedMasterHosts="
                + trustedMasterHosts
                + ", allowInsecureMasterRedirect="
                + allowInsecureMasterRedirect
                + '}';
    }
}
