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

import com.google.common.collect.Lists;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * add test case for {@link WebsocketConfig}.
 */
public class WebsocketConfigTest {

    private static final List<String> URLS = Lists.newArrayList("ws://localhost:9095/websocket");

    private static final String ALLOW_ORIGIN = "ws://localhost:9095";

    private static final String TOKEN = "websocket-sync-token";

    private static final List<String> TRUSTED_MASTER_HOSTS = Lists.newArrayList("master:9095");

    private WebsocketConfig websocketConfig;

    @BeforeEach
    public void setUp() {
        websocketConfig = new WebsocketConfig();
        websocketConfig.setUrls(URLS);
        websocketConfig.setAllowOrigin(ALLOW_ORIGIN);
        websocketConfig.setToken(TOKEN);
        websocketConfig.setTrustedMasterHosts(TRUSTED_MASTER_HOSTS);
    }

    @Test
    public void testGetterSetter() {
        assertEquals(URLS, websocketConfig.getUrls());
        assertEquals(ALLOW_ORIGIN, websocketConfig.getAllowOrigin());
        assertEquals(TOKEN, websocketConfig.getToken());
        assertFalse(websocketConfig.isCompactDiscoveryPayload());
        websocketConfig.setCompactDiscoveryPayload(true);
        assertTrue(websocketConfig.isCompactDiscoveryPayload());
        assertEquals(60, websocketConfig.getMasterCheckInterval());
        websocketConfig.setMasterCheckInterval(30);
        assertEquals(30, websocketConfig.getMasterCheckInterval());
        assertEquals(TRUSTED_MASTER_HOSTS, websocketConfig.getTrustedMasterHosts());
        assertFalse(websocketConfig.isAllowInsecureMasterRedirect());
        websocketConfig.setAllowInsecureMasterRedirect(true);
        assertTrue(websocketConfig.isAllowInsecureMasterRedirect());
    }

    @Test
    public void testEquals() {
        WebsocketConfig that = new WebsocketConfig();
        that.setUrls(URLS);
        that.setAllowOrigin(ALLOW_ORIGIN);
        that.setToken(TOKEN);
        that.setTrustedMasterHosts(TRUSTED_MASTER_HOSTS);
        assertEquals(websocketConfig, websocketConfig);
        assertEquals(websocketConfig, that);
        that.setInitialSyncReadiness(true);
        assertNotEquals(websocketConfig, that);
        that.setInitialSyncReadiness(false);
        that.setCompactDiscoveryPayload(true);
        assertNotEquals(websocketConfig, that);
        that.setCompactDiscoveryPayload(false);
        that.setMasterCheckInterval(10);
        assertNotEquals(websocketConfig, that);
        that.setMasterCheckInterval(60);
        that.setTrustedMasterHosts(Lists.newArrayList("other:9095"));
        assertNotEquals(websocketConfig, that);
        that.setTrustedMasterHosts(TRUSTED_MASTER_HOSTS);
        that.setAllowInsecureMasterRedirect(true);
        assertNotEquals(websocketConfig, that);
        assertNotEquals(websocketConfig, null);
        assertNotEquals(websocketConfig, new Object());
    }

    @Test
    public void testHashCode() {
        assertEquals(Objects.hash(websocketConfig.getUrls(), websocketConfig.getAllowOrigin(),
                websocketConfig.getToken(), websocketConfig.isInitialSyncReadiness(), websocketConfig.isCompactDiscoveryPayload(),
                websocketConfig.getMasterCheckInterval(), websocketConfig.getTrustedMasterHosts(),
                websocketConfig.isAllowInsecureMasterRedirect()), websocketConfig.hashCode());
    }

    @Test
    public void testToString() {
        String toString = "WebsocketConfig{urls='%s', allowOrigin='%s', token='******', masterCheckInterval=%d, "
                + "trustedMasterHosts=%s, allowInsecureMasterRedirect=false}";
        String expected = String.format(toString, URLS, ALLOW_ORIGIN, 60, TRUSTED_MASTER_HOSTS);
        assertEquals(expected, websocketConfig.toString());
        assertFalse(websocketConfig.toString().contains(TOKEN));
    }
}
