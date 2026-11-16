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

package org.apache.shenyu.plugin.sync.data.websocket;

import org.junit.jupiter.api.Test;

import java.net.URI;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests the advertised master trust policy.
 */
final class MasterTargetPolicyTest {

    private static final URI CURRENT = URI.create("ws://seed:9095/websocket");

    @Test
    void seedOriginIsTrusted() {
        assertTrue(MasterTargetPolicy.isTrustedMaster(URI.create("ws://seed:9095/websocket"), CURRENT,
                Collections.emptyList(), List.of("ws://seed:9095/websocket"), Collections.emptyList(), false));
    }

    @Test
    void unlistedOriginIsRejected() {
        assertFalse(MasterTargetPolicy.isTrustedMaster(URI.create("ws://other:9095/websocket"), CURRENT,
                Collections.emptyList(), List.of("ws://seed:9095/websocket"), Collections.emptyList(), false));
    }

    @Test
    void wssNeverFollowsPlaintextByDefault() {
        final URI secure = URI.create("wss://seed:9095/websocket");
        final URI plaintext = URI.create("ws://seed:9095/websocket");
        assertFalse(MasterTargetPolicy.isTrustedMaster(plaintext, secure, Collections.emptyList(),
                List.of("wss://seed:9095/websocket"), Collections.emptyList(), false));
    }

    @Test
    void insecureRedirectRequiresExplicitOptIn() {
        final URI secure = URI.create("wss://seed:9095/websocket");
        final URI plaintext = URI.create("ws://seed:9095/websocket");
        assertTrue(MasterTargetPolicy.isTrustedMaster(plaintext, secure, Collections.emptyList(),
                List.of("wss://seed:9095/websocket"), Collections.emptyList(), true));
    }

    @Test
    void plaintextConnectionMayUpgradeToWss() {
        assertTrue(MasterTargetPolicy.isTrustedMaster(URI.create("wss://seed:9095/websocket"), CURRENT,
                Collections.emptyList(), List.of("ws://seed:9095/websocket"), Collections.emptyList(), false));
    }

    @Test
    void trustedEntriesSupportHostPortAndUriForms() {
        final URI master = URI.create("ws://master.example:9095/websocket");
        assertTrue(MasterTargetPolicy.isTrustedMaster(master, CURRENT, Collections.emptyList(),
                Collections.emptyList(), List.of("master.example"), false));
        assertTrue(MasterTargetPolicy.isTrustedMaster(master, CURRENT, Collections.emptyList(),
                Collections.emptyList(), List.of("master.example:9095"), false));
        assertTrue(MasterTargetPolicy.isTrustedMaster(master, CURRENT, Collections.emptyList(),
                Collections.emptyList(), List.of("ws://master.example:9095/websocket"), false));
        assertFalse(MasterTargetPolicy.isTrustedMaster(master, CURRENT, Collections.emptyList(),
                Collections.emptyList(), List.of("master.example:9096"), false));
    }

    @Test
    void ipv6TrustedHostsNormalizeBrackets() {
        final URI master = URI.create("wss://[2001:db8::1]:9095/websocket");
        final URI current = URI.create("wss://seed:9095/websocket");
        assertTrue(MasterTargetPolicy.isTrustedMaster(master, current, Collections.emptyList(),
                Collections.emptyList(), List.of("[2001:db8::1]"), false));
        assertTrue(MasterTargetPolicy.isTrustedMaster(master, current, Collections.emptyList(),
                Collections.emptyList(), List.of("[2001:db8::1]:9095"), false));
        assertTrue(MasterTargetPolicy.isTrustedMaster(master, current, Collections.emptyList(),
                Collections.emptyList(), List.of("wss://[2001:db8::1]:9095/websocket"), false));
        assertFalse(MasterTargetPolicy.isTrustedMaster(master, current, Collections.emptyList(),
                Collections.emptyList(), List.of("[2001:db8::2]"), false));
    }

    @Test
    void connectedClientOriginIsTrusted() {
        assertTrue(MasterTargetPolicy.isTrustedMaster(URI.create("ws://master:9095/websocket"), CURRENT,
                List.of(URI.create("ws://master:9095/websocket")), Collections.emptyList(), Collections.emptyList(), false));
    }

    @Test
    void seedWithoutSchemeIsParsed() {
        assertTrue(MasterTargetPolicy.isTrustedMaster(URI.create("ws://seed:9095/websocket"), CURRENT,
                Collections.emptyList(), List.of("seed:9095"), Collections.emptyList(), false));
    }

    @Test
    void invalidOrMissingCurrentContextIsRejected() {
        assertFalse(MasterTargetPolicy.isTrustedMaster(URI.create("http://seed:9095/websocket"), CURRENT,
                Collections.emptyList(), List.of("ws://seed:9095/websocket"), Collections.emptyList(), false));
        assertFalse(MasterTargetPolicy.isTrustedMaster(URI.create("ws://seed:9095/websocket"), null,
                Collections.emptyList(), List.of("ws://seed:9095/websocket"), Collections.emptyList(), false));
    }
}
