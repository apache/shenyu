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

package org.apache.shenyu.protocol.tcp.connection;

import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Method;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test Case For {@link ActivityConnectionObserver}.
 */
public class ActivityConnectionObserverTest {

    private final ActivityConnectionObserver observer = new ActivityConnectionObserver("TestObserver");

    @Test
    public void testInWithIpv4Match() throws Exception {
        SocketAddress address = new InetSocketAddress("192.168.1.1", 8080);
        List<DiscoveryUpstreamData> removeList = Collections.singletonList(
                upstreamData("192.168.1.1:8080"));
        assertTrue(invokeIn(removeList, address));
    }

    @Test
    public void testInWithIpv4NoMatch() throws Exception {
        SocketAddress address = new InetSocketAddress("192.168.1.1", 8080);
        List<DiscoveryUpstreamData> removeList = Collections.singletonList(
                upstreamData("192.168.1.2:8080"));
        assertFalse(invokeIn(removeList, address));
    }

    @Test
    public void testInWithIpv6Match() throws Exception {
        SocketAddress address = new InetSocketAddress("2001:db8::1", 12345);
        List<DiscoveryUpstreamData> removeList = Collections.singletonList(
                upstreamData("[2001:db8::1]:12345"));
        assertTrue(invokeIn(removeList, address));
    }

    @Test
    public void testInWithIpv6NoMatch() throws Exception {
        SocketAddress address = new InetSocketAddress("2001:db8::1", 12345);
        List<DiscoveryUpstreamData> removeList = Collections.singletonList(
                upstreamData("[2001:db8::2]:12345"));
        assertFalse(invokeIn(removeList, address));
    }

    @Test
    public void testInWithUnresolvedIpv6Match() throws Exception {
        InetSocketAddress address = InetSocketAddress.createUnresolved("2001:db8::1", 12345);
        List<DiscoveryUpstreamData> removeList = Collections.singletonList(
                upstreamData("2001:db8::1:12345"));
        assertTrue(invokeIn(removeList, address));
    }

    @Test
    public void testInWithUnsupportedAddressType() throws Exception {
        SocketAddress customAddress = new SocketAddress() {
            private static final long serialVersionUID = 1L;
        };
        List<DiscoveryUpstreamData> removeList = Collections.singletonList(
                upstreamData("192.168.1.1:8080"));
        assertFalse(invokeIn(removeList, customAddress));
    }

    @Test
    public void testInWithEmptyRemoveList() throws Exception {
        SocketAddress address = new InetSocketAddress("192.168.1.1", 8080);
        List<DiscoveryUpstreamData> removeList = Collections.emptyList();
        assertFalse(invokeIn(removeList, address));
    }

    private boolean invokeIn(final List<DiscoveryUpstreamData> removeList, final SocketAddress socketAddress) throws Exception {
        Method method = ActivityConnectionObserver.class.getDeclaredMethod("in", List.class, SocketAddress.class);
        method.setAccessible(true);
        return (boolean) method.invoke(observer, removeList, socketAddress);
    }

    private DiscoveryUpstreamData upstreamData(final String url) {
        return DiscoveryUpstreamData.builder().url(url).build();
    }
}