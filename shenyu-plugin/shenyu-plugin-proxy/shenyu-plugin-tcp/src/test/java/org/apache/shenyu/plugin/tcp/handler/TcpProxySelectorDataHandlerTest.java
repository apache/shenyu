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

package org.apache.shenyu.plugin.tcp.handler;

import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.plugin.base.cache.CommonProxySelectorDataSubscriber;
import org.apache.shenyu.protocol.tcp.BootstrapServer;
import org.apache.shenyu.protocol.tcp.TcpServerConfiguration;
import org.apache.shenyu.protocol.tcp.UpstreamProvider;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.net.BindException;
import java.net.ServerSocket;
import java.util.Collections;
import java.util.List;
import java.util.Properties;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public final class TcpProxySelectorDataHandlerTest {

    private static final String FIRST_SELECTOR = "first";

    private static final String SECOND_SELECTOR = "second";

    private static final String FIRST_SELECTOR_ID = "first-id";

    private static final String SECOND_SELECTOR_ID = "second-id";

    private final TcpBootstrapFactory factory = TcpBootstrapFactory.getSingleton();

    @BeforeEach
    public void setUp() {
        factory.clearCache();
    }

    @AfterEach
    public void tearDown() {
        factory.clearCache();
    }

    @Test
    public void shouldApplyChangedPortThroughSubscriberAndPreserveUpstreams() throws IOException {
        ProxySelectorData data = new ProxySelectorData();
        data.setPluginName("tcp");
        data.setName(FIRST_SELECTOR);
        data.setForwardPort(freePort());
        data.setProps(new Properties());
        CommonProxySelectorDataSubscriber subscriber = new CommonProxySelectorDataSubscriber(Collections.singletonList(new TcpProxySelectorDataHandler()));
        subscriber.onSubscribe(data);
        BootstrapServer original = factory.getCache(FIRST_SELECTOR);
        subscriber.onSubscribe(data);
        assertSame(original, factory.getCache(FIRST_SELECTOR));
        UpstreamProvider upstreamProvider = UpstreamProvider.getSingleton();
        List<DiscoveryUpstreamData> upstreams = Collections.singletonList(DiscoveryUpstreamData.builder().url("127.0.0.1:10001").build());
        upstreamProvider.createUpstreams(FIRST_SELECTOR, upstreams);
        upstreamProvider.registerSelector(FIRST_SELECTOR_ID, FIRST_SELECTOR);
        int previousPort = data.getForwardPort();
        data.setForwardPort(freePort());

        subscriber.onSubscribe(data);

        assertNotSame(original, factory.getCache(FIRST_SELECTOR));
        assertSame(upstreams, upstreamProvider.provide(FIRST_SELECTOR));
        assertEquals(FIRST_SELECTOR, upstreamProvider.getSelectorName(FIRST_SELECTOR_ID));
        try (ServerSocket releasedPort = new ServerSocket(previousPort)) {
            assertEquals(previousPort, releasedPort.getLocalPort());
        }
        assertThrows(BindException.class, () -> {
            try (ServerSocket occupiedPort = new ServerSocket(data.getForwardPort())) {
                assertEquals(data.getForwardPort().intValue(), occupiedPort.getLocalPort());
            }
        });
    }

    @Test
    public void testRefreshThroughSubscriber() {
        BootstrapServer firstServer = mock(BootstrapServer.class);
        BootstrapServer secondServer = mock(BootstrapServer.class);
        factory.cache(configuration(FIRST_SELECTOR), firstServer);
        factory.cache(configuration(SECOND_SELECTOR), secondServer);
        UpstreamProvider.getSingleton().createUpstreams(FIRST_SELECTOR, Collections.emptyList());
        UpstreamProvider.getSingleton().createUpstreams(SECOND_SELECTOR, Collections.emptyList());
        UpstreamProvider.getSingleton().registerSelector(FIRST_SELECTOR_ID, FIRST_SELECTOR);
        UpstreamProvider.getSingleton().registerSelector(SECOND_SELECTOR_ID, SECOND_SELECTOR);

        new CommonProxySelectorDataSubscriber(Collections.singletonList(new TcpProxySelectorDataHandler())).refresh();

        verify(firstServer).shutdown();
        verify(secondServer).shutdown();
        assertFalse(factory.inCache(FIRST_SELECTOR));
        assertFalse(factory.inCache(SECOND_SELECTOR));
        assertFalse(UpstreamProvider.getSingleton().inCache(FIRST_SELECTOR));
        assertFalse(UpstreamProvider.getSingleton().inCache(SECOND_SELECTOR));
        assertNull(UpstreamProvider.getSingleton().getSelectorName(FIRST_SELECTOR_ID));
        assertNull(UpstreamProvider.getSingleton().getSelectorName(SECOND_SELECTOR_ID));
    }

    @Test
    public void testRefreshContinuesWhenShutdownFails() {
        BootstrapServer failingServer = mock(BootstrapServer.class);
        BootstrapServer secondServer = mock(BootstrapServer.class);
        doThrow(new IllegalStateException("shutdown failed")).when(failingServer).shutdown();
        factory.cache(configuration(FIRST_SELECTOR), failingServer);
        factory.cache(configuration(SECOND_SELECTOR), secondServer);
        UpstreamProvider.getSingleton().createUpstreams(FIRST_SELECTOR, Collections.emptyList());
        UpstreamProvider.getSingleton().registerSelector(FIRST_SELECTOR_ID, FIRST_SELECTOR);

        assertDoesNotThrow(() -> new TcpProxySelectorDataHandler().refresh());

        verify(failingServer).shutdown();
        verify(secondServer).shutdown();
        assertFalse(factory.inCache(FIRST_SELECTOR));
        assertFalse(factory.inCache(SECOND_SELECTOR));
        assertFalse(UpstreamProvider.getSingleton().inCache(FIRST_SELECTOR));
        assertNull(UpstreamProvider.getSingleton().getSelectorName(FIRST_SELECTOR_ID));
    }

    @Test
    public void testRemoveProxySelector() {
        BootstrapServer bootstrapServer = mock(BootstrapServer.class);
        factory.cache(configuration(FIRST_SELECTOR), bootstrapServer);
        UpstreamProvider.getSingleton().createUpstreams(FIRST_SELECTOR, Collections.emptyList());
        UpstreamProvider.getSingleton().registerSelector(FIRST_SELECTOR_ID, FIRST_SELECTOR);
        TcpProxySelectorDataHandler handler = new TcpProxySelectorDataHandler();

        handler.removeProxySelector(FIRST_SELECTOR);

        verify(bootstrapServer).shutdown();
        assertFalse(factory.inCache(FIRST_SELECTOR));
        assertFalse(UpstreamProvider.getSingleton().inCache(FIRST_SELECTOR));
        assertNull(UpstreamProvider.getSingleton().getSelectorName(FIRST_SELECTOR_ID));
        assertDoesNotThrow(() -> handler.removeProxySelector(FIRST_SELECTOR));
    }

    private static TcpServerConfiguration configuration(final String selectorName) {
        TcpServerConfiguration configuration = new TcpServerConfiguration();
        configuration.setPluginSelectorName(selectorName);
        return configuration;
    }

    private static int freePort() throws IOException {
        try (ServerSocket socket = new ServerSocket(0)) {
            return socket.getLocalPort();
        }
    }
}
