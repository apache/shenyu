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

import org.apache.shenyu.plugin.base.cache.CommonProxySelectorDataSubscriber;
import org.apache.shenyu.protocol.tcp.BootstrapServer;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public final class TcpProxySelectorDataHandlerTest {

    private static final String FIRST_SELECTOR = "first";

    private static final String SECOND_SELECTOR = "second";

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
    public void testRefreshThroughSubscriber() {
        BootstrapServer firstServer = mock(BootstrapServer.class);
        BootstrapServer secondServer = mock(BootstrapServer.class);
        factory.cache(FIRST_SELECTOR, firstServer);
        factory.cache(SECOND_SELECTOR, secondServer);

        new CommonProxySelectorDataSubscriber(Collections.singletonList(new TcpProxySelectorDataHandler())).refresh();

        verify(firstServer).shutdown();
        verify(secondServer).shutdown();
        assertFalse(factory.inCache(FIRST_SELECTOR));
        assertFalse(factory.inCache(SECOND_SELECTOR));
    }

    @Test
    public void testRefreshContinuesWhenShutdownFails() {
        BootstrapServer failingServer = mock(BootstrapServer.class);
        BootstrapServer secondServer = mock(BootstrapServer.class);
        doThrow(new IllegalStateException("shutdown failed")).when(failingServer).shutdown();
        factory.cache(FIRST_SELECTOR, failingServer);
        factory.cache(SECOND_SELECTOR, secondServer);

        assertDoesNotThrow(() -> new TcpProxySelectorDataHandler().refresh());

        verify(failingServer).shutdown();
        verify(secondServer).shutdown();
        assertFalse(factory.inCache(FIRST_SELECTOR));
        assertFalse(factory.inCache(SECOND_SELECTOR));
    }

    @Test
    public void testRemoveProxySelector() {
        BootstrapServer bootstrapServer = mock(BootstrapServer.class);
        factory.cache(FIRST_SELECTOR, bootstrapServer);
        TcpProxySelectorDataHandler handler = new TcpProxySelectorDataHandler();

        handler.removeProxySelector(FIRST_SELECTOR);

        verify(bootstrapServer).shutdown();
        assertFalse(factory.inCache(FIRST_SELECTOR));
        assertDoesNotThrow(() -> handler.removeProxySelector(FIRST_SELECTOR));
    }
}
