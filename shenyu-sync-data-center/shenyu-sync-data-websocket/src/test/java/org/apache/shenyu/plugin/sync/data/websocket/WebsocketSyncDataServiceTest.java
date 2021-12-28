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

import org.apache.shenyu.plugin.sync.data.websocket.client.ShenyuWebsocketClient;
import org.apache.shenyu.plugin.sync.data.websocket.config.WebsocketConfig;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.springframework.core.annotation.Order;

import java.util.List;
import java.util.concurrent.TimeUnit;

import static org.mockito.Mockito.verify;
import static org.powermock.api.mockito.PowerMockito.doNothing;
import static org.powermock.api.mockito.PowerMockito.when;
import static org.powermock.api.mockito.PowerMockito.whenNew;

/**
 * add test case for {@link WebsocketSyncDataService}.
 */

@PrepareForTest(WebsocketSyncDataService.class)
@RunWith(PowerMockRunner.class)
public class WebsocketSyncDataServiceTest {

    private static final String URLS = "ws://localhost:9095/websocket";

    @Mock
    private ShenyuWebsocketClient shenyuWebsocketClient;

    @Mock
    private WebsocketConfig websocketConfig;

    @Mock
    private PluginDataSubscriber pluginDataSubscriber;

    @Mock
    private List<MetaDataSubscriber> metaDataSubscribers;

    @Mock
    private List<AuthDataSubscriber> authDataSubscribers;

    private WebsocketSyncDataService websocketSyncDataService;

    @Before
    public void setUp() throws Exception {
        whenNew(ShenyuWebsocketClient.class).withAnyArguments().thenReturn(shenyuWebsocketClient);
        when(websocketConfig.getUrls()).thenReturn(URLS);
        when(shenyuWebsocketClient.connectBlocking(3000, TimeUnit.MILLISECONDS)).thenReturn(true);
        websocketSyncDataService = new WebsocketSyncDataService(websocketConfig, pluginDataSubscriber, metaDataSubscribers, authDataSubscribers);
        when(shenyuWebsocketClient.reconnectBlocking()).thenReturn(true);
        doNothing().when(shenyuWebsocketClient).sendPing();
        doNothing().when(shenyuWebsocketClient).close();
    }

    @Test
    @Order(1)
    public void testCreate() throws Exception {
        verify(shenyuWebsocketClient).connectBlocking(3000, TimeUnit.MILLISECONDS);
    }

    @Test
    @Order(2)
    public void testSendPing() throws Exception {
        TimeUnit.SECONDS.sleep(11);
        verify(shenyuWebsocketClient).sendPing();
    }

    @Test
    @Order(3)
    public void testClose() {
        websocketSyncDataService.close();
        verify(shenyuWebsocketClient).close();
    }

    @After
    public void close() {
        websocketSyncDataService.close();
    }
}
