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

import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.timer.Timer;
import org.apache.shenyu.common.timer.TimerTask;
import org.apache.shenyu.common.timer.WheelTimerFactory;
import org.apache.shenyu.plugin.sync.data.websocket.client.ShenyuWebsocketClient;
import org.apache.shenyu.plugin.sync.data.websocket.config.WebsocketConfig;
import org.apache.shenyu.sync.data.api.AiProxyApiKeyDataSubscriber;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.ProxySelectorDataSubscriber;
import org.junit.jupiter.api.Test;
import org.mockito.InOrder;
import org.mockito.MockedStatic;
import org.springframework.boot.autoconfigure.web.ServerProperties;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public final class WebsocketSyncDataServiceTest {

    @Test
    @SuppressWarnings("unchecked")
    public void testMasterCheckClosesRemovedClient() throws Exception {
        WebsocketSyncDataService websocketSyncDataService = createWebsocketSyncDataService();
        ShenyuWebsocketClient websocketClient = mock(ShenyuWebsocketClient.class);
        when(websocketClient.isOpen()).thenReturn(false);
        Field clientsField = WebsocketSyncDataService.class.getDeclaredField("clients");
        clientsField.setAccessible(true);
        List<ShenyuWebsocketClient> clients = (List<ShenyuWebsocketClient>) clientsField
                .get(websocketSyncDataService);
        clients.add(websocketClient);

        try {
            Method masterCheck = WebsocketSyncDataService.class.getDeclaredMethod("masterCheck");
            masterCheck.setAccessible(true);
            masterCheck.invoke(websocketSyncDataService);
            verify(websocketClient).nowClose();
            assertTrue(clients.isEmpty());
        } finally {
            websocketSyncDataService.close();
        }
    }

    @Test
    @SuppressWarnings("unchecked")
    public void testCloseShutsDownPrivateTimer() throws Exception {
        Timer sharedTimer = mock(Timer.class);
        Timer privateTimer = mock(Timer.class);
        try (MockedStatic<WheelTimerFactory> wheelTimerFactory = mockStatic(WheelTimerFactory.class)) {
            wheelTimerFactory.when(WheelTimerFactory::getSharedTimer).thenReturn(sharedTimer);
            wheelTimerFactory.when(WheelTimerFactory::newWheelTimer).thenReturn(privateTimer);
            final WebsocketSyncDataService websocketSyncDataService = createWebsocketSyncDataService();
            ShenyuWebsocketClient websocketClient = mock(ShenyuWebsocketClient.class);
            Field clientsField = WebsocketSyncDataService.class.getDeclaredField("clients");
            clientsField.setAccessible(true);
            List<ShenyuWebsocketClient> clients = (List<ShenyuWebsocketClient>) clientsField
                    .get(websocketSyncDataService);
            clients.add(websocketClient);
            TimerTask timerTask = mock(TimerTask.class);
            Field timerTaskField = WebsocketSyncDataService.class.getDeclaredField("timerTask");
            timerTaskField.setAccessible(true);
            timerTaskField.set(websocketSyncDataService, timerTask);

            websocketSyncDataService.close();
            Method masterCheck = WebsocketSyncDataService.class.getDeclaredMethod("masterCheck");
            masterCheck.setAccessible(true);
            masterCheck.invoke(websocketSyncDataService);
            websocketSyncDataService.close();

            InOrder closeOrder = inOrder(timerTask, websocketClient);
            closeOrder.verify(timerTask).cancel();
            closeOrder.verify(websocketClient).nowClose();
            verify(websocketClient, times(1)).nowClose();
            verify(timerTask, times(1)).cancel();
            verify(privateTimer, times(1)).shutdown();
            verify(sharedTimer, never()).shutdown();
            wheelTimerFactory.verify(WheelTimerFactory::getSharedTimer, never());
        }
    }

    @Test
    @SuppressWarnings("unchecked")
    public void testCloseShutsDownPrivateTimerWhenClientCloseFails() throws Exception {
        final Timer privateTimer = mock(Timer.class);
        try (MockedStatic<WheelTimerFactory> wheelTimerFactory = mockStatic(WheelTimerFactory.class)) {
            wheelTimerFactory.when(WheelTimerFactory::newWheelTimer).thenReturn(privateTimer);
            final WebsocketSyncDataService websocketSyncDataService = createWebsocketSyncDataService();
            final ShenyuWebsocketClient websocketClient = mock(ShenyuWebsocketClient.class);
            final IllegalStateException clientCloseException = new IllegalStateException("client close failed");
            doThrow(clientCloseException).when(websocketClient).nowClose();
            final Field clientsField = WebsocketSyncDataService.class.getDeclaredField("clients");
            clientsField.setAccessible(true);
            final List<ShenyuWebsocketClient> clients = (List<ShenyuWebsocketClient>) clientsField
                    .get(websocketSyncDataService);
            clients.add(websocketClient);

            assertThrows(IllegalStateException.class, websocketSyncDataService::close);

            verify(privateTimer).shutdown();
        }
    }

    private WebsocketSyncDataService createWebsocketSyncDataService() {
        WebsocketConfig websocketConfig = new WebsocketConfig();
        websocketConfig.setUrls(Collections.emptyList());
        return new WebsocketSyncDataService(
                websocketConfig,
                new ShenyuConfig(),
                mock(PluginDataSubscriber.class),
                Collections.<MetaDataSubscriber>emptyList(),
                Collections.<AuthDataSubscriber>emptyList(),
                Collections.<ProxySelectorDataSubscriber>emptyList(),
                Collections.<DiscoveryUpstreamDataSubscriber>emptyList(),
                Collections.<AiProxyApiKeyDataSubscriber>emptyList(),
                mock(ServerProperties.class));
    }
}
