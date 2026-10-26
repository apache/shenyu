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
import org.apache.shenyu.common.constant.Constants;
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
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.InOrder;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.net.URI;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockConstruction;
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

    @Test
    public void testConfigurableMasterCheckInterval() throws Exception {
        WebsocketConfig config = new WebsocketConfig();
        config.setUrls(Collections.emptyList());
        config.setMasterCheckInterval(15);
        WebsocketSyncDataService service = new WebsocketSyncDataService(
                config,
                new ShenyuConfig(),
                mock(PluginDataSubscriber.class),
                Collections.emptyList(),
                Collections.emptyList(),
                Collections.emptyList(),
                Collections.emptyList(),
                Collections.emptyList());
        try {
            Field timerTaskField = WebsocketSyncDataService.class.getDeclaredField("timerTask");
            timerTaskField.setAccessible(true);
            TimerTask timerTask = (TimerTask) timerTaskField.get(service);
            Method getDelayMs = TimerTask.class.getDeclaredMethod("getDelayMs");
            getDelayMs.setAccessible(true);
            long delayMs = (long) getDelayMs.invoke(timerTask);
            assertEquals(15000L, delayMs);
        } finally {
            service.close();
        }
    }

    @Test
    @SuppressWarnings("unchecked")
    public void testMasterCheckReseedsClientsWhenAllRemoved() throws Exception {
        WebsocketConfig config = new WebsocketConfig();
        config.setUrls(Collections.singletonList("ws://127.0.0.1:9095/websocket"));
        WebsocketSyncDataService service = new WebsocketSyncDataService(
                config,
                new ShenyuConfig(),
                mock(PluginDataSubscriber.class),
                Collections.emptyList(),
                Collections.emptyList(),
                Collections.emptyList(),
                Collections.emptyList(),
                Collections.emptyList());
        Field clientsField = WebsocketSyncDataService.class.getDeclaredField("clients");
        clientsField.setAccessible(true);
        List<ShenyuWebsocketClient> clients = (List<ShenyuWebsocketClient>) clientsField.get(service);
        clients.clear();
        ShenyuWebsocketClient closedClient = mock(ShenyuWebsocketClient.class);
        when(closedClient.isOpen()).thenReturn(false);
        clients.add(closedClient);
        Field boundPort = WebsocketSyncDataService.class.getDeclaredField("boundPort");
        boundPort.setAccessible(true);
        boundPort.setInt(service, 9195);

        try {
            Method masterCheck = WebsocketSyncDataService.class.getDeclaredMethod("masterCheck");
            masterCheck.setAccessible(true);
            masterCheck.invoke(service);
            verify(closedClient).nowClose();
            assertEquals(1, clients.size());
        } finally {
            service.close();
        }
    }

    @Test
    public void testOnMasterLostTriggersRediscovery() throws Exception {
        WebsocketSyncDataService service = createWebsocketSyncDataService();
        ShenyuWebsocketClient client = mock(ShenyuWebsocketClient.class);
        when(client.getURI()).thenReturn(URI.create("ws://localhost:9095"));

        try {
            Method onMasterLost = WebsocketSyncDataService.class.getDeclaredMethod("onMasterLost", ShenyuWebsocketClient.class);
            onMasterLost.setAccessible(true);
            onMasterLost.invoke(service, client);

            Field rediscoveryScheduledField = WebsocketSyncDataService.class.getDeclaredField("rediscoveryScheduled");
            rediscoveryScheduledField.setAccessible(true);
            AtomicBoolean rediscoveryScheduled = (AtomicBoolean) rediscoveryScheduledField.get(service);
            assertTrue(rediscoveryScheduled.get());
        } finally {
            service.close();
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    @SuppressWarnings("unchecked")
    void testDiscoveryCapabilityOnEveryNewConnection(final boolean compact) throws Exception {
        WebsocketSyncDataService service = createWebsocketSyncDataService();
        service.getWebsocketConfig().setCompactDiscoveryPayload(compact);
        service.getWebsocketConfig().setToken("existing-token");
        service.getWebsocketConfig().setAllowOrigin("https://gateway.example");
        try (MockedConstruction<ShenyuWebsocketClient> clients = mockConstruction(ShenyuWebsocketClient.class, (client, context) -> {
            Map<String, String> headers = (Map<String, String>) context.arguments().get(1);
            assertEquals(compact ? Constants.COMPACT_DISCOVERY_PAYLOAD_V1 : null, headers.get(Constants.X_SHENYU_DISCOVERY_PAYLOAD));
            assertEquals("existing-token", headers.get(Constants.X_SHENYU_SYNC_TOKEN));
            assertEquals("https://gateway.example", headers.get("Origin"));
        })) {
            Method createClient = WebsocketSyncDataService.class.getDeclaredMethod("createClient", String.class);
            createClient.setAccessible(true);
            createClient.invoke(service, "ws://first-admin/websocket");
            createClient.invoke(service, "ws://replacement-admin/websocket");
            assertEquals(2, clients.constructed().size());
        } finally {
            service.close();
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
                Collections.<AiProxyApiKeyDataSubscriber>emptyList());
    }
}
