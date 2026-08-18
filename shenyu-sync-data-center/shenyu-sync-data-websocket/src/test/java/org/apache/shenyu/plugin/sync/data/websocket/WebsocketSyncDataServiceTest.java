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
import org.apache.shenyu.plugin.sync.data.websocket.client.ShenyuWebsocketClient;
import org.apache.shenyu.plugin.sync.data.websocket.config.WebsocketConfig;
import org.apache.shenyu.sync.data.api.AiProxyApiKeyDataSubscriber;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.ProxySelectorDataSubscriber;
import org.junit.jupiter.api.Test;
import org.springframework.boot.autoconfigure.web.ServerProperties;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public final class WebsocketSyncDataServiceTest {

    @Test
    @SuppressWarnings("unchecked")
    public void testMasterCheckClosesRemovedClient() throws Exception {
        WebsocketConfig websocketConfig = new WebsocketConfig();
        websocketConfig.setUrls(Collections.emptyList());
        WebsocketSyncDataService websocketSyncDataService = new WebsocketSyncDataService(
                websocketConfig,
                new ShenyuConfig(),
                mock(PluginDataSubscriber.class),
                Collections.<MetaDataSubscriber>emptyList(),
                Collections.<AuthDataSubscriber>emptyList(),
                Collections.<ProxySelectorDataSubscriber>emptyList(),
                Collections.<DiscoveryUpstreamDataSubscriber>emptyList(),
                Collections.<AiProxyApiKeyDataSubscriber>emptyList(),
                mock(ServerProperties.class));
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
}
