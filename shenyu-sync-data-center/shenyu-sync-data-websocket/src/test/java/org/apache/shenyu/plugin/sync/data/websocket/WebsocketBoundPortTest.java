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
import org.apache.shenyu.common.utils.BoundServerPort;
import org.apache.shenyu.plugin.sync.data.websocket.client.ShenyuWebsocketClient;
import org.apache.shenyu.plugin.sync.data.websocket.config.WebsocketConfig;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.springframework.boot.web.embedded.tomcat.TomcatServletWebServerFactory;
import org.springframework.boot.web.server.WebServer;
import org.springframework.boot.web.servlet.context.ServletWebServerApplicationContext;
import org.springframework.boot.web.servlet.context.ServletWebServerInitializedEvent;

import java.lang.reflect.Field;
import java.net.ServerSocket;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.when;

/**
 * Websocket clients report the port Tomcat actually bound, including {@code server.port=0}.
 */
class WebsocketBoundPortTest {

    @Test
    void waitsForTheBoundPortAndKeepsDistinctIdentities() throws Exception {
        final WebServer ephemeral = start(0);
        final WebServer other = start(0);
        int fixedPort;
        try (ServerSocket reserved = new ServerSocket(0)) {
            fixedPort = reserved.getLocalPort();
        }
        final WebServer fixed = start(fixedPort);
        final WebServer management = start(0);
        List<Integer> reportedPorts = new ArrayList<>();
        try (MockedConstruction<ShenyuWebsocketClient> clients = mockConstruction(ShenyuWebsocketClient.class,
                (mock, context) -> reportedPorts.add((Integer) context.arguments().get(9)))) {
            final WebsocketSyncDataService first = service();
            assertTrue(clientList(first).isEmpty());
            publish(first, management, BoundServerPort.MANAGEMENT_NAMESPACE);
            publish(first, ephemeral, null);
            final WebsocketSyncDataService second = service();
            publish(second, other, null);
            WebsocketSyncDataService explicit = service();
            publish(explicit, fixed, null);
            assertEquals(1, clientList(first).size());
            assertEquals(ephemeral.getPort(), reportedPorts.get(0));
            assertEquals(other.getPort(), reportedPorts.get(1));
            assertEquals(fixedPort, reportedPorts.get(2));
            assertNotEquals(ephemeral.getPort(), other.getPort());
            assertNotEquals(management.getPort(), reportedPorts.get(0));
            first.close();
            publish(first, ephemeral, null);
            assertEquals(3, clients.constructed().size());
            second.close();
            explicit.close();
        } finally {
            ephemeral.stop();
            other.stop();
            fixed.stop();
            management.stop();
        }
    }

    @SuppressWarnings("unchecked")
    private List<ShenyuWebsocketClient> clientList(final WebsocketSyncDataService service) throws Exception {
        Field field = WebsocketSyncDataService.class.getDeclaredField("clients");
        field.setAccessible(true);
        return (List<ShenyuWebsocketClient>) field.get(service);
    }

    private void publish(final WebsocketSyncDataService service, final WebServer server, final String namespace) {
        ServletWebServerApplicationContext applicationContext = mock(ServletWebServerApplicationContext.class);
        when(applicationContext.getServerNamespace()).thenReturn(namespace);
        service.onWebServerInitialized(new ServletWebServerInitializedEvent(server, applicationContext));
    }

    private WebsocketSyncDataService service() {
        WebsocketConfig config = new WebsocketConfig();
        config.setUrls(Collections.singletonList("ws://127.0.0.1:1/websocket"));
        ShenyuConfig shenyuConfig = new ShenyuConfig();
        shenyuConfig.setNamespace("shenyu");
        return new WebsocketSyncDataService(config, shenyuConfig, mock(PluginDataSubscriber.class),
                Collections.emptyList(), Collections.emptyList(), Collections.emptyList(), Collections.emptyList(), Collections.emptyList());
    }

    private WebServer start(final int port) {
        WebServer server = new TomcatServletWebServerFactory(port).getWebServer();
        server.start();
        return server;
    }
}
