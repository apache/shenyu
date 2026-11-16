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
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.WebsocketData;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.sync.data.websocket.client.ShenyuWebsocketClient;
import org.apache.shenyu.plugin.sync.data.websocket.config.WebsocketConfig;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.java_websocket.WebSocket;
import org.java_websocket.handshake.ClientHandshake;
import org.java_websocket.server.WebSocketServer;
import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.lang.reflect.Method;
import java.net.InetSocketAddress;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;

/**
 * Verify capability headers and full-payload fallback against a real loopback peer.
 */
class DiscoveryCapabilityHandshakeTest {

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    @Timeout(15)
    void advertisesOnlyWhenEnabledAndAcceptsLegacyPeer(final boolean compact) throws Exception {
        LegacyPeer server = new LegacyPeer();
        WebsocketConfig config = new WebsocketConfig();
        config.setUrls(List.of());
        config.setCompactDiscoveryPayload(compact);
        config.setToken("sync-token");
        BlockingQueue<DiscoverySyncData> applied = new LinkedBlockingQueue<>();
        DiscoveryUpstreamDataSubscriber subscriber = mock(DiscoveryUpstreamDataSubscriber.class);
        doAnswer(invocation -> {
            applied.add(invocation.getArgument(0));
            return null;
        }).when(subscriber).onSubscribe(any());
        WebsocketSyncDataService service = new WebsocketSyncDataService(config, new ShenyuConfig(), mock(PluginDataSubscriber.class),
                List.of(), List.of(), List.of(), List.of(subscriber), List.of());
        ShenyuWebsocketClient client = null;
        try {
            server.start();
            assertTrue(server.started.await(5, TimeUnit.SECONDS));
            Method create = WebsocketSyncDataService.class.getDeclaredMethod("createClient", String.class);
            create.setAccessible(true);
            client = (ShenyuWebsocketClient) create.invoke(service, "ws://127.0.0.1:" + server.getPort());
            ClientHandshake handshake = server.handshakes.poll(5, TimeUnit.SECONDS);
            assertNotNull(handshake);
            assertEquals(compact ? Constants.COMPACT_DISCOVERY_PAYLOAD_V1 : "", handshake.getFieldValue(Constants.X_SHENYU_DISCOVERY_PAYLOAD));
            assertEquals("sync-token", handshake.getFieldValue(Constants.X_SHENYU_SYNC_TOKEN));
            DiscoverySyncData received = applied.poll(5, TimeUnit.SECONDS);
            assertNotNull(received);
            assertEquals("legacy-binding", received.getDiscoveryHandlerId());
            assertEquals("legacy-selector", received.getSelectorId());
            assertTrue(received.getUpstreamDataList().isEmpty());
            assertNull(server.failure);
        } finally {
            if (Objects.nonNull(client)) {
                client.nowClose();
            }
            service.close();
            server.stop(1000);
        }
    }

    private static final class LegacyPeer extends WebSocketServer {

        private final CountDownLatch started = new CountDownLatch(1);

        private final BlockingQueue<ClientHandshake> handshakes = new LinkedBlockingQueue<>();

        private volatile Exception failure;

        private LegacyPeer() {
            super(new InetSocketAddress("127.0.0.1", 0));
        }

        @Override
        public void onOpen(final WebSocket socket, final ClientHandshake handshake) {
            handshakes.add(handshake);
        }

        @Override
        public void onMessage(final WebSocket socket, final String message) {
            if (DataEventTypeEnum.MYSELF.name().equals(message)) {
                DiscoverySyncData data = new DiscoverySyncData();
                data.setSelectorId("legacy-selector");
                data.setDiscoveryHandlerId("legacy-binding");
                data.setUpstreamDataList(List.of());
                socket.send(GsonUtils.getInstance().toJson(new WebsocketData<>("DISCOVER_UPSTREAM", "MYSELF", List.of(data))));
            }
        }

        @Override
        public void onClose(final WebSocket socket, final int code, final String reason, final boolean remote) {
        }

        @Override
        public void onError(final WebSocket socket, final Exception exception) {
            failure = exception;
        }

        @Override
        public void onStart() {
            started.countDown();
        }
    }
}
