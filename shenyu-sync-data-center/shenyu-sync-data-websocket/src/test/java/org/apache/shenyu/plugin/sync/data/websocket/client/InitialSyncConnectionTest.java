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

package org.apache.shenyu.plugin.sync.data.websocket.client;

import org.apache.shenyu.common.dto.WebsocketSyncFrame;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.InitialSyncApplication;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.java_websocket.WebSocket;
import org.java_websocket.handshake.ClientHandshake;
import org.java_websocket.server.WebSocketServer;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.net.InetSocketAddress;
import java.net.URI;
import java.time.Duration;
import java.util.Collections;
import java.util.Objects;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;

/**
 * Exercises initial synchronization over a real socket, with deferred subscriber application.
 */
class InitialSyncConnectionTest {

    @Test
    void testDeferredApplicationAndDisconnectOverSocket() throws Exception {
        TestServer server = new TestServer();
        server.start();
        assertTrue(server.started.await(5, TimeUnit.SECONDS));
        AtomicBoolean ready = new AtomicBoolean();
        CompletableFuture<Void> application = new CompletableFuture<>();
        PluginDataSubscriber subscriber = mock(PluginDataSubscriber.class);
        doAnswer(invocation -> {
            InitialSyncApplication.register(application);
            return null;
        }).when(subscriber).onSubscribe(any());
        ShenyuWebsocketClient client = null;
        try {
            client = new ShenyuWebsocketClient(URI.create("ws://127.0.0.1:" + server.getPort()), Collections.emptyMap(), subscriber,
                    Collections.emptyList(), Collections.emptyList(), Collections.emptyList(), Collections.emptyList(), Collections.emptyList(),
                    "default", 9195, ready);
            String request = server.requests.poll(5, TimeUnit.SECONDS);
            assertNotNull(request);
            assertTrue(request.startsWith(WebsocketSyncFrame.REQUEST_PREFIX));
            assertFalse(ready.get());
            String id = request.substring(WebsocketSyncFrame.REQUEST_PREFIX.length());
            String payload = "{\"groupType\":\"PLUGIN\",\"eventType\":\"MYSELF\",\"data\":[{\"id\":\"divide\",\"name\":\"divide\",\"enabled\":true}]}";
            server.connection.send(GsonUtils.getInstance().toJson(new WebsocketSyncFrame(id, 0, payload)));
            server.connection.send(GsonUtils.getInstance().toJson(new WebsocketSyncFrame(id, 1, null)));
            Field stateField = ShenyuWebsocketClient.class.getDeclaredField("initialSyncState");
            stateField.setAccessible(true);
            InitialSyncState state = (InitialSyncState) stateField.get(client);
            Field ended = InitialSyncState.class.getDeclaredField("ended");
            ended.setAccessible(true);
            await().atMost(Duration.ofSeconds(5)).until(() -> {
                synchronized (state) {
                    return ended.getBoolean(state);
                }
            });
            assertFalse(ready.get());
            application.complete(null);
            await().atMost(Duration.ofSeconds(5)).untilTrue(ready);
            server.connection.close();
            final ShenyuWebsocketClient connectedClient = client;
            await().atMost(Duration.ofSeconds(5)).until(() -> !connectedClient.isOpen());
            assertTrue(ready.get());
        } finally {
            if (Objects.nonNull(client)) {
                client.nowClose();
            }
            server.stop(1000);
        }
    }

    private static final class TestServer extends WebSocketServer {

        private final CountDownLatch started = new CountDownLatch(1);

        private final BlockingQueue<String> requests = new LinkedBlockingQueue<>();

        private volatile WebSocket connection;

        private TestServer() {
            super(new InetSocketAddress("127.0.0.1", 0));
        }

        @Override
        public void onOpen(final WebSocket socket, final ClientHandshake handshake) {
            connection = socket;
        }

        @Override
        public void onClose(final WebSocket socket, final int code, final String reason, final boolean remote) {
        }

        @Override
        public void onMessage(final WebSocket socket, final String message) {
            if (message.startsWith(WebsocketSyncFrame.REQUEST_PREFIX)) {
                requests.add(message);
            }
        }

        @Override
        public void onError(final WebSocket socket, final Exception exception) {
            throw new IllegalStateException(exception);
        }

        @Override
        public void onStart() {
            started.countDown();
        }
    }
}
