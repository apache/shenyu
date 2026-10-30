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
import org.apache.shenyu.common.timer.WheelTimerFactory;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.sync.data.websocket.client.ShenyuWebsocketClient;
import org.apache.shenyu.plugin.sync.data.websocket.config.WebsocketConfig;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.java_websocket.WebSocket;
import org.java_websocket.handshake.ClientHandshake;
import org.java_websocket.server.WebSocketServer;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.springframework.boot.web.servlet.context.ServletWebServerApplicationContext;
import org.springframework.boot.web.servlet.context.ServletWebServerInitializedEvent;
import org.springframework.boot.web.server.WebServer;

import java.lang.reflect.Field;
import java.net.InetSocketAddress;
import java.net.URI;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Exercises master discovery through real sockets and manually triggered checks.
 */
class WebsocketMasterRedirectTest {

    private final List<TestServer> servers = new ArrayList<>();

    private MockedStatic<WheelTimerFactory> timers;

    private WebsocketSyncDataService service;

    private final CountDownLatch applied = new CountDownLatch(1);

    @BeforeEach
    void setUp() {
        timers = mockStatic(WheelTimerFactory.class);
        timers.when(WheelTimerFactory::newWheelTimer).thenReturn(mock(Timer.class));
        timers.when(WheelTimerFactory::getSharedTimer).thenReturn(mock(Timer.class));
    }

    @AfterEach
    void tearDown() throws InterruptedException {
        try {
            if (Objects.nonNull(service)) {
                service.close();
            }
            for (TestServer server : servers) {
                server.stop(1000);
            }
        } finally {
            timers.close();
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void testRedirectsFollowerConnectionsToOneMaster(final boolean multipleFollowers) throws Exception {
        TestServer master = startServer(true);
        TestServer follower = startServer(false);
        follower.masterUrl = master.url();
        final List<String> seeds = new ArrayList<>(Collections.singletonList(follower.url()));
        if (multipleFollowers) {
            TestServer other = startServer(false);
            other.masterUrl = master.url();
            seeds.add(other.url());
        }
        createService(seeds, "127.0.0.1:" + master.getPort());
        awaitModes();

        check();

        assertTrue(applied.await(5, TimeUnit.SECONDS), "Configuration should arrive from the advertised master");
        await().atMost(Duration.ofSeconds(5)).until(() -> clients().size() == 1 && clients().get(0).isConnectedToMaster());
        assertEquals(URI.create(master.url()), clients().get(0).getURI());
        assertEquals(1, master.opens.get());
        assertTrue(follower.closed.await(5, TimeUnit.SECONDS));
        assertEquals("test-token", master.handshake.getFieldValue(Constants.X_SHENYU_SYNC_TOKEN));
        assertEquals("https://gateway.example", master.handshake.getFieldValue("Origin"));
        assertEquals("cluster-test", master.handshake.getFieldValue(Constants.SHENYU_NAMESPACE_ID));
        assertEquals("9195", master.handshake.getFieldValue(Constants.CLIENT_PORT_NAME));
        assertEquals(seeds, service.getWebsocketConfig().getUrls());
        check();
        assertEquals(1, master.opens.get());
        assertEquals(1, follower.opens.get());
    }

    @Test
    void testKeepsConnectionWhileRunningModeReplyIsPending() throws Exception {
        TestServer server = startServer(true);
        server.replyMode = false;
        createService(Collections.singletonList(server.url()));
        ShenyuWebsocketClient client = clients().get(0);

        check();

        assertEquals(1, clients().size());
        assertSame(client, clients().get(0));
        assertTrue(client.isOpen());
        server.sendMode();
        awaitModes();
        check();
        assertSame(client, clients().get(0));
    }

    @ParameterizedTest
    @NullAndEmptySource
    @ValueSource(strings = {"null", "/websocket", "http://admin/websocket", "ws://[invalid"})
    void testInvalidMasterAddressFallsBackToSeeds(final String address) throws Exception {
        createService(Collections.emptyList());
        ShenyuWebsocketClient follower = mock(ShenyuWebsocketClient.class);
        when(follower.isOpen()).thenReturn(true);
        when(follower.getRunningMode()).thenReturn("CLUSTER");
        when(follower.getMasterUrl()).thenReturn(address);
        clients().add(follower);
        try (MockedConstruction<ShenyuWebsocketClient> constructed = mockConstruction(ShenyuWebsocketClient.class)) {
            check();
            assertTrue(clients().isEmpty());
            assertTrue(constructed.constructed().isEmpty());
            verify(follower).nowClose();
        }
    }

    @Test
    void testReusesExistingMasterConnection() throws Exception {
        createService(Collections.emptyList());
        ShenyuWebsocketClient follower = mock(ShenyuWebsocketClient.class);
        when(follower.isOpen()).thenReturn(true);
        when(follower.getRunningMode()).thenReturn("CLUSTER");
        when(follower.getMasterUrl()).thenReturn("ws://master:9095/websocket");
        when(follower.getURI()).thenReturn(URI.create("ws://follower:9095/websocket"));
        ShenyuWebsocketClient master = mock(ShenyuWebsocketClient.class);
        when(master.isOpen()).thenReturn(true);
        when(master.getRunningMode()).thenReturn("CLUSTER");
        when(master.isConnectedToMaster()).thenReturn(true);
        when(master.getURI()).thenReturn(URI.create("ws://master:9095/websocket"));
        clients().add(follower);
        clients().add(master);
        try (MockedConstruction<ShenyuWebsocketClient> constructed = mockConstruction(ShenyuWebsocketClient.class)) {
            check();
            assertEquals(Collections.singletonList(master), clients());
            assertTrue(constructed.constructed().isEmpty());
        }
    }

    @Test
    void testClosedRedirectConnectionReturnsToConfiguredSeed() throws Exception {
        createService(Collections.emptyList());
        service.getWebsocketConfig().setUrls(Collections.singletonList("ws://seed:9095/websocket"));
        ShenyuWebsocketClient failedMaster = mock(ShenyuWebsocketClient.class);
        clients().add(failedMaster);
        check();
        verify(failedMaster).nowClose();
        try (MockedConstruction<ShenyuWebsocketClient> constructed = mockConstruction(ShenyuWebsocketClient.class, (client, context) ->
                assertEquals(URI.create("ws://seed:9095/websocket"), context.arguments().get(0)))) {
            check();
            assertEquals(1, constructed.constructed().size());
        }
    }

    private void createService(final List<String> seeds, final String... trustedMasterHosts) {
        WebsocketConfig config = new WebsocketConfig();
        config.setUrls(seeds);
        config.setToken("test-token");
        config.setAllowOrigin("https://gateway.example");
        config.setTrustedMasterHosts(List.of(trustedMasterHosts));
        ShenyuConfig shenyu = new ShenyuConfig();
        shenyu.setNamespace("cluster-test");
        PluginDataSubscriber subscriber = mock(PluginDataSubscriber.class);
        doAnswer(invocation -> {
            applied.countDown();
            return null;
        }).when(subscriber).onSubscribe(any());
        service = new WebsocketSyncDataService(config, shenyu, subscriber, Collections.emptyList(), Collections.emptyList(),
                Collections.emptyList(), Collections.emptyList(), Collections.emptyList());
        WebServer webServer = mock(WebServer.class);
        when(webServer.getPort()).thenReturn(9195);
        ServletWebServerApplicationContext applicationContext = mock(ServletWebServerApplicationContext.class);
        when(applicationContext.getServerNamespace()).thenReturn(null);
        service.onWebServerInitialized(new ServletWebServerInitializedEvent(webServer, applicationContext));
    }

    private TestServer startServer(final boolean master) throws InterruptedException {
        TestServer server = new TestServer(master);
        servers.add(server);
        server.start();
        assertTrue(server.started.await(5, TimeUnit.SECONDS));
        return server;
    }

    private void awaitModes() {
        await().atMost(Duration.ofSeconds(5)).until(() -> clients().stream().allMatch(client -> Objects.nonNull(client.getRunningMode())));
    }

    @SuppressWarnings("unchecked")
    private List<ShenyuWebsocketClient> clients() throws ReflectiveOperationException {
        Field field = WebsocketSyncDataService.class.getDeclaredField("clients");
        field.setAccessible(true);
        return (List<ShenyuWebsocketClient>) field.get(service);
    }

    private void check() throws ReflectiveOperationException {
        java.lang.reflect.Method method = WebsocketSyncDataService.class.getDeclaredMethod("masterCheck");
        method.setAccessible(true);
        method.invoke(service);
    }

    private static final class TestServer extends WebSocketServer {

        private final boolean master;

        private final CountDownLatch started = new CountDownLatch(1);

        private final CountDownLatch closed = new CountDownLatch(1);

        private final AtomicInteger opens = new AtomicInteger();

        private volatile String masterUrl;

        private volatile boolean replyMode = true;

        private volatile WebSocket connection;

        private volatile ClientHandshake handshake;

        private TestServer(final boolean master) {
            super(new InetSocketAddress("127.0.0.1", 0));
            this.master = master;
        }

        private String url() {
            return "ws://127.0.0.1:" + getPort() + "/websocket";
        }

        private void sendMode() {
            connection.send(GsonUtils.getInstance().toJson(Map.of("eventType", "RUNNING_MODE", "runningMode", "CLUSTER",
                    "isMaster", master, "masterUrl", master ? url() : masterUrl)));
        }

        @Override
        public void onOpen(final WebSocket socket, final ClientHandshake clientHandshake) {
            connection = socket;
            handshake = clientHandshake;
            opens.incrementAndGet();
        }

        @Override
        public void onClose(final WebSocket socket, final int code, final String reason, final boolean remote) {
            closed.countDown();
        }

        @Override
        public void onMessage(final WebSocket socket, final String message) {
            if ("RUNNING_MODE".equals(message) && replyMode) {
                sendMode();
            } else if ("MYSELF".equals(message) && master) {
                socket.send("{\"groupType\":\"PLUGIN\",\"eventType\":\"CREATE\",\"data\":[{\"id\":\"divide\",\"name\":\"divide\",\"enabled\":true}]}");
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
