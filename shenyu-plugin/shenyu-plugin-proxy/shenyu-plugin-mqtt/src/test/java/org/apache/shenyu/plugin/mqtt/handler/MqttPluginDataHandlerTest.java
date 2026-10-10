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

package org.apache.shenyu.plugin.mqtt.handler;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import io.netty.channel.ChannelFuture;
import io.netty.channel.EventLoopGroup;
import io.netty.util.ResourceLeakDetector;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.protocol.mqtt.MqttBootstrapServer;
import org.apache.shenyu.protocol.mqtt.MqttContext;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.lang.management.ManagementFactory;
import java.lang.management.ThreadInfo;
import java.lang.reflect.Field;
import java.net.BindException;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.concurrent.Callable;
import java.util.concurrent.FutureTask;
import java.util.concurrent.TimeUnit;
import java.util.stream.Stream;

import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test case for {@link MqttPluginDataHandler}.
 */
public class MqttPluginDataHandlerTest {

    private static final String CONFIG = "{\"port\":0,\"workerGroupThreadCount\":1}";

    private MqttPluginDataHandler mqttPluginDataHandlerUnderTest;

    private MqttBootstrapServer server;

    @BeforeEach
    public void setUp() {
        mqttPluginDataHandlerUnderTest = new MqttPluginDataHandler();
        server = getField(mqttPluginDataHandlerUnderTest, "server", MqttBootstrapServer.class);
        new MqttContext().setBossGroupThreadCount(1);
    }

    @AfterEach
    public void tearDown() {
        server.shutdown();
        new MqttContext().setBossGroupThreadCount(0);
        ResourceLeakDetector.setLevel(ResourceLeakDetector.Level.DISABLED);
    }

    @Test
    public void testEnableAndDisableWithoutConfig() {
        mqttPluginDataHandlerUnderTest.handlerPlugin(pluginData(true, CONFIG));
        final ChannelFuture future = getField(server, "future", ChannelFuture.class);
        final EventLoopGroup bossGroup = getField(server, "bossGroup", EventLoopGroup.class);
        final EventLoopGroup workerGroup = getField(server, "workerGroup", EventLoopGroup.class);
        assertTrue(future.channel().isActive());

        mqttPluginDataHandlerUnderTest.handlerPlugin(pluginData(false, null));

        assertFalse(future.channel().isActive());
        assertTrue(bossGroup.isTerminated());
        assertTrue(workerGroup.isTerminated());
        assertNull(getField(server, "future", ChannelFuture.class));
        assertDoesNotThrow(() -> mqttPluginDataHandlerUnderTest.handlerPlugin(pluginData(false, null)));
    }

    @Test
    public void testEquivalentEnabledUpdatesKeepResources() {
        mqttPluginDataHandlerUnderTest.handlerPlugin(pluginData(true, CONFIG));
        final ChannelFuture future = getField(server, "future", ChannelFuture.class);
        final EventLoopGroup bossGroup = getField(server, "bossGroup", EventLoopGroup.class);
        final EventLoopGroup workerGroup = getField(server, "workerGroup", EventLoopGroup.class);

        mqttPluginDataHandlerUnderTest.handlerPlugin(pluginData(true, CONFIG));
        // Defaults, JSON order, level case and the unused boss setting do not change the effective configuration.
        mqttPluginDataHandlerUnderTest.handlerPlugin(pluginData(true, "{\"leakDetectorLevel\":\"disabled\",\"password\":\"shenyu\","
                + "\"userName\":\"shenyu\",\"maxPayloadSize\":65536,\"bossGroupThreadCount\":2,\"workerGroupThreadCount\":1,\"port\":0}"));

        assertSame(future, getField(server, "future", ChannelFuture.class));
        assertSame(bossGroup, getField(server, "bossGroup", EventLoopGroup.class));
        assertSame(workerGroup, getField(server, "workerGroup", EventLoopGroup.class));
        assertTrue(future.channel().isActive());
    }

    @ParameterizedTest
    @MethodSource("configurationChanges")
    public void testChangedConfigurationReplacesResources(final String setting, final String value) throws Exception {
        JsonObject initialConfig = JsonParser.parseString(CONFIG).getAsJsonObject();
        initialConfig.addProperty("port", unusedPort());
        mqttPluginDataHandlerUnderTest.handlerPlugin(pluginData(true, initialConfig.toString()));
        final ChannelFuture oldFuture = getField(server, "future", ChannelFuture.class);
        final EventLoopGroup oldBoss = getField(server, "bossGroup", EventLoopGroup.class);
        final EventLoopGroup oldWorker = getField(server, "workerGroup", EventLoopGroup.class);
        String changedValue = "port".equals(setting) ? Integer.toString(unusedPort()) : value;
        JsonObject changedConfig = initialConfig.deepCopy();
        changedConfig.add(setting, JsonParser.parseString(changedValue));

        mqttPluginDataHandlerUnderTest.handlerPlugin(pluginData(true, changedConfig.toString()));

        assertFalse(oldFuture.channel().isActive());
        assertTrue(oldBoss.isTerminated());
        assertTrue(oldWorker.isTerminated());
        final ChannelFuture replacement = getField(server, "future", ChannelFuture.class);
        assertNotSame(oldFuture, replacement);
        assertTrue(replacement.channel().isActive());
    }

    @Test
    public void testEncryptedPasswordUpdatesKeepResources() {
        String encryptedConfig = "{\"port\":0,\"workerGroupThreadCount\":1,\"password\":\"secret\",\"isEncryptPassword\":true,\"encryptMode\":\"MD5\"}";
        mqttPluginDataHandlerUnderTest.handlerPlugin(pluginData(true, encryptedConfig));
        final ChannelFuture future = getField(server, "future", ChannelFuture.class);
        String effectivePassword = new MqttContext().getPassword();

        mqttPluginDataHandlerUnderTest.handlerPlugin(pluginData(true, encryptedConfig));
        mqttPluginDataHandlerUnderTest.handlerPlugin(pluginData(true, "{\"port\":0,\"workerGroupThreadCount\":1,\"password\":\"" + effectivePassword + "\"}"));

        assertSame(future, getField(server, "future", ChannelFuture.class));
        assertTrue(MqttContext.isValid("shenyu", effectivePassword.getBytes(StandardCharsets.UTF_8)));
    }

    @Test
    public void testChangedPortOnDisableClosesRunningListener() throws Exception {
        mqttPluginDataHandlerUnderTest.handlerPlugin(pluginData(true, CONFIG));
        final ChannelFuture future = getField(server, "future", ChannelFuture.class);

        mqttPluginDataHandlerUnderTest.handlerPlugin(pluginData(false, "{\"port\":" + unusedPort() + "}"));

        assertFalse(future.channel().isActive());
        assertNull(getField(server, "future", ChannelFuture.class));
    }

    @Test
    public void testFailedReplacementCanRetryPreviousConfiguration() throws Exception {
        mqttPluginDataHandlerUnderTest.handlerPlugin(pluginData(true, CONFIG));
        final ChannelFuture previous = getField(server, "future", ChannelFuture.class);
        try (ServerSocket occupied = new ServerSocket(0)) {
            String failedConfig = "{\"port\":" + occupied.getLocalPort() + ",\"workerGroupThreadCount\":1}";
            assertThrows(BindException.class, () -> mqttPluginDataHandlerUnderTest.handlerPlugin(pluginData(true, failedConfig)));

            assertFalse(previous.channel().isActive());
            assertNull(getField(server, "future", ChannelFuture.class));
        }

        mqttPluginDataHandlerUnderTest.handlerPlugin(pluginData(true, CONFIG));

        assertTrue(getField(server, "future", ChannelFuture.class).channel().isActive());
    }

    @Test
    public void testFailedStartCanRetryTheSameConfiguration() throws Exception {
        String config;
        try (ServerSocket occupied = new ServerSocket(0)) {
            config = "{\"port\":" + occupied.getLocalPort() + ",\"workerGroupThreadCount\":1}";
            assertThrows(BindException.class, () -> mqttPluginDataHandlerUnderTest.handlerPlugin(pluginData(true, config)));
        }

        mqttPluginDataHandlerUnderTest.handlerPlugin(pluginData(true, config));

        assertTrue(getField(server, "future", ChannelFuture.class).channel().isActive());
    }

    @Test
    public void testDisableThenEnableTheSameConfiguration() {
        assertDoesNotThrow(() -> mqttPluginDataHandlerUnderTest.handlerPlugin(pluginData(false, CONFIG)));
        mqttPluginDataHandlerUnderTest.handlerPlugin(pluginData(true, CONFIG));
        final ChannelFuture previous = getField(server, "future", ChannelFuture.class);
        mqttPluginDataHandlerUnderTest.handlerPlugin(pluginData(false, CONFIG));

        mqttPluginDataHandlerUnderTest.handlerPlugin(pluginData(true, CONFIG));

        final ChannelFuture current = getField(server, "future", ChannelFuture.class);
        assertNotSame(previous, current);
        assertTrue(current.channel().isActive());
    }

    @Test
    public void testRepeatedUpdatePreservesConnectionAndDisableClosesIt() throws Exception {
        mqttPluginDataHandlerUnderTest.handlerPlugin(pluginData(true, CONFIG));
        final ChannelFuture future = getField(server, "future", ChannelFuture.class);
        int port = ((InetSocketAddress) future.channel().localAddress()).getPort();
        try (Socket client = new Socket("127.0.0.1", port)) {
            client.setSoTimeout(5000);
            ByteArrayOutputStream bytes = new ByteArrayOutputStream();
            DataOutputStream connect = new DataOutputStream(bytes);
            connect.writeUTF("MQTT");
            connect.writeByte(4);
            connect.writeByte(0xC2);
            connect.writeShort(60);
            connect.writeUTF("lifecycle-test-client");
            connect.writeUTF("shenyu");
            connect.writeUTF("shenyu");
            client.getOutputStream().write(0x10);
            client.getOutputStream().write(bytes.size());
            client.getOutputStream().write(bytes.toByteArray());
            assertArrayEquals(new byte[]{0x20, 2, 0, 0}, client.getInputStream().readNBytes(4));

            mqttPluginDataHandlerUnderTest.handlerPlugin(pluginData(true, CONFIG));
            client.getOutputStream().write(new byte[]{(byte) 0xC0, 0});
            assertArrayEquals(new byte[]{(byte) 0xD0, 0}, client.getInputStream().readNBytes(2));

            mqttPluginDataHandlerUnderTest.handlerPlugin(pluginData(false, CONFIG));

            assertEquals(-1, client.getInputStream().read());
        }
    }

    @Test
    public void testConcurrentEnabledUpdatesStartOnlyOnce() throws Exception {
        Callable<ChannelFuture> update = () -> {
            mqttPluginDataHandlerUnderTest.handlerPlugin(pluginData(true, CONFIG));
            return getField(server, "future", ChannelFuture.class);
        };
        final FutureTask<ChannelFuture> first = new FutureTask<>(update);
        final FutureTask<ChannelFuture> second = new FutureTask<>(update);
        Thread firstThread = new Thread(first);
        Thread secondThread = new Thread(second);
        try {
            // Pause the first update at the real server's startup monitor.
            synchronized (server) {
                firstThread.start();
                awaitBlockedBy(firstThread, Thread.currentThread());
                secondThread.start();
                awaitBlockedBy(secondThread, firstThread);
            }

            assertSame(first.get(10, TimeUnit.SECONDS), second.get(10, TimeUnit.SECONDS));
            assertTrue(server.isRunning());
            final EventLoopGroup bossGroup = getField(server, "bossGroup", EventLoopGroup.class);
            final EventLoopGroup workerGroup = getField(server, "workerGroup", EventLoopGroup.class);
            mqttPluginDataHandlerUnderTest.handlerPlugin(pluginData(false, CONFIG));
            assertTrue(bossGroup.isTerminated());
            assertTrue(workerGroup.isTerminated());
        } finally {
            firstThread.interrupt();
            secondThread.interrupt();
            firstThread.join(TimeUnit.SECONDS.toMillis(10));
            secondThread.join(TimeUnit.SECONDS.toMillis(10));
            assertFalse(firstThread.isAlive());
            assertFalse(secondThread.isAlive());
        }
    }

    @Test
    public void testSameConfigurationRestartsClosedListener() {
        mqttPluginDataHandlerUnderTest.handlerPlugin(pluginData(true, CONFIG));
        final ChannelFuture previous = getField(server, "future", ChannelFuture.class);
        final EventLoopGroup oldBoss = getField(server, "bossGroup", EventLoopGroup.class);
        final EventLoopGroup oldWorker = getField(server, "workerGroup", EventLoopGroup.class);
        previous.channel().close().syncUninterruptibly();
        assertFalse(server.isRunning());

        mqttPluginDataHandlerUnderTest.handlerPlugin(pluginData(true, CONFIG));

        assertTrue(server.isRunning());
        assertNotSame(previous, getField(server, "future", ChannelFuture.class));
        assertTrue(oldBoss.isTerminated());
        assertTrue(oldWorker.isTerminated());
    }

    private void awaitBlockedBy(final Thread thread, final Thread owner) {
        await().atMost(Duration.ofSeconds(5)).until(() -> {
            ThreadInfo info = ManagementFactory.getThreadMXBean().getThreadInfo(thread.getId());
            return info.getThreadState() == Thread.State.BLOCKED && info.getLockOwnerId() == owner.getId();
        });
    }

    private static Stream<Arguments> configurationChanges() {
        return Stream.of(Arguments.of("port", "0"), Arguments.of("userName", "\"changed\""), Arguments.of("password", "\"changed\""),
                Arguments.of("maxPayloadSize", "1024"), Arguments.of("workerGroupThreadCount", "2"), Arguments.of("leakDetectorLevel", "\"SIMPLE\""));
    }

    private PluginData pluginData(final boolean enabled, final String config) {
        return new PluginData("pluginId", "mqtt", config, "0", enabled, null);
    }

    private int unusedPort() throws Exception {
        try (ServerSocket socket = new ServerSocket(0)) {
            return socket.getLocalPort();
        }
    }

    private <T> T getField(final Object target, final String name, final Class<T> type) {
        try {
            Field field = target instanceof MqttBootstrapServer ? MqttBootstrapServer.class.getDeclaredField(name) : MqttPluginDataHandler.class.getDeclaredField(name);
            field.setAccessible(true);
            return type.cast(field.get(target));
        } catch (ReflectiveOperationException e) {
            throw new AssertionError(e);
        }
    }
}
