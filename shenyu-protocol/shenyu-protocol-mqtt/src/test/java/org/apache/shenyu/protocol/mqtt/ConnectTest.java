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

package org.apache.shenyu.protocol.mqtt;

import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelInboundHandlerAdapter;
import io.netty.channel.embedded.EmbeddedChannel;
import io.netty.handler.codec.mqtt.MqttConnAckMessage;
import io.netty.handler.codec.mqtt.MqttConnectMessage;
import io.netty.handler.codec.mqtt.MqttConnectPayload;
import io.netty.handler.codec.mqtt.MqttConnectVariableHeader;
import io.netty.handler.codec.mqtt.MqttFixedHeader;
import io.netty.handler.codec.mqtt.MqttMessageType;
import io.netty.handler.codec.mqtt.MqttQoS;
import io.netty.handler.codec.mqtt.MqttVersion;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.protocol.mqtt.repositories.ChannelRepository;
import org.apache.shenyu.protocol.mqtt.repositories.WillRepository;
import org.apache.shenyu.protocol.mqtt.repositories.WillRepository.WillEntry;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

import static io.netty.handler.codec.mqtt.MqttConnectReturnCode.CONNECTION_ACCEPTED;
import static io.netty.handler.codec.mqtt.MqttConnectReturnCode.CONNECTION_REFUSED_BAD_USER_NAME_OR_PASSWORD;
import static io.netty.handler.codec.mqtt.MqttConnectReturnCode.CONNECTION_REFUSED_IDENTIFIER_REJECTED;
import static io.netty.handler.codec.mqtt.MqttConnectReturnCode.CONNECTION_REFUSED_UNACCEPTABLE_PROTOCOL_VERSION;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for {@link Connect}.
 */
public final class ConnectTest {

    private static final String USER_NAME = "test-user";

    private static final String PASSWORD = "test-password";

    private static final String CLIENT_ID = "test-client";

    private static final String WILL_TOPIC = "status/client-001";

    private final List<EmbeddedChannel> channels = new ArrayList<>();

    private ChannelRepository channelRepository;

    private WillRepository willRepository;

    private Connect connect;

    @BeforeAll
    static void setUpCredentials() {
        MqttContext mqttContext = new MqttContext();
        mqttContext.setUserName(USER_NAME);
        mqttContext.setPassword(PASSWORD);
    }

    @AfterAll
    static void clearCredentials() {
        MqttContext mqttContext = new MqttContext();
        mqttContext.setUserName(null);
        mqttContext.setPassword(null);
    }

    @BeforeEach
    public void setUp() {
        connect = new Connect();
        channelRepository = new ChannelRepository();
        willRepository = new WillRepository();
        Singleton.INST.single(ChannelRepository.class, channelRepository);
        Singleton.INST.single(WillRepository.class, willRepository);
    }

    @AfterEach
    public void tearDown() {
        for (EmbeddedChannel channel : channels) {
            channelRepository.remove(channel);
            willRepository.remove(channel);
            channel.finishAndReleaseAll();
        }
        channels.clear();
        Singleton.INST.single(ChannelRepository.class, new ChannelRepository());
        Singleton.INST.single(WillRepository.class, new WillRepository());
    }

    @Test
    public void mqtt31ConnectIsAccepted() {
        connectIsAccepted(MqttVersion.MQTT_3_1);
    }

    @Test
    public void mqtt311ConnectIsAccepted() {
        connectIsAccepted(MqttVersion.MQTT_3_1_1);
    }

    @Test
    public void mqtt5ConnectIsAccepted() {
        connectIsAccepted(MqttVersion.MQTT_5);
    }

    @Test
    public void unsupportedProtocolVersionIsRejected() {
        EmbeddedChannel channel = newChannel();

        connect.connect(context(channel), connectMessage("MQTT", 6));

        MqttConnAckMessage ackMessage = channel.readOutbound();
        assertNotNull(ackMessage);
        assertEquals(CONNECTION_REFUSED_UNACCEPTABLE_PROTOCOL_VERSION, ackMessage.variableHeader().connectReturnCode());
        assertFalse(ackMessage.variableHeader().isSessionPresent());
        assertChannelClosed(channel);
        assertNull(channelRepository.get(channel));
    }

    @Test
    public void emptyClientIdIsRejected() {
        EmbeddedChannel channel = newChannel();

        connect.connect(context(channel), buildConnectMessage(MqttVersion.MQTT_3_1_1.protocolName(),
                MqttVersion.MQTT_3_1_1.protocolLevel(), "", PASSWORD, false, 0, false, null, null));

        MqttConnAckMessage ackMessage = channel.readOutbound();
        assertNotNull(ackMessage);
        assertEquals(CONNECTION_REFUSED_IDENTIFIER_REJECTED, ackMessage.variableHeader().connectReturnCode());
        assertChannelClosed(channel);
        assertNull(channelRepository.get(channel));
    }

    @Test
    public void invalidCredentialsAreRejected() {
        EmbeddedChannel channel = newChannel();

        connect.connect(context(channel), buildConnectMessage(MqttVersion.MQTT_3_1_1.protocolName(),
                MqttVersion.MQTT_3_1_1.protocolLevel(), CLIENT_ID, "invalid-password", false, 0, false, null, null));

        MqttConnAckMessage ackMessage = channel.readOutbound();
        assertNotNull(ackMessage);
        assertEquals(CONNECTION_REFUSED_BAD_USER_NAME_OR_PASSWORD, ackMessage.variableHeader().connectReturnCode());
        assertChannelClosed(channel);
        assertNull(channelRepository.get(channel));
    }

    @Test
    public void duplicateConnectIsRejected() {
        EmbeddedChannel channel = newChannel();
        ChannelHandlerContext ctx = context(channel);

        connect.connect(ctx, connectMessage(MqttVersion.MQTT_3_1_1));
        assertNotNull(channel.readOutbound());

        connect.connect(ctx, connectMessage(MqttVersion.MQTT_3_1_1));

        assertChannelClosed(channel);
        assertNull(channel.readOutbound());
    }

    @Test
    public void testStoresWillOnConnect() {
        EmbeddedChannel channel = newChannel();
        byte[] willMessage = "client disconnected unexpectedly".getBytes(StandardCharsets.UTF_8);

        connect.connect(context(channel), willConnectMessage(1, true, WILL_TOPIC, willMessage));

        WillEntry will = willRepository.get(channel);
        assertNotNull(will);
        assertEquals(WILL_TOPIC, will.getTopic());
        assertArrayEquals(willMessage, will.getMessage());
        assertEquals(1, will.getQos());
        assertTrue(will.isRetain());
    }

    @Test
    public void testDoesNotStoreWillWhenWillFlagIsFalse() {
        EmbeddedChannel channel = newChannel();

        connect.connect(context(channel), connectMessage(MqttVersion.MQTT_3_1_1));

        assertNull(willRepository.get(channel));
    }

    @Test
    public void testWillQosZero() {
        EmbeddedChannel channel = newChannel();
        byte[] willMessage = "qos0 will".getBytes(StandardCharsets.UTF_8);

        connect.connect(context(channel), willConnectMessage(0, false, "topic/qos0", willMessage));

        WillEntry will = willRepository.get(channel);
        assertNotNull(will);
        assertEquals(0, will.getQos());
        assertFalse(will.isRetain());
    }

    @Test
    public void testWillRetainTrue() {
        EmbeddedChannel channel = newChannel();
        byte[] willMessage = "retained will".getBytes(StandardCharsets.UTF_8);

        connect.connect(context(channel), willConnectMessage(2, true, "topic/retained", willMessage));

        WillEntry will = willRepository.get(channel);
        assertNotNull(will);
        assertEquals(2, will.getQos());
        assertTrue(will.isRetain());
    }

    @Test
    public void willIsNotStoredWhenConnectIsRejected() {
        EmbeddedChannel channel = newChannel();

        connect.connect(context(channel), buildConnectMessage("MQTT", 6, CLIENT_ID, PASSWORD,
                true, 1, true, WILL_TOPIC, "retained will".getBytes(StandardCharsets.UTF_8)));

        assertNull(willRepository.get(channel));
    }

    private void connectIsAccepted(final MqttVersion version) {
        EmbeddedChannel channel = newChannel();

        connect.connect(context(channel), connectMessage(version));

        MqttConnAckMessage ackMessage = channel.readOutbound();
        assertNotNull(ackMessage);
        assertEquals(CONNECTION_ACCEPTED, ackMessage.variableHeader().connectReturnCode());
        assertTrue(ackMessage.variableHeader().isSessionPresent());
        assertEquals(CLIENT_ID, channelRepository.get(channel));
    }

    private void assertChannelClosed(final EmbeddedChannel channel) {
        channel.runPendingTasks();
        assertFalse(channel.isActive());
    }

    private EmbeddedChannel newChannel() {
        EmbeddedChannel channel = new EmbeddedChannel(new ChannelInboundHandlerAdapter());
        channels.add(channel);
        return channel;
    }

    private ChannelHandlerContext context(final EmbeddedChannel channel) {
        return channel.pipeline().lastContext();
    }

    private MqttConnectMessage connectMessage(final MqttVersion version) {
        return connectMessage(version.protocolName(), version.protocolLevel());
    }

    private MqttConnectMessage connectMessage(final String protocolName, final int protocolLevel) {
        return buildConnectMessage(protocolName, protocolLevel, CLIENT_ID, PASSWORD, false, 0, false, null, null);
    }

    private MqttConnectMessage willConnectMessage(final int willQos, final boolean willRetain,
            final String willTopic, final byte[] willMessage) {
        return buildConnectMessage(MqttVersion.MQTT_3_1_1.protocolName(), MqttVersion.MQTT_3_1_1.protocolLevel(),
                CLIENT_ID, PASSWORD, true, willQos, willRetain, willTopic, willMessage);
    }

    private MqttConnectMessage buildConnectMessage(final String protocolName, final int protocolLevel,
            final String clientId, final String password, final boolean willFlag, final int willQos,
            final boolean willRetain, final String willTopic, final byte[] willMessage) {
        MqttFixedHeader fixedHeader = new MqttFixedHeader(MqttMessageType.CONNECT, false, MqttQoS.AT_MOST_ONCE, false, 0);
        MqttConnectVariableHeader variableHeader = new MqttConnectVariableHeader(protocolName, protocolLevel,
                true, true, willRetain, willQos, willFlag, false, 60);
        MqttConnectPayload payload = new MqttConnectPayload(clientId, willTopic, willMessage,
                USER_NAME, password.getBytes(StandardCharsets.UTF_8));
        return new MqttConnectMessage(fixedHeader, variableHeader, payload);
    }
}
