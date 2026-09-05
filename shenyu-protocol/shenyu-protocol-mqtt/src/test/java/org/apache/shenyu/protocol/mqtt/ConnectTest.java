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
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.time.Duration;

import static io.netty.handler.codec.mqtt.MqttConnectReturnCode.CONNECTION_ACCEPTED;
import static io.netty.handler.codec.mqtt.MqttConnectReturnCode.CONNECTION_REFUSED_UNACCEPTABLE_PROTOCOL_VERSION;
import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for {@link Connect}.
 */
public final class ConnectTest {

    private static final String CLIENT_ID = "test-client";

    private static final String USER_NAME = "test-user";

    private static final String PASSWORD = "test-password";

    private static ChannelRepository channelRepository;

    @BeforeAll
    static void setUp() {
        channelRepository = new ChannelRepository();
        Singleton.INST.single(ChannelRepository.class, channelRepository);
        new MqttContext().setUserName(USER_NAME);
        new MqttContext().setPassword(PASSWORD);
    }

    @AfterAll
    static void tearDown() {
        new MqttContext().setUserName(null);
        new MqttContext().setPassword(null);
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
        EmbeddedChannel channel = new EmbeddedChannel(new ChannelInboundHandlerAdapter());
        ChannelHandlerContext ctx = channel.pipeline().lastContext();

        new Connect().connect(ctx, connectMessage("MQTT", 6));

        MqttConnAckMessage ackMessage = channel.readOutbound();
        assertNotNull(ackMessage);
        assertEquals(CONNECTION_REFUSED_UNACCEPTABLE_PROTOCOL_VERSION,
                ackMessage.variableHeader().connectReturnCode());
        channel.runPendingTasks();
        assertFalse(channel.isActive());
        assertNull(channelRepository.get(channel));
    }

    @Test
    public void duplicateConnectIsRejected() {
        EmbeddedChannel channel = new EmbeddedChannel(new ChannelInboundHandlerAdapter());
        ChannelHandlerContext ctx = channel.pipeline().lastContext();

        new Connect().connect(ctx, connectMessage(MqttVersion.MQTT_3_1_1.protocolName(), MqttVersion.MQTT_3_1_1.protocolLevel()));
        assertNotNull(channel.readOutbound());

        new Connect().connect(ctx, connectMessage(MqttVersion.MQTT_3_1_1.protocolName(), MqttVersion.MQTT_3_1_1.protocolLevel()));

        channel.runPendingTasks();
        assertFalse(channel.isActive());
        assertNull(channel.readOutbound());
    }

    private void connectIsAccepted(final MqttVersion version) {
        EmbeddedChannel channel = new EmbeddedChannel(new ChannelInboundHandlerAdapter());
        ChannelHandlerContext ctx = channel.pipeline().lastContext();

        new Connect().connect(ctx, connectMessage(version.protocolName(), version.protocolLevel()));

        MqttConnAckMessage ackMessage = channel.readOutbound();
        assertNotNull(ackMessage);
        assertEquals(CONNECTION_ACCEPTED, ackMessage.variableHeader().connectReturnCode());
        assertTrue(ackMessage.variableHeader().isSessionPresent());
        await().atMost(Duration.ofSeconds(5))
                .until(() -> CLIENT_ID.equals(channelRepository.get(channel)));
    }

    private MqttConnectMessage connectMessage(final String protocolName, final int protocolLevel) {
        MqttFixedHeader fixedHeader = new MqttFixedHeader(MqttMessageType.CONNECT, false, MqttQoS.AT_MOST_ONCE, false, 0);
        MqttConnectVariableHeader variableHeader = new MqttConnectVariableHeader(protocolName, protocolLevel,
                true, true, false, 0, false, false, 60);
        MqttConnectPayload payload = new MqttConnectPayload(CLIENT_ID, null, null,
                USER_NAME, PASSWORD.getBytes(StandardCharsets.UTF_8));
        return new MqttConnectMessage(fixedHeader, variableHeader, payload);
    }
}
