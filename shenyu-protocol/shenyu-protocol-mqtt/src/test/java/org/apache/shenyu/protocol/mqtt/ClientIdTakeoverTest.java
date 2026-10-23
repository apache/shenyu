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
import org.apache.shenyu.protocol.mqtt.repositories.SessionRepository;
import org.apache.shenyu.protocol.mqtt.repositories.SubscribeRepository;
import org.apache.shenyu.protocol.mqtt.repositories.WillRepository;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import static io.netty.handler.codec.mqtt.MqttConnectReturnCode.CONNECTION_ACCEPTED;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * A duplicate clientId must replace the existing connection, and the replaced
 * connection closing late must not discard the new session's state (#7918).
 */
public final class ClientIdTakeoverTest {

    private static final String USER_NAME = "test-user";

    private static final String PASSWORD = "test-password";

    private static final String CLIENT_ID = "takeover-client";

    private static final String WILL_TOPIC = "will/takeover";

    private final List<EmbeddedChannel> channels = new ArrayList<>();

    private ChannelRepository channelRepository;

    private SessionRepository sessionRepository;

    private WillRepository willRepository;

    private SubscribeRepository subscribeRepository;

    private Connect connect;

    @BeforeEach
    public void setUp() {
        connect = new Connect();
        channelRepository = new ChannelRepository();
        sessionRepository = new SessionRepository();
        willRepository = new WillRepository();
        subscribeRepository = new SubscribeRepository();
        Singleton.INST.single(ChannelRepository.class, channelRepository);
        Singleton.INST.single(SessionRepository.class, sessionRepository);
        Singleton.INST.single(WillRepository.class, willRepository);
        Singleton.INST.single(SubscribeRepository.class, subscribeRepository);
        MqttContext mqttContext = new MqttContext();
        mqttContext.setUserName(USER_NAME);
        mqttContext.setPassword(PASSWORD);
    }

    @AfterEach
    public void tearDown() {
        for (EmbeddedChannel channel : channels) {
            channelRepository.remove(channel);
            willRepository.remove(channel);
            channel.finishAndReleaseAll();
        }
        channels.clear();
        MqttContext mqttContext = new MqttContext();
        mqttContext.setUserName(null);
        mqttContext.setPassword(null);
    }

    @Test
    public void testTakeoverClosesThePreviousChannel() {
        final EmbeddedChannel first = connect(CLIENT_ID, false);
        assertAccepted(first);

        final EmbeddedChannel second = connect(CLIENT_ID, false);
        assertAccepted(second);

        assertChannelClosed(first);
        assertTrue(second.isActive());
        assertTrue(channelRepository.isOwner(CLIENT_ID, second));
    }

    /**
     * A clean disconnect must release the ownership, so the next connect with
     * the same id is not mistaken for a duplicate-clientId takeover (#7918).
     */
    @Test
    public void testCleanDisconnectReleasesOwnership() {
        final EmbeddedChannel channel = connect(CLIENT_ID, false);
        assertAccepted(channel);
        assertTrue(channelRepository.isOwner(CLIENT_ID, channel));

        Disconnect.cleanChannel(channel);

        assertFalse(channelRepository.isOwner(CLIENT_ID, channel));
    }

    @Test
    public void testStaleChannelCleanupKeepsTheNewSession() {
        final EmbeddedChannel first = connect(CLIENT_ID, true);
        final EmbeddedChannel second = connect(CLIENT_ID, true);
        assertChannelClosed(first);

        // the stale connection closes late, after the takeover: its cleanup
        // must not discard the session the new connection just created (#7918)
        Disconnect.cleanChannel(first);

        assertNotNull(sessionRepository.get(CLIENT_ID),
                "the stale connection's cleanup must not delete the new session");
        assertNull(channelRepository.get(first));
        assertTrue(channelRepository.isOwner(CLIENT_ID, second));
    }

    @Test
    public void testWillOfReplacedConnectionIsDiscarded() {
        final EmbeddedChannel first = connectWithWill(CLIENT_ID);
        assertNotNull(willRepository.get(first));

        final EmbeddedChannel second = connect(CLIENT_ID, false);
        assertAccepted(second);

        // the replaced connection's will is dropped: the client is alive on the new connection
        assertNull(willRepository.get(first));
        assertChannelClosed(first);
        assertFalse(subscribeRepository.getChannelsByTopic(WILL_TOPIC).containsKey(first));
    }

    private EmbeddedChannel connect(final String clientId, final boolean cleanSession) {
        final EmbeddedChannel channel = newChannel();
        connect.connect(context(channel), connectMessage(clientId, cleanSession, false, null, null));
        return channel;
    }

    private EmbeddedChannel connectWithWill(final String clientId) {
        final EmbeddedChannel channel = newChannel();
        connect.connect(context(channel), connectMessage(clientId, true, true,
                WILL_TOPIC, "gone".getBytes(StandardCharsets.UTF_8)));
        return channel;
    }

    private void assertAccepted(final EmbeddedChannel channel) {
        final MqttConnAckMessage ackMessage = channel.readOutbound();
        assertNotNull(ackMessage);
        assertEquals(CONNECTION_ACCEPTED, ackMessage.variableHeader().connectReturnCode());
    }

    private void assertChannelClosed(final EmbeddedChannel channel) {
        channel.runPendingTasks();
        assertFalse(channel.isActive());
    }

    private EmbeddedChannel newChannel() {
        final EmbeddedChannel channel = new EmbeddedChannel(new ChannelInboundHandlerAdapter());
        channels.add(channel);
        return channel;
    }

    private ChannelHandlerContext context(final EmbeddedChannel channel) {
        return channel.pipeline().lastContext();
    }

    private MqttConnectMessage connectMessage(final String clientId, final boolean cleanSession,
            final boolean willFlag, final String willTopic, final byte[] willMessage) {
        final MqttFixedHeader fixedHeader = new MqttFixedHeader(MqttMessageType.CONNECT, false, MqttQoS.AT_MOST_ONCE, false, 0);
        final MqttConnectVariableHeader variableHeader = new MqttConnectVariableHeader(
                MqttVersion.MQTT_3_1_1.protocolName(), MqttVersion.MQTT_3_1_1.protocolLevel(),
                cleanSession, willFlag, false, 0, Objects.nonNull(willTopic), false, 60);
        final MqttConnectPayload payload = new MqttConnectPayload(clientId, willTopic, willMessage,
                USER_NAME, PASSWORD.getBytes(StandardCharsets.UTF_8));
        return new MqttConnectMessage(fixedHeader, variableHeader, payload);
    }
}
