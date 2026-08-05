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

import io.netty.channel.Channel;
import io.netty.channel.ChannelFuture;
import io.netty.channel.ChannelHandlerContext;
import io.netty.handler.codec.mqtt.MqttConnAckMessage;
import io.netty.handler.codec.mqtt.MqttConnectMessage;
import io.netty.handler.codec.mqtt.MqttMessageBuilders;
import io.netty.handler.codec.mqtt.MqttQoS;
import io.netty.handler.codec.mqtt.MqttTopicSubscription;
import io.netty.handler.codec.mqtt.MqttVersion;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.protocol.mqtt.repositories.ChannelRepository;
import org.apache.shenyu.protocol.mqtt.repositories.MqttSession;
import org.apache.shenyu.protocol.mqtt.repositories.SessionRepository;
import org.apache.shenyu.protocol.mqtt.repositories.SubscribeRepository;
import org.awaitility.Awaitility;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.concurrent.TimeUnit;

import static io.netty.handler.codec.mqtt.MqttConnectReturnCode.CONNECTION_REFUSED_UNACCEPTABLE_PROTOCOL_VERSION;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.clearInvocations;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link Connect} session present behavior.
 */
public class ConnectSessionPresentTest {

    private Connect connect;

    private ChannelHandlerContext ctx;

    private Channel channel;

    @BeforeEach
    public void setUp() {
        Singleton.INST.single(ChannelRepository.class, new ChannelRepository());
        Singleton.INST.single(SessionRepository.class, new SessionRepository());
        Singleton.INST.single(SubscribeRepository.class, new SubscribeRepository());
        MqttContext mqttContext = new MqttContext();
        mqttContext.setUserName("shenyu");
        mqttContext.setPassword("shenyu");
        connect = new Connect();
        ctx = mock(ChannelHandlerContext.class);
        channel = mock(Channel.class);
        ChannelFuture channelFuture = mock(ChannelFuture.class);
        when(ctx.channel()).thenReturn(channel);
        when(ctx.writeAndFlush(any())).thenReturn(channelFuture);
        when(ctx.close()).thenReturn(channelFuture);
    }

    @Test
    public void firstConnectWithCleanSessionFalseShouldNotPresentSession() {
        MqttConnAckMessage ack = connectWithCleanSession("client-first", false);
        assertFalse(ack.variableHeader().isSessionPresent());
    }

    @Test
    public void reconnectWithCleanSessionFalseShouldPresentSession() {
        String clientId = "client-reconnect";
        assertFalse(connectWithCleanSession(clientId, false).variableHeader().isSessionPresent());
        assertTrue(connectWithCleanSession(clientId, false).variableHeader().isSessionPresent());
    }

    @Test
    public void connectWithCleanSessionTrueShouldNotPresentSession() {
        MqttConnAckMessage ack = connectWithCleanSession("client-clean", true);
        assertFalse(ack.variableHeader().isSessionPresent());
    }

    @Test
    public void cleanSessionConnectShouldDiscardPreviousSession() {
        String clientId = "client-discard";
        assertFalse(connectWithCleanSession(clientId, false).variableHeader().isSessionPresent());
        assertFalse(connectWithCleanSession(clientId, true).variableHeader().isSessionPresent());
        MqttSession session = Singleton.INST.get(SessionRepository.class).get(clientId);
        assertTrue(session.isCleanSession());
        assertTrue(session.getTopics().isEmpty());
    }

    @Test
    public void resumeSessionShouldReRegisterSubscriptionsToNewChannel() {
        String clientId = "client-resume";
        MqttSession storedSession = new MqttSession(clientId, false);
        storedSession.addTopic("test/topic", MqttQoS.AT_LEAST_ONCE);
        Singleton.INST.get(SessionRepository.class).add(clientId, storedSession);

        MqttConnAckMessage ack = connectWithCleanSession(clientId, false);
        assertTrue(ack.variableHeader().isSessionPresent());

        MqttSession session = Singleton.INST.get(SessionRepository.class).get(clientId);
        assertTrue(session.getTopics().contains("test/topic"));
        List<MqttTopicSubscription> subscriptions = session.getTopicSubscriptions();
        assertEquals(1, subscriptions.size());
        assertEquals("test/topic", subscriptions.get(0).topicName());
        assertEquals(MqttQoS.AT_LEAST_ONCE, subscriptions.get(0).qualityOfService());

        SubscribeRepository subscribeRepository = Singleton.INST.get(SubscribeRepository.class);
        Awaitility.await().atMost(5, TimeUnit.SECONDS)
                .untilAsserted(() -> assertTrue(subscribeRepository.get("test/topic").contains(channel)));
    }

    @Test
    public void unsupportedProtocolVersionShouldNotAuthenticateOrStoreSession() {
        MqttConnectMessage msg = MqttMessageBuilders.connect()
                .clientId("client-mqtt-5")
                .cleanSession(false)
                .protocolVersion(MqttVersion.MQTT_3_1_1)
                .username("shenyu")
                .password("shenyu".getBytes(StandardCharsets.UTF_8))
                .build();

        connect.connect(ctx, msg);

        ArgumentCaptor<Object> captor = ArgumentCaptor.forClass(Object.class);
        verify(ctx).writeAndFlush(captor.capture());
        MqttConnAckMessage ack = (MqttConnAckMessage) captor.getValue();
        assertEquals(CONNECTION_REFUSED_UNACCEPTABLE_PROTOCOL_VERSION, ack.variableHeader().connectReturnCode());
        verify(ctx, never()).channel();
        verify(ctx).close();
        assertNull(Singleton.INST.get(SessionRepository.class).get("client-mqtt-5"));
    }

    private MqttConnAckMessage connectWithCleanSession(final String clientId, final boolean cleanSession) {
        MqttConnectMessage msg = MqttMessageBuilders.connect()
                .clientId(clientId)
                .cleanSession(cleanSession)
                .protocolVersion(MqttVersion.MQTT_3_1)
                .username("shenyu")
                .password("shenyu".getBytes(StandardCharsets.UTF_8))
                .build();
        clearInvocations(ctx);
        connect.connect(ctx, msg);
        ArgumentCaptor<Object> captor = ArgumentCaptor.forClass(Object.class);
        verify(ctx).writeAndFlush(captor.capture());
        return (MqttConnAckMessage) captor.getValue();
    }
}
