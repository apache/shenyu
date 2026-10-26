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

import io.netty.channel.embedded.EmbeddedChannel;
import io.netty.handler.codec.mqtt.MqttConnAckMessage;
import io.netty.handler.codec.mqtt.MqttConnectReturnCode;
import io.netty.handler.codec.mqtt.MqttMessage;
import io.netty.handler.codec.mqtt.MqttMessageBuilders;
import io.netty.handler.codec.mqtt.MqttMessageType;
import io.netty.handler.codec.mqtt.MqttQoS;
import io.netty.handler.codec.mqtt.MqttTopicSubscription;
import io.netty.handler.codec.mqtt.MqttVersion;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.protocol.mqtt.repositories.ChannelRepository;
import org.apache.shenyu.protocol.mqtt.repositories.MqttSession;
import org.apache.shenyu.protocol.mqtt.repositories.SessionRepository;
import org.apache.shenyu.protocol.mqtt.repositories.SubscribeRepository;
import org.apache.shenyu.protocol.mqtt.repositories.TopicRepository;
import org.apache.shenyu.protocol.mqtt.utils.MqttPacketIdGenerator;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Regression tests for session cleanup on graceful and abnormal disconnects.
 */
public final class MqttSessionDisconnectTest {

    private static final String CLIENT_ID = "session-disconnect-client";

    private static final String TOPIC = "test/session-disconnect";

    private static final String OTHER_TOPIC = "test/session-disconnect-other";

    private static final String USER_NAME = "test-user";

    private static final String PASSWORD = "test-password";

    private final List<EmbeddedChannel> channels = new ArrayList<>();

    private final ChannelRepository channelRepository = new ChannelRepository();

    private final SessionRepository sessionRepository = new SessionRepository();

    private final SubscribeRepository subscribeRepository = new SubscribeRepository();

    @BeforeEach
    public void setUp() {
        Singleton.INST.single(ChannelRepository.class, channelRepository);
        Singleton.INST.single(SessionRepository.class, sessionRepository);
        Singleton.INST.single(SubscribeRepository.class, subscribeRepository);
        Singleton.INST.single(TopicRepository.class, new TopicRepository());
        sessionRepository.remove(CLIENT_ID);
        new MqttContext().setUserName(USER_NAME);
        new MqttContext().setPassword(PASSWORD);
    }

    @AfterEach
    public void tearDown() {
        channels.forEach(channel -> {
            channel.finishAndReleaseAll();
            channelRepository.remove(channel);
            subscribeRepository.remove(channel);
            MqttPacketIdGenerator.remove(channel);
        });
        await().atMost(Duration.ofSeconds(5)).untilAsserted(() -> channels.forEach(channel -> {
            assertFalse(subscribeRepository.get(TOPIC).containsKey(channel));
            assertFalse(subscribeRepository.get(OTHER_TOPIC).containsKey(channel));
        }));
        sessionRepository.remove(CLIENT_ID);
        new MqttContext().setUserName(null);
        new MqttContext().setPassword(null);
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    public void cleanSessionDisconnectDiscardsStateBeforeReconnect(final boolean graceful) {
        EmbeddedChannel channel = connect(true, false);
        subscribe(channel);
        assertTrue(sessionRepository.get(CLIENT_ID).isCleanSession());
        assertEquals(2, sessionRepository.get(CLIENT_ID).getTopics().size());
        assertEquals(1, MqttPacketIdGenerator.next(channel));

        disconnect(channel, graceful);

        assertFalse(channel.isActive());
        assertNull(channelRepository.get(channel));
        assertNull(sessionRepository.get(CLIENT_ID));
        assertUnsubscribed(channel);
        assertEquals(1, MqttPacketIdGenerator.next(channel));

        EmbeddedChannel reconnected = connect(false, false);
        assertTrue(sessionRepository.get(CLIENT_ID).getTopics().isEmpty());
        assertUnsubscribed(reconnected);
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    public void persistentSessionDisconnectPreservesSubscriptionsForReconnect(final boolean graceful) {
        EmbeddedChannel channel = connect(false, false);
        subscribe(channel);
        final MqttSession session = sessionRepository.get(CLIENT_ID);

        disconnect(channel, graceful);

        assertFalse(channel.isActive());
        assertNull(channelRepository.get(channel));
        assertSame(session, sessionRepository.get(CLIENT_ID));
        assertEquals(2, session.getTopics().size());
        assertUnsubscribed(channel);

        EmbeddedChannel reconnected = connect(false, true);
        await().atMost(Duration.ofSeconds(5)).untilAsserted(() -> {
            assertEquals(MqttQoS.AT_LEAST_ONCE, subscribeRepository.get(TOPIC).get(reconnected));
            assertEquals(MqttQoS.EXACTLY_ONCE, subscribeRepository.get(OTHER_TOPIC).get(reconnected));
        });
        assertUnsubscribed(channel);
    }

    @Test
    public void closeFutureDiscardsCleanSessionBeforeChannelInactive() {
        EmbeddedChannel channel = connect(true, false);
        channel.closeFuture().addListener(new MqttTransportHandler());
        subscribe(channel);

        channel.close();

        assertNull(sessionRepository.get(CLIENT_ID));
        assertNull(channelRepository.get(channel));
        assertUnsubscribed(channel);
    }

    @Test
    public void abruptCloseImmediatelyAfterSubscribeRemovesOnlyDisconnectedChannel() {
        EmbeddedChannel otherChannel = newChannel();
        subscribeRepository.add(otherChannel,
                Collections.singletonList(new MqttTopicSubscription(TOPIC, MqttQoS.AT_MOST_ONCE)));
        EmbeddedChannel channel = connect(true, false);
        channel.writeInbound(MqttMessageBuilders.subscribe()
                .messageId(1)
                .addSubscription(MqttQoS.AT_LEAST_ONCE, TOPIC)
                .build());

        channel.close();

        assertNull(sessionRepository.get(CLIENT_ID));
        assertFalse(subscribeRepository.get(TOPIC).containsKey(channel));
        assertEquals(MqttQoS.AT_MOST_ONCE, subscribeRepository.get(TOPIC).get(otherChannel));
    }

    @Test
    public void channelWithoutStoredSessionCanClose() {
        EmbeddedChannel channel = newChannel();
        channelRepository.add(channel, CLIENT_ID);

        channel.close();

        assertFalse(channel.isActive());
        assertNull(channelRepository.get(channel));
        assertNull(sessionRepository.get(CLIENT_ID));
    }

    private EmbeddedChannel connect(final boolean cleanSession, final boolean sessionPresent) {
        EmbeddedChannel channel = newChannel();
        channel.writeInbound(MqttMessageBuilders.connect()
                .clientId(CLIENT_ID)
                .cleanSession(cleanSession)
                .protocolVersion(MqttVersion.MQTT_3_1_1)
                .username(USER_NAME)
                .password(PASSWORD.getBytes(StandardCharsets.UTF_8))
                .build());
        MqttConnAckMessage ack = channel.readOutbound();
        assertNotNull(ack);
        assertEquals(MqttConnectReturnCode.CONNECTION_ACCEPTED, ack.variableHeader().connectReturnCode());
        assertEquals(sessionPresent, ack.variableHeader().isSessionPresent());
        return channel;
    }

    private EmbeddedChannel newChannel() {
        EmbeddedChannel channel = new EmbeddedChannel(new MqttTransportHandler());
        channels.add(channel);
        return channel;
    }

    private void subscribe(final EmbeddedChannel channel) {
        channel.writeInbound(MqttMessageBuilders.subscribe()
                .messageId(1)
                .addSubscription(MqttQoS.AT_LEAST_ONCE, TOPIC)
                .addSubscription(MqttQoS.EXACTLY_ONCE, OTHER_TOPIC)
                .build());
        MqttMessage ack = channel.readOutbound();
        assertNotNull(ack);
        assertEquals(MqttMessageType.SUBACK, ack.fixedHeader().messageType());
        await().atMost(Duration.ofSeconds(5)).untilAsserted(() -> {
            assertEquals(MqttQoS.AT_LEAST_ONCE, subscribeRepository.get(TOPIC).get(channel));
            assertEquals(MqttQoS.EXACTLY_ONCE, subscribeRepository.get(OTHER_TOPIC).get(channel));
        });
    }

    private void disconnect(final EmbeddedChannel channel, final boolean graceful) {
        if (graceful) {
            channel.writeInbound(MqttMessageBuilders.disconnect().build());
        } else {
            channel.close();
        }
        channel.runPendingTasks();
    }

    private void assertUnsubscribed(final EmbeddedChannel channel) {
        await().atMost(Duration.ofSeconds(5)).untilAsserted(() -> {
            assertFalse(subscribeRepository.get(TOPIC).containsKey(channel));
            assertFalse(subscribeRepository.get(OTHER_TOPIC).containsKey(channel));
        });
    }
}
