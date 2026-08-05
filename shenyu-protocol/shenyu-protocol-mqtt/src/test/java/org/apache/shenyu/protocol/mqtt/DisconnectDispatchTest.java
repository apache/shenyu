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
import io.netty.channel.ChannelHandlerContext;
import io.netty.handler.codec.mqtt.MqttMessageBuilders;
import io.netty.handler.codec.mqtt.MqttQoS;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.protocol.mqtt.repositories.ChannelRepository;
import org.apache.shenyu.protocol.mqtt.repositories.MqttSession;
import org.apache.shenyu.protocol.mqtt.repositories.SessionRepository;
import org.apache.shenyu.protocol.mqtt.repositories.SubscribeRepository;
import org.awaitility.Awaitility;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link MqttFactory} DISCONNECT dispatch and clean-session cleanup.
 */
public class DisconnectDispatchTest {

    private ChannelHandlerContext ctx;

    private Channel channel;

    @BeforeEach
    public void setUp() {
        Singleton.INST.single(ChannelRepository.class, new ChannelRepository());
        Singleton.INST.single(SessionRepository.class, new SessionRepository());
        Singleton.INST.single(SubscribeRepository.class, new SubscribeRepository());
        ctx = mock(ChannelHandlerContext.class);
        channel = mock(Channel.class);
        when(ctx.channel()).thenReturn(channel);
    }

    @Test
    public void cleanSessionDisconnectShouldRemoveStoredSession() {
        String clientId = "client-clean";
        registerChannel(clientId);
        Singleton.INST.get(SessionRepository.class).add(clientId, new MqttSession(clientId, true));

        new MqttFactory(MqttMessageBuilders.disconnect().build(), ctx).connect();

        assertNull(Singleton.INST.get(SessionRepository.class).get(clientId));
    }

    @Test
    public void persistentSessionDisconnectShouldKeepStoredSessionAndUnregisterChannel() {
        String clientId = "client-persistent";
        String topic = "test/persistent";
        registerChannel(clientId);
        MqttSession session = new MqttSession(clientId, false);
        session.addTopic(topic, MqttQoS.AT_MOST_ONCE);
        Singleton.INST.get(SessionRepository.class).add(clientId, session);
        Singleton.INST.get(SubscribeRepository.class).add(channel, session.getTopicSubscriptions());
        awaitSubscribed(topic);

        new MqttFactory(MqttMessageBuilders.disconnect().build(), ctx).connect();

        assertNotNull(Singleton.INST.get(SessionRepository.class).get(clientId));
        Awaitility.await().atMost(5, TimeUnit.SECONDS).untilAsserted(() ->
                assertFalse(Singleton.INST.get(SubscribeRepository.class).get(topic).contains(channel)));
    }

    @Test
    public void cleanSessionDisconnectShouldUnregisterSubscribedChannel() {
        String clientId = "client-clean-sub";
        String topic = "test/clean-sub";
        registerChannel(clientId);
        MqttSession session = new MqttSession(clientId, true);
        session.addTopic(topic, MqttQoS.AT_MOST_ONCE);
        Singleton.INST.get(SessionRepository.class).add(clientId, session);
        Singleton.INST.get(SubscribeRepository.class).add(channel, session.getTopicSubscriptions());
        awaitSubscribed(topic);

        new MqttFactory(MqttMessageBuilders.disconnect().build(), ctx).connect();

        Awaitility.await().atMost(5, TimeUnit.SECONDS).untilAsserted(() ->
                assertFalse(Singleton.INST.get(SubscribeRepository.class).get(topic).contains(channel)));
    }

    private void registerChannel(final String clientId) {
        Singleton.INST.get(ChannelRepository.class).add(channel, clientId);
        Awaitility.await().atMost(5, TimeUnit.SECONDS).untilAsserted(() ->
                assertEquals(clientId, Singleton.INST.get(ChannelRepository.class).get(channel)));
    }

    private void awaitSubscribed(final String topic) {
        Awaitility.await().atMost(5, TimeUnit.SECONDS).untilAsserted(() ->
                assertEquals(1, Singleton.INST.get(SubscribeRepository.class).get(topic).size()));
    }
}
