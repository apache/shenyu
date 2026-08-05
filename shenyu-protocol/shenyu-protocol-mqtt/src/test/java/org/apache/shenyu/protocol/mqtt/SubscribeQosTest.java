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
import io.netty.handler.codec.mqtt.MqttMessageBuilders;
import io.netty.handler.codec.mqtt.MqttQoS;
import io.netty.handler.codec.mqtt.MqttSubscribeMessage;
import io.netty.handler.codec.mqtt.MqttTopicSubscription;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.protocol.mqtt.repositories.ChannelRepository;
import org.apache.shenyu.protocol.mqtt.repositories.MqttSession;
import org.apache.shenyu.protocol.mqtt.repositories.SessionRepository;
import org.apache.shenyu.protocol.mqtt.repositories.SubscribeRepository;
import org.apache.shenyu.protocol.mqtt.repositories.TopicRepository;
import org.awaitility.Awaitility;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link Subscribe} QoS retention in {@link MqttSession}.
 */
public class SubscribeQosTest {

    private ChannelHandlerContext ctx;

    private Channel channel;

    @BeforeEach
    public void setUp() {
        Singleton.INST.single(ChannelRepository.class, new ChannelRepository());
        Singleton.INST.single(SessionRepository.class, new SessionRepository());
        Singleton.INST.single(SubscribeRepository.class, new SubscribeRepository());
        Singleton.INST.single(TopicRepository.class, new TopicRepository());
        ctx = mock(ChannelHandlerContext.class);
        channel = mock(Channel.class);
        when(ctx.channel()).thenReturn(channel);
        when(ctx.writeAndFlush(any())).thenReturn(mock(ChannelFuture.class));
        when(channel.writeAndFlush(any())).thenReturn(mock(ChannelFuture.class));
    }

    @Test
    public void subscribeShouldStoreQosInSession() {
        String clientId = "client-qos";
        Singleton.INST.get(ChannelRepository.class).add(channel, clientId);
        MqttSession session = new MqttSession(clientId, false);
        Singleton.INST.get(SessionRepository.class).add(clientId, session);
        Awaitility.await().atMost(5, TimeUnit.SECONDS).untilAsserted(() ->
                assertEquals(clientId, Singleton.INST.get(ChannelRepository.class).get(channel)));

        MqttSubscribeMessage msg = MqttMessageBuilders.subscribe()
                .messageId(1)
                .addSubscription(MqttQoS.AT_LEAST_ONCE, "test/qos")
                .addSubscription(MqttQoS.EXACTLY_ONCE, "test/qos2")
                .build();

        new Subscribe().subscribe(ctx, msg);

        MqttTopicSubscription qos1 = subscription(session, "test/qos");
        MqttTopicSubscription qos2 = subscription(session, "test/qos2");
        assertEquals(MqttQoS.AT_LEAST_ONCE, qos1.qualityOfService());
        assertEquals(MqttQoS.EXACTLY_ONCE, qos2.qualityOfService());
    }

    @Test
    public void resubscribeShouldReplacePreviousQos() {
        String clientId = "client-resub";
        Singleton.INST.get(ChannelRepository.class).add(channel, clientId);
        MqttSession session = new MqttSession(clientId, false);
        Singleton.INST.get(SessionRepository.class).add(clientId, session);
        Awaitility.await().atMost(5, TimeUnit.SECONDS).untilAsserted(() ->
                assertEquals(clientId, Singleton.INST.get(ChannelRepository.class).get(channel)));

        new Subscribe().subscribe(ctx, MqttMessageBuilders.subscribe()
                .messageId(1)
                .addSubscription(MqttQoS.EXACTLY_ONCE, "test/qos")
                .build());
        new Subscribe().subscribe(ctx, MqttMessageBuilders.subscribe()
                .messageId(2)
                .addSubscription(MqttQoS.AT_MOST_ONCE, "test/qos")
                .build());

        MqttTopicSubscription subscription = subscription(session, "test/qos");
        assertEquals(MqttQoS.AT_MOST_ONCE, subscription.qualityOfService());
    }

    private MqttTopicSubscription subscription(final MqttSession session, final String topic) {
        return session.getTopicSubscriptions().stream()
                .filter(subscription -> topic.equals(subscription.topicName()))
                .findFirst()
                .orElseThrow(AssertionError::new);
    }
}
