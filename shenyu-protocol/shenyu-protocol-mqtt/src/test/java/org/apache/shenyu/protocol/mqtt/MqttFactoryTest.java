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

import io.netty.buffer.Unpooled;
import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelInboundHandlerAdapter;
import io.netty.channel.embedded.EmbeddedChannel;
import io.netty.handler.codec.mqtt.MqttConnectMessage;
import io.netty.handler.codec.mqtt.MqttConnectPayload;
import io.netty.handler.codec.mqtt.MqttConnectVariableHeader;
import io.netty.handler.codec.mqtt.MqttFixedHeader;
import io.netty.handler.codec.mqtt.MqttMessage;
import io.netty.handler.codec.mqtt.MqttMessageIdVariableHeader;
import io.netty.handler.codec.mqtt.MqttMessageType;
import io.netty.handler.codec.mqtt.MqttPubAckMessage;
import io.netty.handler.codec.mqtt.MqttPublishMessage;
import io.netty.handler.codec.mqtt.MqttPublishVariableHeader;
import io.netty.handler.codec.mqtt.MqttQoS;
import io.netty.handler.codec.mqtt.MqttSubscribeMessage;
import io.netty.handler.codec.mqtt.MqttSubscribePayload;
import io.netty.handler.codec.mqtt.MqttTopicSubscription;
import io.netty.handler.codec.mqtt.MqttUnsubscribeMessage;
import io.netty.handler.codec.mqtt.MqttUnsubscribePayload;
import io.netty.handler.codec.mqtt.MqttVersion;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.protocol.mqtt.repositories.ChannelRepository;
import org.apache.shenyu.protocol.mqtt.repositories.WillRepository;
import org.apache.shenyu.protocol.mqtt.repositories.WillRepository.WillEntry;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for {@link MqttFactory}.
 */
public final class MqttFactoryTest {

    private static final String CLIENT_ID = "factory-client";

    private static final String USER_NAME = "factory-user";

    private static final String PASSWORD = "factory-password";

    private ChannelRepository channelRepository;

    private WillRepository willRepository;

    @BeforeEach
    public void setUp() {
        channelRepository = new ChannelRepository();
        willRepository = new WillRepository();
        Singleton.INST.single(ChannelRepository.class, channelRepository);
        Singleton.INST.single(WillRepository.class, willRepository);
        new MqttContext().setUserName(USER_NAME);
        new MqttContext().setPassword(PASSWORD);
    }

    @AfterEach
    public void tearDown() {
        Singleton.INST.single(ChannelRepository.class, new ChannelRepository());
        Singleton.INST.single(WillRepository.class, new WillRepository());
        new MqttContext().setUserName(null);
        new MqttContext().setPassword(null);
    }

    @Test
    public void messageWithoutFixedHeaderShouldBeIgnored() {
        EmbeddedChannel channel = new EmbeddedChannel(new ChannelInboundHandlerAdapter());
        ChannelHandlerContext ctx = channel.pipeline().lastContext();

        new MqttFactory(new MqttMessage(null, null), ctx).connect();

        assertTrue(channel.isActive());
        channel.finishAndReleaseAll();
    }

    @Test
    public void connectShouldBeDispatchedToConnectHandler() {
        EmbeddedChannel channel = new EmbeddedChannel(new ChannelInboundHandlerAdapter());
        ChannelHandlerContext ctx = channel.pipeline().lastContext();

        new MqttFactory(connectMessage(), ctx).connect();

        assertNotNull(channel.readOutbound());
        assertTrue(channel.isActive());
        channel.finishAndReleaseAll();
    }

    @Test
    public void publishBeforeConnectShouldBeDispatchedAndCloseChannel() {
        MqttPublishMessage publish = new MqttPublishMessage(
                fixedHeader(MqttMessageType.PUBLISH),
                new MqttPublishVariableHeader("topic", 0),
                Unpooled.EMPTY_BUFFER);

        assertDispatchedMessageClosesChannel(publish);
    }

    @Test
    public void subscribeBeforeConnectShouldBeDispatchedAndCloseChannel() {
        MqttSubscribeMessage subscribe = new MqttSubscribeMessage(
                fixedHeader(MqttMessageType.SUBSCRIBE),
                MqttMessageIdVariableHeader.from(1),
                new MqttSubscribePayload(Collections.singletonList(
                        new MqttTopicSubscription("topic", MqttQoS.AT_MOST_ONCE))));

        assertDispatchedMessageClosesChannel(subscribe);
    }

    @Test
    public void unsubscribeBeforeConnectShouldBeDispatchedAndCloseChannel() {
        MqttUnsubscribeMessage unsubscribe = new MqttUnsubscribeMessage(
                fixedHeader(MqttMessageType.UNSUBSCRIBE),
                MqttMessageIdVariableHeader.from(1),
                new MqttUnsubscribePayload(Collections.singletonList("topic")));

        assertDispatchedMessageClosesChannel(unsubscribe);
    }

    @Test
    public void pingReqBeforeConnectShouldBeDispatchedAndCloseChannel() {
        assertDispatchedMessageClosesChannel(new MqttMessage(fixedHeader(MqttMessageType.PINGREQ)));
    }

    @Test
    public void pubAckShouldFallThroughToNoOp() {
        EmbeddedChannel channel = new EmbeddedChannel(new ChannelInboundHandlerAdapter());
        ChannelHandlerContext ctx = channel.pipeline().lastContext();

        MqttPubAckMessage pubAck = new MqttPubAckMessage(
                new MqttFixedHeader(MqttMessageType.PUBACK, false, MqttQoS.AT_LEAST_ONCE, false, 0),
                MqttMessageIdVariableHeader.from(1));
        new MqttFactory(pubAck, ctx).connect();

        assertTrue(channel.isActive());
        channel.finishAndReleaseAll();
    }

    @Test
    public void disconnectShouldBeDispatchedAndClearConnectionState() {
        EmbeddedChannel channel = new EmbeddedChannel(new ChannelInboundHandlerAdapter());
        ChannelHandlerContext ctx = channel.pipeline().lastContext();
        channelRepository.add(channel, CLIENT_ID);
        willRepository.add(channel, new WillEntry("status/will", "goodbye".getBytes(StandardCharsets.UTF_8), 1, true));

        new MqttFactory(new MqttMessage(fixedHeader(MqttMessageType.DISCONNECT)), ctx).connect();
        channel.runPendingTasks();

        assertFalse(channel.isActive());
        assertNull(channelRepository.get(channel));
        assertNull(willRepository.get(channel));
        channel.finishAndReleaseAll();
    }

    private void assertDispatchedMessageClosesChannel(final MqttMessage message) {
        EmbeddedChannel channel = new EmbeddedChannel(new ChannelInboundHandlerAdapter());
        ChannelHandlerContext ctx = channel.pipeline().lastContext();

        new MqttFactory(message, ctx).connect();
        channel.runPendingTasks();

        assertFalse(channel.isActive());
        channel.finishAndReleaseAll();
    }

    private MqttFixedHeader fixedHeader(final MqttMessageType messageType) {
        return new MqttFixedHeader(messageType, false, MqttQoS.AT_MOST_ONCE, false, 0);
    }

    private MqttConnectMessage connectMessage() {
        MqttFixedHeader fixedHeader = new MqttFixedHeader(MqttMessageType.CONNECT, false, MqttQoS.AT_MOST_ONCE, false, 0);
        MqttConnectVariableHeader variableHeader = new MqttConnectVariableHeader(
                MqttVersion.MQTT_3_1_1.protocolName(), MqttVersion.MQTT_3_1_1.protocolLevel(),
                true, true, false, 0, false, false, 60);
        MqttConnectPayload payload = new MqttConnectPayload(CLIENT_ID, null, null,
                USER_NAME, PASSWORD.getBytes(StandardCharsets.UTF_8));
        return new MqttConnectMessage(fixedHeader, variableHeader, payload);
    }
}
