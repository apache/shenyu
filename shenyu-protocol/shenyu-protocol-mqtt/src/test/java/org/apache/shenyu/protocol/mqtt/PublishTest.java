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
import io.netty.handler.codec.mqtt.MqttMessageType;
import io.netty.handler.codec.mqtt.MqttPublishMessage;
import io.netty.handler.codec.mqtt.MqttPublishVariableHeader;
import io.netty.handler.codec.mqtt.MqttQoS;
import io.netty.handler.codec.mqtt.MqttVersion;
import io.netty.util.CharsetUtil;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.protocol.mqtt.repositories.ChannelRepository;
import org.apache.shenyu.protocol.mqtt.repositories.SubscribeRepository;
import org.apache.shenyu.protocol.mqtt.repositories.TopicRepository;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.time.Duration;

import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * Test cases for {@link Publish}.
 */
public final class PublishTest {

    private static final String RETAINED_TOPIC = "test/retained";

    private static final String NON_RETAINED_TOPIC = "test/non-retained";

    private static final String CLEARED_TOPIC = "test/cleared";

    private static final String UNCONNECTED_TOPIC = "test/unconnected";

    private static final String END_TO_END_TOPIC = "test/end-to-end";

    private static final String CLIENT_ID = "test-client";

    private static final String USER_NAME = "test-user";

    private static final String PASSWORD = "test-password";

    private static TopicRepository topicRepository;

    @BeforeAll
    static void setUp() {
        topicRepository = new TopicRepository();
        Singleton.INST.single(TopicRepository.class, topicRepository);
        Singleton.INST.single(SubscribeRepository.class, new SubscribeRepository());
        Singleton.INST.single(ChannelRepository.class, new ChannelRepository());
        new MqttContext().setUserName(USER_NAME);
        new MqttContext().setPassword(PASSWORD);
    }

    @AfterAll
    static void tearDown() {
        new MqttContext().setUserName(null);
        new MqttContext().setPassword(null);
    }

    @Test
    public void retainedPublishStoresMessage() {
        new Publish().publish(connectedContext(), publishMessage(RETAINED_TOPIC, "hello", true));
        await().atMost(Duration.ofSeconds(5))
                .until(() -> "hello".equals(topicRepository.get(RETAINED_TOPIC)));
    }

    @Test
    public void nonRetainedPublishDoesNotStoreMessage() {
        new Publish().publish(connectedContext(), publishMessage(NON_RETAINED_TOPIC, "hello", false));
        assertNull(topicRepository.get(NON_RETAINED_TOPIC));
    }

    @Test
    public void publishBeforeConnectClosesChannel() {
        EmbeddedChannel channel = new EmbeddedChannel(new ChannelInboundHandlerAdapter());
        ChannelHandlerContext ctx = channel.pipeline().lastContext();

        new Publish().publish(ctx, publishMessage(UNCONNECTED_TOPIC, "hello", true));

        channel.runPendingTasks();
        assertFalse(channel.isActive());
        assertNull(topicRepository.get(UNCONNECTED_TOPIC));
    }

    @Test
    public void publishAfterConnectOnSameChannelIsAccepted() {
        EmbeddedChannel channel = new EmbeddedChannel(new ChannelInboundHandlerAdapter());
        ChannelHandlerContext ctx = channel.pipeline().lastContext();

        new Connect().connect(ctx, connectMessage());
        new Publish().publish(ctx, publishMessage(END_TO_END_TOPIC, "hello", true));

        await().atMost(Duration.ofSeconds(5))
                .until(() -> "hello".equals(topicRepository.get(END_TO_END_TOPIC)));
    }

    @Test
    public void zeroByteRetainedPublishClearsRetainedMessage() {
        Publish publish = new Publish();
        publish.publish(connectedContext(), publishMessage(CLEARED_TOPIC, "hello", true));
        await().atMost(Duration.ofSeconds(5))
                .until(() -> "hello".equals(topicRepository.get(CLEARED_TOPIC)));
        publish.publish(connectedContext(), publishMessage(CLEARED_TOPIC, "", true));
        assertNull(topicRepository.get(CLEARED_TOPIC));
    }

    private ChannelHandlerContext connectedContext() {
        EmbeddedChannel channel = new EmbeddedChannel(new ChannelInboundHandlerAdapter());
        new MessageType().setConnected(channel, true);
        return channel.pipeline().lastContext();
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

    private MqttPublishMessage publishMessage(final String topic, final String payload, final boolean retain) {
        MqttFixedHeader fixedHeader = new MqttFixedHeader(MqttMessageType.PUBLISH, false, MqttQoS.AT_MOST_ONCE, retain, 0);
        MqttPublishVariableHeader variableHeader = new MqttPublishVariableHeader(topic, 1);
        return new MqttPublishMessage(fixedHeader, variableHeader, Unpooled.copiedBuffer(payload, CharsetUtil.UTF_8));
    }
}
