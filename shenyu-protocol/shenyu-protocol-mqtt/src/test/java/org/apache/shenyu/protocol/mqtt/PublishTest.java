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

import io.netty.buffer.ByteBuf;
import io.netty.buffer.Unpooled;
import io.netty.channel.Channel;
import io.netty.channel.ChannelHandlerContext;
import io.netty.handler.codec.mqtt.MqttFixedHeader;
import io.netty.handler.codec.mqtt.MqttMessage;
import io.netty.handler.codec.mqtt.MqttMessageIdVariableHeader;
import io.netty.handler.codec.mqtt.MqttMessageType;
import io.netty.handler.codec.mqtt.MqttPubAckMessage;
import io.netty.handler.codec.mqtt.MqttPublishMessage;
import io.netty.handler.codec.mqtt.MqttPublishVariableHeader;
import io.netty.handler.codec.mqtt.MqttQoS;
import io.netty.handler.codec.mqtt.MqttTopicSubscription;
import io.netty.util.CharsetUtil;
import io.netty.util.ReferenceCountUtil;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.protocol.mqtt.repositories.SubscribeRepository;
import org.apache.shenyu.protocol.mqtt.repositories.TopicRepository;
import org.apache.shenyu.protocol.mqtt.utils.MqttPacketIdGenerator;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import java.util.Collections;
import java.util.concurrent.TimeUnit;

import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test cases for Publish.
 */
public class PublishTest {

    private static final String TOPIC = "test/topic";

    private static final String PAYLOAD = "hello";

    private static final int PUBLISHER_PACKET_ID = 12345;

    private final SubscribeRepository subscribeRepository = new SubscribeRepository();

    private final ChannelHandlerContext ctx = mock(ChannelHandlerContext.class);

    private final Channel subscriberChannel = mock(Channel.class);

    @BeforeEach
    public void setUp() {
        Singleton.INST.single(SubscribeRepository.class, subscribeRepository);
        Singleton.INST.single(TopicRepository.class, new TopicRepository());
        when(subscriberChannel.isActive()).thenReturn(true);
    }

    @AfterEach
    public void tearDown() {
        MqttPacketIdGenerator.remove(subscriberChannel);
        subscribeRepository.remove(Collections.singletonList(TOPIC));
        await().atMost(5, TimeUnit.SECONDS).untilAsserted(() -> assertTrue(subscribeRepository.get(TOPIC).isEmpty()));
    }

    @Test
    public void testPublishDeliversAtGrantedQosWithOwnPacketId() {
        addSubscriber(subscriberChannel, MqttQoS.AT_LEAST_ONCE);
        publish(MqttQoS.EXACTLY_ONCE);

        MqttPublishMessage message = captureMessage(subscriberChannel);
        assertEquals(MqttQoS.AT_LEAST_ONCE, message.fixedHeader().qosLevel());
        assertEquals(1, message.variableHeader().packetId());
        assertEquals(TOPIC, message.variableHeader().topicName());
        assertEquals(PAYLOAD, message.payload().toString(CharsetUtil.UTF_8));
    }

    @Test
    public void testPublishDeliversQos0SubscriberWithZeroPacketId() {
        addSubscriber(subscriberChannel, MqttQoS.AT_MOST_ONCE);
        publish(MqttQoS.EXACTLY_ONCE);

        MqttPublishMessage message = captureMessage(subscriberChannel);
        assertEquals(MqttQoS.AT_MOST_ONCE, message.fixedHeader().qosLevel());
        assertEquals(0, message.variableHeader().packetId());
    }

    @Test
    public void testPublishQos0FanOutDeliversAtMostOnce() {
        addSubscriber(subscriberChannel, MqttQoS.EXACTLY_ONCE);
        publish(MqttQoS.AT_MOST_ONCE);

        MqttPublishMessage message = captureMessage(subscriberChannel);
        assertEquals(MqttQoS.AT_MOST_ONCE, message.fixedHeader().qosLevel());
        assertEquals(0, message.variableHeader().packetId());
    }

    @Test
    public void testPublishAllocatesPacketIdFromSubscriberIdSpace() {
        Channel otherSubscriberChannel = mock(Channel.class);
        when(otherSubscriberChannel.isActive()).thenReturn(true);
        addSubscriber(subscriberChannel, MqttQoS.EXACTLY_ONCE);
        addSubscriber(otherSubscriberChannel, MqttQoS.EXACTLY_ONCE);
        try {
            publish(MqttQoS.EXACTLY_ONCE);

            ArgumentCaptor<MqttPublishMessage> captor = ArgumentCaptor.forClass(MqttPublishMessage.class);
            verify(subscriberChannel, timeout(5000)).writeAndFlush(captor.capture());
            verify(otherSubscriberChannel, timeout(5000)).writeAndFlush(captor.capture());
            assertEquals(1, captor.getAllValues().get(0).variableHeader().packetId());
            assertEquals(1, captor.getAllValues().get(1).variableHeader().packetId());

            publish(MqttQoS.EXACTLY_ONCE);
            ArgumentCaptor<MqttPublishMessage> secondCaptor = ArgumentCaptor.forClass(MqttPublishMessage.class);
            verify(subscriberChannel, timeout(5000).times(2)).writeAndFlush(secondCaptor.capture());
            assertEquals(1, secondCaptor.getAllValues().get(0).variableHeader().packetId());
            assertEquals(2, secondCaptor.getAllValues().get(1).variableHeader().packetId());
        } finally {
            MqttPacketIdGenerator.remove(otherSubscriberChannel);
        }
    }

    @Test
    public void testPublishFanOutRetainsPayloadPerSubscriber() {
        Channel otherSubscriberChannel = mock(Channel.class);
        when(otherSubscriberChannel.isActive()).thenReturn(true);
        addSubscriber(subscriberChannel, MqttQoS.AT_LEAST_ONCE);
        addSubscriber(otherSubscriberChannel, MqttQoS.AT_LEAST_ONCE);
        ByteBuf payload = Unpooled.copiedBuffer(PAYLOAD, CharsetUtil.UTF_8);
        try {
            publish(MqttQoS.AT_LEAST_ONCE, payload);
            await().atMost(5, TimeUnit.SECONDS).untilAsserted(() -> assertEquals(3, payload.refCnt()));

            ArgumentCaptor<MqttPublishMessage> captor = ArgumentCaptor.forClass(MqttPublishMessage.class);
            verify(subscriberChannel, timeout(5000)).writeAndFlush(captor.capture());
            verify(otherSubscriberChannel, timeout(5000)).writeAndFlush(captor.capture());
            assertEquals(PAYLOAD, captor.getAllValues().get(0).payload().toString(CharsetUtil.UTF_8));
            captor.getAllValues().forEach(ReferenceCountUtil::release);
            assertEquals(1, payload.refCnt());
        } finally {
            ReferenceCountUtil.release(payload);
            MqttPacketIdGenerator.remove(otherSubscriberChannel);
        }
    }

    @Test
    public void testPublishQos1SendsPubAckToPublisher() {
        publish(MqttQoS.AT_LEAST_ONCE);

        ArgumentCaptor<MqttPubAckMessage> captor = ArgumentCaptor.forClass(MqttPubAckMessage.class);
        verify(ctx, timeout(5000)).writeAndFlush(captor.capture());
        assertEquals(PUBLISHER_PACKET_ID, captor.getValue().variableHeader().messageId());
        assertEquals(MqttQoS.AT_MOST_ONCE, captor.getValue().fixedHeader().qosLevel());
    }

    @Test
    public void testPublishQos2SendsPubRecToPublisher() {
        publish(MqttQoS.EXACTLY_ONCE);

        ArgumentCaptor<MqttMessage> captor = ArgumentCaptor.forClass(MqttMessage.class);
        verify(ctx, timeout(5000)).writeAndFlush(captor.capture());
        MqttMessage pubRec = captor.getValue();
        assertEquals(MqttMessageType.PUBREC, pubRec.fixedHeader().messageType());
        assertEquals(MqttQoS.AT_MOST_ONCE, pubRec.fixedHeader().qosLevel());
        assertEquals(PUBLISHER_PACKET_ID, ((MqttMessageIdVariableHeader) pubRec.variableHeader()).messageId());
    }

    private void addSubscriber(final Channel channel, final MqttQoS qos) {
        subscribeRepository.add(channel, Collections.singletonList(new MqttTopicSubscription(TOPIC, qos)));
        await().atMost(5, TimeUnit.SECONDS).untilAsserted(() -> assertEquals(qos, subscribeRepository.get(TOPIC).get(channel)));
    }

    private void publish(final MqttQoS qos) {
        publish(qos, Unpooled.copiedBuffer(PAYLOAD, CharsetUtil.UTF_8));
    }

    private void publish(final MqttQoS qos, final ByteBuf payload) {
        MqttFixedHeader fixedHeader = new MqttFixedHeader(MqttMessageType.PUBLISH, false, qos, false, 0);
        MqttPublishVariableHeader variableHeader = new MqttPublishVariableHeader(TOPIC, PUBLISHER_PACKET_ID);
        MqttPublishMessage message = new MqttPublishMessage(fixedHeader, variableHeader, payload);
        new Publish().publish(ctx, message);
    }

    private MqttPublishMessage captureMessage(final Channel channel) {
        ArgumentCaptor<MqttPublishMessage> captor = ArgumentCaptor.forClass(MqttPublishMessage.class);
        verify(channel, timeout(5000)).writeAndFlush(captor.capture());
        return captor.getValue();
    }

}
