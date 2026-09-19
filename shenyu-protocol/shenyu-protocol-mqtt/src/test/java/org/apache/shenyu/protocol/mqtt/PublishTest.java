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
import io.netty.handler.codec.mqtt.MqttTopicSubscription;
import io.netty.handler.codec.mqtt.MqttVersion;
import io.netty.util.CharsetUtil;
import io.netty.util.ReferenceCountUtil;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.protocol.mqtt.repositories.ChannelRepository;
import org.apache.shenyu.protocol.mqtt.repositories.SubscribeRepository;
import org.apache.shenyu.protocol.mqtt.repositories.TopicRepository;
import org.apache.shenyu.protocol.mqtt.utils.MqttPacketIdGenerator;
import org.awaitility.core.ThrowingRunnable;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link Publish}.
 */
public final class PublishTest {

    private static final String RETAINED_TOPIC = "test/retained";

    private static final String NON_RETAINED_TOPIC = "test/non-retained";

    private static final String CLEARED_TOPIC = "test/cleared";

    private static final String UNCONNECTED_TOPIC = "test/unconnected";

    private static final String END_TO_END_TOPIC = "test/end-to-end";

    private static final String TOPIC = "test/topic";

    private static final List<String> ALL_TOPICS = Arrays.asList(
            RETAINED_TOPIC, NON_RETAINED_TOPIC, CLEARED_TOPIC, UNCONNECTED_TOPIC, END_TO_END_TOPIC, TOPIC);

    private static final String CLIENT_ID = "test-client";

    private static final String USER_NAME = "test-user";

    private static final String PASSWORD = "test-password";

    private static final String PAYLOAD = "hello";

    private static final int PUBLISHER_PACKET_ID = 12345;

    private static final Duration TIMEOUT = Duration.ofSeconds(5);

    private static final Duration POLL_INTERVAL = Duration.ofMillis(10);

    /**
     * The repositories hold their state in static maps shared with the other test classes of this module,
     * so they are released around every test.
     */
    private static final SubscribeRepository SUBSCRIBE_REPOSITORY = new SubscribeRepository();

    private static final TopicRepository TOPIC_REPOSITORY = new TopicRepository();

    private static final ChannelRepository CHANNEL_REPOSITORY = new ChannelRepository();

    private final Channel subscriberChannel = mock(Channel.class);

    private final Channel otherSubscriberChannel = mock(Channel.class);

    private EmbeddedChannel publisherChannel;

    @BeforeEach
    public void setUp() {
        Singleton.INST.single(SubscribeRepository.class, SUBSCRIBE_REPOSITORY);
        Singleton.INST.single(TopicRepository.class, TOPIC_REPOSITORY);
        Singleton.INST.single(ChannelRepository.class, CHANNEL_REPOSITORY);
        new MqttContext().setUserName(USER_NAME);
        new MqttContext().setPassword(PASSWORD);

        when(subscriberChannel.isActive()).thenReturn(true);
        when(otherSubscriberChannel.isActive()).thenReturn(true);

        clearSharedState();
        publisherChannel = channel(true);
    }

    @AfterEach
    public void tearDown() {
        MqttPacketIdGenerator.remove(subscriberChannel);
        MqttPacketIdGenerator.remove(otherSubscriberChannel);
        publisherChannel.finishAndReleaseAll();
        clearSharedState();

        new MqttContext().setUserName(null);
        new MqttContext().setPassword(null);
    }

    @Test
    public void retainedPublishStoresMessage() {
        new Publish().publish(publisherContext(), publishMessage(RETAINED_TOPIC, PAYLOAD, true));
        awaitAssert(() -> assertEquals(PAYLOAD, TOPIC_REPOSITORY.get(RETAINED_TOPIC)));
    }

    @Test
    public void nonRetainedPublishDoesNotStoreMessage() {
        new Publish().publish(publisherContext(), publishMessage(NON_RETAINED_TOPIC, PAYLOAD, false));
        assertNull(TOPIC_REPOSITORY.get(NON_RETAINED_TOPIC));
    }

    @Test
    public void retainedPublishReadsMessageFromDirectPayload() {
        ByteBuf payload = Unpooled.directBuffer().writeBytes(PAYLOAD.getBytes(StandardCharsets.UTF_8));
        try {
            new Publish().publish(publisherContext(), publishMessage(RETAINED_TOPIC, payload, true));
            awaitAssert(() -> assertEquals(PAYLOAD, TOPIC_REPOSITORY.get(RETAINED_TOPIC)));
        } finally {
            payload.release();
        }
    }

    @Test
    public void publishBeforeConnectClosesChannel() {
        EmbeddedChannel channel = channel(false);

        new Publish().publish(channel.pipeline().lastContext(), publishMessage(UNCONNECTED_TOPIC, PAYLOAD, true));
        channel.runPendingTasks();

        assertFalse(channel.isActive());
        assertNull(TOPIC_REPOSITORY.get(UNCONNECTED_TOPIC));
        channel.finishAndReleaseAll();
    }

    @Test
    public void publishAfterConnectOnSameChannelIsAccepted() {
        EmbeddedChannel channel = channel(false);
        ChannelHandlerContext ctx = channel.pipeline().lastContext();

        new Connect().connect(ctx, connectMessage());
        new Publish().publish(ctx, publishMessage(END_TO_END_TOPIC, PAYLOAD, true));

        awaitAssert(() -> assertEquals(PAYLOAD, TOPIC_REPOSITORY.get(END_TO_END_TOPIC)));
        assertEquals(CLIENT_ID, CHANNEL_REPOSITORY.get(channel));

        CHANNEL_REPOSITORY.remove(channel);
        channel.finishAndReleaseAll();
    }

    @Test
    public void zeroByteRetainedPublishClearsRetainedMessage() {
        Publish publish = new Publish();
        ChannelHandlerContext ctx = publisherContext();

        publish.publish(ctx, publishMessage(CLEARED_TOPIC, PAYLOAD, true));
        awaitAssert(() -> assertEquals(PAYLOAD, TOPIC_REPOSITORY.get(CLEARED_TOPIC)));

        publish.publish(ctx, publishMessage(CLEARED_TOPIC, "", true));
        assertNull(TOPIC_REPOSITORY.get(CLEARED_TOPIC));
    }

    @Test
    public void testPublishDeliversAtGrantedQosWithOwnPacketId() {
        addSubscriber(subscriberChannel, MqttQoS.AT_LEAST_ONCE);
        publishToSubscribers(MqttQoS.EXACTLY_ONCE);

        MqttPublishMessage message = captureMessage(subscriberChannel);
        assertEquals(MqttQoS.AT_LEAST_ONCE, message.fixedHeader().qosLevel());
        assertEquals(1, message.variableHeader().packetId());
        assertEquals(TOPIC, message.variableHeader().topicName());
        assertEquals(PAYLOAD, message.payload().toString(CharsetUtil.UTF_8));
    }

    @Test
    public void testPublishDeliversQos0SubscriberWithZeroPacketId() {
        addSubscriber(subscriberChannel, MqttQoS.AT_MOST_ONCE);
        publishToSubscribers(MqttQoS.EXACTLY_ONCE);

        MqttPublishMessage message = captureMessage(subscriberChannel);
        assertEquals(MqttQoS.AT_MOST_ONCE, message.fixedHeader().qosLevel());
        assertEquals(0, message.variableHeader().packetId());
    }

    @Test
    public void testPublishQos0FanOutDeliversAtMostOnce() {
        addSubscriber(subscriberChannel, MqttQoS.EXACTLY_ONCE);
        publishToSubscribers(MqttQoS.AT_MOST_ONCE);

        MqttPublishMessage message = captureMessage(subscriberChannel);
        assertEquals(MqttQoS.AT_MOST_ONCE, message.fixedHeader().qosLevel());
        assertEquals(0, message.variableHeader().packetId());
    }

    @Test
    public void testPublishSkipsInactiveSubscriber() {
        addSubscriber(subscriberChannel, MqttQoS.AT_LEAST_ONCE);
        addSubscriber(otherSubscriberChannel, MqttQoS.AT_LEAST_ONCE);
        when(subscriberChannel.isActive()).thenReturn(false);

        publishToSubscribers(MqttQoS.AT_LEAST_ONCE);

        assertEquals(MqttQoS.AT_LEAST_ONCE, captureMessage(otherSubscriberChannel).fixedHeader().qosLevel());
        verify(subscriberChannel, never()).writeAndFlush(any(MqttPublishMessage.class));
    }

    @Test
    public void testPublishAllocatesPacketIdFromSubscriberIdSpace() {
        addSubscriber(subscriberChannel, MqttQoS.EXACTLY_ONCE);
        addSubscriber(otherSubscriberChannel, MqttQoS.EXACTLY_ONCE);

        publishToSubscribers(MqttQoS.EXACTLY_ONCE);
        publishToSubscribers(MqttQoS.EXACTLY_ONCE);

        List<MqttPublishMessage> messages = captureMessages(subscriberChannel, 2);
        assertEquals(1, messages.get(0).variableHeader().packetId());
        assertEquals(2, messages.get(1).variableHeader().packetId());

        List<MqttPublishMessage> otherMessages = captureMessages(otherSubscriberChannel, 2);
        assertEquals(1, otherMessages.get(0).variableHeader().packetId());
        assertEquals(2, otherMessages.get(1).variableHeader().packetId());
    }

    @Test
    public void testPublishFanOutRetainsPayloadPerSubscriber() {
        addSubscriber(subscriberChannel, MqttQoS.AT_LEAST_ONCE);
        addSubscriber(otherSubscriberChannel, MqttQoS.AT_LEAST_ONCE);
        ByteBuf payload = Unpooled.copiedBuffer(PAYLOAD, CharsetUtil.UTF_8);
        try {
            publishToSubscribers(MqttQoS.AT_LEAST_ONCE, payload);
            awaitAssert(() -> assertEquals(3, payload.refCnt()));

            MqttPublishMessage delivered = captureMessage(subscriberChannel);
            MqttPublishMessage otherDelivered = captureMessage(otherSubscriberChannel);
            assertEquals(PAYLOAD, delivered.payload().toString(CharsetUtil.UTF_8));
            assertEquals(PAYLOAD, otherDelivered.payload().toString(CharsetUtil.UTF_8));

            ReferenceCountUtil.release(delivered);
            ReferenceCountUtil.release(otherDelivered);
            assertEquals(1, payload.refCnt());
        } finally {
            ReferenceCountUtil.release(payload);
        }
    }

    @Test
    public void testPublishQos1SendsPubAckToPublisher() {
        publishToSubscribers(MqttQoS.AT_LEAST_ONCE);

        MqttPubAckMessage pubAck = awaitOutbound(publisherChannel);
        assertEquals(PUBLISHER_PACKET_ID, pubAck.variableHeader().messageId());
        assertEquals(MqttQoS.AT_MOST_ONCE, pubAck.fixedHeader().qosLevel());
    }

    @Test
    public void testPublishQos2SendsPubRecToPublisher() {
        publishToSubscribers(MqttQoS.EXACTLY_ONCE);

        MqttMessage pubRec = awaitOutbound(publisherChannel);
        assertEquals(MqttMessageType.PUBREC, pubRec.fixedHeader().messageType());
        assertEquals(MqttQoS.AT_MOST_ONCE, pubRec.fixedHeader().qosLevel());
        assertEquals(PUBLISHER_PACKET_ID, ((MqttMessageIdVariableHeader) pubRec.variableHeader()).messageId());
    }

    /**
     * Creates a real channel, optionally already connected, so that {@link Publish} can
     * read the connection attribute and close the channel as it does in production.
     *
     * @param connected whether the channel completed the CONNECT handshake
     * @return the channel
     */
    private EmbeddedChannel channel(final boolean connected) {
        EmbeddedChannel channel = new EmbeddedChannel(new ChannelInboundHandlerAdapter());
        if (connected) {
            new MessageType().setConnected(channel, true);
        }
        return channel;
    }

    private ChannelHandlerContext publisherContext() {
        return publisherChannel.pipeline().lastContext();
    }

    private void addSubscriber(final Channel channel, final MqttQoS qos) {
        SUBSCRIBE_REPOSITORY.add(channel, Collections.singletonList(new MqttTopicSubscription(TOPIC, qos)));
        awaitAssert(() -> assertEquals(qos, SUBSCRIBE_REPOSITORY.get(TOPIC).get(channel)));
    }

    private void publishToSubscribers(final MqttQoS qos) {
        publishToSubscribers(qos, Unpooled.copiedBuffer(PAYLOAD, CharsetUtil.UTF_8));
    }

    private void publishToSubscribers(final MqttQoS qos, final ByteBuf payload) {
        MqttFixedHeader fixedHeader = new MqttFixedHeader(MqttMessageType.PUBLISH, false, qos, false, 0);
        MqttPublishVariableHeader variableHeader = new MqttPublishVariableHeader(TOPIC, PUBLISHER_PACKET_ID);
        new Publish().publish(publisherContext(), new MqttPublishMessage(fixedHeader, variableHeader, payload));
    }

    private MqttPublishMessage captureMessage(final Channel channel) {
        return captureMessages(channel, 1).get(0);
    }

    private List<MqttPublishMessage> captureMessages(final Channel channel, final int times) {
        ArgumentCaptor<MqttPublishMessage> captor = ArgumentCaptor.forClass(MqttPublishMessage.class);
        verify(channel, timeout(TIMEOUT.toMillis()).times(times)).writeAndFlush(captor.capture());
        return captor.getAllValues();
    }

    /**
     * Polls the messages written back to the publisher, such as PUBACK and PUBREC.
     *
     * @param channel the publisher channel
     * @param <T> the expected message type
     * @return the first outbound message
     */
    private <T> T awaitOutbound(final EmbeddedChannel channel) {
        channel.runPendingTasks();
        awaitAssert(() -> assertFalse(channel.outboundMessages().isEmpty()));
        return channel.readOutbound();
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
        return publishMessage(topic, Unpooled.copiedBuffer(payload, CharsetUtil.UTF_8), retain);
    }

    private MqttPublishMessage publishMessage(final String topic, final ByteBuf payload, final boolean retain) {
        MqttFixedHeader fixedHeader = new MqttFixedHeader(MqttMessageType.PUBLISH, false, MqttQoS.AT_MOST_ONCE, retain, 0);
        MqttPublishVariableHeader variableHeader = new MqttPublishVariableHeader(topic, 1);
        return new MqttPublishMessage(fixedHeader, variableHeader, payload);
    }

    /**
     * Subscriptions are registered asynchronously on the common pool,
     * so assertions are retried until the mutation becomes visible.
     *
     * @param assertion assertion to retry
     */
    private void awaitAssert(final ThrowingRunnable assertion) {
        await().atMost(TIMEOUT).pollInterval(POLL_INTERVAL).untilAsserted(assertion);
    }

    private void clearSharedState() {
        ALL_TOPICS.forEach(TOPIC_REPOSITORY::remove);
        SUBSCRIBE_REPOSITORY.remove(ALL_TOPICS);
        awaitAssert(() -> ALL_TOPICS.forEach(topic -> assertTrue(SUBSCRIBE_REPOSITORY.get(topic).isEmpty())));
    }
}
