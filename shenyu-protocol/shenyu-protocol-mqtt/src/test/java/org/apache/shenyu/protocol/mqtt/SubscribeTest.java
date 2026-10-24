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
import io.netty.handler.codec.mqtt.MqttFixedHeader;
import io.netty.handler.codec.mqtt.MqttMessageBuilders;
import io.netty.handler.codec.mqtt.MqttMessageIdVariableHeader;
import io.netty.handler.codec.mqtt.MqttMessageType;
import io.netty.handler.codec.mqtt.MqttPublishMessage;
import io.netty.handler.codec.mqtt.MqttQoS;
import io.netty.handler.codec.mqtt.MqttSubAckMessage;
import io.netty.handler.codec.mqtt.MqttSubscribeMessage;
import io.netty.handler.codec.mqtt.MqttSubscribePayload;
import io.netty.handler.codec.mqtt.MqttTopicSubscription;
import io.netty.util.CharsetUtil;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.protocol.mqtt.repositories.ChannelRepository;
import org.apache.shenyu.protocol.mqtt.repositories.RetainedMessage;
import org.apache.shenyu.protocol.mqtt.repositories.SessionRepository;
import org.apache.shenyu.protocol.mqtt.repositories.SubscribeRepository;
import org.apache.shenyu.protocol.mqtt.repositories.TopicRepository;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test case for {@link Subscribe}.
 */
class SubscribeTest {

    private static final String CLIENT_ID = "subscribe-test-client";

    private SubscribeRepository subscribeRepository;

    private TopicRepository topicRepository;

    private final Subscribe subscribe = new Subscribe();

    private final List<EmbeddedChannel> channels = new ArrayList<>();

    /**
     * SUBSCRIBE resolves the client session through these repositories, so they must be
     * present even when a test only exercises topic routing.
     */
    @BeforeEach
    void setUp() {
        subscribeRepository = new SubscribeRepository();
        topicRepository = new EmptyTopicRepository();
        Singleton.INST.single(ChannelRepository.class, new ChannelRepository());
        Singleton.INST.single(SessionRepository.class, new SessionRepository());
        Singleton.INST.single(SubscribeRepository.class, subscribeRepository);
        Singleton.INST.single(TopicRepository.class, topicRepository);
    }

    @AfterEach
    void tearDown() {
        SubscribeRepository repository = Singleton.INST.get(SubscribeRepository.class);
        if (Objects.nonNull(repository)) {
            channels.forEach(repository::remove);
        }
        channels.forEach(EmbeddedChannel::finishAndReleaseAll);
        channels.clear();
    }

    @Test
    void testSubscribeRejectsInvalidTopicFilter() throws InterruptedException {
        EmbeddedChannel channel = track(new EmbeddedChannel());
        channel.pipeline().addLast(new ChannelInboundHandlerAdapter());

        SubscribeRepository repository = new SubscribeRepository();
        Singleton.INST.single(SubscribeRepository.class, repository);
        Singleton.INST.single(TopicRepository.class, new TopicRepository());
        new MessageType().setConnected(channel, true);
        Singleton.INST.get(ChannelRepository.class).add(channel, CLIENT_ID);

        ChannelHandlerContext ctx = channel.pipeline().firstContext();
        MqttSubscribeMessage msg = MqttMessageBuilders.subscribe()
                .messageId(1)
                .addSubscription(MqttQoS.AT_MOST_ONCE, "sport/#")
                .addSubscription(MqttQoS.AT_MOST_ONCE, "bad#filter")
                .build();

        new Subscribe().subscribe(ctx, msg);
        assertTrue(repository.get("sport/#").containsKey(channel));
        assertTrue(repository.get("bad#filter").isEmpty());

        MqttSubAckMessage subAck = readSubAck(channel);
        List<Integer> granted = subAck.payload().grantedQoSLevels();
        assertEquals(2, granted.size());
        assertEquals(0, granted.get(0).intValue());
        assertEquals(0x80, granted.get(1).intValue());
    }

    @Test
    void testMultiLevelWildcardReceivesRetainedMessages() throws InterruptedException {
        EmbeddedChannel channel = track(new EmbeddedChannel());
        channel.pipeline().addLast(new ChannelInboundHandlerAdapter());

        Singleton.INST.single(SubscribeRepository.class, new SubscribeRepository());
        TopicRepository topicRepository = new TopicRepository();
        Singleton.INST.single(TopicRepository.class, topicRepository);
        new MessageType().setConnected(channel, true);
        Singleton.INST.get(ChannelRepository.class).add(channel, CLIENT_ID);

        topicRepository.add("retained-hash/tennis/player1", "retained-1".getBytes(CharsetUtil.UTF_8));
        topicRepository.add("retained-hash/tennis/player2", "retained-2".getBytes(CharsetUtil.UTF_8));
        topicRepository.add("retained-other/weather/today", "retained-3".getBytes(CharsetUtil.UTF_8));

        try {
            ChannelHandlerContext ctx = channel.pipeline().firstContext();
            MqttSubscribeMessage msg = MqttMessageBuilders.subscribe()
                    .messageId(11)
                    .addSubscription(MqttQoS.AT_MOST_ONCE, "retained-hash/#")
                    .build();

            new Subscribe().subscribe(ctx, msg);

            Map<String, String> retained = readRetainedMessages(channel, 2);
            assertEquals("retained-1", retained.get("retained-hash/tennis/player1"));
            assertEquals("retained-2", retained.get("retained-hash/tennis/player2"));
            readSubAck(channel);
        } finally {
            topicRepository.remove("retained-hash/tennis/player1");
            topicRepository.remove("retained-hash/tennis/player2");
            topicRepository.remove("retained-other/weather/today");
        }
    }

    @Test
    void testSingleLevelWildcardReceivesRetainedMessageOfOneLevel() throws InterruptedException {
        EmbeddedChannel channel = track(new EmbeddedChannel());
        channel.pipeline().addLast(new ChannelInboundHandlerAdapter());

        Singleton.INST.single(SubscribeRepository.class, new SubscribeRepository());
        TopicRepository topicRepository = new TopicRepository();
        Singleton.INST.single(TopicRepository.class, topicRepository);
        new MessageType().setConnected(channel, true);
        Singleton.INST.get(ChannelRepository.class).add(channel, CLIENT_ID);

        topicRepository.add("retained-plus/tennis", "retained-level".getBytes(CharsetUtil.UTF_8));
        topicRepository.add("retained-plus/tennis/player1", "retained-deeper".getBytes(CharsetUtil.UTF_8));

        try {
            ChannelHandlerContext ctx = channel.pipeline().firstContext();
            MqttSubscribeMessage msg = MqttMessageBuilders.subscribe()
                    .messageId(12)
                    .addSubscription(MqttQoS.AT_MOST_ONCE, "retained-plus/+")
                    .build();

            new Subscribe().subscribe(ctx, msg);

            Map<String, String> retained = readRetainedMessages(channel, 1);
            assertEquals("retained-level", retained.get("retained-plus/tennis"));
            readSubAck(channel);
        } finally {
            topicRepository.remove("retained-plus/tennis");
            topicRepository.remove("retained-plus/tennis/player1");
        }
    }

    @Test
    void testOverlappingFiltersReceiveRetainedMessageOnce() throws InterruptedException {
        EmbeddedChannel channel = track(new EmbeddedChannel());
        channel.pipeline().addLast(new ChannelInboundHandlerAdapter());

        Singleton.INST.single(SubscribeRepository.class, new SubscribeRepository());
        TopicRepository topicRepository = new TopicRepository();
        Singleton.INST.single(TopicRepository.class, topicRepository);
        new MessageType().setConnected(channel, true);
        Singleton.INST.get(ChannelRepository.class).add(channel, CLIENT_ID);

        topicRepository.add("retained-once/tennis", "retained-level".getBytes(CharsetUtil.UTF_8));

        try {
            ChannelHandlerContext ctx = channel.pipeline().firstContext();
            MqttSubscribeMessage msg = MqttMessageBuilders.subscribe()
                    .messageId(13)
                    .addSubscription(MqttQoS.AT_MOST_ONCE, "retained-once/#")
                    .addSubscription(MqttQoS.AT_MOST_ONCE, "retained-once/+")
                    .build();

            new Subscribe().subscribe(ctx, msg);

            Map<String, String> retained = readRetainedMessages(channel, 1);
            assertEquals("retained-level", retained.get("retained-once/tennis"));
            MqttSubAckMessage subAck = readSubAck(channel);
            assertEquals(2, subAck.payload().grantedQoSLevels().size());
        } finally {
            topicRepository.remove("retained-once/tennis");
        }
    }

    @Test
    void testSubAckGrantsRequestedQoS() {
        EmbeddedChannel channel = connectedChannel();
        MqttSubscribeMessage msg = subscribeMessage(10,
                new MqttTopicSubscription("topic/qos2", MqttQoS.EXACTLY_ONCE),
                new MqttTopicSubscription("topic/qos0", MqttQoS.AT_MOST_ONCE),
                new MqttTopicSubscription("topic/qos1", MqttQoS.AT_LEAST_ONCE));

        subscribe.subscribe(channel.pipeline().lastContext(), msg);

        MqttSubAckMessage subAck = readSubAck(channel);
        assertEquals(10, subAck.variableHeader().messageId());
        assertEquals(Arrays.asList(2, 0, 1), subAck.payload().grantedQoSLevels());
    }

    @Test
    void testSubAckReturnsFailureCodeForFailureSubscription() {
        EmbeddedChannel channel = connectedChannel();
        MqttSubscribeMessage msg = subscribeMessage(20,
                new MqttTopicSubscription("topic/qos1", MqttQoS.AT_LEAST_ONCE),
                new MqttTopicSubscription("topic/failure", MqttQoS.FAILURE),
                new MqttTopicSubscription("topic/qos0", MqttQoS.AT_MOST_ONCE));

        subscribe.subscribe(channel.pipeline().lastContext(), msg);

        MqttSubAckMessage subAck = readSubAck(channel);
        assertEquals(Arrays.asList(1, 0x80, 0), subAck.payload().grantedQoSLevels());
        assertTrue(subscribeRepository.get("topic/failure").isEmpty());
    }

    @Test
    void testSubscribeBeforeConnectClosesChannel() {
        EmbeddedChannel channel = track(new EmbeddedChannel(new ChannelInboundHandlerAdapter()));
        MqttSubscribeMessage msg = subscribeMessage(30,
                new MqttTopicSubscription("topic/qos0", MqttQoS.AT_MOST_ONCE));

        subscribe.subscribe(channel.pipeline().lastContext(), msg);

        channel.runPendingTasks();
        assertFalse(channel.isActive());
        assertNull(channel.readOutbound());
    }

    @Test
    void testRetainedReplayUsesMinQosAndPacketId() {
        EmbeddedChannel channel = connectedChannel();
        TopicRepository retainedRepository = new TopicRepository();
        Singleton.INST.single(TopicRepository.class, retainedRepository);
        retainedRepository.add("retained-qos/one", "retained-qos".getBytes(CharsetUtil.UTF_8), MqttQoS.AT_LEAST_ONCE);

        subscribe.subscribe(channel.pipeline().lastContext(), subscribeMessage(40,
                new MqttTopicSubscription("retained-qos/+", MqttQoS.EXACTLY_ONCE)));

        Object first = channel.readOutbound();
        assertTrue(first instanceof MqttPublishMessage);
        MqttPublishMessage publish = (MqttPublishMessage) first;
        assertEquals(MqttQoS.AT_LEAST_ONCE, publish.fixedHeader().qosLevel());
        assertTrue(publish.fixedHeader().isRetain());
        assertTrue(publish.variableHeader().packetId() > 0);
        publish.release();

        MqttSubAckMessage subAck = readSubAck(channel);
        assertEquals(Collections.singletonList(2), subAck.payload().grantedQoSLevels());
        retainedRepository.remove("retained-qos/one");
    }

    @Test
    void testRetainedReplayUsesQosZeroWithoutPacketId() {
        EmbeddedChannel channel = connectedChannel();
        TopicRepository retainedRepository = new TopicRepository();
        Singleton.INST.single(TopicRepository.class, retainedRepository);
        retainedRepository.add("retained-qos/zero", "retained-zero".getBytes(CharsetUtil.UTF_8), MqttQoS.EXACTLY_ONCE);

        subscribe.subscribe(channel.pipeline().lastContext(), subscribeMessage(41,
                new MqttTopicSubscription("retained-qos/zero", MqttQoS.AT_MOST_ONCE)));

        Object first = channel.readOutbound();
        assertTrue(first instanceof MqttPublishMessage);
        MqttPublishMessage publish = (MqttPublishMessage) first;
        assertEquals(MqttQoS.AT_MOST_ONCE, publish.fixedHeader().qosLevel());
        assertEquals(0, publish.variableHeader().packetId());
        publish.release();

        readSubAck(channel);
        retainedRepository.remove("retained-qos/zero");
    }

    private EmbeddedChannel track(final EmbeddedChannel channel) {
        channels.add(channel);
        return channel;
    }

    private EmbeddedChannel connectedChannel() {
        EmbeddedChannel channel = track(new EmbeddedChannel(new ChannelInboundHandlerAdapter()));
        new MessageType().setConnected(channel, true);
        Singleton.INST.get(ChannelRepository.class).add(channel, CLIENT_ID);
        return channel;
    }

    private MqttSubscribeMessage subscribeMessage(final int packetId, final MqttTopicSubscription... subscriptions) {
        MqttFixedHeader fixedHeader = new MqttFixedHeader(MqttMessageType.SUBSCRIBE, false, MqttQoS.AT_LEAST_ONCE, false, 0);
        MqttMessageIdVariableHeader variableHeader = MqttMessageIdVariableHeader.from(packetId);
        return new MqttSubscribeMessage(fixedHeader, variableHeader, new MqttSubscribePayload(Arrays.asList(subscriptions)));
    }

    private Map<String, String> readRetainedMessages(final EmbeddedChannel channel, final int expected) {
        Map<String, String> messages = new HashMap<>();
        for (int i = 0; i < expected; i++) {
            MqttPublishMessage publish = channel.readOutbound();
            assertNotNull(publish);
            assertTrue(publish.fixedHeader().isRetain());
            messages.put(publish.variableHeader().topicName(), publish.payload().toString(CharsetUtil.UTF_8));
            publish.release();
        }
        return messages;
    }

    private MqttSubAckMessage readSubAck(final EmbeddedChannel channel) {
        Object message = channel.readOutbound();
        assertTrue(message instanceof MqttSubAckMessage);
        assertNull(channel.readOutbound());
        return (MqttSubAckMessage) message;
    }

    private static final class EmptyTopicRepository extends TopicRepository {

        @Override
        public Map<String, RetainedMessage> getMatchingRetainedMessages(final String topicFilter) {
            return Collections.emptyMap();
        }
    }
}
